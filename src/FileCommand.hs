{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module FileCommand (s) where
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Control.Applicative
import Control.Monad
import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.Process
import Filesystem.Path hiding (null, concat)
import Filesystem.Path.CurrentOS (encodeString)
import Data.Maybe
import qualified Data.Text as T

type Parser = Parsec String ()

data FilePart 
  = Path     
  | Root     
  | Directory
  | Parent   
  | FileName 
  | DirName  
  | BaseName 
  | Ext      
  deriving (Show, Eq)
  
pFilePart :: Parser FilePart
pFilePart = char '$' *> pFilePart'
  
pFilePart' :: Parser FilePart
pFilePart' 
   =  Path      <$ string "path"
  <|> Root      <$ string "root"
  <|> Directory <$ string "directory"
  <|> Parent    <$ string "parent"
  <|> FileName  <$ string "filename"
  <|> DirName   <$ string "dirname"
  <|> BaseName  <$ string "basename"
  <|> Ext       <$ string "ext"
      
type Fragment = String

pFragment :: Parser Fragment
pFragment = do 
  frag <- many $ noneOf "\\$"
  xs <- option [] $ try $ do 
    slash <- char '\\'
    c     <- anyChar
    return $ [slash, c]
  case xs of 
    [] 
     | null frag -> fail "empty cmd fragment"
     | otherwise -> return frag
    "\\$" -> return $ frag ++ "$"
    xs'   -> return $ frag ++ xs'

data Expr 
  = EFragment      Fragment
  | EFilePart FilePart
  deriving (Show, Eq)

pExpr :: Parser Expr
pExpr = EFilePart <$> pFilePart <|> EFragment <$> pFragment 

parser :: Parser [Expr]
parser = many1 pExpr

eval :: [Expr] -> Q Exp
eval xs = do 
  filePathName <- newName "filePath"
  let qs = map (evalExpr filePathName) xs
  let body = [|system $ concat $ $(listE qs) |]
  lamE [varP filePathName] body

evalExpr :: Name -> Expr -> Q Exp
evalExpr filePathName = \case
  EFragment      frag     -> [| $(stringE frag) |]
  EFilePart filePart -> evalFilePart filePathName filePart
  
evalFilePart :: Name -> FilePart -> Q Exp 
evalFilePart filePathName = \case
  Path         -> [| encodeString $(varE filePathName) |]
  Root         -> [| encodeString $ root      $(varE filePathName) |]
  Directory    -> [| encodeString $ directory $(varE filePathName) |]
  Parent       -> [| encodeString $ parent    $(varE filePathName) |]
  FileName     -> [| encodeString $ filename  $(varE filePathName) |]
  DirName      -> [| encodeString $ dirname   $(varE filePathName) |]
  BaseName     -> [| encodeString $ basename  $(varE filePathName) |]
  Ext          -> [| T.unpack 
                   $ fromMaybe 
                      (error 
                      $ "Failed to get extension for " ++ encodeString $(varE filePathName)
                      )
                      $ extension $(varE filePathName) 
                   |]


-- | A simple quasiquoter for executing system commands on a filepath
--  for example
--  
--  >> [s|echo $filename|] "/home/test/thing.txt"
--  
--  will return
--  
--  @
--   thing.txt
--   ExitSuccess
--  @
--  
--  You can think of @[s|echo $filename|]@ essentially converts into
--  
--  @
--    \\path -> system $ "echo" ++ encodeString (filename path)
--  @
--  
--  Here is another example
--  
--  >> [s|gcc $path -o $directory$basename.o|] "/home/test/thing.c"
--  
--  All "file parts" start with a \'$\'. The \'$\' can be escaped by preceding it with a \'\\\'
--  
--  There are the following options for "file parts" 
--  
--  * $path
--  * $root
--  * $directory
--  * $parent
--  * $filename
--  * $dirname
--  * $basename
--  * $ext
s :: QuasiQuoter 
s = QuasiQuoter 
     { quoteExp  = either (error . show) eval . parse parser "" 
     , quotePat  = error "s quotePat not implemented"
     , quoteType = error "s quoteType not implemented"
     , quoteDec  = error "s quoteDec not implemented"
     }