{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module FileCommand.Grammar where
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

type Parser = Parsec String ()

-- I need parse everything but $
-- unless it is proceeded by a \
-- unless \ is proceeded by a \
-- ?

-- so read until \ or $ 
-- if I hit a \ lookahead 
-- if I see a $
-- then it is a string '$'
-- if I see anything else print the '\' and the next character
-- if I see a $ treat the next bit as FilePart

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
      
type Cmd = String

pCmd :: Parser Cmd
pCmd = do 
  frag <- many $ noneOf ['\\', '$']
  xs <- option [] $ try $ (\x y -> x:y:[]) <$> char '\\' <*> anyChar
  case xs of 
    [] 
     | null frag -> fail "empty cmd fragment"
     | otherwise -> return frag
    "\\$" -> fmap (\x -> frag ++ "$" ++ x) pCmd
    xs'  -> fmap (\x -> frag ++ xs' ++ x) pCmd

data Expr 
  = ECmd      Cmd
  | EFilePart FilePart
  deriving (Show, Eq)

pExpr :: Parser Expr
pExpr = EFilePart <$> pFilePart <|> ECmd <$> pCmd 

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
  ECmd      frag     -> [| $(stringE frag) |]
  EFilePart filePart -> evalFilePart filePathName filePart
  
evalFilePart :: Name -> FilePart -> Q Exp 
evalFilePart filePathName = \case
  Path         -> [| $(varE filePathName) |]
  Root         -> [| encodeString $ root      $(varE filePathName) |]
  Directory    -> [| encodeString $ directory $(varE filePathName) |]
  Parent       -> [| encodeString $ parent    $(varE filePathName) |]
  FileName     -> [| encodeString $ filename  $(varE filePathName) |]
  DirName      -> [| encodeString $ dirname   $(varE filePathName) |]
  BaseName     -> [| encodeString $ basename  $(varE filePathName) |]
  Ext          -> [| encodeString $ fromMaybe ("Failed to get extension for " ++ $(varE filePathName))
                   $ extension $(varE filePathName) |]

s :: QuasiQuoter 
s = QuasiQuoter 
     { quoteExp  = either (error . show) eval . parse parser "" 
     , quotePat  = error "s quotePat not implemented"
     , quoteType = error "s quoteType not implemented"
     , quoteDec  = error "s quoteDec not implemented"
     }













