file-command-qq is a simple quasiquoter for running system commands that take a filepath as an argument.

For instance

```
> :set -XOverloadedStrings
> import FileCommand
> import Filesystem.Path
> [s|echo $filename|] "/home/test/thing.txt"
```

will return

```
thing.txt
ExitSuccess
```

You can think of `[s|echo $filename|]` essentially converts into

```haskell
\path -> system $ "echo" ++ encodeString (filename path)
```

All "file parts" start with a '$'. The '$' can be escaped by preceding it with a '\'

There are the following options for "file parts" 


* $path
* $root
* $directory
* $parent
* $filename
* $dirname
* $basename
* $ext

Which correspond to the respective functions in https://hackage.haskell.org/package/system-filepath-0.4.6/docs/Filesystem-Path.html#g:1
