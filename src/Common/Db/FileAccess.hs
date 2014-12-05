{-|
Module      : Common.Db.FileAccess
Description : Read and write from files
Copyright   : (C) Marcus Pedersén, 2014

License     : GPLv3
Maintainer  : marcux@marcux.org
Stability   : Stable
Portability : Portable
Version     : v1.0.0

Handles read and write from files in a generic way.
Saves files in a subdirectory named "db".
-}

module Common.Db.FileAccess where

import System.FilePath
import System.Directory
import System.IO

{-|
  Saves string to given file on a new line.
  If file or subdirectory does not exist,
  directory and/or file will be created.
  File is relative to execution directory
  and will be saved at: db/filename.ext
-}
appendDataToFileLn :: String -> String -> IO ()
appendDataToFileLn str2write fileName = do
    createDirectoryIfMissing True "db" 
    appendFile file $ str2write ++ "\n"
    where 
      file = "db" ++ pathSeparator:(takeFileName fileName)


{-|
  Reads a newline separated file
  and returns an array of strings 
  where each string is a line in the file.
  File is relative to execution directory
  and must exist in subfolder "db".
-}
readFileLnSeparated :: String -> IO ([String])
readFileLnSeparated fileName = do
    cont <- readFile file
    length cont `seq` (return $ lines cont)
    where file = "db" ++ pathSeparator:(takeFileName fileName)

{-|
  Deletes given string line from 
  file if it exists.
  File is relative to execution directory
  and must exist in subfolder "db".
-}
deleteLnFromFile :: String -> String -> IO ()
deleteLnFromFile str2del fileName = do
    strings <- readFileLnSeparated fileName
    writeFile file . unlines $ delStr str2del strings
    where file = "db" ++ pathSeparator:(takeFileName fileName)
          delStr str (x:xs) 
            | str == x  = delStr str xs
            | otherwise = x:delStr str xs
          delStr _ [] = []
