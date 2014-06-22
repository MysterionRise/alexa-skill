{-# LANGUAGE DoAndIfThenElse #-}

module Helpers where

import Control.Exception
import Control.DeepSeq
import System.Process (system)
import System.Directory
import System.Posix.User
import Category

delimeter = "---------------------------------------------"

-- not portable, posix stuff
greetUserInPosixWay = do
        user <- getRealUserID >>= getUserEntryForID
        putStrLn $ "Hi, " ++ (userName user)

-- for non posix stuff
greetHardcodedUser = putStrLn $ "Hi, User!"

greetUser os = if os == "windows" then greetHardcodedUser else greetUserInPosixWay

runSysCmd = system

startShell = do
        putStrLn "Octo Shell starting..."
        content <- getCurrentDirectory >>= getDirectoryContents
        files <- getListOfFiles $ filter (\a -> a /= "." && a /= ".." ) content
        print files
        dirs <- getListOfDirs $ filter (\a -> a /= "." && a /= ".." ) content
        print dirs
        allFiles <- getListOfFilesRecursively $ filter (\a -> a /= "." && a /= ".." ) content
        categories <- categorizedFiles allFiles
        print categories
        -- more to come
        exitShell

exitShell = putStrLn delimeter >> putStrLn "Octo Shell exiting..."

-- dummy version of categorizer
categorizedFiles :: [FilePath] -> IO [Category]
categorizedFiles [] = return []
categorizedFiles (_:xs) = do
                         t <- categorizedFiles xs
                         return (Uncategorized : t)

getListOfFilesRecursively :: [FilePath] -> IO [FilePath]
getListOfFilesRecursively [] = return []
getListOfFilesRecursively (x:xs) = do
                                 flag <- doesFileExist x
                                 if flag
                                 then do
                                       t <- getListOfFilesRecursively xs
                                       return (x : t)
                                 else do
                                       f <- getListOfFilesRecursively [x]
                                       t <- getListOfFilesRecursively xs
                                       return (force f ++ force t)


-- todo need to be fixed
-- take a look - http://stackoverflow.com/questions/3982491/find-out-whether-all-given-files-exists-in-haskell
getListOfFiles :: [FilePath] -> IO [FilePath]
getListOfFiles [] = return []
getListOfFiles (x:xs) = do
                         flag <- doesFileExist x
                         if flag
                         then do
                              t <- getListOfFiles xs
                              return (x : t)
                         else do
                              t <- getListOfFiles xs
                              return t


getListOfDirs :: [FilePath] -> IO [FilePath]
getListOfDirs [] = return []
getListOfDirs (x:xs) = do
                         flag <- doesDirectoryExist x
                         if flag
                         then do
                              t <- getListOfDirs xs
                              return (x : t)
                         else do
                              t <- getListOfDirs xs
                              return t
