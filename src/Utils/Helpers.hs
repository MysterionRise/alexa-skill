{-# LANGUAGE DoAndIfThenElse #-}

module Helpers where

import Control.Exception
import System.Process (system)
import System.Directory
import System.Posix.User
import Category

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
        categories <- categorizedFiles files
        print categories
        -- more to come
        exitShell

exitShell = do
          putStrLn "Octo Shell exiting..."
          -- more to come

categorizedFiles :: [FilePath] -> IO [Category]
categorizedFiles content = undefined

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
