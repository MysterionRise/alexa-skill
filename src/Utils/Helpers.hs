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
        dirs <- getListOfDirs $ filter (\a -> a /= "." && a /= ".." ) content
        categories <- categorizedFiles files
        -- more to come
        exitShell

exitShell = do
          putStrLn "Octo Shell exiting..."
          -- more to come

categorizedFiles :: [FilePath] -> IO [Category]
categorizedFiles content = undefined

getListOfFiles :: [FilePath] -> IO [FilePath]
getListOfFiles = undefined
--getListOfFiles [] = []
--getListOfFiles (x:xs) = do
--                    isFile <- doesFileExist x
--                    if isFile then x : getListOfFiles xs else getListOfFiles xs

getListOfDirs :: [FilePath] -> IO [FilePath]
getListOfDirs paths = undefined
