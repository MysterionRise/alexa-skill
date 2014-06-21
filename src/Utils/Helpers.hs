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

getListOfFiles :: [FilePath] -> IO [FilePath]
getListOfFiles p =  if length p == 0
                    then return []
                    else do
                         flag <- doesFileExist $ head p
                         if flag
                         then do
                              t <- getListOfFiles $ tail p
                              return (head p : t)
                         else do
                              t <- getListOfFiles $ tail p
                              return t

getListOfDirs :: [FilePath] -> IO [FilePath]
getListOfDirs = undefined
--getListOfDirs [] = return []
--getListOfDirs (x:xs) = doesDirectoryExist x >>= (\a -> if a then return $ x : getListOfDirs xs else getListOfDirs xs)
