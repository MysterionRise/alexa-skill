{-# LANGUAGE DoAndIfThenElse #-}

module Helpers where

import Control.Exception
import Control.DeepSeq (force)
import System.Directory
import System.Posix.User
import Data.List (isInfixOf)
import Category

delimeter = "---------------------------------------------"

-- not portable, posix stuff
greetUserInPosixWay = do
        user <- getRealUserID >>= getUserEntryForID
        putStrLn $ "Hi, " ++ (userName user)

-- for non posix stuff
greetHardcodedUser = putStrLn $ "Hi, User!"

greetUser os = if os == "windows" then greetHardcodedUser else greetUserInPosixWay

startShell = do
        putStrLn "Octo Shell starting..."
        content <- getCurrentDirectory >>= getDirectoryContents
        files <- getListOfFiles $ filterDotsInFilePaths content
        print files
        dirs <- getListOfDirs $ filterDotsInFilePaths content
        print dirs
        allFiles <- getListOfFilesRecursively $ filterDotsInFilePaths content
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




filterDotsInFilePaths :: [FilePath] -> [FilePath]
filterDotsInFilePaths = filter (\x -> not (x `isInfixOf` ".") && not (x `isInfixOf` ".."))

getListOfFilesRecursively :: [FilePath] -> IO [FilePath]
getListOfFilesRecursively [] = return []
getListOfFilesRecursively (x:xs) = do
                                 flag <- doesFileExist x
                                 if flag
                                 then do
                                       t <- getListOfFilesRecursively xs
                                       return (x : t)
                                 else do
                                       dirContents <- getDirectoryContents x
                                       let filteredDir = filterDotsInFilePaths dirContents
                                       let rel = map (\z -> x ++ "/" ++ z) filteredDir
                                       print x
                                       print rel
                                       f <- getListOfFilesRecursively $ rel
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
