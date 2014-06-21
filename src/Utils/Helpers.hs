module Helpers where

import System.Process
import System.Directory
import System.Posix.User
import Category

-- not portable, use posix stuff
greetUser = do
        user <- getRealUserID >>= getUserEntryForID
        putStrLn $ "Hi, " ++ (userName user)

runSysCmd = system

startShell = do
        putStrLn "Octo Shell starting..."
        currentDir <- getCurrentDirectory
        categorizeFolder currentDir
        -- more to come
        exitShell

exitShell = do
          putStrLn "Octo Shell exiting..."
          -- more to come

categorizeFolder :: FilePath -> IO [Category]
categorizeFolder dir = undefined
