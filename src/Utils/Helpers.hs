module Helpers where

import System.Process 
import System.Directory
import Category

hello user = "Hi, " ++ user

getCurrentUser = undefined

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
