module Helpers where

import System.Process 
import Category

hello user = "Hi, " ++ user

runSysCmd = system

startShell = do	
	putStrLn "Octo Shell starting..."	
	categorizeFolder
	-- more to come
	exitShell

exitShell = do
	putStrLn "Octo Shell exiting..."
	-- more to come

categorizeFolder = undefined
