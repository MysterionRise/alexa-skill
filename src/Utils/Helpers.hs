module Helpers where

import System.Process 

hello user = "Hi, " ++ user

runSysCmd = system