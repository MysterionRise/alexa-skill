import Helpers
import System.Info (os)

main :: IO()
main = do
    greetUser os
    putStrLn ""
    putStrLn delimeter
    putStrLn "Enter your command, please: "
    input <- getLine
    if input == "start" then startShell else exitShell
