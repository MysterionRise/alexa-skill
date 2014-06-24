import Helpers
import System.Info (os)

main :: IO()
main = do
    greetUser os
    putStrLn "Enter your command, please: "
    putStrLn delimeter
    input <- getLine
    if input == "start" then runShell else exit