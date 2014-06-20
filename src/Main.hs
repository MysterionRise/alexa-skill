import Helpers
import Category

main :: IO()
main = do
    putStrLn $ hello "K"
    input <- getLine
    if input == "start" then startShell else exitShell
    