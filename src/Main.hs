import Helpers

main :: IO()
main = do
    putStrLn $ hello getCurrentUser
    putStrLn "---------------------------------------------"
    putStrLn "Enter your command, please: "
    input <- getLine
    if input == "start" then startShell else exitShell
    