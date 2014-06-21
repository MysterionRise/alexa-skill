import Helpers

main :: IO()
main = do
    greetUser
    putStrLn ""
    putStrLn "---------------------------------------------"
    putStrLn "Enter your command, please: "
    input <- getLine
    if input == "start" then startShell else exitShell

