import Helpers

main :: IO()
main = do
    putStrLn $ hello "K"
    putStrLn "Octo Shell starting..."
    runSysCmd "ls -al"
    putStrLn "Octo Shell exiting..."