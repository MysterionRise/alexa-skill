import Data.Text
import Helpers

main :: IO()
main = do
     putStrLn "Input your text, please"
     lineFromUser <- getLine
     putStrLn $ "We've got: " ++ lineFromUser
