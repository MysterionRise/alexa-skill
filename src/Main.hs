import Data.Text
import Helpers
import Control.Exception

tryToOpenFile :: FilePath -> IO String
tryToOpenFile path =
     readFile path `catch` errors
     where
         errors :: IOException -> IO String
         errors error = return $ show error

main :: IO()
main = do
     putStrLn "Input path to your text file, please"
     lineFromUser <- getLine
     putStrLn $ "We've got: " ++ lineFromUser
     fileContent <- tryToOpenFile lineFromUser
     putStrLn fileContent
