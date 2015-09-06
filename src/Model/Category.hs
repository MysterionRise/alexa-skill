module Category where

data Category = Uncategorized FilePath
               | Image FilePath
               | Music FilePath
               | Text FilePath
               | Video FilePath
               deriving (Show)