module Category where

data Category = Uncategorized FilePath
               | Picture FilePath
               | Music FilePath
               | Text FilePath
               deriving (Show)