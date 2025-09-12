module Types where

data Usuario = Usuario {
     nome :: String,
     matricula :: Int,
     email :: String
} deriving (show)

