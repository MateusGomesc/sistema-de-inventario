module Types where

data Usuario = Usuario {
     nome :: String,
     matricula :: String,
     email :: String
} deriving (show)

