module Validation where

import Types(Matricula, Codigo)
import Data.List(isInfixOf)
import Data.Char(toLower)

usuárioExiste :: Matricula -> String -> Bool
usuárioExiste matrícula conteúdo = isInfixOf (show matrícula) (map toLower conteúdo)

itemExiste :: Codigo -> String -> Bool
itemExiste código conteúdo = isInfixOf (show código) (map toLower conteúdo)
