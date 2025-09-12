module Types where

import Data.Time.Calendar(Day)

data Usuario = Usuario {
        nome :: String,
        matricula :: String,
        email :: String
    } deriving (Show, Eq)

data TipoItem = Filme | Jogo | Livro
    deriving(Show, Eq)

data StatusItem = Emprestado | Disponivel
    deriving(Show, Eq)

data Item = Item{
        codigo :: Int,   
        titulo :: String,   
        autor :: String,   
        ano :: Int,
        tipo :: TipoItem,
        status :: StatusItem
    }
    deriving(Show, Eq)

data StatusEmprestimo = Devolvido | OnTime | Atrasado
    deriving(Show, Eq)

data Emprestimo = Emprestimo {
        empCodigoItem :: Int,
        matriculaUsuario :: Int,
        dataEmprestimo :: Day,
        dataEsperadaDevolucao :: Day,
        dataEfetuadaDevolucao :: Day
    }
    deriving(Show, Eq)

data Espera = Espera {
        espCodigoItem :: Int,
        lista :: [Int]
    }
    deriving(Show, Eq)
