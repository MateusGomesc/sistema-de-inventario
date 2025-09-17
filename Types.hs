module Types where

import Data.Time.Calendar(Day)

type Matricula = Int
type Codigo = Int

data Usuario = Usuario {
        nome :: String,
        matricula :: Matricula,
        email :: String
    } deriving (Show, Eq)

data TipoItem = Filme | Jogo | Livro
    deriving(Show, Eq, Read) -- Adicione Read aqui

data StatusItem = Emprestado | Disponivel
    deriving(Show, Eq, Read) -- Adicione Read aqui

data Item = Item{
        codigo :: Codigo,   
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
        empCodigoItem :: Codigo,
        matriculaUsuario :: Matricula,
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
