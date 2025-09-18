module RegistrarLog where

import System.IO
import Data.Time
import Types

-- Gera a linha de log formatada
registrarLog :: String -> Item -> Usuario -> String -> IO ()
registrarLog operacao item usuario resultado = do
    zonedTime <- getZonedTime
    let timestamp = formatTime defaultTimeLocale "[%Y-%m-%d %H:%M]" zonedTime
        linha = timestamp ++ " " ++ operacao ++ ": " ++
                tipoStr (tipo item) ++ " \"" ++ titulo item ++ "\" (" ++ show (codigo item) ++ ")" ++
                " para usuário: \"" ++ nome usuario ++ "\" (" ++ show (matricula usuario) ++ ") " ++
                "(" ++ resultado ++ ")\n"
    appendFile "Files/logs.txt" linha

-- Converte tipo de item para string
tipoStr :: TipoItem -> String
tipoStr Livro = "livro"
tipoStr Filme = "filme"
tipoStr Jogo  = "jogo"
