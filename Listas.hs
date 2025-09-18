module Listas where

import Data.Time.Calendar
import Data.List (group, sortOn, sortBy)
import Data.Function (on)
import System.IO
import Types

-- tipo split on
splitByDelimiter :: Char -> String -> [String]
splitByDelimiter _ "" = [""]
splitByDelimiter delim str =
    let (part, rest) = span (/= delim) str
    in part : case rest of
        [] -> []
        (_:rest') -> splitByDelimiter delim rest'

-- Leitura de usuários
lerUsuariosCSV :: FilePath -> IO [Usuario]
lerUsuariosCSV caminho = do
    conteudo <- readFile caminho
    let linhas = tail $ lines conteudo
    return $ map parseUsuario linhas

parseUsuario :: String -> Usuario
parseUsuario linha =
    let [nome, matriculaStr, email] = splitByDelimiter ',' linha
    in Usuario nome (read matriculaStr) email

-- Leitura de itens
lerItensCSV :: FilePath -> IO [Item]
lerItensCSV caminho = do
    conteudo <- readFile caminho
    let linhas = tail $ lines conteudo
    return $ map parseItem linhas

parseItem :: String -> Item
parseItem linha =
    let [codigoStr, titulo, autor, anoStr, tipoStr, statusStr] = splitByDelimiter ',' linha
    in Item (read codigoStr) titulo autor (read anoStr) (read tipoStr) (read statusStr)

-- Leitura de empréstimos
lerEmprestimosCSV :: FilePath -> IO [Emprestimo]
lerEmprestimosCSV caminho = do
    conteudo <- readFile caminho
    let linhas = tail $ lines conteudo
    return $ map parseEmprestimo linhas

parseEmprestimo :: String -> Emprestimo
parseEmprestimo linha =
    let [codItemStr, matStr, dataEmpStr, dataPrevDevStr, dataEfetDevStr] = splitByDelimiter ',' linha
    in Emprestimo (read codItemStr) (read matStr) (readDay dataEmpStr) (readDay dataPrevDevStr) (readDay dataEfetDevStr)

-- Leitura de listas de espera
lerListasEsperaCSV :: FilePath -> IO [Espera]
lerListasEsperaCSV caminho = do
    conteudo <- readFile caminho
    let linhas = tail $ lines conteudo
    return $ map parseEspera linhas

parseEspera :: String -> Espera
parseEspera linha =
    let (codItemStr:matriculasStrs) = splitByDelimiter ',' linha
    in Espera (read codItemStr) (map read matriculasStrs)

-- Função auxiliar para datas no formato YYYY-MM-DD
readDay :: String -> Day
readDay str =
    let
        [yStr, mStr, dStr] = splitByDelimiter '-' str
        y = read yStr :: Integer
        m = read mStr :: Int
        d = read dStr :: Int
    in fromGregorian y m d

-- Definir os caminhos dos arquivos (o caminho depende e os arquivos devem estar juntos)
usuariosPath :: FilePath
usuariosPath = "usuarios.csv"
emprestimosPath :: FilePath
emprestimosPath = "emprestimos.csv"
listasEsperaPath :: FilePath
listasEsperaPath = "listas_espera.csv"
itensPath :: FilePath
itensPath = "itens.csv"
