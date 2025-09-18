-- CadastroItems.hs

module CadastroItems where

import Types
import Data.List (delete)
import Listas (splitByDelimiter)
import Text.Read (readMaybe)
import Control.Exception (evaluate)
import System.IO

-- Define o caminho do arquivo (ajuste se necessário)
itensPath :: FilePath
itensPath = "Files/itens.csv"

-- Função auxiliar para mapear uma linha do CSV para um tipo Item
mapearParaItem :: [String] -> Maybe Item
mapearParaItem [codStr, tit, aut, anoStr, tipoStr, statusStr] = do
    cod <- readMaybe codStr
    ano <- readMaybe anoStr
    tipo <- readMaybe tipoStr
    status <- readMaybe statusStr
    return (Item cod tit aut ano tipo status)
mapearParaItem _ = Nothing

-- Função auxiliar para encontrar a linha de um item pelo código
encontrarLinhaItem :: Codigo -> [String] -> Maybe (Int, String)
encontrarLinhaItem cod linhas =
    let
        parsedLines = zip [0..] linhas
        isMatch (idx, linha) =
            case mapearParaItem (splitByDelimiter ',' linha) of
                Just item -> codigo item == cod
                Nothing -> False
    in
        case filter isMatch parsedLines of
            [] -> Nothing
            (foundLine:_) -> Just foundLine

-- 1. Adicionar item
adicionarItem :: IO ()
adicionarItem = do
    putStrLn "\n--- Cadastro de Novo Item ---"
    codigoStr <- prompt "Código do item"
    case readMaybe codigoStr of
        Nothing -> putStrLn "Erro: código inválido. Deve ser um número."
        Just cod -> do
            conteudo <- readFile itensPath
            _ <- evaluate (length conteudo)
            let linhas = lines conteudo
            case encontrarLinhaItem cod linhas of
                Just _ -> putStrLn "Erro: Já existe um item com esse código."
                Nothing -> do
                    titulo <- prompt "Título do item"
                    autor <- prompt "Autor/Diretor"
                    anoStr <- prompt "Ano de Lançamento"
                    case readMaybe anoStr of
                        Nothing -> putStrLn "Erro: ano inválido. Deve ser um número."
                        Just (ano :: Int) -> do  -- <-- Added type annotation here
                            tipoStr <- prompt "Tipo do item (Filme, Jogo, Livro)"
                            case readMaybe tipoStr of
                                Nothing -> putStrLn "Erro: tipo inválido. Use 'Filme', 'Jogo' ou 'Livro'."
                                Just (tipo :: TipoItem) -> do
                                    let novoItemStr = show cod ++ "," ++ titulo ++ "," ++ autor ++ "," ++ show ano ++ "," ++ show tipo ++ "," ++ show Disponivel ++ "\n"
                                    appendFile itensPath novoItemStr
                                    putStrLn "Item adicionado com sucesso."

-- 2. Remover item
removerItem :: IO ()
removerItem = do
    putStrLn "\n--- Remoção de Item ---"
    codigoStr <- prompt "Código do item a remover"
    case readMaybe codigoStr of
        Nothing -> putStrLn "Erro: código inválido."
        Just cod -> do
            conteudo <- readFile itensPath
            _ <- evaluate (length conteudo)
            let linhas = lines conteudo
            case encontrarLinhaItem cod linhas of
                Nothing -> putStrLn $ "Erro: Item com código " ++ show cod ++ " não encontrado."
                Just (numLinha, _) -> do
                    let novasLinhas = unlines (removerLinha numLinha linhas)
                    writeFile itensPath novasLinhas
                    putStrLn "Item removido com sucesso."

-- 3. Listar itens
listarItens :: IO ()
listarItens = do
    putStrLn "\n--- Lista de Itens Cadastrados ---"
    conteudo <- readFile itensPath
    _ <- evaluate (length conteudo)
    let linhas = lines conteudo
    if null linhas
        then putStrLn "Nenhum item cadastrado."
        else do
            mapM_ (\linha ->
                case mapearParaItem (splitByDelimiter ',' linha) of
                    Nothing -> putStrLn "Erro: linha corrompida no arquivo."
                    Just item -> mostrarItem item
                ) linhas

-- Função auxiliar para remover linha pelo índice
removerLinha :: Int -> [String] -> [String]
removerLinha n xs = take n xs ++ drop (n+1) xs

-- Função auxiliar para exibir um item
mostrarItem :: Item -> IO ()
mostrarItem item =
    putStrLn $ "Código: " ++ show (codigo item) ++
               " | Título: " ++ titulo item ++
               " | Autor: " ++ autor item ++
               " | Ano: " ++ show (ano item) ++
               " | Tipo: " ++ show (tipo item) ++
               " | Status: " ++ show (status item)

-- Função auxiliar genérica para entrada de dados
prompt :: String -> IO String
prompt texto = do
    putStrLn texto
    hFlush stdout
    getLine
