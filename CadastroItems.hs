module CadastroItems where
import Types


import Types
import Data.List (delete)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Control.Exception (evaluate)

-- 1. Adicionar item
adicionarItem :: IO ()
adicionarItem = do
    codigoStr <- prompt "Código do item"
    case readMaybe codigoStr of
        Nothing -> putStrLn "Erro: código inválido. Deve ser um número."
        Just cod -> do
            nome <- prompt "Nome do item"
            precoStr <- prompt "Preço do item"
            case readMaybe precoStr of
                Nothing -> putStrLn "Erro: preço inválido. Deve ser numérico."
                Just preco -> do
                    conteudo <- readFile "Files/itens.csv"
                    _ <- evaluate (length conteudo)  -- força leitura
                    let linhas = lines conteudo
                    case encontrarLinhaItem cod linhas of
                        Just _  -> putStrLn "Erro: já existe um item com esse código."
                        Nothing -> do
                            appendFile "Files/itens.csv" (show cod ++ "," ++ nome ++ "," ++ show preco ++ "\n")
                            putStrLn "Item adicionado com sucesso."

-- 2. Remover item
removerItem :: IO ()
removerItem = do
    codigoStr <- prompt "Código do item a remover"
    case readMaybe codigoStr of
        Nothing -> putStrLn "Erro: código inválido."
        Just cod -> do
            conteudo <- readFile "Files/itens.csv"
            _ <- evaluate (length conteudo)
            let linhas = lines conteudo
            case encontrarLinhaItem cod linhas of
                Nothing -> putStrLn $ "Erro: Item com código " ++ show cod ++ " não encontrado."
                Just (numLinha, _) -> do
                    let novasLinhas = unlines (removerLinha numLinha linhas)
                    writeFile "Files/itens.csv" novasLinhas
                    putStrLn "Item removido com sucesso."

-- 3. Listar itens
listarItens :: IO ()
listarItens = do
    conteudo <- readFile "Files/itens.csv"
    _ <- evaluate (length conteudo)
    let linhas = lines conteudo
    if null linhas
        then putStrLn "Nenhum item cadastrado."
        else do
            mapM_ (\linha ->
                case mapearParaItem (splitOn "," linha) of
                    Nothing -> putStrLn "Erro: linha corrompida."
                    Just item -> mostrarItem item
                ) linhas

-- Função auxiliar para remover linha pelo índice
removerLinha :: Int -> [String] -> [String]
removerLinha n xs = take n xs ++ drop (n+1) xs

