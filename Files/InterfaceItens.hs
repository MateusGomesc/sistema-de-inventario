module Main where

import Types
import CadastroItens
import Data.Time.Calendar (fromGregorian)
import System.IO (hFlush, stdout)

-- Função principal do menu
main :: IO ()
main = loop []

-- Loop do menu, recebendo a lista de itens atual
loop :: [Item] -> IO ()
loop itens = do
    putStrLn "\n=== Sistema de Cadastro de Itens ==="
    putStrLn "1 - Adicionar Item"
    putStrLn "2 - Remover Item"
    putStrLn "3 - Listar Itens"
    putStrLn "4 - Sair"
    putStr "Escolha uma opção: "
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> do
            novoItem <- lerItem
            let itensAtualizados = adicionarItem itens novoItem
            putStrLn "Item adicionado com sucesso!"
            loop itensAtualizados
        "2" -> do
            putStr "Digite o código do item a remover: "
            hFlush stdout
            codStr <- getLine
            let cod = read codStr :: Codigo
            let itensAtualizados = removerItem itens cod
            putStrLn "Item removido com sucesso!"
            loop itensAtualizados
        "3" -> do
            listarItens itens
            loop itens
        "4" -> putStrLn "Saindo..."
        _   -> do
            putStrLn "Opção inválida!"
            loop itens

-- Função para ler dados do usuário e criar um novo item
lerItem :: IO Item
lerItem = do
    putStr "Código: "
    hFlush stdout
    codStr <- getLine
    let cod = read codStr :: Codigo

    putStr "Título: "
    hFlush stdout
    titulo <- getLine

    putStr "Autor/Diretor/Criador: "
    hFlush stdout
    autor <- getLine

    putStr "Ano: "
    hFlush stdout
    anoStr <- getLine
    let ano = read anoStr :: Int

    putStrLn "Tipo de Item: 1 - Livro, 2 - Filme, 3 - Jogo"
    putStr "Escolha: "
    hFlush stdout
    tipoStr <- getLine
    let tipo = case tipoStr of
            "1" -> Livro
            "2" -> Filme
            "3" -> Jogo
            _   -> Livro

    -- Todos os itens começam como Disponível
    let status = Disponivel

    return $ Item cod titulo autor ano tipo status
