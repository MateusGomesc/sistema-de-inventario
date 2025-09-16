module Interface where

import Types
import Validation (valida)

exibirOpções :: [String] -> IO ()
exibirOpções opções = do
    putStrLn ""
    putStrLn "Opções:"
    putStrLn ""
    putStrLn (unlines opções)
    putStrLn ""

lerOpção :: Int -> Int -> IO Int
lerOpção minimo maximo = do
    putStr "Escolha uma opção: "
    opção <- readLn
    valida opção minimo maximo

cabecalho :: String -> IO ()
cabecalho title = do
    putStrLn ""
    putStrLn "=========================="
    putStrLn $ "  " ++ title
    putStrLn "=========================="

menu :: String -> [String] -> IO Int
menu titulo texto = do 
    cabecalho titulo
    exibirOpções texto
    lerOpção 0 (length texto - 1)

lacoMenuPrincipal :: IO ()
lacoMenuPrincipal = do
    opção <- menu "Menu Principal" [
        "1 - ",
        "2 - ",
        "3 - ",
        "4 - ",
        "5 - ",
        "6 - ",
        "7 - ",
        "8 - ",
        "0 - "]
    case opção of
        1 -> return ()
        2 -> return ()
        3 -> return ()
        4 -> return ()
        5 -> return ()
        6 -> return ()
        7 -> return ()
        8 -> return ()
        0 -> return ()
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuPrincipal

lacoMenuCadastroItem :: IO ()
lacoMenuCadastroItem = do
    opção <- menu "Cadastro de Itens" [
        "1 - ",
        "2 - ",
        "3 - ",
        "4 - ",
        "5 - ",
        "6 - ",
        "7 - ",
        "8 - ",
        "0 - "]
    case opção of
        1 -> return ()
        2 -> return ()
        3 -> return ()
        4 -> return ()
        5 -> return ()
        6 -> return ()
        7 -> return ()
        8 -> return ()
        0 -> return ()
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuCadastroItem

lacoMenuCadastroUsuario :: IO ()
lacoMenuCadastroUsuario = do
    opção <- menu "Cadastro de usuário" [
        "1 - ",
        "2 - ",
        "3 - ",
        "4 - ",
        "5 - ",
        "6 - ",
        "7 - ",
        "8 - ",
        "0 - "]
    case opção of
        1 -> return ()
        2 -> return ()
        3 -> return ()
        4 -> return ()
        5 -> return ()
        6 -> return ()
        7 -> return ()
        8 -> return ()
        0 -> return ()
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuCadastroUsuario

lacoMenuEmprestimoDevolucao :: IO ()
lacoMenuEmprestimoDevolucao = do
    opção <- menu "Empréstimo e Devolução" [
        "1 - ",
        "2 - ",
        "3 - ",
        "4 - ",
        "5 - ",
        "6 - ",
        "7 - ",
        "8 - ",
        "0 - "]
    case opção of
        1 -> return ()
        2 -> return ()
        3 -> return ()
        4 -> return ()
        5 -> return ()
        6 -> return ()
        7 -> return ()
        8 -> return ()
        0 -> return ()
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuEmprestimoDevolucao

lacoMenuBuscaAvancada :: IO ()
lacoMenuBuscaAvancada = do
    opção <- menu "Menu Principal" [
        "1 - ",
        "2 - ",
        "3 - ",
        "4 - ",
        "5 - ",
        "6 - ",
        "7 - ",
        "8 - ",
        "0 - "]
    case opção of
        1 -> return ()
        2 -> return ()
        3 -> return ()
        4 -> return ()
        5 -> return ()
        6 -> return ()
        7 -> return ()
        8 -> return ()
        0 -> return ()
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuBuscaAvancada

lacoMenuRelatorio :: IO ()
lacoMenuRelatorio = do
    opção <- menu "Relatório e Estátisticas" [
        "1 - ",
        "2 - ",
        "3 - ",
        "4 - ",
        "5 - ",
        "6 - ",
        "7 - ",
        "8 - ",
        "0 - "]
    case opção of
        1 -> return ()
        2 -> return ()
        3 -> return ()
        4 -> return ()
        5 -> return ()
        6 -> return ()
        7 -> return ()
        8 -> return ()
        0 -> return ()
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuRelatorio

lacoMenuEdicao :: IO ()
lacoMenuEdicao = do
    opção <- menu "Edições" [
        "1 - ",
        "2 - ",
        "3 - ",
        "4 - ",
        "5 - ",
        "6 - ",
        "7 - ",
        "8 - ",
        "0 - "]
    case opção of
        1 -> return ()
        2 -> return ()
        3 -> return ()
        4 -> return ()
        5 -> return ()
        6 -> return ()
        7 -> return ()
        8 -> return ()
        0 -> return ()
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuEdicao

lacoMenuExportacao :: IO ()
lacoMenuExportacao = do
    opção <- menu "Exportação / Importação CSV" [
        "1 - ",
        "2 - ",
        "3 - ",
        "4 - ",
        "5 - ",
        "6 - ",
        "7 - ",
        "8 - ",
        "0 - "]
    case opção of
        1 -> return ()
        2 -> return ()
        3 -> return ()
        4 -> return ()
        5 -> return ()
        6 -> return ()
        7 -> return ()
        8 -> return ()
        0 -> return ()
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuExportacao

lacoMenuAuditoriaHistorico :: IO ()
lacoMenuAuditoriaHistorico = do
    opção <- menu "Aurditória / Histórico" [
        "1 - ",
        "2 - ",
        "3 - ",
        "4 - ",
        "5 - ",
        "6 - ",
        "7 - ",
        "8 - ",
        "0 - "]
    case opção of
        1 -> return ()
        2 -> return ()
        3 -> return ()
        4 -> return ()
        5 -> return ()
        6 -> return ()
        7 -> return ()
        8 -> return ()
        0 -> return ()
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuAuditoriaHistorico
