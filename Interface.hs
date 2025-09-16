module Interface where

import Types
import Validation (valida)
import Edit ( editarUsuario, editarItem )

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
        "1 - Cadastro de itens",
        "2 - Cadastro de usuários",
        "3 - Empréstimos e devoluções",
        "4 - Busca e Listagem Avançada",
        "5 - Relatórios e Estatísticas",
        "6 - Edição de dados",
        "7 - Exportação / Importação de dados",
        "8 - Auditoria e Histórico",
        "0 - Salvar e Sair"]
    case opção of
        1 -> lacoMenuCadastroItem
        2 -> lacoMenuCadastroUsuario
        3 -> lacoMenuEmprestimoDevolucao
        4 -> lacoMenuBuscaAvancada
        5 -> lacoMenuRelatorio
        6 -> lacoMenuEdicao
        7 -> lacoMenuExportacao
        8 -> lacoMenuAuditoriaHistorico
        0 -> return ()
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuPrincipal

lacoMenuCadastroItem :: IO ()
lacoMenuCadastroItem = do
    opção <- menu "Cadastro de Itens" [
        "1 - Adicionar novo item",
        "2 - Remover item",
        "3 - Listar itens cadastrados",
        "0 - Voltar ao menu principal"]
    case opção of
        1 -> return ()
        2 -> return ()
        3 -> return ()
        0 -> lacoMenuPrincipal
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuCadastroItem

lacoMenuCadastroUsuario :: IO ()
lacoMenuCadastroUsuario = do
    opção <- menu "Cadastro de usuário" [
        "1 - Adicionar novo usuário",
        "2 - Remover usuário",
        "3 - Listar usuários cadastrados",
        "0 - Voltar ao menu principal"]
    case opção of
        1 -> return ()
        2 -> return ()
        3 -> return ()
        0 -> lacoMenuPrincipal
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuCadastroUsuario

lacoMenuEmprestimoDevolucao :: IO ()
lacoMenuEmprestimoDevolucao = do
    opção <- menu "Empréstimo e Devolução" [
        "1 - Registrar empréstimo",
        "2 - Resgistrar devolução",
        "3 - Visualizar empréstimos ativos",
        "4 - Renovar empréstimo",
        "5 - Empréstimo/devolução em lote",
        "0 - Voltar ao menu principal"]
    case opção of
        1 -> return ()
        2 -> return ()
        3 -> return ()
        4 -> return ()
        5 -> return ()
        0 -> lacoMenuPrincipal
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuEmprestimoDevolucao

lacoMenuBuscaAvancada :: IO ()
lacoMenuBuscaAvancada = do
    opção <- menu "Menu Principal" [
        "1 - Buscar por título",
        "2 - Buscar por autor/diretor",
        "3 - Busca combinada (múltiplos campos)",
        "4 - Filtar por categoria",
        "5 - Ordenar resultados (título, ano, autor/diretor)",
        "0 - Voltar ao menu principal"]
    case opção of
        1 -> return ()
        2 -> return ()
        3 -> return ()
        4 -> return ()
        5 -> return ()
        0 -> lacoMenuPrincipal
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuBuscaAvancada

lacoMenuRelatorio :: IO ()
lacoMenuRelatorio = do
    opção <- menu "Relatório e Estátisticas" [
        "1 - Empréstimos ativos (por categoria)",
        "2 - Usuários mais ativos",
        "3 - Itens mais emprestados",
        "4 - Frequência de empréstimos por período",
        "5 - Itens com lista de espera",
        "6 - Relatório de operações (por usuário/tipo de item)",
        "0 - Voltar ao menu principal"]
    case opção of
        1 -> return ()
        2 -> return ()
        3 -> return ()
        4 -> return ()
        5 -> return ()
        6 -> return ()
        0 -> lacoMenuPrincipal
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuRelatorio

lacoMenuEdicao :: IO ()
lacoMenuEdicao = do
    opção <- menu "Edições" [
        "1 - Editar item",
        "2 - Editar usuário",
        "0 - Voltar ao menu principal"]
    case opção of
        1 -> editarItem
        2 -> editarUsuario
        0 -> lacoMenuPrincipal
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuEdicao

lacoMenuExportacao :: IO ()
lacoMenuExportacao = do
    opção <- menu "Exportação / Importação CSV" [
        "1 - Exportar dados para CSV",
        "2 - Importar dados de CSV",
        "0 - Voltar ao menu principal"]
    case opção of
        1 -> return ()
        2 -> return ()
        0 -> lacoMenuPrincipal
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuExportacao

lacoMenuAuditoriaHistorico :: IO ()
lacoMenuAuditoriaHistorico = do
    opção <- menu "Aurditória / Histórico" [
        "1 - Exibir log de operações",
        "2 - Exibir histórico de alterações",
        "0 - Voltar ao menu principal"]
    case opção of
        1 -> return ()
        2 -> return ()
        0 -> lacoMenuPrincipal
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuAuditoriaHistorico
