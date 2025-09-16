module InterfaceUsuario where

import Types
import CadastroUsuario 

menuUsuarios :: FilePath -> [Usuario] -> IO ()
menuUsuarios arquivo usuarios = do
    putStrLn "\n=============================="
    putStrLn "  Cadastro de Usuários"
    putStrLn "=============================="
    putStrLn "1 - Adicionar usuário"
    putStrLn "2 - Remover usuário"
    putStrLn "3 - Listar usuários"
    putStrLn "4 - Voltar ao menu principal"
    putStr   "Escolha uma opção: "
    opcao <- getLine
    case opcao of
        "1" -> do
            putStrLn "Digite o nome:"
            n <- getLine
            putStrLn "Digite a matrícula:"
            m <- getLine
            putStrLn "Digite o e-mail:"
            e <- getLine
            let novo = criarUsuario n m e
            case adicionarUsuario usuarios novo of
                Left erro -> putStrLn erro >> menuUsuarios arquivo usuarios
                Right novaLista -> do
                    salvarUsuariosCSV arquivo novaLista
                    putStrLn "Usuário adicionado com sucesso!"
                    menuUsuarios arquivo novaLista

        "2" -> do
            putStrLn "Digite a matrícula do usuário a remover:"
            m <- getLine
            case removerUsuario usuarios m of
                Left erro -> putStrLn erro >> menuUsuarios arquivo usuarios
                Right novaLista -> do
                    salvarUsuariosCSV arquivo novaLista
                    putStrLn "Usuário removido com sucesso!"
                    menuUsuarios arquivo novaLista

        "3" -> do
            putStrLn "\nUsuários cadastrados:"
            putStrLn (listarUsuarios usuarios)
            menuUsuarios arquivo usuarios

        "4" -> putStrLn "Voltando ao menu principal..."
        _   -> do
            putStrLn "Opção inválida, tente novamente."
            menuUsuarios arquivo usuarios
