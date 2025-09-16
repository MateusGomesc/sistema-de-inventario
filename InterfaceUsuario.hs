module InterfaceUsuario where

import Types
import CadastroUsuario 

<<<<<<< HEAD
menuCadastro :: [Usuario] -> IO ()
menuCadastro listaUsuarios = do
  putStrLn "============================="
  putStrLn "    Cadastro de Usuários"
  putStrLn "============================="
  putStrLn "1 - Adicionar novo usuário"
  putStrLn "2 - Remover usuário"
  putStrLn "3 - Listar usuários"
  putStrLn "4 - Voltar ao menu principal"
  putStr "Escolha uma opção: "
  opcao <- getLine
  case opcao of
    "1" -> do
      putStrLn "\n--- Adicionar novo usuário ---"
      putStr "Digite o nome do usuário: "
      nomeUsuario <- getLine
      putStr "Digite a matrícula do usuário: "
      matriculaUsuario <- getLine
      putStr "Digite o email do usuário: "
      emailUsuario <- getLine
      let novoUsuario = Usuario nomeUsuario matriculaUsuario emailUsuario
          listaAtualizada = adicionarUsuario listaUsuarios novoUsuario
      putStrLn "Usuário adicionado com sucesso!"
      menuCadastro listaAtualizada
    "2" -> do
      putStrLn "\n--- Remover usuário ---"
      putStr "Digite a matrícula do usuário a ser removido: "
      matRemover <- getLine
      let listaAtualizada = removerUsuario listaUsuarios matRemover
      putStrLn "Usuário removido (se existia) com sucesso!"
      menuCadastro listaAtualizada
    "3" -> do
      listarUsuarios :: [Usuario] -> (Usuario -> [Char]) -> IO ()
      menuCadastro listaUsuarios
    "4" -> do
      putStrLn "Voltando ao menu principal..."
      menuPrincipal listaUsuarios
    _ -> do
      putStrLn "Opção inválida, tente novamente."
      menuCadastro listaUsuarios
=======
-- Submenu de Cadastro de Usuários
menuUsuarios :: [Usuario] -> IO ()
menuUsuarios usuarios = do
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
                Left erro -> putStrLn erro >> menuUsuarios usuarios
                Right novaLista -> do
                    putStrLn "Usuário adicionado com sucesso!"
                    menuUsuarios novaLista

        "2" -> do
            putStrLn "Digite a matrícula do usuário a remover:"
            m <- getLine
            case removerUsuario usuarios m of
                Left erro -> putStrLn erro >> menuUsuarios usuarios
                Right novaLista -> do
                    putStrLn "Usuário removido com sucesso!"
                    menuUsuarios novaLista

        "3" -> do
            putStrLn "\nUsuários cadastrados:"
            putStrLn (listarUsuarios usuarios)
            menuUsuarios usuarios

        "4" -> putStrLn "Voltando ao menu principal..."
        _   -> do
            putStrLn "Opção inválida, tente novamente."
            menuUsuarios usuarios
>>>>>>> 16214d78b94adc8bf6aa7db6f25c8e703a36306f
