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
      listarUsuarios listaUsuarios
      menuCadastro listaUsuarios
    "4" -> do
      putStrLn "Voltando ao menu principal..."
      menuPrincipal listaUsuarios
    _ -> do
      putStrLn "Opção inválida, tente novamente."
      menuCadastro listaUsuarios

