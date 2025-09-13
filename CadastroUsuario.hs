module CadastroUsuario where

Import Types (Usuario)

adicionarUsuario :: [Usuario] -> Usuario -> [Usuario]
adicionarUsuario lista novo = novo : lista

removerUsuario :: [Usuario] -> String -> [Usuario]
removerUsuario lista mat = filter (\u -> matricula u /= mat) lista

listarUsuarios :: [Usuario] -> _ -> IO ()
listarUsuarios [] nome = putStrLn "Nenhum usuário cadastrado."
listarUsuarios usuarios nome = do
  putStrLn "\nLista de usuários:"
  mapM_ (\u -> putStrLn $ "Nome: " ++ nome u ++ ", Matrícula: " ++ matricula u ++ ", Email: " ++ email u) usuarios
