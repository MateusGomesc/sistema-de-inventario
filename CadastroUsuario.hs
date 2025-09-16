module CadastroUsuario where

import Types

adicionarUsuario :: [Usuario] -> Usuario -> Either String [Usuario]
adicionarUsuario usuarios novo
    | matriculaDuplicada = Left ("Erro: matrícula \"" ++ matricula novo ++ "\" já cadastrada.")
    | emailInvalido      = Left ("Erro: e-mail \"" ++ email novo ++ "\" está mal formatado.")
    | otherwise          = Right (novo : usuarios)
  where
    matriculaDuplicada = any (\u -> matricula u == matricula novo) usuarios
    emailInvalido = not ('@' `elem` email novo && '.' `elem` email novo)
    
removerUsuario :: [Usuario] -> String -> Either String [Usuario]
removerUsuario usuarios mat
    | any (\u -> matricula u == mat) usuarios = Right (filter (\u -> matricula u /= mat) usuarios)
    | otherwise = Left ("Erro: matrícula \"" ++ mat ++ "\" não encontrada.")

listarUsuarios :: [Usuario] -> String
listarUsuarios [] = "Nenhum usuário cadastrado."
listarUsuarios usuarios = unlines (map formatar usuarios)
  where
    formatar u = "Nome: " ++ nome u ++
                 " | Matrícula: " ++ matricula u ++
                 " | E-mail: " ++ email u
