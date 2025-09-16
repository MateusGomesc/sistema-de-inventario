module CadastroUsuario where

import Types

-- Função para adicionar usuário com validações
adicionarUsuario :: [Usuario] -> Usuario -> Either String [Usuario]
adicionarUsuario usuarios novo
    | matriculaDuplicada = Left ("Erro: matrícula \"" ++ show (matricula novo) ++ "\" já cadastrada.")
    | emailInvalido      = Left ("Erro: e-mail \"" ++ email novo ++ "\" está mal formatado.")
    | otherwise          = Right (novo : usuarios)
  where
    matriculaDuplicada = any (\u -> matricula u == matricula novo) usuarios
    emailInvalido = not ('@' `elem` email novo && '.' `elem` email novo)
    
-- Função para remover usuário pela matrícula    
removerUsuario :: [Usuario] -> String -> Either String [Usuario]
removerUsuario usuarios mat
    | any (\u -> show (matricula u) == mat) usuarios = Right (filter (\u -> show (matricula u) /= mat) usuarios)
    | otherwise = Left ("Erro: matrícula \"" ++ mat ++ "\" não encontrada.")
    
-- Função para listar usuários
listarUsuarios :: [Usuario] -> String
listarUsuarios [] = "Nenhum usuário cadastrado."
listarUsuarios usuarios = unlines (map formatar usuarios)
  where
    formatar u = "Nome: " ++ nome u ++ " | Matrícula: " ++ show (matricula u) ++ " | E-mail: " ++ email u

-- Criar usuário
criarUsuario :: String -> String -> String -> Usuario
criarUsuario n m e = Usuario {nome = n, matricula = m, email = e}
