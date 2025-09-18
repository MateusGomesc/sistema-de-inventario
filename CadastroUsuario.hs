-- CadastroUsuario.hs

module CadastroUsuario where

import Types
import System.IO
import Control.Exception (catch, IOException)
import Text.Read (readMaybe)
import Listas (splitByDelimiter)

-- Define o caminho do arquivo (ajuste se necessário)
usuariosPath :: FilePath
usuariosPath = "Files/usuarios.csv"

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
removerUsuario :: [Usuario] -> Int -> Either String [Usuario]
removerUsuario usuarios mat =
    if any (\u -> matricula u == mat) usuarios
        then Right (filter (\u -> matricula u /= mat) usuarios)
        else Left ("Erro: matrícula \"" ++ show mat ++ "\" não encontrada.")

-- Função para listar usuários
listarUsuarios :: [Usuario] -> String
listarUsuarios [] = "Nenhum usuário cadastrado."
listarUsuarios usuarios = unlines (map formatar usuarios)
  where
    formatar u = "Nome: " ++ nome u ++ " | Matrícula: " ++ show (matricula u) ++ " | E-mail: " ++ email u

-- Cria um novo usuário
criarUsuario :: String -> String -> String -> Maybe Usuario
criarUsuario n m e = do
    matInt <- readMaybe m :: Maybe Int
    return Usuario {nome = n, matricula = matInt, email = e}

-- Salvar no CSV
salvarUsuariosCSV :: FilePath -> [Usuario] -> IO ()
salvarUsuariosCSV arquivo usuarios = do
    let linhas = map (\u -> nome u ++ "," ++ show (matricula u) ++ "," ++ email u) usuarios
    writeFile arquivo (unlines linhas)

-- Ler do CSV

lerUsuariosCSV :: FilePath -> IO [Usuario]
lerUsuariosCSV arquivo = catch ler handleErro
  where
    ler = do
        conteudo <- readFile arquivo
        let linhas = filter (not . null) (lines conteudo)
        let usuariosParsed = mapMaybe linhaParaUsuario linhas
        return usuariosParsed

    handleErro :: IOException -> IO [Usuario]
    handleErro _ = return []

    linhaParaUsuario :: String -> Maybe Usuario
    linhaParaUsuario linha =
        case splitByDelimiter ',' linha of  -- <-- Usando splitByDelimiter
            [n, m, e] -> criarUsuario n m e
            _         -> Nothing
            
-- Adicionado para usar mapMaybe no lerUsuariosCSV
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f [] = []
mapMaybe f (x:xs) = case f x of
    Just y -> y : mapMaybe f xs
    Nothing -> mapMaybe f xs
