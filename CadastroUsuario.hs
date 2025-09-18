module CadastroUsuario where

-- Módulos padrão
import Control.Exception (evaluate, catch, IOException)
import Data.Char (toLower)
import Data.List (find, isInfixOf)
import System.IO
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)

-- Módulos do seu projeto
import Log (logOperation)
import Types

-- =================================
-- FUNÇÕES AUXILIARES E LÓGICA PURA
-- =================================

-- | Separa uma string numa lista de strings, usando um caractere como delimitador.
splitOn :: Char -> String -> [String]
splitOn separador str = go [] str
  where
    go acc []
      | null acc  = []
      | otherwise = [reverse acc]
    go acc (caractere : restoStr)
      | caractere == separador = (reverse acc) : go [] restoStr
      | otherwise = go (caractere : acc) restoStr

-- | Valida um e-mail de forma simples.
validarEmail :: String -> Bool
validarEmail str = '@' `elem` str && '.' `elem` str

-- | Mapeia uma linha CSV para um Maybe Usuario, tratando erros de conversão.
mapearParaUsuario :: [String] -> Maybe Usuario
mapearParaUsuario [nome, matriculaStr, email] =
    case readMaybe matriculaStr of
        Just mat -> Just (Usuario {nome = nome, matricula = mat, email = email})
        Nothing  -> Nothing
mapearParaUsuario _ = Nothing

-- | Converte um Usuario de volta para o formato de linha CSV.
usuarioParaLinhaCSV :: Usuario -> String
usuarioParaLinhaCSV usr = nome usr ++ "," ++ show (matricula usr) ++ "," ++ email usr

-- | Lógica pura para adicionar um usuário a uma lista, com validações.
adicionarUsuario :: Usuario -> [Usuario] -> Either String [Usuario]
adicionarUsuario novo usuarios
    | matriculaDuplicada = Left ("Erro: matrícula \"" ++ show (matricula novo) ++ "\" já cadastrada.")
    | emailInvalido      = Left ("Erro: e-mail \"" ++ email novo ++ "\" está mal formatado.")
    | otherwise          = Right (novo : usuarios)
  where
    matriculaDuplicada = any (\u -> matricula u == matricula novo) usuarios
    emailInvalido = not (validarEmail (email novo))

-- | Lógica pura para remover um usuário de uma lista.
removerUsuario :: Matricula -> [Usuario] -> Either String [Usuario]
removerUsuario mat usuarios
    | any (\u -> matricula u == mat) usuarios = Right (filter (\u -> matricula u /= mat) usuarios)
    | otherwise = Left ("Erro: matrícula \"" ++ show mat ++ "\" não encontrada.")

-- =================================
-- FUNÇÕES DE INTERAÇÃO COM FICHEIROS (IO)
-- =================================

-- | Define o caminho do ficheiro de utilizadores.
userFile :: FilePath
userFile = "Files/users.csv"

-- | Carrega os utilizadores do ficheiro CSV, ignorando linhas corrompidas.
carregarUsuarios :: IO [Usuario]
carregarUsuarios = catch ler handleErro
  where
    ler = do
        conteudo <- readFile userFile
        _ <- evaluate (length conteudo) -- Força o fecho do ficheiro
        let linhas = lines conteudo
        return (catMaybes (map (mapearParaUsuario . splitOn ',') linhas))
    handleErro :: IOException -> IO [Usuario]
    handleErro _ = return [] -- Retorna lista vazia se o ficheiro não existir

-- | Salva uma lista de utilizadores no ficheiro CSV, substituindo o conteúdo.
salvarUsuarios :: [Usuario] -> IO ()
salvarUsuarios usuarios = do
    let linhas = map usuarioParaLinhaCSV usuarios
    writeFile userFile (unlines linhas)

-- =================================
-- FUNÇÕES DE INTERFACE COM O UTILIZADOR (IO)
-- =================================

prompt :: String -> IO String
prompt msg = do
    putStr (msg ++ ": ")
    getLine

-- | Orquestra o processo de adicionar um novo utilizador.
uiAdicionarUsuario :: IO ()
uiAdicionarUsuario = do
    putStrLn "\n--- Adicionar Novo Utilizador ---"
    nomeStr <- prompt "Nome completo"
    matriculaStr <- prompt "Matrícula (número)"
    emailStr <- prompt "E-mail"

    case readMaybe matriculaStr of
        Nothing -> putStrLn "Erro: A matrícula deve ser um número."
        Just mat -> do
            let novoUsuario = Usuario {nome = nomeStr, matricula = mat, email = emailStr}
            
            usuariosAtuais <- carregarUsuarios
            
            case adicionarUsuario novoUsuario usuariosAtuais of
                Left erro -> putStrLn erro
                Right novosUsuarios -> do
                    salvarUsuarios novosUsuarios
                    putStrLn "Utilizador adicionado com sucesso!"
                    logOperation ("Cadastro do utilizador com matrícula " ++ show mat)

-- | Orquestra o processo de remover um utilizador.
uiRemoverUsuario :: IO ()
uiRemoverUsuario = do
    putStrLn "\n--- Remover Utilizador ---"
    matriculaStr <- prompt "Matrícula do utilizador a remover"

    case readMaybe matriculaStr of
        Nothing -> putStrLn "Erro: A matrícula deve ser um número."
        Just mat -> do
            usuariosAtuais <- carregarUsuarios
            
            case removerUsuario mat usuariosAtuais of
                Left erro -> putStrLn erro
                Right novosUsuarios -> do
                    salvarUsuarios novosUsuarios
                    putStrLn "Utilizador removido com sucesso!"
                    logOperation ("Remoção do utilizador com matrícula " ++ show mat)

-- | Carrega e lista todos os utilizadores cadastrados.
uiListarUsuarios :: IO ()
uiListarUsuarios = do
    putStrLn "\n--- Lista de Utilizadores Cadastrados ---"
    usuarios <- carregarUsuarios
    if null usuarios
        then putStrLn "Nenhum utilizador cadastrado."
        else mapM_ (putStrLn . formatarUsuario) usuarios
  where
    formatarUsuario u = "Matrícula: " ++ show (matricula u) ++ " | Nome: " ++ nome u ++ " | E-mail: " ++ email u
