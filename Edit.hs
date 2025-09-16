module Edit where

import Types ( Usuario(..), Matricula, Item(..), TipoItem(..), StatusItem(..), Codigo )
import Text.Read ( readMaybe )
import Data.List(find, isInfixOf)
import Control.Exception(evaluate)
import Data.Char(toLower)

prompt :: String -> IO String
prompt msg = do
    putStr (msg ++ ": ")
    getLine

splitOn :: Char -> String -> [String]
splitOn separador str = go [] str
    where
        go acc []
            | null acc = []
            | otherwise = [reverse acc]
        go acc (caractere:restoStr)
            | caractere == separador = (reverse acc) : go [] restoStr
            | otherwise = go (caractere : acc) restoStr

mapearParaUsuario :: [String] -> Maybe Usuario
mapearParaUsuario [nome, matriculaStr, email] = 
    case readMaybe matriculaStr of
        Just mat -> Just (Usuario {nome = nome, matricula = mat, email = email})
        Nothing -> Nothing
mapearParaUsuario _ = Nothing

converterTipo :: String -> Maybe TipoItem
converterTipo "filme" = Just Filme
converterTipo "livro" = Just Livro
converterTipo "jogo" = Just Jogo
converterTipo _ = Nothing

converterStatus :: String -> Maybe StatusItem
converterStatus "emprestado" = Just Emprestado
converterStatus "disponivel" = Just Disponivel
converterStatus _ = Nothing

mapearParaItem :: [String] -> Maybe Item
mapearParaItem [codigoStr, titulo, autor, anoStr, tipoStr, statusStr] = 
    case readMaybe codigoStr of
        Just cod -> case readMaybe anoStr of
            Just ano -> case converterTipo (map toLower tipoStr) of
                Just tipo -> case converterStatus (map toLower statusStr) of
                    Just status -> Just (Item {codigo=cod, titulo=titulo, autor=autor, ano=ano, tipo=tipo, status=status})
                    Nothing -> Nothing
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing
mapearParaItem _ = Nothing

usuarioParaLinhaCSV :: Usuario -> String
usuarioParaLinhaCSV usr = nome usr ++ "," ++ show (matricula usr) ++ "," ++ email usr

itemParaLinhaCSV :: Item -> String
itemParaLinhaCSV item = (show $ codigo item) ++ "," ++ titulo item ++ "," ++ autor item ++ "," ++ (show $ ano item) ++ "," ++ (show $ tipo item) ++ "," ++ (show $ status item)


validarEmail :: String -> Bool
validarEmail str = '@' `elem` str && '.' `elem` str

encontrarLinhaUsuario :: Matricula -> [String] -> Maybe (Int, String)
encontrarLinhaUsuario mat allLines = let indexedLines = zip [1..] allLines 
                                    in find (\(_, line) -> isInfixOf (show mat) line) indexedLines

encontrarLinhaItem :: Codigo -> [String] -> Maybe (Int, String)
encontrarLinhaItem cod allLines = let indexedLines = zip [1..] allLines
                                in find (\(_, line) -> isInfixOf (show cod) line) indexedLines

substituirLinha :: Int -> String -> [String] -> [String]
substituirLinha lineNum newLine allLines = let (antes, _:depois) = splitAt (lineNum - 1) allLines
                                        in antes ++ [newLine] ++ depois

mostrarUsuario :: Usuario -> IO ()
mostrarUsuario user = do
    putStrLn ""
    putStrLn "======= Usuário ======"
    putStrLn ""
    putStrLn $ "Nome: " ++ nome user
    putStrLn $ "Email: " ++ email user
    putStrLn ""
    putStrLn "======================"
    putStrLn ""

mostrarItem :: Item -> IO ()
mostrarItem item = do
    putStrLn ""
    putStrLn "========= Item ========"
    putStrLn ""
    putStrLn $ "Título: " ++ titulo item
    putStrLn $ "Autor: " ++ autor item
    putStrLn $ "Ano: " ++ (show $ ano item)
    putStrLn $ "Tipo: " ++ (show $ tipo item)
    putStrLn $ "Status: " ++ (show $ status item)
    putStrLn ""
    putStrLn "======================"
    putStrLn ""

editarUsuario :: IO ()
editarUsuario = do
    matriculaStr <- prompt "Matrícula"
    case readMaybe matriculaStr of
        Nothing -> putStrLn "Erro: Matrícula inválida. Por favor, insira um número."
        Just mat -> do
            conteudo <- readFile "Files/users.csv"
            _ <- evaluate (length conteudo)
            let linhas = lines conteudo
            case encontrarLinhaUsuario mat linhas of
                Nothing -> putStrLn $ "Erro: Usuário com matrícula " ++ show mat ++ " não encontrado."
                Just (numLinha, linhaEncontrado) -> do
                    case mapearParaUsuario (splitOn ',' linhaEncontrado) of
                        Nothing -> putStrLn "Erro: a linha de dados do usuário desejado esta corrompida."
                        Just usuarioAtual -> do
                            mostrarUsuario usuarioAtual
                            putStrLn "Escolha o campo para editar:"
                            putStrLn "1 - Nome"
                            putStrLn "2 - E-mail"
                            opcao <- prompt "Opção"
                            processarEdicaoUser numLinha usuarioAtual linhas opcao

editarItem :: IO ()
editarItem = do
    codigoStr <- prompt "Código"
    case readMaybe codigoStr of
        Nothing -> putStrLn "Erro: Código inválido. Por favor, insira um número."
        Just cod -> do
            conteudo <- readFile "Files/itens.csv"
            _ <- evaluate (length conteudo)
            let linhas = lines conteudo
            case encontrarLinhaItem cod linhas of
                Nothing -> putStrLn $ "Erro: Item com código " ++ show cod ++ " não encontrado."
                Just (numLinha, linhaEncontrado) -> do
                    case mapearParaItem (splitOn ',' linhaEncontrado) of
                        Nothing -> putStrLn "Erro: a linha de dados do item desejado esta corrompida."
                        Just itemAtual -> do
                            mostrarItem itemAtual
                            putStrLn "Escolha o campo para editar:"
                            putStrLn "1 - Título"
                            putStrLn "2 - Autor"
                            putStrLn "3 - Ano"
                            putStrLn "4 - Tipo"
                            putStrLn "5 - Status"
                            opcao <- prompt "Opção"
                            processarEdicaoItem numLinha itemAtual linhas opcao

processarEdicaoUser :: Int -> Usuario -> [String] -> String -> IO ()
processarEdicaoUser numLinha usr linhas "1" = do
    novoNome <- prompt "Novo nome"
    let userAtualizado = usr {nome = novoNome}
    confirmarEAtualizarUser numLinha userAtualizado linhas
processarEdicaoUser numLinha usr linhas "2" = do
    novoEmail <- prompt "Novo Email"
    if not (validarEmail novoEmail)
        then putStrLn "Erro: formato de e-mail inválido."
        else do
            let userAtualizado = usr {email = novoEmail}
            confirmarEAtualizarUser numLinha userAtualizado linhas
processarEdicaoUser _ _ _ _ = putStrLn "Opção inválida!"

processarEdicaoItem :: Int -> Item -> [String] -> String -> IO ()
processarEdicaoItem numLinha item linhas "1" = do
    novoTitulo <- prompt "Novo título"
    let itemAtualizado = item {titulo = novoTitulo}
    confirmarEAtualizarItem numLinha itemAtualizado linhas
processarEdicaoItem numLinha item linhas "2" = do
    novoAutor <- prompt "Novo autor"
    let itemAtualizado = item {autor = novoAutor}
    confirmarEAtualizarItem numLinha itemAtualizado linhas
processarEdicaoItem numLinha item linhas "3" = do
    novoAno <- prompt "Novo ano"
    case readMaybe novoAno of
        Just ano -> do
            let itemAtualizado = item {ano = ano}
            confirmarEAtualizarItem numLinha itemAtualizado linhas
        Nothing -> putStrLn "Erro: ano inválido. A edição foi cancelada."
processarEdicaoItem numLinha item linhas "4" = do
    novoTipo <- prompt "Novo tipo"
    case converterTipo (map toLower novoTipo) of
        Just tipo -> do
            let itemAtualizado = item {tipo = tipo}
            confirmarEAtualizarItem numLinha itemAtualizado linhas
        Nothing -> putStrLn "Erro: tipo inválido. A edição foi cancelada."
processarEdicaoItem numLinha item linhas "5" = do
    novoStatus <- prompt "Novo Status"
    case converterStatus (map toLower novoStatus) of
        Just status -> do
            let itemAtualizado = item {status = status}
            confirmarEAtualizarItem numLinha itemAtualizado linhas
        Nothing -> putStrLn "Erro: status inválido. A edição foi cancelada."
processarEdicaoItem _ _ _ _ = putStrLn "Opção inválida!"

confirmarEAtualizarUser :: Int -> Usuario -> [String] -> IO ()
confirmarEAtualizarUser numLinha usrAtualizado linhas = do
    confirmacao <- prompt "Confirmar edição (s/n)"
    if confirmacao `elem` ["S", "s"]
        then do
            let novaLinha = usuarioParaLinhaCSV usrAtualizado
            let novoConteudo = unlines (substituirLinha numLinha novaLinha linhas)
            writeFile "Files/users.csv" novoConteudo
            putStrLn "Usuário atualizado com sucesso!"
        else putStrLn "Edição cancelada pelo usuário."

confirmarEAtualizarItem :: Int -> Item -> [String] -> IO ()
confirmarEAtualizarItem numLinha itemAtualizado linhas = do
    confirmacao <- prompt "Confirmar edição (s/n)"
    if confirmacao `elem` ["S", "s"]
        then do
            let novaLinha = itemParaLinhaCSV itemAtualizado
            let novoConteudo = unlines (substituirLinha numLinha novaLinha linhas)
            writeFile "Files/itens.csv" novoConteudo
            putStrLn "Item atualizado com sucesso!"
        else putStrLn "Edição cancelada pelo usuário."
