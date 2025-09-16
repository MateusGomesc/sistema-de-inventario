module ListaEspera where

import Types
import Listas
import RegistrarLog
import System.IO (hSetEncoding, stdout, utf8)
import Data.Char (toLower)
import Data.List (find)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Calendar (fromGregorian)
import Control.Monad (when)

obterCodigoItemParaEspera :: [Item] -> IO (Maybe Item)
obterCodigoItemParaEspera itens = do
    putStrLn "Digite o código do item:"
    codStr <- getLine
    let cod = read codStr :: Int
    case find (\i -> codigo i == cod) itens of
        Nothing -> do
            putStrLn "❌ Item não encontrado."
            return Nothing
        Just item ->
            if status item == Disponivel
                then do
                    putStrLn $ "✅ O item \"" ++ titulo item ++ "\" está disponível. Não é necessário entrar na lista de espera."
                    return Nothing
                else return (Just item)

adicionarNaEspera :: Int -> Int -> [Espera] -> ([Espera], Int)
adicionarNaEspera cod mat esperas =
    let atualizada = case find (\e -> espCodigoItem e == cod) esperas of
            Just e ->
                if mat `elem` lista e
                    then esperas  -- já está na fila
                    else map (\x -> if espCodigoItem x == cod then x { lista = lista x ++ [mat] } else x) esperas
            Nothing -> Espera cod [mat] : esperas
        posicao = case find (\e -> espCodigoItem e == cod) atualizada of
            Just e -> length (lista e)
            Nothing -> 1
    in (atualizada, posicao)

confirmarInclusaoEspera :: Item -> Usuario -> Int -> IO Bool
confirmarInclusaoEspera item usuario pos = do
    hSetEncoding stdout utf8
    putStrLn $ "\n" ++ negrito "Confirme os dados para inclusão na lista de espera:"
    putStrLn $ negrito "Item: " ++ titulo item ++ " (" ++ show (tipo item) ++ ")"
    putStrLn $ negrito "Usuário: " ++ nome usuario ++ " - Matrícula: " ++ matricula usuario
    putStrLn $ "Posição na fila: " ++ show pos
    putStrLn "Confirmar inclusão? (s/n)"
    resp <- getLine
    return (map toLower resp == "s")

incluirNaListaEspera :: [Item] -> [Usuario] -> [Espera] -> IO [Espera]
incluirNaListaEspera itens usuarios esperas = do
    putStrLn "[ INCLUIR NA LISTA DE ESPERA ]"
    mItem <- obterCodigoItemParaEspera itens
    case mItem of
        Nothing -> return esperas
        Just item -> do
            putStrLn "Digite a matrícula do usuário:"
            matStr <- getLine
            let mat = read matStr :: Int
            case find (\u -> read (matricula u) == mat) usuarios of
                Nothing -> do
                    putStrLn "❌ Usuário não encontrado."
                    return esperas
                Just usuario -> do
                    let (esperasAtualizadas, posicao) = adicionarNaEspera (codigo item) mat esperas
                    confirmado <- confirmarInclusaoEspera item usuario posicao
                    if confirmado
                        then do
                            putStrLn "\n✅ Usuário incluído na lista de espera com sucesso!"
                            registrarLog "Fila de espera (inclusão)" item usuario "Sucesso"
                            return esperasAtualizadas
                        else do
                            putStrLn "🚫 Inclusão cancelada."
                            registrarLog "Fila de espera (inclusão)" item usuario "Cancelado"
                            return esperas

negrito :: String -> String
negrito s = "\ESC[1m" ++ s ++ "\ESC[0m"

verificarFilaNaDevolucao :: Item -> [Espera] -> [Usuario] -> IO ()
verificarFilaNaDevolucao item esperas usuarios =
    case find (\e -> espCodigoItem e == codigo item && not (null (lista e))) esperas of
        Nothing -> return ()
        Just espera -> do
            let matPrimeiro = head (lista espera)
            case find (\u -> read (matricula u) == matPrimeiro) usuarios of
                Just usuario -> do
                    putStrLn $ "📧 Item com fila de espera. O primeiro usuário na fila (" ++ nome usuario ++ ") foi notificado por email: " ++ email usuario
                    registrarLog "Fila de espera (notificação)" item usuario "Sucesso"
                Nothing -> do
                    putStrLn "⚠️ Item com fila de espera, mas usuário não encontrado para notificação."
                    let usuarioFake = Usuario "Desconhecido" (show matPrimeiro) "sem@email.com"
                    registrarLog "Fila de espera (notificação)" item usuarioFake "Erro - Usuário não encontrado"


removerDaFilaSeForPrimeiro :: Item -> Usuario -> [Espera] -> IO [Espera]
removerDaFilaSeForPrimeiro item usuario esperas =
    let codItem = codigo item
        matUsuario = read (matricula usuario)
    in case find (\e -> espCodigoItem e == codItem) esperas of
        Nothing -> return esperas
        Just espera ->
            if not (null (lista espera)) && head (lista espera) == matUsuario
                then do
                    let novaFila = tail (lista espera)
                        esperasAtualizadas = map (\e -> if espCodigoItem e == codItem then e { lista = novaFila } else e) esperas
                    putStrLn "✅ Usuário era o primeiro da fila e foi removido da lista de espera."
                    registrarLog "Fila de espera (remoção)" item usuario "Sucesso"
                    return esperasAtualizadas
                else do
                    putStrLn "⚠️ Usuário não era o primeiro da fila. A posição na fila permanece."
                    registrarLog "Fila de espera (remoção)" item usuario "Erro - Usuário não era o primeiro da fila"
                    return esperas

