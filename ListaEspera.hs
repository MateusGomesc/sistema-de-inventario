module ListaEspera where

import System.IO
import Types
import Data.Char (toLower)
import Data.List (find)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
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
                            return esperasAtualizadas
                        else do
                            putStrLn "🚫 Inclusão cancelada."
                            return esperas

negrito :: String -> String
negrito s = "\ESC[1m" ++ s ++ "\ESC[0m"

verificarFilaNaDevolucao :: Int -> [Espera] -> [Usuario] -> IO ()
verificarFilaNaDevolucao codItem esperas usuarios =
    case find (\e -> espCodigoItem e == codItem && not (null (lista e))) esperas of
        Nothing -> return ()
        Just espera -> do
            let matPrimeiro = head (lista espera)
            case find (\u -> read (matricula u) == matPrimeiro) usuarios of
                Just usuario -> putStrLn $ "📧 Item com fila de espera. O primeiro usuário na fila (" ++ nome usuario ++ ") foi notificado por email: " ++ email usuario
                Nothing -> putStrLn "⚠️ Item com fila de espera, mas usuário não encontrado para notificação."


removerDaFilaSeForPrimeiro :: Int -> Int -> [Espera] -> IO [Espera]
removerDaFilaSeForPrimeiro codItem matUsuario esperas =
    case find (\e -> espCodigoItem e == codItem) esperas of
        Nothing -> return esperas
        Just espera ->
            if not (null (lista espera)) && head (lista espera) == matUsuario
                then do
                    let novaFila = tail (lista espera)
                        esperasAtualizadas = map (\e -> if espCodigoItem e == codItem then e { lista = novaFila } else e) esperas
                    putStrLn "✅ Usuário era o primeiro da fila e foi removido da lista de espera."
                    return esperasAtualizadas
                else do
                    putStrLn "⚠️ Usuário não era o primeiro da fila. A posição na fila permanece."
                    return esperas
