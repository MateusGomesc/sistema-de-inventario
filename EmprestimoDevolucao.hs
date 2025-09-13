module EmprestimoDevolucao where

--Chamar registrarEmprestimo com: (emprestimosAtualizados, itensAtualizados) <- registrarEmprestimo itens usuarios emprestimos
--Chamar registrarDevolucao com: (emprestimos, itens) <- registrarDevolucao itens usuarios emprestimos

import Types
import System.IO
import Data.Time.Calendar
import Data.Time.Clock
import Data.List (find)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Calendar (fromGregorian)
import Control.Monad (when)
import Data.Char (toLower)

obterCodigoItem :: [Item] -> IO (Maybe Item)
obterCodigoItem itens = do
    putStrLn "Digite o código do item:"
    codStr <- getLine
    let cod = read codStr :: Int
    case find (\i -> codigo i == cod) itens of
        Nothing -> do
            putStrLn "❌ Item não encontrado."
            return Nothing
        Just item ->
            if status item /= Disponivel
                then do
                    putStrLn $ "⚠️ O item \"" ++ titulo item ++ "\" está indisponível."
                    return Nothing
                else return (Just item)

obterMatriculaUsuario :: [Usuario] -> IO (Maybe Usuario)
obterMatriculaUsuario usuarios = do
    putStrLn "Digite a matrícula do usuário:"
    matStr <- getLine
    let mat = read matStr :: Int
    return $ find (\u -> read (matricula u) == mat) usuarios

confirmarEmprestimo :: Item -> Usuario -> IO Bool
confirmarEmprestimo item usuario = do
    putStrLn $ "\n" ++ negrito "Confirme os dados:"
    putStrLn $ negrito "Item: " ++ titulo item ++ " (" ++ show (tipo item) ++ ")"
    putStrLn $ negrito "Usuário: " ++ nome usuario ++ " - Matrícula: " ++ matricula usuario
    putStrLn "Confirmar empréstimo? (s/n)"
    resp <- getLine
    return (map toLower resp == "s")


gerarEmprestimo :: Item -> Usuario -> Day -> [Emprestimo] -> [Item] -> ([Emprestimo], [Item])
gerarEmprestimo item usuario hoje emprestimos itens =
    let dias = case tipo item of
                    Filme -> 2
                    _     -> 5
        dataLimite = adicionarDiasUteis hoje dias
        novoEmp = Emprestimo (codigo item) (read $ matricula usuario) hoje dataLimite (fromGregorian 0000 1 1)
        itensAtualizados = atualizarStatusItem (codigo item) Emprestado itens
    in (adicionarEmprestimo emprestimos novoEmp, itensAtualizados)

registrarEmprestimo :: [Item] -> [Usuario] -> [Emprestimo] -> IO ([Emprestimo], [Item])
registrarEmprestimo itens usuarios emprestimos = do
    putStrLn "[ REGISTRAR EMPRÉSTIMO ]"
    mItem <- obterCodigoItem itens
    case mItem of
        Nothing -> return (emprestimos, itens)
        Just item -> do
            mUsuario <- obterMatriculaUsuario usuarios
            case mUsuario of
                Nothing -> do
                    putStrLn "❌ Usuário não encontrado."
                    return (emprestimos, itens)
                Just usuario -> do
                    confirmado <- confirmarEmprestimo item usuario
                    if confirmado
                        then do
                            filaDeEspera <- removerDaFilaSeForPrimeiro (codigo item) (read (matricula usuario)) filaDeEspera
                            hoje <- utctDay <$> getCurrentTime
                            let (novosEmprestimos, itensAtualizados) =
                                    gerarEmprestimo item usuario hoje emprestimos itens
                            putStrLn "\n✅ Empréstimo registrado com sucesso!"
                            putStrLn $ "📅 Data do empréstimo: " ++ show hoje
                            putStrLn $ "📅 Data limite para devolução: " ++ show (dataEsperadaDevolucao $ head novosEmprestimos)
                            return (novosEmprestimos, itensAtualizados)
                        else do
                            putStrLn "🚫 Empréstimo cancelado."
                            return (emprestimos, itens)

atualizarStatusItem :: Int -> StatusItem -> [Item] -> [Item]
atualizarStatusItem cod novoStatus =
    map (\item -> if codigo item == cod then item { status = novoStatus } else item)


-- Adiciona N dias úteis (ignorando finais de semana)
adicionarDiasUteis :: Day -> Int -> Day
adicionarDiasUteis dia n = go dia 0
  where
    go d count
        | count == n = d
        | otherwise =
            let d' = addDays 1 d
                (_, _, wd) = toWeekDate d'
            in if wd < 6 then go d' (count + 1) else go d' count

diasPorTipo :: TipoItem -> Int
diasPorTipo Filme = 2
diasPorTipo _     = 5

adicionarEmprestimo :: [Emprestimo] -> Emprestimo -> [Emprestimo]
adicionarEmprestimo lista novo = novo : lista


obterCodigoItemParaDevolucao :: [Emprestimo] -> IO (Maybe Emprestimo)
obterCodigoItemParaDevolucao emprestimos = do
    putStrLn "[ REGISTRAR DEVOLUÇÃO ]"
    putStrLn "Digite o código do item a devolver:"
    codStr <- getLine
    let codItem = read codStr :: Int
    let mEmp = find (\e -> empCodigoItem e == codItem && dataEfetuadaDevolucao e == fromGregorian 0000 1 1) emprestimos
    if mEmp == Nothing
        then putStrLn "❌ Nenhum empréstimo ativo encontrado para este item."
        else return ()
    return mEmp

verificarAtraso :: Emprestimo -> Day -> IO ()
verificarAtraso emp hoje = do
    let atraso = diffDays hoje (dataEsperadaDevolucao emp)
    when (atraso > 0) $
        putStrLn $ "⚠️ Devolução atrasada em " ++ show atraso ++ " dias"

confirmarDevolucao :: Item -> Usuario -> IO Bool
confirmarDevolucao item usuario = do
    putStrLn $ "\n" ++ negrito "Confirme os dados da devolução:"
    putStrLn $ negrito "Item: " ++ titulo item ++ " (" ++ show (tipo item) ++ ")"
    putStrLn $ negrito "Usuário: " ++ nome usuario ++ " - Matrícula: " ++ matricula usuario
    putStrLn "Confirmar devolução? (s/n)"
    resp <- getLine
    return (map toLower resp == "s")

gerarDevolucao :: Int -> Day -> [Emprestimo] -> [Item] -> ([Emprestimo], [Item])
gerarDevolucao cod hoje emprestimos itens =
    let emprestimosAtualizados = map (atualizarDevolucao cod hoje) emprestimos
        itensAtualizados = atualizarStatusItem cod Disponivel itens
    in (emprestimosAtualizados, itensAtualizados)


registrarDevolucao :: [Item] -> [Usuario] -> [Emprestimo] -> IO ([Emprestimo], [Item])
registrarDevolucao itens usuarios emprestimos = do
    mEmp <- obterCodigoItemParaDevolucao emprestimos
    case mEmp of
        Nothing -> return (emprestimos, itens)
        Just emp -> do
            hoje <- utctDay <$> getCurrentTime
            verificarAtraso emp hoje

            let mItem = find (\i -> codigo i == empCodigoItem emp) itens
            let mUsuario = find (\u -> read (matricula u) == matriculaUsuario emp) usuarios

            case (mItem, mUsuario) of
                (Just item, Just usuario) -> do
                    confirmado <- confirmarDevolucao item usuario
                    if confirmado
                        then do
                            let (emprestimosAtualizados, itensAtualizados) =
                                    gerarDevolucao (empCodigoItem emp) hoje emprestimos itens
                            verificarFilaNaDevolucao (codigo item) filaDeEspera usuarios
                            putStrLn "\n✅ Devolução registrada com sucesso!"
                            putStrLn $ "📅 Data da devolução: " ++ show hoje
                            return (emprestimosAtualizados, itensAtualizados)
                        else do
                            putStrLn "🚫 Devolução cancelada."
                            return (emprestimos, itens)
                _ -> do
                    putStrLn "❌ Dados do item ou usuário não encontrados."
                    return (emprestimos, itens)


atualizarDevolucao :: Int -> Day -> Emprestimo -> Emprestimo
atualizarDevolucao cod hoje emp
    | empCodigoItem emp == cod = emp { dataEfetuadaDevolucao = hoje }
    | otherwise = emp
