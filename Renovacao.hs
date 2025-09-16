module Renovacao where

-- Chamar com: emprestimos <- renovarEmprestimo itens usuarios emprestimos filaDeEspera

import Types
import Listas 
import ListaEspera
import EmprestimoDevolucao
import RegistrarLog 
import System.IO (putStrLn, getLine)
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar (Day, addDays, diffDays, fromGregorian)
import Data.List (find)
import Data.Char (toLower)
import Control.Monad (when)


buscarEmprestimoAtivo :: Int -> Int -> [Emprestimo] -> Maybe Emprestimo
buscarEmprestimoAtivo cod mat =
    find (\e -> empCodigoItem e == cod && matriculaUsuario e == mat && dataEfetuadaDevolucao e == fromGregorian 0000 1 1)

itemTemFila :: Int -> [Espera] -> Bool
itemTemFila cod =
    any (\e -> espCodigoItem e == cod && not (null (lista e)))

calcularNovaData :: TipoItem -> Day -> Day
calcularNovaData tipo hoje =
    let dias = case tipo of
                    Filme -> 2
                    Jogo  -> 2
                    Livro -> 5
    in adicionarDiasUteis hoje dias

atualizarRenovacao :: Int -> Int -> Day -> [Emprestimo] -> [Emprestimo]
atualizarRenovacao cod mat novaData =
    map (\e -> if empCodigoItem e == cod && matriculaUsuario e == mat
               then e { dataEsperadaDevolucao = novaData }
               else e)



renovarEmprestimo :: [Item] -> [Usuario] -> [Emprestimo] -> [Espera] -> IO [Emprestimo]
renovarEmprestimo itens usuarios emprestimos esperas = do
    putStrLn "[ RENOVAR EMPRÉSTIMO ]"
    putStrLn "Digite o código do item:"
    codStr <- getLine
    let codItem = read codStr :: Int

    case find (\i -> codigo i == codItem) itens of
        Nothing -> do
            putStrLn "❌ Item não encontrado."
            return emprestimos
        Just item -> do
            putStrLn "Digite a matrícula do usuário:"
            matStr <- getLine
            let matUsuario = read matStr :: Int

            case buscarEmprestimoAtivo codItem matUsuario emprestimos of
                Nothing -> do
                    putStrLn "❌ Empréstimo ativo não encontrado. Sugestão: realize um novo empréstimo."
                    return emprestimos
                Just emp -> do
                    if itemTemFila codItem esperas
                        then do
                            putStrLn "🚫 Este item possui lista de espera. Renovações não são permitidas."
                            let usuarioFake = Usuario "Desconhecido" "0000" "sem@email.com"
                            registrarLog "Renovação" item usuarioFake "Erro - Item com file de espera"
                            return emprestimos
                        else do
                            hoje <- utctDay <$> getCurrentTime
                            let novaData = calcularNovaData (tipo item) hoje
                            let mUsuario = find (\u -> read (matricula u) == matUsuario) usuarios

                            case mUsuario of
                                Nothing -> do
                                    putStrLn "❌ Usuário não encontrado."
                                    return emprestimos
                                Just usuario -> do
                                    let atraso = diffDays hoje (dataEsperadaDevolucao emp)
                                    when (atraso > 0) $
                                        putStrLn $ "⚠️ Atenção: este empréstimo está atrasado em " ++ show atraso ++ " dias."

                                    putStrLn $ "\n" ++ negrito "Confirme os dados da renovação:"
                                    putStrLn $ negrito "Item: " ++ titulo item ++ " (" ++ show (tipo item) ++ ")"
                                    putStrLn $ negrito "Usuário: " ++ nome usuario ++ " - Matrícula: " ++ matricula usuario
                                    putStrLn $ "Data atual de devolução: " ++ show (dataEsperadaDevolucao emp)
                                    putStrLn $ "Nova data de devolução: " ++ show novaData
                                    putStrLn "Confirmar renovação? (s/n)"
                                    resp <- getLine

                                    if map toLower resp == "s"
                                        then do
                                            let emprestimosAtualizados = atualizarRenovacao codItem matUsuario novaData emprestimos
                                            putStrLn "\n✅ Empréstimo renovado com sucesso!"
                                            putStrLn $ "📅 Nova data de devolução: " ++ show novaData
                                            registrarLog "Renovação" item usuario "Sucesso"
                                            return emprestimosAtualizados
                                        else do
                                            putStrLn "🚫 Renovação cancelada."
                                            registrarLog "Renovação" item usuario "Cancelado"
                                            return emprestimos

