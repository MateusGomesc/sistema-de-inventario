module CadastroItens where

import Types
import Data.List (delete)

-- 1. Adicionar um item
-- Recebe a lista atual de itens e o novo item, retorna a lista atualizada
adicionarItem :: [Item] -> Item -> [Item]
adicionarItem itens novoItem = novoItem : itens

-- 2. Remover um item pelo código
-- Recebe a lista de itens e o código, retorna a lista sem o item
removerItem :: [Item] -> Codigo -> [Item]
removerItem itens cod = filter (\i -> codigo i /= cod) itens

-- 3. Listar todos os itens
listarItens :: [Item] -> IO ()
listarItens [] = putStrLn "Nenhum item cadastrado."
listarItens itens = mapM_ printItem itens


