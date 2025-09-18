module Relatorios where

import Data.Time.Calendar
import Data.List (group, sortOn, sortBy)
import Data.Function (on)
import System.IO
import Types
import Listas

-- (a) Listar empréstimos ativos (por categoria)
---------------------------------

-- Função principal para listar empréstimos ativos, agrupados por TipoItem
listarEmprestimosAtivos :: [Emprestimo] -> [Item] -> [Usuario] -> IO ()
listarEmprestimosAtivos emprestimos itens usuarios = do
    putStrLn "Empréstimos ativos - Livros"
    putStrLn "---------------------------"
    imprimirEmprestimosPorTipo Livro ativosLivros
    putStrLn ""
    putStrLn "Empréstimos ativos - Filmes"
    putStrLn "---------------------------"
    imprimirEmprestimosPorTipo Filme ativosFilmes
    putStrLn ""
    putStrLn "Empréstimos ativos - Jogos"
    putStrLn "--------------------------"
    imprimirEmprestimosPorTipo Jogo ativosJogos
  where
    emprestimosAtivos = filter (\e -> dataEfetuadaDevolucao e == fromGregorian 1 1 1) emprestimos
    ativosLivros = filtrarEmprestimosAtivosPorTipo Livro emprestimosAtivos itens usuarios
    ativosFilmes = filtrarEmprestimosAtivosPorTipo Filme emprestimosAtivos itens usuarios
    ativosJogos = filtrarEmprestimosAtivosPorTipo Jogo emprestimosAtivos itens usuarios

filtrarEmprestimosAtivosPorTipo :: TipoItem -> [Emprestimo] -> [Item] -> [Usuario] -> [(Item, Usuario, Emprestimo)]
filtrarEmprestimosAtivosPorTipo tipoEmprestimo emprestimosAtivos itens usuarios =
    [ (item, usuario, emp)
    | emp <- emprestimosAtivos
    , item <- itens
    , usuario <- usuarios
    , empCodigoItem emp == codigo item
    , matriculaUsuario emp == matricula usuario
    , tipo item == tipoEmprestimo
    ]

-- **teste para correção de erro de tipo**
imprimirEmprestimosPorTipo :: TipoItem -> [(Item, Usuario, Emprestimo)] -> IO ()
imprimirEmprestimosPorTipo tipo emprestimosAtivos = do
    if null emprestimosAtivos
        then putStrLn "  Nenhum empréstimo ativo nesta categoria."
        else mapM_ (\(item, usuario, emprestimo) -> do
            putStrLn $ "  - Título: " ++ titulo item
            putStrLn $ "    Usuário: " ++ nome usuario ++ " (mat. " ++ show (matricula usuario) ++ ")"
            putStrLn $ "    Data de Empréstimo: " ++ show (dataEmprestimo emprestimo)
          ) emprestimosAtivos

-- (b) Históricos detalhados de usuário
---------------------------------

-- dois tipos novos para representar eventos no histórico (colocar no types.hs)
data Operacao = EmprestimoOp | DevolucaoOp | RenovacaoOp | AtrasoOp
    deriving (Show, Eq)

data EventoHistorico = EventoHistorico {
    dataEvento :: Day,
    detalhes :: String
} deriving (Show, Eq)

-- Função auxiliar para encontrar o nome do usuário
encontrarNomeUsuario :: Matricula -> [Usuario] -> String
encontrarNomeUsuario mat usuarios =
    case filter (\u -> matricula u == mat) usuarios of
        (u:_) -> nome u
        _ -> "Usuário não encontrado"

-- A função abaixo gera o histórico de um usuário específico, a partir de todos os empréstimos
historicoUsuario :: Matricula -> [Emprestimo] -> [Item] -> [Usuario] -> IO ()
historicoUsuario mat emprestimos itens usuarios = do
    putStrLn $ "Histórico do usuário: " ++ encontrarNomeUsuario mat usuarios ++ " (matrícula " ++ show mat ++ ")"
    putStrLn "-----------------------------"
    mapM_ (putStrLn . formatarEvento) historicoOrdenado
  where
    emprestimosUsuario = filter (\e -> matriculaUsuario e == mat) emprestimos
    
    encontrarNomeItem cod =
        case filter (\i -> codigo i == cod) itens of
            (i:_) -> titulo i
            _ -> "Item não encontrado"

    eventos = concatMap (\e ->
        let nomeItem = encontrarNomeItem (empCodigoItem e)
            dataEmp = dataEmprestimo e
            detalheEmp = "Empréstimo do item \"" ++ nomeItem ++ "\""
            eventoEmp = EventoHistorico dataEmp detalheEmp
            
            dataDev = dataEfetuadaDevolucao e
            detalheDev = "Devolução do item \"" ++ nomeItem ++ "\""
            eventoDev = EventoHistorico dataDev detalheDev
        in 
            if dataDev == fromGregorian 1 1 1
                then [eventoEmp]
                else [eventoEmp, eventoDev]
      ) emprestimosUsuario

    historicoOrdenado = sortBy (compare `on` dataEvento) eventos

    formatarEvento (EventoHistorico dataEv detalhe) =
        "[" ++ show dataEv ++ "] " ++ detalhe

-- (c) Itens com listas de espera não vazias
---------------------------------

-- Função que lista itens com listas de espera não vazias
-- recebe a lista de 'Espera', 'Itens' e 'Usuarios'
relatorioItensComEspera :: [Espera] -> [Item] -> [Usuario] -> IO ()
relatorioItensComEspera listasEspera itens usuarios = do
    putStrLn "Itens com lista de espera"
    putStrLn "-------------------------"
    mapM_ imprimirItemComEspera (itensComEspera listasEspera itens usuarios)

itensComEspera :: [Espera] -> [Item] -> [Usuario] -> [(Item, [Usuario])]
itensComEspera listasEspera itens usuarios =
    [ (item, usuariosEmEspera)
    | espera <- filter (not . null . lista) listasEspera
    , item <- filter (\i -> codigo i == espCodigoItem espera) itens
    , let usuariosEmEspera = [u | u <- usuarios, matricula u `elem` lista espera]
    ]

imprimirItemComEspera :: (Item, [Usuario]) -> IO ()
imprimirItemComEspera (item, usuarios) = do
    putStrLn $ show (tipo item) ++ ": \"" ++ titulo item ++ "\" (Código " ++ show (codigo item) ++ ")"
    putStrLn "Lista de espera:"
    mapM_ imprimirUsuario (zip [1..] usuarios)
  where
    imprimirUsuario (idx, user) =
        putStrLn $ "  " ++ show idx ++ ". " ++ nome user ++ " (mat. " ++ show (matricula user) ++ ")"

-- (d) Relatório de uso do sistema
---------------------------------

-- Função principal dos relatorios
relatorioUsoSistema :: [Emprestimo] -> [Item] -> [Usuario] -> IO ()
relatorioUsoSistema emprestimos itens usuarios = do
    putStrLn "Relatório de uso do sistema"
    putStrLn "--------------------------"
    putStrLn "Operações por tipo de item:"
    imprimirOperacoesPorTipoItem emprestimos itens
    putStrLn ""
    putStrLn "Operações por usuário:"
    imprimirOperacoesPorUsuario emprestimos usuarios

imprimirOperacoesPorTipoItem :: [Emprestimo] -> [Item] -> IO ()
imprimirOperacoesPorTipoItem emprestimos itens = do
    let emprestimosFilmes = filtrarPorTipo Filme emprestimos itens
    let emprestimosLivros = filtrarPorTipo Livro emprestimos itens
    let emprestimosJogos  = filtrarPorTipo Jogo emprestimos itens
    putStrLn $ "- Livros: " ++ show (length emprestimosLivros) ++ " empréstimos, " ++ show (contarDevolucoes emprestimosLivros) ++ " devoluções"
    putStrLn $ "- Filmes: " ++ show (length emprestimosFilmes) ++ " empréstimos, " ++ show (contarDevolucoes emprestimosFilmes) ++ " devoluções"
    putStrLn $ "- Jogos: "  ++ show (length emprestimosJogos)  ++ " empréstimos, " ++ show (contarDevolucoes emprestimosJogos) ++ " devoluções"
  where
    filtrarPorTipo tipoItem emprestimos' itens' =
        [e | e <- emprestimos', item <- itens', codigo item == empCodigoItem e, tipo item == tipoItem]
    contarDevolucoes emprestimos' = length $ filter (\e -> dataEfetuadaDevolucao e /= fromGregorian 1 1 1) emprestimos'

imprimirOperacoesPorUsuario :: [Emprestimo] -> [Usuario] -> IO ()
imprimirOperacoesPorUsuario emprestimos usuarios =
    let
        -- Cria uma lista de pares matricula, 1 por emprestimo
        operacoesPorUsuario = map (\e -> (matriculaUsuario e, 1)) emprestimos
        -- Agrupa e soma as operações por matrícula
        operacoesAgrupadas = group $ sortOn fst operacoesPorUsuario
        totalOperacoes = map (\g -> (fst (head g), length g)) operacoesAgrupadas
        
        -- Mapeia a matrícula para o nome do usuário para a impressão
        saidaFormatada = map (\(mat, count) ->
            let nomeUsuario = encontrarNomeUsuario mat usuarios
            in "- " ++ nomeUsuario ++ ": " ++ show count ++ " operações"
            ) totalOperacoes
    in mapM_ putStrLn saidaFormatada

-- Exemplos para testes
livroHaskell = Item 1002 "Aprendendo Haskell" "Carlos" 2024 Livro Emprestado
filme2001 = Item 9001 "2001: Uma Odisseia" "Stanley" 1968 Filme Emprestado
jogoWitness = Item 3003 "The Witness" "Jonathan" 2016 Jogo Emprestado
livroEstruturas = Item 1010 "Estruturas de Dados" "Ana" 2020 Livro Disponivel
filmeInterstelar = Item 9004 "Interestelar" "Chris" 2014 Filme Disponivel
jogoCeleste = Item 3002 "Celeste" "Matt" 2018 Jogo Emprestado


ana = Usuario "Ana Souza" 20230001 "ana@email.com"
bruno = Usuario "Bruno Lima" 20231300 "bruno@email.com"
carla = Usuario "Carla Dias" 20232201 "carla@email.com"

emprestimoAna = Emprestimo 9004 (matricula ana) (fromGregorian 2025 9 10) (fromGregorian 2025 9 17) (fromGregorian 1 1 1)
emprestimoBruno = Emprestimo 3003 (matricula bruno) (fromGregorian 2025 9 9) (fromGregorian 2025 9 16) (fromGregorian 1 1 1)
emprestimoCarla = Emprestimo 1002 (matricula carla) (fromGregorian 2025 9 1) (fromGregorian 2025 9 8) (fromGregorian 1 1 1)
emprestimoCarla2 = Emprestimo 3002 (matricula carla) (fromGregorian 2025 9 8) (fromGregorian 2025 9 15) (fromGregorian 1 1 1)
devolucaoCarla = Emprestimo 1002 (matricula carla) (fromGregorian 2025 9 1) (fromGregorian 2025 9 8) (fromGregorian 2025 9 8)

listaEsperaLivro = Espera (codigo livroEstruturas) [matricula ana, matricula bruno]
listaEsperaFilme = Espera (codigo filmeInterstelar) [matricula carla]

-- Exemplos:
dbItens = [livroHaskell, filme2001, jogoWitness, livroEstruturas, filmeInterstelar, jogoCeleste]
dbUsuarios = [ana, bruno, carla]
dbEmprestimos = [emprestimoAna, emprestimoBruno, emprestimoCarla, emprestimoCarla2, devolucaoCarla]
dbListasEspera = [listaEsperaLivro, listaEsperaFilme]

-- Como usar: exemplos na hora de colocar no menu tem q usar com argumentos certos
-- > listarEmprestimosAtivos dbEmprestimos dbItens dbUsuarios
-- > historicoUsuario (matricula carla) dbEmprestimos dbItens dbUsuarios
-- > relatorioItensComEspera dbListasEspera dbItens dbUsuarios
-- > relatorioUsoSistema dbEmprestimos dbItens dbUsuarios
