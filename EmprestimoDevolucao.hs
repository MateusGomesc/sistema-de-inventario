--Chamar registrarEmprestimo com: (emprestimosAtualizados, itensAtualizados) <- registrarEmprestimo itens usuarios emprestimos

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
    return (resp == "s")


gerarEmprestimo :: Item -> Usuario -> Day -> [Emprestimo] -> [Item] -> ([Emprestimo], [Item])
gerarEmprestimo item usuario hoje emprestimos itens =
    let dias = case tipo item of
                    Filme -> 2
                    _     -> 5
        dataLimite = adicionarDiasUteis hoje dias
        novoEmp = Emprestimo (codigo item) (read $ matricula usuario) hoje dataLimite hoje
        itensAtualizados = atualizarStatusItem (codigo item) Emprestado itens
    in (adicionarEmprestimo emprestimos novoEmp, itensAtualizados)

registrarEmprestimo :: [Item] -> [Usuario] -> [Emprestimo] -> IO ([Emprestimo], [Item])
registrarEmprestimo itens usuarios emprestimos = do
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

negrito :: String -> String
negrito s = "\ESC[1m" ++ s ++ "\ESC[0m"
