### Sistema de Inventário

## Funções


## Cadastro de itens 

```haskell
mapearParaItem :: [String] -> Maybe Item
```
Recebe uma lista de strings representando os campos de um item no CSV.<br/>
Tenta converter os campos numéricos (código, ano, tipo, status) para seus respectivos tipos.<br/>
Se todas as conversões forem bem-sucedidas, retorna Just Item com os dados.<br/>
Caso contrário, retorna Nothing indicando erro no parsing.<br/>

```haskell
encontrarLinhaItem :: Codigo -> [String] -> Maybe (Int, String)
```
Recebe um código e uma lista de linhas do arquivo CSV.<br/>
Percorre cada linha, tentando convertê-la em um Item.<br/>
Compara o código informado com o código do item da linha.<br/>
Se encontrar, retorna Just (n, linha) onde n é o índice da linha.<br/>
Se não encontrar, retorna Nothing.<br/>

```haskell
adicionarItem :: IO ()
```
Exibe um menu para cadastrar um novo item.<br/>
Solicita ao usuário código, título, autor, ano e tipo do item.<br/>
Verifica se o código já existe no arquivo antes de cadastrar.<br/>
Caso o código seja único e os dados válidos, grava uma nova linha no CSV com o item.<br/>
Exibe mensagens de erro quando os dados inseridos forem inválidos.<br/>





## Cadastro de Usuários

```haskell
splitOn :: Char -> String -> [String]
```

Recebe um caractere que será utilizado como separador.<br/>
Recebe uma string que será dividida de acordo com esse separador.<br/>
Percorre a string acumulando caracteres até encontrar o separador.<br/>
Quando encontra o separador, finaliza a substring acumulada e reinicia a coleta para a próxima parte.<br/>
Retorna uma lista de strings resultantes da divisão da string original.<br/>

```haskell
validarEmail :: String -> Bool
```

Recebe uma string representando um email.<br/>
Verifica se contém ao menos um caractere '@' e um '.'.<br/>
Retorna True se for válido ou False caso contrário.<br/>

```haskell
mapearParaUsuario :: [String] -> Maybe Usuario
```

Mapeia uma lista de strings (proveniente de uma linha CSV) para um 'Maybe Usuario'.<br/>
Espera exatamente três campos: nome, matrícula e email.<br/>
Converte a matrícula de String para Int utilizando 'readMaybe'.<br/>
Retorna 'Just Usuario' em caso de sucesso ou 'Nothing' em caso de falha na conversão.<br/>

```haskell
usuarioParaLinhaCSV :: Usuario -> String
```

Converte um 'Usuario' em uma linha CSV (nome,matrícula,email).<br/>

```haskell
adicionarUsuario :: Usuario -> [Usuario] -> Either String [Usuario]
```

Lógica pura para adicionar um usuário a uma lista, com validações.<br/>
Recebe um 'Usuario' novo e uma lista de usuários já cadastrados.<br/>
Verifica se a matrícula já existe ou se o e-mail está mal formatado.<br/>
Retorna 'Left erro' em caso de falha ou 'Right listaAtualizada' em caso de sucesso.<br/>

```haskell
removerUsuario :: Matricula -> [Usuario] -> Either String [Usuario]
```

Lógica pura para remover um usuário de uma lista.<br/>
Recebe uma matrícula e a lista de usuários cadastrados.<br/>
Se a matrícula existir, retorna 'Right listaSemUsuario'.<br/>
Caso contrário, retorna 'Left erro'.<br/>

```haskell
userFile :: FilePath
```

 Define o caminho do ficheiro CSV onde os utilizadores são armazenados.<br/>

```haskell
carregarUsuarios :: IO [Usuario]
```

Carrega os utilizadores do ficheiro CSV.<br/>
Converte cada linha em um 'Usuario', ignorando linhas corrompidas.<br/>
Retorna uma lista vazia caso o ficheiro não exista.<br/>

```haskell
salvarUsuarios :: [Usuario] -> IO ()
```

Salva uma lista de utilizadores no ficheiro CSV.<br/>
Substitui todo o conteúdo do ficheiro com a lista fornecida.<br/>

```haskell
prompt :: String -> IO String
```

Exibe uma mensagem no terminal e lê a entrada do usuário.<br/>

```haskell
uiAdicionarUsuario :: IO ()
```

Orquestra o processo de adicionar um novo utilizador.<br/>
Solicita nome, matrícula e e-mail pelo terminal.<br/>
Valida a matrícula e o e-mail antes de salvar.<br/>
Em caso de erro, exibe mensagem; em caso de sucesso, salva no ficheiro e registra log.<br/>

```haskell
uiRemoverUsuario :: IO ()
```

Orquestra o processo de remover um utilizador.<br/>
Solicita a matrícula pelo terminal.<br/>
Se a matrícula existir, remove o utilizador do ficheiro e registra log.<br/>
Caso contrário, exibe mensagem de erro.<br/>

```haskell
uiListarUsuarios :: IO ()
```
Carrega e lista todos os utilizadores cadastrados no ficheiro.<br/>
Se não houver nenhum, informa que a lista está vazia.<br/>

### Edição

```haskell
usuárioExiste :: Matricula -> String -> Bool
```

Recebe um tipo álgebrico Matricula que representa o usuário a ser procurado. <br/>
Recebe uma string representando o conteúdo do arquivo que guarda os usuários. <br/>
Faz a busca por meio da função isInfixOf. <br/>
Retorna um Bool, True se o usuário existe ou False se o usuário não existe. <br/>

```haskell
itemExiste :: Codigo -> String -> Bool
```

Recebe um tipo álgebrico Codigo que representa o item a ser procurado. <br/>
Recebe uma string representando o conteúdo do arquivo que guarda os itens. <br/>
Faz a busca por meio da função isInfixOf. <br/>
Retorna um Bool, True se o item existe ou False se o item não existe. <br/>

```haskell
prompt :: String -> IO String
```

Recebe uma string que representa a mensagem de prompt. <br/>
Exibe a mensagem no console e espera pela entrada do usuário. <br/>
Retorna a string digitada. <br/>

```haskell
splitOn :: Char -> String -> [String]
```

Recebe um caractere separador e uma string. <br/>
Divide a string em partes de acordo com o separador informado. <br/>
Retorna uma lista de strings resultantes da divisão. <br/>

```haskell
mapearParaUsuario :: [String] -> Maybe Usuario
```

Recebe uma lista de strings representando os campos de um usuário (nome, matrícula e email). <br/>
Tenta converter a matrícula para o tipo numérico esperado. <br/>
Retorna Just Usuario se a conversão for bem-sucedida ou Nothing caso contrário. <br/>

```haskell
converterTipo :: String -> Maybe TipoItem
```

Recebe uma string representando o tipo de item. <br/>
Converte para o tipo algébrico TipoItem (Filme, Livro ou Jogo). <br/>
Retorna Just TipoItem em caso válido ou Nothing se o valor não corresponder. <br/>

```haskell
converterStatus :: String -> Maybe StatusItem
```

Recebe uma string representando o status do item. <br/>
Converte para o tipo algébrico StatusItem (Emprestado ou Disponivel). <br/>
Retorna Just StatusItem em caso válido ou Nothing caso contrário. <br/>

```haskell
mapearParaItem :: [String] -> Maybe Item
```

Recebe uma lista de strings representando os campos de um item. <br/>
Converte os valores numéricos (código e ano) e os campos de tipo e status. <br/>
Retorna Just Item se todos os campos forem válidos ou Nothing em caso de falha. <br/>

```haskell
usuarioParaLinhaCSV :: Usuario -> String
```

Recebe um Usuario. <br/>
Converte os campos do usuário para uma string formatada em CSV. <br/>
Retorna a linha CSV correspondente. <br/>

```haskell
itemParaLinhaCSV :: Item -> String
```

Recebe um Item. <br/>
Converte os campos do item para uma string formatada em CSV. <br/>
Retorna a linha CSV correspondente. <br/>

```haskell
validarEmail :: String -> Bool
```

Recebe uma string representando um e-mail. <br/>
Verifica se contém ao menos um '@' e um '.'. <br/>
Retorna True se for válido ou False caso contrário. <br/>

```haskell
encontrarLinhaUsuario :: Matricula -> [String] -> Maybe (Int, String)
```

Recebe uma matrícula e a lista de linhas de um arquivo CSV. <br/>
Procura pela linha que contém a matrícula informada. <br/>
Retorna Just (número da linha, conteúdo da linha) ou Nothing se não encontrado. <br/>

```haskell
encontrarLinhaItem :: Codigo -> [String] -> Maybe (Int, String)
```

Recebe um código e a lista de linhas de um arquivo CSV. <br/>
Procura pela linha que contém o código informado. <br/>
Retorna Just (número da linha, conteúdo da linha) ou Nothing se não encontrado. <br/>

```haskell
substituirLinha :: Int -> String -> [String] -> [String]
```

Recebe o número da linha a ser substituída, a nova string e a lista de linhas originais. <br/>
Substitui a linha correspondente pela nova. <br/>
Retorna a lista de linhas atualizada. <br/>

```haskell
mostrarUsuario :: Usuario -> IO ()
```

Recebe um Usuario. <br/>
Exibe no console as informações formatadas do usuário. <br/>
Não retorna valor, apenas imprime. <br/>

```haskell
mostrarItem :: Item -> IO ()
```

Recebe um Item. <br/>
Exibe no console as informações formatadas do item. <br/>
Não retorna valor, apenas imprime. <br/>

```haskell
editarUsuario :: IO ()
```

Interage com o usuário solicitando a matrícula de um usuário a ser editado. <br/>
Localiza e exibe os dados atuais do usuário. <br/>
Permite escolher e editar campos (nome ou e-mail). <br/>
Executa a atualização no arquivo se confirmada. <br/>

```haskell
editarItem :: IO ()
```

Interage com o usuário solicitando o código de um item a ser editado. <br/>
Localiza e exibe os dados atuais do item. <br/>
Permite escolher e editar campos (título, autor, ano, tipo ou status). <br/>
Executa a atualização no arquivo se confirmada. <br/>

```haskell
processarEdicaoUser :: Int -> Usuario -> [String] -> String -> IO ()
```

Recebe o número da linha, o usuário atual, as linhas do arquivo e a opção de edição. <br/>
Atualiza o campo escolhido (nome ou e-mail). <br/>
Chama a função de confirmação antes de salvar. <br/>

```haskell
processarEdicaoItem :: Int -> Item -> [String] -> String -> IO ()
```

Recebe o número da linha, o item atual, as linhas do arquivo e a opção de edição. <br/>
Atualiza o campo escolhido (título, autor, ano, tipo ou status). <br/>
Chama a função de confirmação antes de salvar. <br/>

```haskell
confirmarEAtualizarUser :: Int -> Usuario -> [String] -> IO ()
```

Recebe o número da linha, o usuário atualizado e as linhas do arquivo. <br/>
Solicita confirmação ao usuário. <br/>
Se confirmado, substitui a linha no arquivo e salva. <br/>
Registra a operação no log. <br/>

```haskell
confirmarEAtualizarItem :: Int -> Item -> [String] -> IO ()
```

Recebe o número da linha, o item atualizado e as linhas do arquivo. <br/>
Solicita confirmação ao usuário. <br/>
Se confirmado, substitui a linha no arquivo e salva. <br/>
Registra a operação no log. <br/>

### Registrar Empréstimo

```haskell
obterCodigoItem :: [Item] -> IO (Maybe Item)
```
Solicita ao usuário o código de um item e retorna o item correspondente se estiver disponível. <br/>
Retorna Nothing se o item não existir ou estiver indisponível. <br/>

```haskell
obterMatriculaUsuario :: [Usuario] -> IO (Maybe Usuario)
```
Solicita ao usuário a matrícula e retorna o Usuario correspondente se encontrado. <br/>
Retorna Nothing se a matrícula não estiver cadastrada. <br/>

```haskell
gerarEmprestimo :: Item -> Usuario -> Day -> [Emprestimo] -> [Item] -> ([Emprestimo], [Item])
```
Cria um novo empréstimo com data limite calculada conforme o tipo do item. <br/>
Atualiza o status do item para Emprestado. <br/>
Retorna a nova lista de empréstimos e itens atualizados. <br/>

```haskell
registrarEmprestimo :: [Item] -> [Usuario] -> [Emprestimo] -> IO ([Emprestimo], [Item])
```
Função principal com fluxo comlpeto para registrar um empréstimo: <br/>
1. Solicita código do item e matrícula do usuário. <br/>
2. Verifica disponibilidade e existência. <br/>
3. Confirma dados. <br/>
4. Atualiza listas e registra log. <br/>

```haskell
atualizarStatusItem :: Int -> StatusItem -> [Item] -> [Item]
```
Atualiza o status de um item específico na lista de itens. <br/>

```haskell
adicionarDiasUteis :: Day -> Int -> Day
```
Calcula a data de devolução futura adicionando apenas dias úteis (ignora sábados e domingos). <br/>

```haskell
diasPorTipo :: TipoItem -> Int
```
Define o número de dias de empréstimo conforme o tipo do item: <br/>
 - Filmes e jogos: 2 dias úteis <br/>
 - Livros: 5 dias úteis <br/>

```haskell
adicionarEmprestimo :: [Emprestimo] -> Emprestimo -> [Emprestimo]
```
Adiciona um novo empréstimo à lista existente. <br/>

### Registrar Devolução

```haskell
obterCodigoItemParaDevolucao :: [Emprestimo] -> IO (Maybe Emprestimo)
```
Solicita o código do item a ser devolvido e retorna o empréstimo ativo correspondente. <br/>
Retorna Nothing se não houver empréstimo ativo. <br/>

```haskell
verificarAtraso :: Emprestimo -> Day -> IO ()
```
Verifica se o empréstimo está atrasado em relação à data esperada de devolução. <br/>
Exibe alerta se houver atraso. <br/>

```haskell
confirmarDevolucao :: Item -> Usuario -> IO Bool
```
Exibe os dados do item e do usuário e solicita confirmação da devolução. <br/>
Retorna True se o usuário confirmar com "s". <br/>

```haskell
gerarDevolucao :: Int -> Day -> [Emprestimo] -> [Item] -> ([Emprestimo], [Item])
```
Atualiza a data de devolução do empréstimo e o status do item para Disponivel. <br/>

```haskell
registrarDevolucao :: [Item] -> [Usuario] -> [Emprestimo] -> IO ([Emprestimo], [Item])
```
Função principal com o fluxo completo para registrar uma devolução: <br/>
1. Solicita código do item. <br/>
2. Verifica empréstimo ativo. <br/>
3. Confirma dados. <br/>
4. Atualiza listas e registra log. <br/>
5. Verifica fila de espera e notifica o próximo usuário se necessário. <br/>

```haskell
atualizarDevolucao :: Int -> Day -> Emprestimo -> Emprestimo
```
Atualiza a data efetiva de devolução de um empréstimo específico. <br/>

### Registrar Renovação de Empréstimo

```haskell
buscarEmprestimoAtivo :: Int -> Int -> [Emprestimo] -> Maybe Emprestimo
```
Busca um empréstimo ativo (ainda não devolvido) com base no código do item e na matrícula do usuário. <br/>
Retorna Just Emprestimo se encontrado. <br/>
Retorna Nothing se não houver empréstimo ativo correspondente. <br/>

```haskell
itemTemFila :: Int -> [Espera] -> Bool
```
Verifica se o item possui fila de espera. <br/>
Retorna True se houver pelo menos um usuário na fila. <br/>
Retorna False se não houver fila para o item. <br/>

```haskell
calcularNovaData :: TipoItem -> Day -> Day
```
Calcula a nova data de devolução com base no tipo do item e na data atual. <br/>
Filmes e jogos: +2 dias úteis <br/>
Livros: +5 dias úteis <br/>
Utiliza a função adicionarDiasUteis para ignorar finais de semana. <br/>

```haskell
atualizarRenovacao :: Int -> Int -> Day -> [Emprestimo] -> [Emprestimo]
```
Atualiza a data de devolução esperada de um empréstimo específico. <br/>
Identifica o empréstimo pelo código do item e matrícula do usuário. <br/>
Retorna a lista de empréstimos atualizada. <br/>

```haskell
renovarEmprestimo :: [Item] -> [Usuario] -> [Emprestimo] -> [Espera] -> IO [Emprestimo]
```
Função com o fluxo completo para renovar um empréstimo: <br/>
1. Solicita código do item e matrícula do usuário. <br/>
2. Verifica se o empréstimo está ativo. <br/>
3. Verifica se o item possui fila de espera (bloqueia renovação). <br/>
4. Calcula nova data de devolução. <br/>
5. Exibe dados e solicita confirmação. <br/>
6. Atualiza empréstimo e registra log. <br/>
 - Se confirmado, atualiza a data e registra sucesso. <br/>
 - Se cancelado ou bloqueado, exibe mensagem e registra erro. <br/>

### Gerenciar a Lista de Espera

```haskell
obterCodigoItemParaEspera :: [Item] -> IO (Maybe Item)
```
Solicita ao usuário o código de um item e verifica se ele está indisponível. <br/>
Retorna Just item se o item existe e está indisponível. <br/>
Retorna Nothing se o item não existe ou está disponível (não precisa de fila). <br/>

```haskell
adicionarNaEspera :: Int -> Int -> [Espera] -> ([Espera], Int)
```
Adiciona um usuário (pela matrícula) à fila de espera de um item. <br/>
Evita duplicatas: não adiciona se o usuário já estiver na fila. <br/>
Retorna a lista de espera atualizada e a posição do usuário na fila. <br/>

```haskell
confirmarInclusaoEspera :: Item -> Usuario -> Int -> IO Bool
```
Exibe os dados do item e do usuário, além da posição na fila, e solicita confirmação da inclusão. <br/>
Retorna True se o usuário confirmar com "s". <br/>

```haskell
incluirNaListaEspera :: [Item] -> [Usuario] -> [Espera] -> IO [Espera]
```
Fluxo completo para incluir um usuário na fila de espera: <br/>
1. Solicita código do item e matrícula do usuário. <br/>
2. Verifica disponibilidade e existência. <br/>
3. Confirma dados. <br/>
4. Atualiza fila e registra log. <br/>

```haskell
negrito :: String -> String
```
Formata uma string para ser exibida em negrito no terminal para realce visual. <br/>

```haskell
verificarFilaNaDevolucao :: Item -> [Espera] -> [Usuario] -> IO ()
```
Verifica se o item devolvido possui fila de espera. <br/>
Se houver, notifica o primeiro usuário da fila por mensagem no terminal. <br/>
Registra log de notificação (sucesso ou erro). <br/>

```haskell
removerDaFilaSeForPrimeiro :: Item -> Usuario -> [Espera] -> IO [Espera]
```
Remove o usuário da fila de espera se ele for o primeiro da fila para o item. <br/>
Atualiza a fila e registra log de sucesso. <br/>
Se o usuário não for o primeiro, mantém a fila e registra log de erro. <br/>

### Registro de Log

```haskell
registrarLog :: String -> Item -> Usuario -> String -> IO ()
```
Registra uma linha de log no arquivo Files/logs.txt, contendo informações sobre uma operação realizada no sistema. <br/>

```haskell
tipoStr :: TipoItem -> String
```

Converte o tipo de item (Livro, Filme, Jogo) para uma string legível que é usada no log. <br/>

### Relatorio

```haskell
listarEmprestimosAtivos :: [Emprestimo] -> [Item] -> [Usuario] -> IO ()
```
Cria relatórios categorizados por tipo de item (Livro, Filme, Jogo), facilitando a visualização de todos os empréstimos em andamento.

```haskell
historicoUsuario :: Matricula -> [Emprestimo] -> [Item] -> [Usuario] -> IO ()
```
Gera um relatório detalhado para um usuário específico, mostrando o histórico completo de empréstimos e devoluções.

```haskell
relatorioItensComEspera :: [Espera] -> [Item] -> [Usuario] -> IO ()
```
Identifica e lista todos os itens que têm uma fila de espera, mostrando quem são os usuários na fila.

```haskell
relatorioUsoSistema :: [Emprestimo] -> [Item] -> [Usuario] -> IO ()
```
Oferece um panorama geral do sistema, detalhando a quantidade de operações realizadas por tipo de item e por usuário, fornecendo uma visão estatística sobre a utilização da biblioteca.
