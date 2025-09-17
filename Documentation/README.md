### Sistema de Inventário

## Funções

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
