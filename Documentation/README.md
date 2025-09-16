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
