module Main where

import Interface ( lacoMenuPrincipal )

main :: IO ()

putStrLn "Iniciando a leitura dos arquivos CSV..."

-- Definir os caminhos dos arquivos (o caminho depende e os arquivos devem estar juntos)
let usuariosPath = "usuarios.csv"
let itensPath = "itens.csv"
let emprestimosPath = "emprestimos.csv"
let listasEsperaPath = "listas_espera.csv"

-- Ler todos os dados do CSV
usuarios <- lerUsuariosCSV usuariosPath
itens <- lerItensCSV itensPath
emprestimos <- lerEmprestimosCSV emprestimosPath
listasEspera <- lerListasEsperaCSV listasEsperaPath

main = lacoMenuPrincipal
