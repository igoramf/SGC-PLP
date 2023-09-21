module Venda(
    Venda(Venda),
    Vendas(Vendas),
    escreverArquivoVendas
) where

import System.IO
import System.Directory
import System.IO.Unsafe
import Util

data Venda = Venda {
    idVenda :: String,
    cpfFuncionario :: String,
    cpfCliente :: String,
    dataVendas :: String,
    filmes :: [String]
} deriving (Show, Read)


data Vendas = Vendas{
    vendas :: [(String, Venda)]
} deriving Show

-- Getters
getIdVenda :: Venda -> String
getIdVenda (Venda {idVenda = i}) = i

getFuncionarioVenda :: Venda -> String
getFuncionarioVenda (Venda {cpfFuncionario = f}) = f

getClienteVenda :: Venda -> String
getClienteVenda (Venda {cpfCliente = c}) = c

getDataVenda :: Venda -> String
getDataVenda (Venda {dataVendas = d}) = d

-- fazer o retorno em lista de filmes talvez, acho q melhor n
getFilmesVenda :: Venda -> [String]
getFilmesVenda (Venda {filmes = p}) = p

getFilmesVendas :: [Venda] -> String
getFilmesVendas [] = []
getFilmesVendas (venda:vendas) = (getFuncionarioVenda venda) ++ "," ++ getFilmesVendaToString (getFilmesVenda venda) ++ getFilmesVendas vendas

getFilmesVendaToString :: [String] -> String
getFilmesVendaToString [] = []
getFilmesVendaToString (filme:filmes) = if length filmes > 0 then filme ++ "," ++ getFilmesVendaToString filmes else filme ++ "\n"

-- Get vendas retornando uma lista de Venda
getVendas :: Vendas -> [Venda]
getVendas (Vendas {vendas = p}) = getVendasFromTuple p

getVendasFromTuple :: [(String, Venda)] -> [Venda]
getVendasFromTuple [] = []
getVendasFromTuple ((_,c): cs) = c : getVendasFromTuple cs
-----------------------

-- Salvando o arquivo
escreverArquivoVendas :: [Venda] -> IO ()
escreverArquivoVendas vendas = do
    arq <- openFile "../arquivos/Vendas.csv" AppendMode
    arq1 <- openFile "../arquivos/FilmesVendas.csv" AppendMode
    let listaFilmes = getFilmesVendas vendas
    print vendas
    hPutStr arq (formataParaEscrita vendas)
    hPutStr arq1 (listaFilmes)
    hClose arq
    hClose arq1

-- Parse pra String
formataParaEscrita :: [Venda] -> String
formataParaEscrita [] = []
formataParaEscrita (c:cs) = vendaToString c ++ "\n" ++ formataParaEscrita cs

vendaToString :: Venda -> String
vendaToString Venda {idVenda = id, cpfFuncionario = cpfF, cpfCliente = cpfC, dataVendas = d} = id ++ "," ++ cpfF ++ "," ++ cpfC ++ "," ++ d
--------------------------------------------------------------------------------------------------------
