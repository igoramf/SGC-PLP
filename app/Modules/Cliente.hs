module Cliente (
    Cliente(Cliente),
    Clientes(Clientes),
    escreverArquivoCliente,
    getClientesEmLista,
    escreverIngressosCliente,
    iteraIngressosClientes,
    iteraSalasFilmesClientes,
    escreverApenasIngressosCliente
) where

import Filme(getFilmePeloId, getIdFilme, fromIO, converteSalasEmLista, getFilmesEmLista,
    getSalasFilme,
    getSalasFilmeToString,
    filmeToString,
    getSalasFilmes,
    getFilmes,
    formataParaEscrita,
    Filmes,
    Filme(Filme))
import System.IO
import Util
import System.IO.Unsafe


data Cliente = Cliente{
    nomeCliente :: String,
    cpf :: String,
    data_cadastro :: String,
    ingressos :: [Filme],
    salasCliente :: [String]
} deriving (Show, Read)


data Clientes = Clientes{
    clientes :: [(String, Cliente)]
} deriving Show

---------------------ClienteGetters----------------------------


getAtributosCliente :: Cliente -> String
getAtributosCliente (Cliente {nomeCliente = n, cpf = c, data_cadastro = d}) = n++","++c ++","++ d


getClientes :: Clientes -> [Cliente]
getClientes (Clientes {clientes = c}) = getClientesFromTuple c


getSalasCliente :: Cliente -> [String]
getSalasCliente Cliente {salasCliente = c} = c

getClientesFromTuple :: [(String, Cliente)] -> [Cliente]
getClientesFromTuple [] = []
getClientesFromTuple ((_,c): cs) = c : getClientesFromTuple cs

getIngressosCliente :: Cliente -> [Filme]
getIngressosCliente Cliente {ingressos = c} = c

getCpf :: Cliente -> String
getCpf Cliente {cpf = c} = c

----------------------CADASTRACLIENTE----------------------------

mapeiaCpf :: [Cliente] -> [(String, Cliente)]
mapeiaCpf [] = []
mapeiaCpf (c:cs)= (getCpf c, c) : mapeiaCpf cs


adicionaFilmeCliente :: [Cliente] -> String -> Filme -> Maybe [Cliente]
adicionaFilmeCliente [] cpfCliente novoFilme = Nothing
adicionaFilmeCliente (Cliente {nomeCliente = n, cpf= c, data_cadastro =d, ingressos =comp, salasCliente=s}:cs) cpfCliente novoFilme
    | c == cpfCliente = Just ([Cliente n c d (comp++[novoFilme]) s] ++ cs)
    | otherwise = adicionaFilmeCliente cs cpfCliente novoFilme


getSalasClientesToCsv :: [Cliente] -> String
getSalasClientesToCsv [] = []
getSalasClientesToCsv (c:cs) = if length salasCli > 0 then getCpf c ++ "," ++ getSalasClienteToString (salasCli) ++ getSalasClientesToCsv cs
    else []
    where
        salasCli = getSalasCliente c


getSalasClienteToString :: [String] -> String
getSalasClienteToString [] = []
getSalasClienteToString (c:cs) = if length cs > 0 then c ++ "," ++ getSalasClienteToString cs else c ++ "\n"

getIngressosClientesToCsv :: [Cliente] -> String
getIngressosClientesToCsv [] = []
getIngressosClientesToCsv (c:cs) = if length ingressosCli > 0 then getCpf c ++ "," ++ getIngressosClienteToString (ingressosCli) ++ getIngressosClientesToCsv cs
    else []
    where
        ingressosCli = getIngressosCliente c

getIngressosClienteToString :: [Filme] -> String
getIngressosClienteToString [] = []

--------------------------IOCLIENTES---------------------------


escreverArquivoCliente :: [Cliente] -> IO ()
escreverArquivoCliente clientes = do
    arq <- openFile "../arquivos/Clientes.csv" AppendMode
    arqSalasClientes <- openFile "../arquivos/SalasClientes.csv" AppendMode
    let dataSalasCliente = getSalasClientesToCsv clientes
    hPutStr arq (formataParaEscritaClientes clientes)
    hPutStr arqSalasClientes dataSalasCliente
    hClose arq
    hClose arqSalasClientes

iteraIngressosClientes :: [Cliente] -> String
iteraIngressosClientes [] = []
iteraIngressosClientes (c:cs) = iteraFormatoFilmes cpfAtual prodsAtuais ++ iteraIngressosClientes cs
    where
        prodsAtuais = getIngressosCliente c
        cpfAtual = getCpf c

iteraFormatoFilmes :: String -> [Filme] -> String
iteraFormatoFilmes cpf [] = []
iteraFormatoFilmes cpf (p:ps) = cpf ++ "," ++ show(getIdFilme p) ++ "\n" ++ iteraFormatoFilmes cpf ps

iteraSalasFilmesClientes :: [Cliente] -> String
iteraSalasFilmesClientes [] = []
iteraSalasFilmesClientes (c:cs) = iteraFormatoSalas cpfAtual prodsAtuais ++ iteraSalasFilmesClientes cs
    where
        prodsAtuais = getIngressosCliente c
        cpfAtual = getCpf c

iteraFormatoSalas :: String -> [Filme] -> String
iteraFormatoSalas cpf [] = []
iteraFormatoSalas cpf (s:ss) = cpf ++ "," ++ getSalasFilmeToString salas ++ iteraFormatoSalas cpf ss
    where
        salas = getSalasFilme s

escreverIngressosCliente :: String -> String -> IO ()
escreverIngressosCliente filmes salas = do
    arq <- openFile "../arquivos/IngressosClientes.csv" AppendMode
    arq1 <- openFile "../arquivos/SalasFilmesClientes.csv" AppendMode

    hPutStr arq (filmes)
    hPutStr arq1 (salas)
    hClose arq1
    hClose arq
-------------------------VISUALIZACAO---------------------------

getClientesEmLista :: IO [Cliente]
getClientesEmLista = do
    listaClienteStr <- lerClientes
    let clientes = (converteEmListaCliente listaClienteStr)
    return clientes

converteEmListaCliente :: [String] -> [Cliente]
converteEmListaCliente [] = []
converteEmListaCliente (filme:lista) =
    converteEmCliente (split filme ',') : converteEmListaCliente lista

converteEmCliente :: [String] -> Cliente
converteEmCliente cliente = Cliente nome cpf dataCadastro ingressos salas
    where
        nome = cliente !! 0
        cpf = cliente !! 1
        dataCadastro = cliente !! 2
        salas = fromIO(getSalasClienteEmLista cpf)
        ingressos = removeJustListOfMaybe(unsafePerformIO(filtraFilmesCliente cpf))

filtraFilmesCliente :: String -> IO [Maybe Filme]
filtraFilmesCliente  cpf = do
    filmes <- getFilmesEmLista
    listaFilmeClientesStr <- lerFilmesClientes
    let listaFilmes = converteIngressosEmLista listaFilmeClientesStr
    return $ (varreListaFilmes cpf filmes listaFilmes)

varreListaFilmes :: String ->[Filme] ->[[String]] -> [Maybe Filme]
varreListaFilmes cpf filmes [] = []
varreListaFilmes cpf filmes [[]] = []
varreListaFilmes cpf filmes (prodClientedb:lista1)
    | cpf == head prodClientedb = getFilmePeloId idFilme filmes : varreListaFilmes cpf filmes lista1
    | otherwise = varreListaFilmes cpf filmes lista1
    where
        idFilme = (read (prodClientedb !! 1)::Int)

getSalasClienteEmLista :: String -> IO [String]
getSalasClienteEmLista cpf = do
    salas <- openFile "../arquivos/SalasClientes.csv" ReadMode
    listaSalas <- lines <$> hGetContents salas
    let salasFinal = converteSalasEmLista listaSalas
    return $ (filtraSalaCliente cpf salasFinal)

lerClientes :: IO[String]
lerClientes = do
    arq <- openFile "../arquivos/Clientes.csv" ReadMode
    conteudo <- lines <$> hGetContents arq
    return conteudo


lerFilmesClientes :: IO [String]
lerFilmesClientes = do
    arq <- openFile "../arquivos/IngressosClientes.csv" ReadMode
    conteudo <- lines <$> hGetContents arq
    return conteudo

lerSalasClientes :: IO [String]
lerSalasClientes = do
    arq <- openFile "../arquivos/SalasClientes.csv" ReadMode
    conteudo <- lines <$> hGetContents arq
    return  conteudo



---------------------------UTIL------------------------------

filtraSalaCliente :: String -> [[String]] -> [String]
filtraSalaCliente cpf [[]] = []
filtraSalaCliente cpf [] = []
filtraSalaCliente cpf (s:ss)
    | cpf == head s = tail s
    | otherwise =  filtraSalaCliente cpf ss

filtraIdFilmeCliente :: String -> [[String]] -> [String]
filtraIdFilmeCliente cpf [[]] = []
filtraIdFilmeCliente cpf [] = []
filtraIdFilmeCliente cpf (s:ss)
    | cpf == head s = tail s
    | otherwise =  filtraSalaCliente cpf ss

formataParaEscritaClientes :: [Cliente] -> String
formataParaEscritaClientes [] = []
formataParaEscritaClientes (c:cs) = getAtributosCliente c ++ "\n" ++ formataParaEscritaClientes cs

converteIngressosEmLista :: [String] -> [[String]]
converteIngressosEmLista [] = []
converteIngressosEmLista (sala:lista) =
    (split sala ',') : converteIngressosEmLista lista


quebraCliente :: String -> [String]
quebraCliente entrada = split entrada ','

formataExibicaoCliente :: [String] -> String
formataExibicaoCliente lista = "Nome: " ++ (lista !! 0) ++ " | cpf:" ++ (lista !! 1) ++ " | data de cadastro:" ++ (lista !! 2)

escreverApenasIngressosCliente :: String -> [String] -> IO()
escreverApenasIngressosCliente idCliente filmes = do
    arq <- openFile "../arquivos/IngressosClientes.csv" AppendMode
    let ingressos = formataIngressos idCliente filmes
    hPutStrLn arq ingressos
    return() 

formataIngressos :: String -> [String] -> String
formataIngressos cpf [] = []
formataIngressos cpf (c:cs) = if length cs > 0 then cpf ++ "," ++ c ++ "\n" ++ formataIngressos cpf cs
    else cpf ++ "," ++ c
----------------------------------------------------------------------------------------------------
