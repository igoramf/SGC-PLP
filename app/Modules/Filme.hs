--filme tem id, nome, preço, salas e tempo de duracao
--atualizar preço de um filme permitindo que descontos sejam aplicados
--visualizar lista de filmes (filmes existentes no cinema e filmes de acordo com as salas)

module Filme (getFilmePeloId, getIdFilme, fromIO, converteSalasEmLista, getFilmesEmLista,
    getSalasFilme,
    getSalasFilmeToString,
    filmeToString,
    getSalasFilmes,
    getFilmes,
    formataParaEscrita,
    escreverArquivo,
    converteEmLista,
    setPreco,
    getFilmesPuros,
    Filmes(Filmes),
    Filme(Filme)
) where
import System.IO
import System.Directory
import Util
import System.IO.Unsafe

data Filme = Filme {
    idFilme :: Int,
    nomeFilme :: String,
    preco :: Double,
    salasFilme :: [String],
    duracao :: String
} deriving (Show, Read)



data Filmes = Filmes {
    filmes :: [(Int, Filme)]
} deriving Show


----------------------------PRODUTOGetters--------------------

getFilmes :: Filmes -> [Filme]
getFilmes (Filmes {filmes = p}) = getFilmesFromTuple p

getFilmesFromTuple :: [(Int, Filme)] -> [Filme]
getFilmesFromTuple [] = []
getFilmesFromTuple ((_,c): cs) = c : getFilmesFromTuple cs

getIdFilme :: Filme -> Int
getIdFilme Filme {idFilme = i} = i

getNomeFilme :: Filme -> String
getNomeFilme Filme {nomeFilme = n} = n

getPreco :: Filme -> Double
getPreco Filme {preco = p} = p 

getFilmePeloId :: Int -> [Filme] -> Maybe Filme
getFilmePeloId id [] = Nothing
getFilmePeloId id (p:ps) = if id == getIdFilme p then Just p
    else getFilmePeloId id ps

getSalasFilme :: Filme -> [String]
getSalasFilme Filme {salasFilme = s} = s

getSalasFilmes :: [Filme] -> String
getSalasFilmes [] = []
getSalasFilmes (c:cs) = show (getIdFilme c) ++ "," ++ getSalasFilmeToString (getSalasFilme c) ++ getSalasFilmes cs

getSalasFilmeToString :: [String] -> String
getSalasFilmeToString [] = []
getSalasFilmeToString (c:cs) = if length cs > 0 then c ++ "," ++ getSalasFilmeToString cs else c ++ "\n"

getDuracao :: Filme -> String
getDuracao Filme {duracao = v} = v



setPreco :: [Filme] -> Int -> Double -> [Filme]
setPreco [] x novoPreco = []
setPreco (c:cs) x novoPreco
    | idAtual == x = ([Filme x nomeFilmeAtual novoPreco salasFilmeAtual duracaoAtual] ++ cs)
    | otherwise = setPreco cs x novoPreco
    where
        idAtual = getIdFilme c
        nomeFilmeAtual = getNomeFilme c
        salasFilmeAtual = getSalasFilme c
        duracaoAtual = getDuracao c



filmeToString :: Filme -> String
filmeToString Filme {idFilme = i, nomeFilme = n, preco = p, duracao = v} = show i ++"," ++ n ++ "," ++ show p ++ "," ++ v



filmesToString :: [Filme] -> String
filmesToString [] = []
filmesToString (p:ps) = if length ps > 0 then do "["++filmeToString p ++"]," ++ filmesToString ps
    else do "[" ++ filmeToString p ++ "]"



salasToString :: [String] -> String
salasToString [] = []
salasToString (s:sw) = s ++ salasToString sw

-----------------------------IOFilme---------------------------------

escreverArquivo :: [Filme] -> IO ()
escreverArquivo filme = do
    arq <- openFile "../arquivos/Filmes.csv" AppendMode
    arq1 <- openFile "../arquivos/SalasFilme.csv" AppendMode
    
    print (filme) -- mudar isso, mas fazer depois

    let dataSalasFilme = getSalasFilmes filme
    hPutStr arq1 (dataSalasFilme)
    hPutStr arq (formataParaEscrita filme)
    hClose arq
    hClose arq1


formataParaEscrita :: [Filme] -> String
formataParaEscrita [] = []
formataParaEscrita (c:cs) = filmeToString c ++ "\n" ++ formataParaEscrita cs


------------------------- Visualização de Filmes -------------------------
getFilmesPuros :: [Filme]
getFilmesPuros = (unsafePerformIO getFilmesEmLista :: [Filme])

getFilmesEmLista :: IO [Filme]
getFilmesEmLista = do
    filmes <- openFile "../arquivos/Filmes.csv" ReadMode
    listaFilmes <- lines <$> hGetContents filmes
    hClose filmes
    return $ (converteEmLista listaFilmes)

converteEmLista :: [String] -> [Filme]
converteEmLista [] = []
converteEmLista (filme:lista) =
    converteEmFilme (split filme ',') : converteEmLista lista

converteEmFilme :: [String] -> Filme
converteEmFilme filme = Filme id nome preco salasFilme dataDuracao
    where 
        id = (read (filme !! 0) :: Int)
        nome = filme !! 1
        preco = (read (filme !! 2) :: Double)
        salasFilme = fromIO(getSalasEmLista id)
        dataDuracao = filme !! 3

-- Converte IO em puro
fromIO :: IO[String] -> [String]
fromIO x = (unsafePerformIO x :: [String])

------------------------- Visualização de Salas -------------------------

getSalasEmLista :: Int -> IO [String]
getSalasEmLista id = do
    salas <- openFile "../arquivos/SalasFilme.csv" ReadMode
    listaSalas <- lines <$> hGetContents salas
    let salasFinal = converteSalasEmLista listaSalas
    hClose salas
    return $ (filtraSala id salasFinal)

converteSalasEmLista :: [String] -> [[String]]
converteSalasEmLista [] = []
converteSalasEmLista (sala:lista) =
    (split sala ',') : converteSalasEmLista lista
    
filtraSala :: Int -> [[String]] -> [String]
filtraSala _ [] = []
filtraSala id (sala:salas)
    | id == idCliente = tail sala
    | otherwise = filtraSala id salas
    where
        idCliente = read (head sala) :: Int
---------------------------------------------------------------------------------------
