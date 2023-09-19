--Filme tem id, titulo, preço, sala e duração
--atualizar preço de um Filme permitindo que descontos sejam aplicados
--visualizar lista de Filmes (Filmes existentes no cinema e Filmes de acordo com as sala)

module Filme (getFilmePeloTitulo, getTituloFilme, fromIO, converteSalaEmLista, getFilmesEmLista,
    getSalaFilme,
    getSalaFilmeToString,
    filmeToString,
    getSalaFilmes,
    getFilmes,
    formataParaEscrita,
    escreverArquivo,
    converteEmLista,
    setPreco,
    Filmes(Filmes),
    Filme(Filme)
) where
import System.IO
import System.Directory
import Util
import System.IO.Unsafe

data Filme = Filme {
    idFilme :: Int,
    tituloFilme :: String,
    preco :: Double,
    salaFilme :: [String],
    duracao :: String
} deriving (Show, Read)

data Filmes = Filmes {
    Filmes :: [(Int, Filme)]
} deriving Show


----------------------------FilmeGetters--------------------

getFilmes :: Filmes -> [Filme]
getFilmes (Filmes {Filmes = p}) = getFilmesFromTuple p

getFilmesFromTuple :: [(Int, Filme)] -> [Filme]
getFilmesFromTuple [] = []
getFilmesFromTuple ((_,c): cs) = c : getFilmesFromTuple cs

getTituloFilme :: Filme -> Int
getTituloFilme Filme {idFilme = i} = i

getTituloFilme :: Filme -> String
getTituloFilme Filme {tituloFilme = n} = n

getPreco :: Filme -> Double
getPreco Filme {preco = p} = p 

getFilmePeloTitulo :: Int -> [Filme] -> Maybe Filme
getFilmePeloTitulo id [] = Nothing
getFilmePeloTitulo id (p:ps) = if id == getTituloFilme p then Just p
    else getFilmePeloTitulo id ps

getSalaFilme :: Filme -> [String]
getSalaFilme Filme {salaFilme = s} = s

getSalaFilmes :: [Filme] -> String
getSalaFilmes [] = []
getSalaFilmes (c:cs) = show (getTituloFilme c) ++ "," ++ getSalaFilmeToString (getSalaFilme c) ++ getSalaFilmes cs

getSalaFilmeToString :: [String] -> String
getSalaFilmeToString [] = []
getSalaFilmeToString (c:cs) = if length cs > 0 then c ++ "," ++ getSalaFilmeToString cs else c ++ "\n"

getDuracao :: Filme -> String
getDuracao Filme {duracao = v} = v



setPreco :: [Filme] -> Int -> Double -> [Filme]
setPreco [] x novoPreco = []
setPreco (c:cs) x novoPreco
    | idAtual == x = ([Filme x tituloFilmeAtual novoPreco salaFilmeAtual duracaoAtual] ++ cs)
    | otherwise = setPreco cs x novoPreco
    where
        idAtual = getTituloFilme c
        tituloFilmeAtual = gettituloFilme c
        salaFilmeAtual = getSalaFilme c
        duracaoAtual = getDuracao c



filmeToString :: Filme -> String
filmeToString Filme {idFilme = i, tituloFilme = n, preco = p, duracao = v} = show i ++"," ++ n ++ "," ++ show p ++ "," ++ v



FilmesToString :: [Filme] -> String
FilmesToString [] = []
FilmesToString (p:ps) = if length ps > 0 then do "["++filmeToString p ++"]," ++ FilmesToString ps
    else do "[" ++ filmeToString p ++ "]"



salaToString :: [String] -> String
salaToString [] = []
salaToString (s:sw) = s ++ salaToString sw

-----------------------------IOFilme---------------------------------

escreverArquivo :: [Filme] -> IO ()
escreverArquivo Filme = do
    arq <- openFile "../arquivos/Filmes.csv" AppendMode
    arq1 <- openFile "../arquivos/salaFilme.csv" AppendMode
    
    print (Filme) -- mudar isso, mas fazer depois

    let datasalaFilme = getSalaFilmes Filme
    hPutStr arq1 (datasalaFilme)
    hPutStr arq (formataParaEscrita Filme)
    hClose arq
    hClose arq1


formataParaEscrita :: [Filme] -> String
formataParaEscrita [] = []
formataParaEscrita (c:cs) = filmeToString c ++ "\n" ++ formataParaEscrita cs


------------------------- Visualização de Filmes -------------------------

getFilmesemLista :: IO [Filme]
getFilmesemLista = do
    Filmes <- openFile "../arquivos/Filmes.csv" ReadMode
    listaFilmes <- lines <$> hGetContents Filmes
    hClose Filmes
    return $ (converteEmLista listaFilmes)

converteEmLista :: [String] -> [Filme]
converteEmLista [] = []
converteEmLista (Filme:lista) =
    converteEmFilme (split Filme ',') : converteEmLista lista

converteEmFilme :: [String] -> Filme
converteEmFilme Filme = Filme id titulo preco salaFilme dataDuracao
    where 
        id = (read (Filme !! 0) :: Int)
        titulo = Filme !! 1
        preco = (read (Filme !! 2) :: Double)
        salaFilme = fromIO(getsalaEmLista id)
        dataDuracao = Filme !! 3

-- Converte IO em puro
fromIO :: IO[String] -> [String]
fromIO x = (unsafePerformIO x :: [String])

------------------------- Visualização de sala -------------------------

getsalaEmLista :: Int -> IO [String]
getsalaEmLista id = do
    sala <- openFile "../arquivos/salaFilme.csv" ReadMode
    listasala <- lines <$> hGetContents sala
    let salaFinal = converteSalaEmLista listasala
    hClose sala
    return $ (filtraSala id salaFinal)

converteSalaEmLista :: [String] -> [[String]]
converteSalaEmLista [] = []
converteSalaEmLista (sala:lista) =
    (split sala ',') : converteSalaEmLista lista
    
filtraSala :: Int -> [[String]] -> [String]
filtraSala _ [] = []
filtraSala id (sala:sala)
    | id == idCliente = tail sala
    | otherwise = filtraSala id sala
    where
        idCliente = read (head sala) :: Int
---------------------------------------------------------------------------------------
