import System.IO 
import System.IO.Unsafe
import Control.Exception
import System.IO.Error 
import System.Process
import Control.Monad (when)
import Text.Printf
import System.IO.Unsafe

--------------------------------------------IMPORTS DAS ENTIDADES-------------------------------------
import Filme
import Funcionario
import Cliente
import Venda
import Util
---------------------------------------------METODOS AUXILIARES---------------------------------------

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

showSimpleScreen :: [String] -> Integer -> Integer -> IO()
showSimpleScreen [] cursor contador = return ()
showSimpleScreen (o:os) cursor contador = do
   if contador == cursor
      then 
      putStrLn("->" ++ o)
   else
      putStrLn("  " ++ o)
   showSimpleScreen os cursor (contador+1)

lerEntradaString :: IO String
lerEntradaString = do
         hSetBuffering stdin LineBuffering
         hSetEcho stdin True
         x <- getLine
         return x

lerEntradaInt :: IO Int
lerEntradaInt = do
         hSetBuffering stdin LineBuffering
         hSetEcho stdin True
         x <- readLn
         return x

lerEntradaDouble :: IO Double
lerEntradaDouble = do
         hSetBuffering stdin LineBuffering
         hSetEcho stdin True
         x <- readLn
         return x
------------------------------------------------TELA INICIAL-----------------------------------------------

opcoesTelaInicial :: [String]
opcoesTelaInicial = ["Entrar como gestor", "Entrar como funcionário", "Entrar como cliente", "Sair"]

doTelaInicial :: Integer -> [Char] -> IO ()
doTelaInicial cursor action | action == "\ESC[B" = telaInicial ((cursor+1) `mod` 4)
                                                    | action == "\ESC[A" && cursor /= 0 = telaInicial (cursor-1)
                                                    | action == "\ESC[A" && cursor == 0 = telaInicial 3
                                                    | action == "\ESC[C" = mudarTelaInicial cursor
                                                    | action == "\ESC[D" = putStrLn("\n           Volte Sempre!           \n")
                                                    | otherwise = telaInicial cursor

mudarTelaInicial :: Integer -> IO()
mudarTelaInicial cursor                          | cursor == 0 = do telaOpcoesGestor 0
                                                 | cursor == 1 = do telaOpcoesFuncionario 0            
                                                 | cursor == 2 = do telaOpcoesCliente 0
                                                 | cursor == 3 = do telaSair


telaInicial :: Integer -> IO ()
telaInicial cursor = do
   
   system "clear"
   putStrLn("Bem-vindo ao cinema Campinense!")
   putStrLn("Como desejas acessar?\n")
   putStrLn("\n|| Utilize os direcionais do teclado para mover o cursor ||\n")
   showSimpleScreen opcoesTelaInicial cursor 0
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doTelaInicial cursor action

-------------------------------------------------TELA GESTOR-----------------------------------------

opcoesTelaGestor :: [String]
opcoesTelaGestor = ["Cadastrar filme", "Cadastrar funcionário", "Visualizar filmes", "Visualizar clientes", "Visualizar vendas"]

mudarTelaOpcoesGestor :: Integer -> IO ()
mudarTelaOpcoesGestor cursor
   | cursor == 0 = cadastroFilmesTela
   | cursor == 1 = cadastroFuncionarioTela
   | cursor == 2 = telaOpcoesVisualizarFilmes 0
   | cursor == 3 = visualizarClientesTela
   | cursor == 4 = visualizarVendasTela

doOpcoesGestor :: Integer -> [Char] -> IO ()
doOpcoesGestor cursor action | action == "\ESC[B" = telaOpcoesGestor ((cursor+1) `mod` 5)
                                                    | action == "\ESC[A" && cursor /= 0 = telaOpcoesGestor (cursor-1)
                                                    | action == "\ESC[A" && cursor == 0 = telaOpcoesGestor 4
                                                    | action == "\ESC[C" = mudarTelaOpcoesGestor cursor
                                                    | action == "\ESC[D" = telaInicial 0
                                                    | otherwise = telaOpcoesGestor cursor


telaOpcoesGestor :: Integer -> IO ()
telaOpcoesGestor cursor = do
   
   system "clear"
   putStrLn ("Bem vindo, GESTOR! \n\n|| Aperte (Seta Direita) para escolher qual opcão acessar e (Seta Esquerda) para voltar à tela anterior. ||\n")
   showSimpleScreen opcoesTelaGestor cursor 0
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doOpcoesGestor cursor action

-- Cadastrar filme
cadastroFilmesTela :: IO ()
cadastroFilmesTela = do
   system "clear"

   putStrLn ("Digite o id do filme:")
   id <- lerEntradaInt

   putStrLn ("\nDigite o nome do filme: (Sem caracteres especiais)")
   nome <- lerEntradaString

   putStrLn ("\nDigite o preço do filme:")
   preco <- lerEntradaDouble

   putStrLn ("\nDigite as salas do filme (separados por ',' e sem caracteres especiais):")
   salas <- lerEntradaString
   let listaSalas = split salas ','

   putStrLn ("\nDigite a duracao do filme (com '/'):")
   duracao <- lerEntradaString

   let filme = Filme id nome preco listaSalas duracao 

   let listaFilmes = [filme]
   let adicionarFilme = listaFilmes
   escreverArquivo adicionarFilme
   
   putStrLn("\nO filme foi cadastrado com sucesso!\n")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   putStrLn (" ")

   telaInicial 0

-- Cadastrar funcionario
listaVendasInicialFuncionario:: [String]
listaVendasInicialFuncionario = []

cadastroFuncionarioTela :: IO ()
cadastroFuncionarioTela = do
   system "clear"

   putStrLn ("Digite o nome do funcionário: (Sem caracteres especiais)")
   nomeFuncionario <- lerEntradaString

   putStrLn ("\nDigite o cpf do funcionário: (Apenas números)")
   cpfFuncionario <- lerEntradaString

   putStrLn ("\nDigite a data de admissão do funcionário (usando '/'):")
   dataAdmissao <- lerEntradaString

   putStrLn ("\nDigite o salário do funcionário: (Apenas numeros)")
   salario <- lerEntradaDouble

   putStrLn("\nO funcionário foi cadastrado com sucesso!\n")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False

   let funcionario = Funcionario nomeFuncionario cpfFuncionario dataAdmissao listaVendasInicialFuncionario salario 

   let listaFuncionarios = [funcionario]
   let adicionarFuncionario = listaFuncionarios
   escreverArquivoFuncionario adicionarFuncionario

   action <- getKey
   putStrLn (" ")
   telaInicial 0

-- Visualizar Clientes
visualizarClientesTela :: IO ()
visualizarClientesTela = do
      system "clear"
      clientes <- openFile "../arquivos/Clientes.csv" ReadMode
      listaClientes <- lines <$> hGetContents clientes
      print listaClientes
      
      action <- getKey
      putStrLn (" ")
      telaInicial 0

-- Visualizar vendas
visualizarVendasTela :: IO ()
visualizarVendasTela = do
      system "clear"
      vendas <- openFile "../arquivos/Vendas.csv" ReadMode
      listaVendas <- lines <$> hGetContents vendas
      print listaVendas
      
      action <- getKey
      putStrLn (" ")
      telaInicial 0

-----------------------------------------------TELA FUNCIONÁRIO---------------------------------------------

opcoesTelaFuncionario :: [String]
opcoesTelaFuncionario = ["Cadastrar cliente", "Cadastrar venda", "Visualizar clientes", "Visualizar filmes"]

mudarTelaOpcoesFuncionario :: Integer -> IO ()
mudarTelaOpcoesFuncionario cursor
   | cursor == 0 = cadastroClienteTela
   | cursor == 1 = cadastroVendaTela
   | cursor == 2 = visualizarClientesTela
   | cursor == 3 = telaOpcoesVisualizarFilmes 0

doOpcoesFuncionario :: Integer -> [Char] -> IO ()
doOpcoesFuncionario cursor action | action == "\ESC[B" = telaOpcoesFuncionario ((cursor+1) `mod` 4)
                                                    | action == "\ESC[A" && cursor /= 0 = telaOpcoesFuncionario (cursor-1)
                                                    | action == "\ESC[A" && cursor == 0 = telaOpcoesFuncionario 3
                                                    | action == "\ESC[C" = mudarTelaOpcoesFuncionario cursor
                                                    | action == "\ESC[D" = telaInicial 0
                                                    | otherwise = telaOpcoesFuncionario cursor

telaOpcoesFuncionario :: Integer -> IO ()
telaOpcoesFuncionario cursor = do
   
   system "clear"
   putStrLn ("Bem vindo, FUNCIONÁRIO! \n\n|| Aperte (Seta Direita) para escolher qual opcão acessar e (Seta Esquerda) para voltar à tela anterior. ||\n")
   showSimpleScreen opcoesTelaFuncionario cursor 0
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doOpcoesFuncionario cursor action

-- Cadastrar Cliente
listaComprasInicialCliente:: [Filme]
listaComprasInicialCliente = []

cadastroClienteTela :: IO ()
cadastroClienteTela = do
   system "clear"

   putStrLn ("Digite o nome do cliente: (Sem caracteres especiais)")
   nomeCliente <- lerEntradaString

   putStrLn ("\nDigite o cpf do cliente: (Apenas números)")
   cpfCliente <- lerEntradaString 

   putStrLn ("\nDigite a data de cadastro do cliente (usando '/'):")
   dataCadastro <- lerEntradaString

   putStrLn ("\nDigite as salas do cliente: (Sem caracteres especiais)")
   salasCliente <- lerEntradaString
   let listaSalasCliente = split salasCliente ','

   putStrLn("\nO cliente foi cadastrado com sucesso!\n")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False

   let cliente = Cliente nomeCliente cpfCliente dataCadastro listaComprasInicialCliente listaSalasCliente

   let listaCliente = [cliente]
   let adicionarCliente = listaCliente
   escreverArquivoCliente adicionarCliente
   
   action <- getKey
   telaInicial 0

-- Cadastrar Venda
cadastroVendaTela :: IO ()
cadastroVendaTela = do
   system "clear"

   putStrLn ("Digite o id da venda:")
   idVenda <- lerEntradaString

   putStrLn ("\nDigite o cpf do cliente: (Apenas números e que *esteja cadastrado*)")
   cpfCliente <- lerEntradaString
   
   putStrLn ("\nDigite o cpf do funcionário: (Apenas números)")
   cpfFuncionario <- lerEntradaString 

   putStrLn ("\nDigite a data de cadastro da venda (usando '/'):")
   dataVenda <- lerEntradaString

   putStrLn ("\nDigite os ids dos filmes (separados por ',' e apenas números):")
   filmesVendidos <- lerEntradaString
   let listaFilmes = split filmesVendidos ','

   putStrLn("\nA venda foi cadastrada com sucesso!\n")

   let venda = Venda idVenda cpfCliente cpfFuncionario dataVenda listaFilmes
   let listaVenda = [venda]
   let adicionarVenda = listaVenda
   escreverArquivoVendas adicionarVenda
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   
   action <- getKey
   putStrLn(" ")
   telaInicial 0

-----------------------------------------------TELA CLIENTE------------------------------------------

opcoesTelaCliente :: [String]
opcoesTelaCliente = ["Visualizar filmes", "Comprar filme"]

mudarTelaOpcoesCliente :: Integer -> IO ()
mudarTelaOpcoesCliente cursor
   | cursor == 0 = telaOpcoesVisualizarFilmes 0--
   | cursor == 1 = comprarFilmeTela--

doOpcoesCliente :: Integer -> [Char] -> IO ()
doOpcoesCliente cursor action | action == "\ESC[B" = telaOpcoesCliente ((cursor+1) `mod` 2)
                                                    | action == "\ESC[A" && cursor /= 0 = telaOpcoesCliente (cursor-1)
                                                    | action == "\ESC[A" && cursor == 0 = telaOpcoesCliente 1
                                                    | action == "\ESC[C" = mudarTelaOpcoesCliente cursor
                                                    | action == "\ESC[D" = telaInicial 0
                                                    | otherwise = telaOpcoesCliente cursor

telaOpcoesCliente :: Integer -> IO ()
telaOpcoesCliente cursor = do
   
   system "clear"
   putStrLn ("Bem vindo, CLIENTE \n\n| Aperte (Seta Direita) para escolher qual opcão acessar e (Seta Esquerda) para voltar à tela anterior. ||\n")
   showSimpleScreen opcoesTelaCliente cursor 0
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doOpcoesCliente cursor action

-- Comprar filme
comprarFilmeTela :: IO ()
comprarFilmeTela = do
   system "clear"

   putStrLn ("Digite os ids dos filmes (separados por vírgula e apenas números):")
   filmes <- lerEntradaString
   let listaFilmes = split filmes ','
   
   putStrLn ("\nDigite o cpf do cliente: (Apenas números)")
   cpf <- lerEntradaString

   putStrLn("\nA compra foi cadastrada com sucesso! Volte sempre!\n")

   escreverApenasComprasCliente cpf listaFilmes
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   
   action <- getKey
   putStrLn(" ")
   telaInicial 0

--------------------------------------------TELA VISUALIZAR FilmeS-----------------------------------

opcoesTelaVisualizarFilmes :: [String]
opcoesTelaVisualizarFilmes = ["Visualizar por sintoma", "Visualizar por existentes no estoque"]

mudarTelaOpcoesVisualizarFilmes :: Integer -> IO ()
mudarTelaOpcoesVisualizarFilmes cursor
   | cursor == 0 = visualizarFilmesSalasTela
   | cursor == 1 = visualizarFilmesEstoqueTela
   

doOpcoesVisualizarFilmes :: Integer -> [Char] -> IO ()
doOpcoesVisualizarFilmes cursor action | action == "\ESC[B" = telaOpcoesVisualizarFilmes ((cursor+1) `mod` 2)
                                                    | action == "\ESC[A" && cursor /= 0 = telaOpcoesVisualizarFilmes (cursor-1)
                                                    | action == "\ESC[A" && cursor == 0 = telaOpcoesVisualizarFilmes 1
                                                    | action == "\ESC[C" = mudarTelaOpcoesVisualizarFilmes cursor
                                                    | action == "\ESC[D" = telaInicial 0 -- n sei se eh tela Inicial mesmo
                                                    | otherwise = telaOpcoesVisualizarFilmes cursor

telaOpcoesVisualizarFilmes :: Integer -> IO ()
telaOpcoesVisualizarFilmes cursor = do
   
   system "clear"
   putStrLn ("Bem vindo à seção de visualzação de filmes!\n\n|| Aperte (Seta Direita) para escolher qual opcão acessar e (Seta Esquerda) para voltar à tela inicial. ||\n")

   showSimpleScreen opcoesTelaVisualizarFilmes cursor 0
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doOpcoesVisualizarFilmes cursor action

-- Visualizar filmes pelo estoque
visualizarFilmesEstoqueTela :: IO ()
visualizarFilmesEstoqueTela = do
      system "clear"

      filmes <- openFile "../arquivos/Filmes.csv" ReadMode
      
      listaFilmes <- lines <$> hGetContents filmes
      print listaFilmes

      action <- getKey
      putStrLn (" ")
      telaInicial 0

-- Visualizar filmes pelos salas
visualizarFilmesSalasTela :: IO ()
visualizarFilmesSalasTela = do
      system "clear"
      filmes <- openFile "../arquivos/SalasFilme.csv" ReadMode
      listaFilmes <- lines <$> hGetContents filmes
      print (listaFilmes)
      
      action <- getKey
      putStrLn (" ")
      telaInicial 0

----------------------------------------------TELA SAIR------------------------------------------------
-- Tela de sair
doTelaSair :: String -> IO ()
doTelaSair action    | action == "s" = return() 
                     | otherwise = telaInicial 0
    
telaSair :: IO ()
telaSair = do
   system "clear"
   putStrLn("Digite (s) para encerrar a execução ou (Outra tecla) para voltar para o menu");
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doTelaSair action

----------------------------------------------EXECUTAR--------------------------------------------------
run :: IO ()
run = do
   {catch (iniciar) error;}
   where
      iniciar = do
      {
         telaInicial 0;
         return ()
      }
      error = ioError 

main :: IO ()
main = do
   run
