module Modules.Menus.Menus where

import Control.Monad
import System.Directory
import System.Exit (exitSuccess)
import System.FilePath
import System.IO
import Modules.Util.ClearScreen (clearScreen)
import Modules.User.User

menuInicial :: IO ()
menuInicial = do
  clearScreen
  print "|Bem vindo a SGC                                   |"
  print "|--------------------------------------------------|"
  print "|                                                  |"
  print "|Selecione uma opção:                              |"
  print "|                                                  |"
  print "|1. Cadastrar novo usuário                         |"
  print "|2. Cadastrar novo administrador                   |"
  print "|3. Fazer Login                                    |"
  print "|4. Exibir Filmes                                  |"
  print "|5. Minha Conta                                    |"
  print "|6. Sair                                           |"
  print "|                                                  |"
  print "|--------------------------------------------------|"
  opcao <- getLine
  case opcao of
    "1" -> cadastroUsuario
    "2" -> cadastroAdmin
    "3" -> login
    "4" -> exibirFilmes
    "5" -> minhaConta
    "6" -> exitSuccess
    _ -> do
      putStrLn "Opção inválida, tente novamente.\n"
      menuInicial


cadastroUsuario :: IO ()
cadastroUsuario = do
  clearScreen
  putStrLn "Menu>Cadastro"
  putStrLn " .--------------------------------------------------------------------------------------------."
  putStrLn " | CADASTRAR USUARIO                                                                          |"
  putStrLn " '--------------------------------------------------------------------------------------------'"
  putStrLn ""
  putStrLn "Digite seu username: "
  name <- getLine
  putStrLn "Digite seu nome: "
  username <- getLine
  putStrLn "Digite sua senha: "
  password <- getLine
  --createUser name username password "isAdm: false"
  let user = createUser name username password False
  print user
  menuInicial


cadastroAdmin :: IO ()
cadastroAdmin = do
  clearScreen
  putStrLn "Menu>Cadastro"
  putStrLn " .--------------------------------------------------------------------------------------------."
  putStrLn " | CADASTRAR ADMINISTRADOR                                                                    |"
  putStrLn " '--------------------------------------------------------------------------------------------'"
  putStrLn ""
  putStrLn "Digite seu username: "
  name <- getLine
  putStrLn "Digite seu nome: "
  username <- getLine
  putStrLn "Digite sua senha: "
  password <- getLine
  --createUser name username password isAdm: true
  let user = createAdm name username password True
  print user
  menuInicial


login :: IO ()
login = do
    clearScreen
    putStrLn "Menu>Login"
    putStrLn " .---------------------------------------------------------."
    putStrLn " | REALIZAR LOGIN                                          |"
    putStrLn " '---------------------------------------------------------'"
    putStrLn ""
    putStrLn "Username: "
    username <- getLine
    putStrLn "Senha: "
    password <- getLine
    --statusLogin <- loginUser username password
    putStrLn "Dados Incorretos"
    putStrLn "Tentar Novamente?"
    print "Sim (S) - Não (N)"
    resp <- getLine
    case resp of
        "S" -> login
        "N" -> menuInicial
        "s" -> login
        "n" -> menuInicial
        _ -> do 
            print "Opção Inválida"
            login

exibirFilmes :: IO ()
exibirFilmes = do
  clearScreen
  putStrLn "Menu>Exibir filmes"
  putStrLn " .--------------------------------------------------------------------------------------------."
  putStrLn " | FILMES EM CARTAZ                                                                           |"
  putStrLn " '--------------------------------------------------------------------------------------------'"
  putStrLn ""
  putStrLn "FILME 1"
  putStrLn "1- Ver mais informações "
  putStrLn "2- Comprar ingresso"
  putStrLn "3- Voltar ao menu"
  respFilme <- getLine
  case respFilme of
      "1" -> infoFilme
      "2" -> compraIngresso --direciona apenas se estiver logado
      "3" -> menuInicial
      _ -> do 
          print "Opção Inválida"
          exibirFilmes

infoFilme :: IO ()
infoFilme = do
  clearScreen
  putStrLn "Menu>Exibir filmes>Informações sobre o filme"
  putStrLn " .--------------------------------------------------------------------------------------------."
  putStrLn " | INFORMAÇÕES SOBRE O FILME                                                                  |"
  putStrLn " '--------------------------------------------------------------------------------------------'"
  putStrLn ""
  putStrLn "FILME 1" 
  putStrLn "Lorem ipsum dolor sit amet. Ut laboriosam corporis est ipsa doloremque et voluptatem doloremque" --dados do filme
  putStrLn "1- Comprar ingresso"
  putStrLn "2- Voltar"
  respInfo <- getLine
  case respInfo of
      "1" -> compraIngresso --direciona apenas se estiver logado
      "2" -> menuInicial
      _ -> do 
          print "Opção Inválida"
          infoFilme

compraIngresso :: IO ()
compraIngresso = do
  clearScreen
  putStrLn "Menu>Exibir filmes>Comprar Ingresso"
  putStrLn " .--------------------------------------------------------------------------------------------."
  putStrLn " | Escolher assentos                                                                          |"
  putStrLn " '--------------------------------------------------------------------------------------------'"
  putStrLn ""
  putStrLn "FILME 1"
  putStrLn "Escolha o assento desejado "
  putStrLn " .--------------------------------------------------------------------------------------------."
  putStrLn " |                                     E1 E2 E3 E4 E5                                         |"
  putStrLn " |                                     D1 D2 D3 D4 D5                                         |"
  putStrLn " |                                     C1 C2 C3 C4 C5                                         |"
  putStrLn " |                                     B1 B2 B3 B4 B5                                         |"
  putStrLn " |                                     A1 A2 A3 A4 A5                                         |"
  putStrLn " '--------------------------------------------------------------------------------------------'"
  putStrLn ""
  putStrLn "X- Voltar ao menu"
  respIngresso <- getLine
  case respIngresso of
      "A1" -> menuInicial --guarda o assento comprado
      "A2" -> menuInicial
      "A3" -> menuInicial
      "A4" -> menuInicial
      "A5" -> menuInicial
      "B1" -> menuInicial 
      "B2" -> menuInicial
      "B3" -> menuInicial
      "B4" -> menuInicial
      "B5" -> menuInicial
      "C1" -> menuInicial 
      "C2" -> menuInicial
      "C3" -> menuInicial
      "C4" -> menuInicial
      "C5" -> menuInicial
      "D1" -> menuInicial 
      "D2" -> menuInicial
      "D3" -> menuInicial
      "D4" -> menuInicial
      "D5" -> menuInicial
      "E1" -> menuInicial 
      "E2" -> menuInicial
      "E3" -> menuInicial
      "E4" -> menuInicial
      "E5" -> menuInicial
      "x"  -> menuInicial
      "X"  -> menuInicial
      _ -> do 
          print "Opção Inválida"
          compraIngresso

minhaConta :: IO ()
minhaConta = do
  clearScreen
  putStrLn "Menu>Minha Conta"
  putStrLn " .--------------------------------------------------------------------------------------------."
  putStrLn " | Minhas informações                                                                         |"
  putStrLn " '--------------------------------------------------------------------------------------------'"
  putStrLn ""
  putStrLn "INFORMAÇÕES DA CONTA" --pega os dados do usuário
  putStrLn "MEUS INGRESSOS" --pega os assentos escolhidos
  putStrLn "X- Voltar ao menu"
  respConta <- getLine
  case respConta of
      "x" -> menuInicial
      "X" -> menuInicial
      _ -> do 
          print "Opção Inválida"
          exibirFilmes