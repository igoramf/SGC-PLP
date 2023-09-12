module Modules.Menus.Menus where

import Control.Monad
import System.Directory
import System.Exit (exitSuccess)
import System.FilePath
import System.IO
import Modules.Util.ClearScreen (clearScreen)

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
  print "|4. Sair                                           |"
  print "|                                                  |"
  print "|--------------------------------------------------|"
  opcao <- getLine
  case opcao of
    "1" -> cadastroUsuario
    "2" -> cadastroAdmin
    "3" -> login
    "4" -> exitSuccess
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