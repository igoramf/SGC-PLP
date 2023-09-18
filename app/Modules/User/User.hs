module Modules.User.User where

import Modules.Ingresso.Ingresso

data User = User { username :: String, name :: String, password :: String, isAdm :: Bool } | Adm { username :: String, name :: String, password :: String, isAdm :: Bool } deriving (Show)


getUserName :: User -> String
getUserName = username

getName :: User -> String
getName = name

getPassword :: User -> String
getPassword = password

checkUserType :: User -> Bool
checkUserType = isAdm

createUser :: String -> String -> String -> Bool -> User
createUser username name password isAdm = User username name password isAdm
createAdm :: String -> String -> String -> Bool -> User
createAdm username name password isAdm = Adm username name password isAdm 