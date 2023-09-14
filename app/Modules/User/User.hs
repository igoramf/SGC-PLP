module Modules.Users.Users where


data User = User { username :: String, name :: String, password :: String, isAdm :: Bool } deriving (Show)


getUserName :: User -> String
getUserName = username

getName :: User -> String
getName = name

getPassword :: User -> String
getPassword = password

checkUserType :: User -> Bool
checkUserType = isAdm