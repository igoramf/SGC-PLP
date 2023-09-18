module Modules.Users.Users where


data User = User { username :: String, name :: String, password :: String, isAdm :: Bool, ingressos :: [String] } deriving (Show)


getUserName :: User -> String
getUserName = username

getName :: User -> String
getName = name

getPassword :: User -> String
getPassword = password

getIngressos :: User -> [String]
getIngressos = ingressos

checkUserType :: User -> Bool
checkUserType = isAdm