module Modules.Users.Users where
import System.IO


data User = User { username :: String, name :: String, password :: String, isAdm :: Bool }

