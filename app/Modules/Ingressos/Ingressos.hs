module Modules.Ingressos.Ingressos where


data Ingresso = Ingresso { valor :: Float, sala :: String, horario :: String, assento :: String } deriving (Show)


getValor :: Ingresso -> Float
getValor = valor

getSala :: Ingresso -> String
getSala = sala

getHorario :: Ingresso -> String
getHorario = horario

getAssento :: Ingresso -> String
getAssento = assento