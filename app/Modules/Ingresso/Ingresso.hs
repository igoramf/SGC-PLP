module Modules.Ingresso.Ingresso where

import Data.Time.Clock (UTCTime, getCurrentTime)



data Ingresso = Ingresso { valor :: Float, sala :: String, horario :: String, assento :: String } deriving (Show)


getValor :: Ingresso -> Float
getValor = valor

getSala :: Ingresso -> String
getSala = sala

getHorario :: Ingresso -> String
getHorario = horario

getAssento :: Ingresso -> String
getAssento = assento