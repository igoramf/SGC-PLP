module Sala where


import Filmes (Filme, cadastraFilme, registraCaixaFilme, defineMetaArrecadacaoFilme)


-- Estrutura da "classe" para uma sala.
data Sala = Sala {
 numero :: Int,
 capacidade :: Int,
 filmeExibicao :: Maybe Filme,
 horarioExibicao :: Maybe String,
 assentosDisponiveis :: [[Bool]] -- Matriz de assentos
}


-- Lista de salas.
type Salas = [Sala]


-- Função para cadastrar uma nova sala.
cadastraSala :: Int -> Int -> Salas -> Salas
cadastraSala numeroSala capacidadeSala salas =
 salas ++ [Sala numeroSala capacidadeSala Nothing Nothing (indexarAssentos capacidadeSala)]


-- Função para listar salas disponíveis.
listaSalasDisponiveis :: Salas -> [Sala]
listaSalasDisponiveis salas =
 filter (\s -> isSalaDisponivel s) salas


isSalaDisponivel :: Sala -> Bool
isSalaDisponivel sala =
 case filmeExibicao sala of
   Nothing -> True
   Just _  -> False


-- Função para associar uma sala a um filme e horário.
associarSalaFilmeHorario :: Int -> Filme -> String -> Salas -> Salas
associarSalaFilmeHorario numeroSala filme horario salas =
 map (\s -> if numero s == numeroSala then s { filmeExibicao = Just filme, horarioExibicao = Just horario } else s) salas


-- Função para indexar os assentos disponíveis em uma sala.
indexarAssentos :: Int -> [[Bool]]
indexarAssentos capacidadeSala = replicate capacidadeSala (replicate capacidadeSala True)


-- Função para verificar a disponibilidade de assentos em uma sala.
verificarDisponibilidadeAssentos :: Sala -> (Int, Int) -> Bool
verificarDisponibilidadeAssentos sala (fila, assento) =
 let assentos = assentosDisponiveis sala
 in
   if fila >= 1 && fila <= capacidade sala && assento >= 1 && assento <= capacidade sala
     then (assentos !! (fila - 1)) !! (assento - 1)
     else False


-- Função para exibir os assentos disponíveis de forma visual.
exibirAssentosDisponiveis :: Sala -> IO ()
exibirAssentosDisponiveis sala =
 let matrizAssentos = assentosDisponiveis sala
 in do
   putStrLn "Assentos Disponíveis:"
   mapM_ (putStrLn . formatarFileira) matrizAssentos
 where
   formatarFileira fila = unwords [if assento then "O" else "X" | assento <- fila]


-- Instância Show para Sala
instance Show Sala where
 show sala = "Sala " ++ show (numero sala) ++ " - Capacidade: " ++ show (capacidade sala)


-- Teste rápido de uso.
main :: IO ()
main = do
 let salas = cadastraSala 1 10 []
 putStrLn "Salas Disponíveis:"
 mapM_ print $ listaSalasDisponiveis salas


 let filme = Filme "Auto da Compadecida" "Comédia" 180 0
 let salasAtualizadas = associarSalaFilmeHorario 1 filme "19:00" salas
 putStrLn "\nSalas Após Associação com Filme e Horário:"
 mapM_ print salasAtualizadas


 putStrLn "\nAssentos Disponíveis na Sala 1:"
 let sala1 = head salasAtualizadas
 exibirAssentosDisponiveis sala1


 putStrLn "\nVerificação de Disponibilidade do Assento (1, 5) na Sala 1:"
 let assentoDisponivel = verificarDisponibilidadeAssentos sala1 (1, 5)
 putStrLn $ if assentoDisponivel then "Assento (1, 5) disponível." else "Assento (1, 5) ocupado."
