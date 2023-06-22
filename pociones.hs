
data Persona = Persona {
  nombrePersona :: String,
  suerte :: Int,
  inteligencia :: Int,
  fuerza :: Int
} deriving (Show, Eq)

data Pocion = Pocion {
  nombrePocion :: String,
  ingredientes :: [Ingrediente]
}

type Efecto = Persona -> Persona

data Ingrediente = Ingrediente {
  nombreIngrediente :: String,
  efectos :: [Efecto]
}

nombresDeIngredientesProhibidos = [
 "sangre de unicornio",
 "veneno de basilisco",
 "patas de cabra",
 "efedrina"]

maximoSegun :: Ord b => (a -> b) -> [a] -> a
-- casos base
maximoSegun _ [ x ] = x  
maximoSegun  f ( x : y : xs) | f x > f y = maximoSegun f (x:xs)
                             | otherwise = maximoSegun f (y:xs)

sumaDeNiveles :: Persona -> Int
--   sumaDeNiveles persona = suerte persona + inteligencia persona + fuerza persona
sumaDeNiveles= sum .niveles 

diferenciaDeNiveles:: Persona->Int  
diferenciaDeNiveles persona = (maximum . niveles)persona - (minimum . niveles) persona

niveles:: Persona -> [Int]
niveles persona = [suerte persona, inteligencia persona, fuerza persona]

nivelesMayoresA:: Persona -> Int -> Int
nivelesMayoresA persona cotaInferior = (length . filter (>cotaInferior).niveles)persona

efectosDePocion :: Pocion -> [Efecto]

efectosDePocion pocion = (concat.map efectos.ingredientes)pocion

nombreDePocionesHardcore:: [Pocion]->[String]
nombreDePocionesHardcore = map nombrePocion . filter ((>=4).length.efectosDePocion)

--- cantidad de pociones prohibidas ----

cantidadDeProhibidos :: [Pocion] -> Int
cantidadDeProhibidos = length . filter pocionProhibida

pocionProhibida:: Pocion -> Bool
pocionProhibida  = any (esProhibido) . ingredientes

esProhibido :: Ingrediente -> Bool
esProhibido ingrediente = elem (nombreIngrediente ingrediente) nombresDeIngredientesProhibidos

------------------------

todosDulces :: [Pocion] -> Bool
todosDulces = all esDulce

esDulce :: Pocion -> Bool
esDulce = any ( ("azucar"==) . nombreIngrediente) . ingredientes

----------------------

tomarPocion:: Pocion -> Persona -> Persona

tomarPocion pocion personaInicial = (foldl (\persona efecto -> efecto persona) personaInicial . efectosDePocion) pocion
                                  -- foldl (\funcion reductora) semilla lista
-- aplicarEfecto :: Persona -> Efecto -> Persona

esAntidotoDe:: Pocion -> Pocion -> Persona -> Bool
esAntidotoDe pocion antidoto persona = ((persona==). tomarPocion antidoto . tomarPocion pocion) persona
-- esAntidotoDe pocion antidoto poersona = ("persona"==). tomarPocion antidoto . (tomarPocion pocion persona)

personaMasAfectada :: Pocion -> (Persona -> Int) -> [Persona] -> Persona
personaMasAfectada pocion cuantificador persona = maximoSegun ( cuantificador . tomarPocion pocion) persona
-- NuevapersonaMasAfectada pocion cuantificador persona = (maximoSegun cuantificador . map (tomarPocion pocion)) persona

