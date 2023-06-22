type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto {
 ambientes :: Int,
 superficie :: Int,
 precio :: Int,
 barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
   mail :: Mail,
   busquedas :: [Busqueda]
}

ordenarSegun ::(a-> a->Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
 (ordenarSegun criterio . filter (not . criterio x)) xs ++
 [x] ++
 (ordenarSegun criterio . filter (criterio x)) xs
between :: Ord a => a -> a -> a -> Bool
between cotaInferior cotaSuperior valor =
 valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
 Depto 3 80 7500 "Palermo",
 Depto 1 45 3500 "Villa Urquiza",
 Depto 2 50 5000 "Palermo",
 Depto 1 45 5500 "Recoleta"]

--1)
-- A)Definir las funciones mayor y menor que reciban una función y dos valores,
-- y retorna true si el resultado de evaluar esa función sobre el primer valor 
-- es mayor o menor que el resultado de evaluarlo sobre el segundo valor respectivamente.

mayor :: Ord b =>  (a -> b) ->( a -> a -> Bool)
mayor f valorA valorB =  f valorA > f valorB

menor :: Ord b => (a -> b) -> (a -> a -> Bool)
menor f valor1 valor2 =  f valor1 < f valor2


-- B)Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de strings
--  en base a su longitud usando ordenarSegun.
-- ejemploOrdenar :: ( a-> b) -> [a] -> [a]
ejemploOrdenar = ordenarSegun (mayor length) ["asd" , "asdd" , "lp"]

--2)
-- Definir las siguientes funciones para que puedan ser usadas como requisitos de búsqueda:
-- a) ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero 
-- si el departamento se encuentra en alguno de los barrios de la lista.

ubicadoEn :: [Barrio] -> Depto -> Bool

ubicadoEn barriols depto = elem (barrio depto) barriols

-- b) cumpleRango que a partir de una función y dos números,
-- indique si el valor retornado por la función al ser aplicada con el departamento se encuentra entre los dos valores indicados.

-- cumpleRango :: Int -> Int -> Depto -> Bool 

cumpleRango :: Ord a => (Depto -> a) -> a -> a -> Depto -> Bool

cumpleRango f valor1 valor2 = (between valor1 valor2) . f 

--3)
-- a) Definir la función cumpleBusqueda que se cumple si todos los requisitos
--  de una búsqueda se verifican para un departamento dado.

-- cumpleBusqueda :: [Barrio]-> Depto -> Int -> Int-> Bool

cumpleBusqueda :: Depto -> (Busqueda -> Bool)
cumpleBusqueda depto = all (\requisito -> requisito depto)


-- b) Definir la función buscar que a partir de una búsqueda,
-- un criterio de ordenamiento y una lista de departamentos retorne todos aquellosque
--  cumplen con la búsqueda ordenados en base al criterio recibido

buscar :: Busqueda -> (Depto -> Depto -> Bool) ->([Depto] -> [Depto])

buscar busqueda criterio  =   ordenarSegun criterio . filter ( flip cumpleBusqueda busqueda) 


-- 4)
--  bDefinir la función mailsDePersonasInteresadas que a partir de un departamento
-- y una lista de personas retorne los mails de las personas que tienen alguna búsqueda que se cumpla para el departamento dado.

mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]

mailsDePersonasInteresadas depto  = map mail . filter (any (cumpleBusqueda depto) . busquedas)
