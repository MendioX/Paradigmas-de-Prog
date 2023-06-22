data Elemento = UnElemento { tipo :: String,
				  ataque :: (Personaje-> Personaje),
				  defensa :: (Personaje-> Personaje) }

data Personaje = UnPersonaje { nombre :: String,
				    salud :: Float,
				    elementos :: [Elemento],
				    anioPresente :: Int }


mandaAlAnio :: Int -> Personaje -> Personaje

mandaAlAnio year pj =  Personaje { anioPresente = year }


meditar:: Personaje -> Personaje

meditar pj = Personaje {salud = salud pj + (salud pj)/2 } 

causarDanio :: Personaje -> Float ->  Personaje

causarDanio pj damage | (salud pj - damage) >= 0 = Personaje {salud = (salud pj - damage)}
                      | otherwise =   Personaje {salud = 0 }


esMalvado :: Personaje -> Bool

esMalvado = (any (tipo == "Maldad")) . elementos

danioQueProduce :: Personaje -> Elemento -> Float

danioQueProduce pj elemento = ((salud pj) - ). (salud pj -) . ataque elemento
-- danioQueProduce pj elemento = salud . (causarDanio pj) . ataque  

enemigosMortales:: Personaje -> [Personaje] -> [Personaje]

enemigosMortales pj enemigosMortales =  filter ( enemigoEsMortal pj )  

enemigoEsMortal :: Personaje -> Personaje -> Bool

enemigoEsMortal pj enemigo = any(elementoEsmortal pj) . map (elementos) enemigo

elementoEsmortal:: Personaje -> Elemento -> Bool

elementoEsmortal pj elemento = (0 == salud ) . (causarDanio pj) . ataque elemento     


