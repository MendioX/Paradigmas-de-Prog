data Elemento = UnElemento { tipo :: String,
				  ataque :: (Personaje-> Personaje),
				  defensa :: (Personaje-> Personaje) }

data Personaje = UnPersonaje { nombre :: String,
				    salud :: Float,
				    elementos :: [Elemento],
				    anioPresente :: Int }


mandaAlAnio :: Int -> Personaje -> Personaje

mandaAlAnio year pj =  pj { anioPresente = year }


meditar:: Personaje -> Personaje

meditar pj = pj {salud = salud pj + (salud pj)/2 } 

causarDanio ::  Float ->  Personaje -> Personaje

causarDanio damage pj | ((salud pj - damage) >= 0) = pj {salud = (salud pj - damage)}
                      | otherwise =   pj {salud = 0.0 }


esMalvado :: Personaje -> Bool

esMalvado = any (elementoMaldad) . elementos 

elementoMaldad :: Elemento -> Bool
elementoMaldad elemnto = "Maldad" == tipo elemnto
 
danioQueProduce :: Personaje -> Elemento -> Float

danioQueProduce pj elemento= salud pj -  salud(ataque elemento pj)
-- danioQueProduce pj elemento = salud . (causarDanio pj) . ataque  

enemigosMortales:: Personaje -> [Personaje] -> [Personaje]

enemigosMortales pj =  filter ( enemigoEsMortal pj )  

enemigoEsMortal :: Personaje -> Personaje -> Bool

enemigoEsMortal pj = any(elementoEsmortal pj) . elementos

elementoEsmortal:: Personaje -> Elemento -> Bool

elementoEsmortal pj  = (0==) . salud . flip causarDanio pj . (danioQueProduce pj)


concentracion:: Int -> Elemento

concentracion nivelDeConcentracion = UnElemento {
		tipo = "Magia",
		ataque =  causarDanio 0,
		defensa = (!! nivelDeConcentracion) . iterate meditar}

esbirrosMalvados :: Int -> [Elemento]

esbirrosMalvados cantidad = replicate cantidad unEsbirro


unEsbirro :: Elemento

unEsbirro = UnElemento {
	tipo = "Maldad",
	ataque = causarDanio 1,
	defensa = causarDanio 0
}

jack :: Personaje
katanaMagica:: Elemento
katanaMagica = UnElemento {
	tipo = "Magia",
	ataque = causarDanio 1000,
	defensa= causarDanio 0
}

jack = UnPersonaje { nombre = "Jack",
				    salud = 300,
				    elementos = [katanaMagica],
				    anioPresente = 200 }

aku :: Int -> Float -> Personaje

aku year salud = UnPersonaje {
	nombre = "Aku",
	salud= salud,
	anioPresente= year,
	elementos =concentracion 4: esbirrosMalvados (100 * year) : portalFuturo (100*year)
	

}
portalFuturo:: Int -> Elemento

portalFuturo year = UnElemento {
	tipo = "Magia",
	ataque = (mandaAlAnio year+2800),
	defensa= causarDanio 0
}

