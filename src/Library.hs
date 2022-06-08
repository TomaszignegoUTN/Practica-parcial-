module Library where
import PdePreludat

data Participante = UnParticipante {
    nombre :: String,
    experiencia :: Number,
    inteligencia :: Number,
    rol :: Rol,
    destreza :: Number
} deriving (Show, Eq)

data Arma = Arma {
    valorDeCombate :: Number,
    experienciaMinima :: Number
} deriving (Show)

type Rol = Participante -> Number 


indeterminado :: Rol
indeterminado unaParticipante = inteligencia unaParticipante + destreza unaParticipante

soporte :: Rol
soporte  = (*7).inteligencia 

primeraLinea :: Arma -> Rol
primeraLinea arma participante =  (destreza participante + potenciaArma arma participante) * experiencia participante / 100


--primeraLinea unArma unParticipante = (/).(*).(+) (destreza unParticipante) (potencia unArma) (experiencia unParticipante) 100   

--primeraLinea unArma unParticipante = div (( experiencia unParticipante  * ).(destreza unParticipante +) $ potencia unArma) 100   

juan = UnParticipante "Juan" 40 20 indeterminado 21

participanteEjemplo = UnParticipante {
    nombre = "Elegido",
    experiencia = 1000,
    inteligencia = 20,
    rol = indeterminado,
    destreza = 12
}

conocerPoder :: Rol
conocerPoder participante = (experiencia participante)*(rol participante participante)


maximoSegun :: Ord a => (b -> a) -> [b] -> b
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord a => (p -> a) -> p -> p -> p
mayorSegun f a b
    | f a > f b = a
    | otherwise = b


elegirNuevoRol :: [Rol] -> Participante -> Participante
elegirNuevoRol listaRoles participante =  participante{rol = maximoSegun ($ participante) listaRoles }


rolesDisponibles = [indeterminado,soporte,primeraLinea armaEjemplo]

potenciaArma arma participante 
    | experiencia participante >= experienciaMinima arma = valorDeCombate arma
    | otherwise = div (experiencia participante) 2

armaEjemplo = Arma {
    valorDeCombate = 20,
    experienciaMinima = 750
}

armaEjemplo2 = Arma {
    valorDeCombate = 40,
    experienciaMinima = 750
}

armaEjemplo3 = Arma {
    valorDeCombate = 50,
    experienciaMinima = 750
}

type Armas = [Arma]
armas = [armaEjemplo,armaEjemplo2,armaEjemplo3]

armasAptasRol :: Participante -> Arma -> Bool
armasAptasRol unParticipante unArma =  experienciaMinima unArma <= experiencia unParticipante

tomar3primerasArmasAptas :: Armas -> Participante -> Armas
tomar3primerasArmasAptas armas unParticipante = take 3 ( filter (armasAptasRol unParticipante) armas ) 



--maestroDeArmas :: [Armas] -> Rol
--maestroDeArmas armas unParticipante = sum.(map (potenciaArma unParticipante) ) $ tomar3primerasArmasAptas armas UnParticipante

-- Punto 2D

--Si ya que gracias a la evaluacion diferida, al generar las primeras 3 armas que cumplen con la condicion deja de evaluarse, no hace falta que llegues hasta el infinito


-- Punto 3

type GrupoDeParticipantes = [Participante]

seEncuentraParticipante :: GrupoDeParticipantes -> Participante -> Bool
seEncuentraParticipante grupoDeParticipantes participante = any (== nombre participante) (map nombre grupoDeParticipantes)

nivelDelParcipante participante = destreza participante + inteligencia participante 

calcularRecompensa grupoDeParticipantes grupoDeGanadores = mayorSegun
