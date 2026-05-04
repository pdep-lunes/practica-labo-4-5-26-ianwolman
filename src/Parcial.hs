module Parcial where
import Text.Show.Functions()

--Parte A
data Perro = UnPerro { 
                    raza :: String,
                    juguetesFavoritos :: [String],
                    tiempoEnGuarderia :: Int,
                    energiaDisponible :: Int
                    }deriving (Show, Eq)    
         
modificarEnergia :: (Int->Int) -> Perro -> Perro
modificarEnergia unaFuncion unPerro = unPerro{energiaDisponible = (max 0).unaFuncion.energiaDisponible $ unPerro}

jugar :: Perro -> Perro
jugar unPerro = modificarEnergia (subtract 10) unPerro

mitad :: Int -> Int
mitad unaCant = div unaCant 2

ladrar :: Int -> Perro -> Perro
ladrar cantidadLadridos unPerro = modificarEnergia (+mitad cantidadLadridos) unPerro

regalar :: String -> Perro -> Perro
regalar unJuguete unPerro = unPerro{juguetesFavoritos = unJuguete : juguetesFavoritos unPerro}

diaDeSpa :: Perro -> Perro 
diaDeSpa unPerro 
 |masDe50Minutos unPerro || esDeRazaExtravagante unPerro = (regalar "peine de goma").subirEnergiaA100 $ unPerro
 |otherwise = unPerro

masDe50Minutos :: Perro -> Bool
masDe50Minutos unPerro = tiempoEnGuarderia unPerro >= 50

esDeRazaExtravagante :: Perro -> Bool
esDeRazaExtravagante unPerro = esDeRaza "pomerania" unPerro || esDeRaza "dalmata" unPerro

esDeRaza :: String -> Perro -> Bool
esDeRaza unaRaza unPerro = unaRaza == raza unPerro

subirEnergiaA100 :: Perro -> Perro
subirEnergiaA100 unPerro = unPerro{energiaDisponible = 100}

diaDeCampo :: Perro -> Perro
diaDeCampo unPerro = unPerro{juguetesFavoritos = drop 1 (juguetesFavoritos unPerro)}

zara :: Perro
zara = UnPerro "dalmata"  ["Pelota", "Mantita"] 90 80

data Guarderia = UnaGuarderia {
            nombreGuarderia :: String,
            actividades :: [Actividad]
            }deriving (Show)

type Actividad = ((Perro->Perro), Int)

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = UnaGuarderia "GuarderiaPdePerritos" [(jugar, 30), (ladrar 18, 20), (regalar "pelota", 0), (diaDeSpa, 120), (diaDeCampo, 720)]

--ParteB
tiempo :: Actividad -> Int
tiempo (_, tiempo) = tiempo

tiempoTotalDeGuarderia :: Guarderia -> Int
tiempoTotalDeGuarderia unaGuarderia = sum.(map tiempo).actividades $ unaGuarderia

puedeEstarEnGuarderia :: Perro -> Guarderia -> Bool
puedeEstarEnGuarderia unPerro unaGuarderia = tiempoEnGuarderia unPerro > tiempoTotalDeGuarderia unaGuarderia

perroResponsable :: Perro -> Bool
perroResponsable unPerro = 3 < cantidadDeJuguetesDespuesDelDiaDeCampo unPerro

cantidadDeJuguetesDespuesDelDiaDeCampo :: Perro -> Int
cantidadDeJuguetesDespuesDelDiaDeCampo unPerro = length.juguetesFavoritos.diaDeCampo $ unPerro 
