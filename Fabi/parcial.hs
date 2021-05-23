data Color = Azul | Verde | Rojo deriving Show
data Torre = Base | Bloque Color Torre deriving Show

--Propósito: dada una torre, indica la cantidad de bloques que contiene.
cantidadDeBloques :: Torre -> Int
cantidadDeBloques Base = 0
cantidadDeBloques (Bloque c t) = 1 + cantidadDeBloques t

--Propósito: dados un color c y una torre, indica si todos los bloques de la torre son de color c.
todos :: Color -> Torre -> Bool
todos c Base = True
todos c (Bloque ct t) = sonElMismoColor c ct && todos c t

sonElMismoColor :: Color -> Color -> Bool
sonElMismoColor Azul Azul = True
sonElMismoColor Rojo Rojo = True
sonElMismoColor Verde Verde = True
sonElMismoColor _ _ = False

--Propósito: dados un número n, un color c y una torre, agrega n bloques de cemento de color c a la torre, luego del primer bloque de color c.
agregarOtrosN :: Int -> Color -> Torre -> Torre
agregarOtrosN 0 c t = t
agregarOtrosN n c Base = Base
agregarOtrosN n c (Bloque ct t) = 
	if sonElMismoColor c ct 
		then agregarBloque c (agregarOtrosN (n-1) c (Bloque ct t))
		else Bloque ct (agregarOtrosN n c t)

agregarBloque :: Color -> Torre -> Torre
agregarBloque c Base = Bloque c Base
agregarBloque c (Bloque ct t) = Bloque ct (Bloque c t)

--Propósito: dada una lista de colores y una torre, quita de la torre todos los bloques cuyo color sea alguno de los de la lista.
sinColores :: [Color] -> Torre -> Torre
sinColores [] t = t
sinColores _ Base = Base
sinColores xs (Bloque c t) = 
	if perteneceColor c xs 
		then sinColores xs t
		else Bloque c (sinColores xs t)

perteneceColor :: Color -> [Color] -> Bool
perteneceColor e [] = False
perteneceColor e (x:xs) = sonElMismoColor x e || perteneceColor e xs

--Propósito: dada una torre, denota la lista de pares donde el primer elemento es un color y el segundo elemento es la
--cantidad de veces que aparece dicho color.
--Nota: si el color no aparece, no hace falta que esté en la lista.
aparicionesDeColores :: Torre -> [(Color,Int)]
aparicionesDeColores Base = []
aparicionesDeColores t = aparicionesDeCadaColor t

aparicionesDeCadaColor :: Torre -> [(Color, Int)]
aparicionesDeCadaColor t = parDeColorEnTorre Azul t : parDeColorEnTorre Rojo t : parDeColorEnTorre Verde t : []

parDeColorEnTorre :: Color -> Torre -> (Color, Int)
parDeColorEnTorre Azul t = (Azul, (cantidadDeColorEnTorre Azul t))
parDeColorEnTorre Rojo t = (Rojo, (cantidadDeColorEnTorre Rojo t))
parDeColorEnTorre Verde t = (Verde, (cantidadDeColorEnTorre Verde t))

cantidadDeColorEnTorre :: Color -> Torre -> Int
cantidadDeColorEnTorre c Base = 0
cantidadDeColorEnTorre c (Bloque ct t) = 
	if sonElMismoColor c ct 
		then 1 + cantidadDeColorEnTorre c t 
		else cantidadDeColorEnTorre c t

type Energia = Int
type Clave = Int
data Direccion = Izquierda | Centro | Derecha deriving Show
data Dispositivo = Dispositivo Energia [Clave] deriving Show
data Escenario = Acceso Clave | Pared Dispositivo | Punto Dispositivo Escenario Escenario Escenario deriving Show

--Propósito: dado un escenario, denota la cantidad de accesos que contiene.
cantidadDeAccesos :: Escenario -> Int
cantidadDeAccesos (Acceso c) = 1
cantidadDeAccesos (Pared d) = 0
cantidadDeAccesos (Punto d e1 e2 e3) = cantidadDeAccesos e1 + cantidadDeAccesos e2 + cantidadDeAccesos e3

--Propósito: dado un escenario, denota la lista sin repetidos de todas las claves que hay en él.
todasLasClaves :: Escenario -> [Clave]
todasLasClaves e = sinRepetidos (todasLasClaves' e)

todasLasClaves' :: Escenario -> [Clave]
todasLasClaves' (Pared ds) = clavesDeDispositivo ds
todasLasClaves' (Acceso c) = [c]
todasLasClaves' (Punto ds e1 e2 e3) = 
	clavesDeDispositivo ds 
	++ todasLasClaves' e1 
	++ todasLasClaves' e2 
	++ todasLasClaves' e3

clavesDeDispositivo :: Dispositivo -> [Clave]
clavesDeDispositivo (Dispositivo _ xs) = xs

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
						then sinRepetidos xs
						else x : sinRepetidos xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = x == e || pertenece e xs

--Propósito: dada una cantidad de energía y un escenario, indica si es posible pasar de escenario con esa cantidad energía.
sePuedeSalirCon :: Energia -> Escenario -> Bool
sePuedeSalirCon _ (Acceso c) = True
sePuedeSalirCon n (Pared ds) = False
sePuedeSalirCon n (Punto ds e1 e2 e3) = 
	n >= energiaDeDispositivo ds && (
		sePuedeSalirCon (n - energiaDeDispositivo ds) e1 || 
		sePuedeSalirCon (n - energiaDeDispositivo ds) e2 || 
		sePuedeSalirCon (n - energiaDeDispositivo ds) e3)

energiaDeDispositivo :: Dispositivo -> Energia
energiaDeDispositivo (Dispositivo e _) = e

--Propósito: dado un camino y un escenario, indica si siguiendo el camino se pasa de escenario.
--Precondición: el camino es válido en el escenario.
--Nota: se asume suficiente cantidad de energía.
caminoGanador :: [Direccion] -> Escenario -> Bool
caminoGanador [] (Acceso c) = True
caminoGanador _ (Pared ds) = False
caminoGanador (x:xs) (Punto ds e1 e2 e3) = caminoGanador xs (proximoEscenarioEnCamino x (Punto ds e1 e2 e3))

proximoEscenarioEnCamino :: Direccion -> Escenario -> Escenario
proximoEscenarioEnCamino Izquierda (Punto ds e1 e2 e3) = e1
proximoEscenarioEnCamino Centro (Punto ds e1 e2 e3) = e2
proximoEscenarioEnCamino Derecha (Punto ds e1 e2 e3) = e3

--Propósito: dado un escenario, indica todos los caminos que pueden tomarse en él.
--Nota: un camino puede terminar en un acceso o en una pared.
todosLosCaminos :: Escenario -> [[Direccion]]
todosLosCaminos (Acceso _) = [[]]
todosLosCaminos (Pared _) = [[]]
todosLosCaminos (Punto ds e1 e2 e3) = 
	agregarATodasLasDirecciones Izquierda (todosLosCaminos e1) 
	++ agregarATodasLasDirecciones Centro (todosLosCaminos e2) 
	++ agregarATodasLasDirecciones Derecha (todosLosCaminos e3)

agregarATodasLasDirecciones :: Direccion -> [[Direccion]] -> [[Direccion]]
agregarATodasLasDirecciones d [] = []
agregarATodasLasDirecciones d (xs:xss) = (d : xs) : (agregarATodasLasDirecciones d xss)  