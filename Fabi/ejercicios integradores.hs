{-
Tenemos los siguientes tipos de datos:
data Pizza = Prepizza

| Capa Ingrediente Pizza

data Ingrediente = Salsa
| Queso
| Jamon
| Aceitunas Int
Definir las siguientes funciones:
cantidadDeCapas :: Pizza -> Int
Dada una pizza devuelve la lista de capas de ingredientes
armarPizza :: [Ingrediente] -> Pizza
Dada una lista de ingredientes construye una pizza
sacarJamon :: Pizza -> Pizza
Le saca los ingredientes que sean jamón a la pizza
tieneSoloSalsaYQueso :: Pizza -> Bool
Dice si una pizza tiene salsa y queso
duplicarAceitunas :: Pizza -> Pizza
Recorre cada ingrediente y si es aceitunas duplica su cantidad
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
ingredientes de la pizza, y la respectiva pizza como segunda componente.
-}

data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int deriving Show

muzarella = Capa Salsa (Capa Queso Prepizza)
jamon = Capa Salsa (Capa Queso (Capa Jamon (Capa Queso (Capa Jamon Prepizza))))
aceitunas = Capa Salsa (Capa Queso (Capa (Aceitunas 2) Prepizza))

--Dada una pizza devuelve la lista de capas de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

--Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) = Capa x (armarPizza xs)

--Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa i p) = if esJamon i then sacarJamon p else (Capa i (sacarJamon p))

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False

--Dice si una pizza tiene salsa y queso
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa i p) = esQuesoOSalsa i && tieneSoloSalsaYQueso p

esQuesoOSalsa :: Ingrediente -> Bool
esQuesoOSalsa Queso = True
esQuesoOSalsa Salsa = True
esQuesoOSalsa x = False


--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) = if esAceituna i then (Capa (duplicarAceitunasEnIngrediente i) (duplicarAceitunas p)) else (Capa i (duplicarAceitunas p))

duplicarAceitunasEnIngrediente :: Ingrediente -> Ingrediente
duplicarAceitunasEnIngrediente (Aceitunas c) = (Aceitunas (c * 2))

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas _) = True
esAceituna _ = False

--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
--ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (x:xs) = (cantidadDeCapas x, x) : cantCapasPorPizza xs

{-
Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y
cada cofre tiene un objeto, que puede ser chatarra o un tesoro.
data Dir = Izq | Der
data Objeto = Tesoro | Chatarra
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre

| Bifurcacion Cofre Mapa Mapa

1. hayTesoro :: Mapa -> Bool
Indica si hay un tesoro en alguna parte del mapa.
2. hayTesoroEn :: [Dir] -> Mapa -> Bool
Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
lista vacía de direcciones.
3. caminoAlTesoro :: Mapa -> [Dir]
Indica el camino al tesoro. Precondición: existe un tesoro y es único.
4. caminoDeLaRamaMasLarga :: Mapa -> [Dir]
Indica el camino de la rama más larga.
5. tesorosPorNivel :: Mapa -> [[Objeto]]
Devuelve los tesoros separados por nivel en el árbol.
6. todosLosCaminos :: Mapa -> [[Dir]]
Devuelve todos lo caminos en el mapa.
-}

data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra deriving Show
data Cofre = Cofre [Objeto] deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa deriving Show


mapaTesoro = Bifurcacion (Cofre []) 
				(Bifurcacion (Cofre []) 
					(Bifurcacion (Cofre []) 
						(Fin (Cofre [])) 
						(Fin (Cofre [Tesoro])))
					(Fin (Cofre [])))
				(Fin (Cofre []))

mapaSinTesoro = Bifurcacion (Cofre []) 
				(Bifurcacion (Cofre [Chatarra]) 
					(Fin (Cofre []))
					(Fin (Cofre [])))
				(Fin (Cofre []))

mapaLargo = Bifurcacion (Cofre [Tesoro]) 
				(Bifurcacion (Cofre []) 
					(Bifurcacion (Cofre []) 
						(Bifurcacion (Cofre []) 
							(Fin(Cofre []))
							(Bifurcacion (Cofre []) 
								(Fin(Cofre []))
								(Fin(Cofre [])))) 
						(Fin (Cofre [Tesoro])))
					(Fin (Cofre [])))
				(Fin (Cofre [Tesoro]))

direccionATesoro :: [Dir]
direccionATesoro = [Izq,Izq,Der]

--Indica si hay un tesoro en alguna parte del mapa.
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = hayTesoroEnCofre c
hayTesoro (Bifurcacion c mi md) = hayTesoroEnCofre c || hayTesoro mi || hayTesoro md

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre []) = False
hayTesoroEnCofre (Cofre (x:xs)) = esTesoro x || hayTesoroEnCofre (Cofre xs)

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

--Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
--lista vacía de direcciones.
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Bifurcacion c mi md) = hayTesoroEnCofre c
hayTesoroEn [] (Fin cofre) = hayTesoroEnCofre cofre
hayTesoroEn (x:xs) (Bifurcacion _ mi md) = if esDerecha x then hayTesoroEn xs md else hayTesoroEn xs mi
hayTesoroEn _ (Fin cofre) = False


esDerecha :: Dir -> Bool
esDerecha Der = True
esDerecha _ = False

--Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin _) = []
caminoAlTesoro (Bifurcacion c mi md) = if hayTesoro mi then (Izq:caminoAlTesoro mi)  else (Der:caminoAlTesoro md)

--Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _) = []
caminoDeLaRamaMasLarga (Bifurcacion c mi md) = if heightMapa mi > heightMapa md then (Izq:caminoDeLaRamaMasLarga mi) else (Der:caminoDeLaRamaMasLarga md)

heightMapa :: Mapa -> Int
heightMapa (Fin _) = 0
heightMapa (Bifurcacion c mi md) = if isLeaf (Bifurcacion c mi md) then max (heightMapa mi) (heightMapa md) else 1 + max (heightMapa mi) (heightMapa md)

isLeaf :: Mapa -> Bool
isLeaf (Fin _) = False
isLeaf (Bifurcacion _ mi md) = esFin mi && esFin md

esFin :: Mapa -> Bool
esFin (Fin _) = True
esFin _ = False

--Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = [tesoros (objetos c)]
tesorosPorNivel (Bifurcacion c mi md) = (tesoros (objetos c)) : juntarTesorosPorNivel (tesorosPorNivel mi) (tesorosPorNivel md)

juntarTesorosPorNivel :: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
juntarTesorosPorNivel [] [] = []
juntarTesorosPorNivel [] yss = yss
juntarTesorosPorNivel xss [] = xss
juntarTesorosPorNivel (xs:xss) (ys:yss) = (xs ++ ys) : juntarTesorosPorNivel xss yss

objetos :: Cofre -> [Objeto]
objetos (Cofre o) = o

tesoros :: [Objeto] -> [Objeto]
tesoros [] = []
tesoros  (x:xs) = if esTesoro x then x : tesoros xs else tesoros xs

{-
modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial,
dividida en sectores, a los cuales podemos asignar tripulantes y componentes. La representación
es la siguiente:
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible
data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Nave = N (Tree Sector)
Implementar las siguientes funciones utilizando recursión estructural:
1. sectores :: Nave -> [SectorId]
Propósito: Devuelve todos los sectores de la nave.
2. poderDePropulsion :: Nave -> Int
Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
el poder de propulsión es el número que acompaña al constructor de motores.
3. barriles :: Nave -> [Barril]
Propósito: Devuelve todos los barriles de la nave.
4. agregarASector :: [Componente] -> SectorId -> Nave -> Nave
Propósito: Añade una lista de componentes a un sector de la nave.
Nota: ese sector puede no existir, en cuyo caso no añade componentes.
5. asignarTripulanteA :: Tripulante -> [SectorId] -> Nave ->\ Nave
Propósito: Incorpora un tripulante a una lista de sectores de la nave.
Precondición: Todos los id de la lista existen en la nave.
6. sectoresAsignados :: Tripulante -> Nave -> [SectorId]
Propósito: Devuelve los sectores en donde aparece un tripulante dado.
-}

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show
data Sector = S SectorId [Componente] [Tripulante] deriving Show
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
data Nave = N (Tree Sector) deriving Show

nave = N (NodeT (S "1" [(Motor 5), (Almacen [Comida, Oxigeno])] []) 
			(NodeT (S "2" [(Motor 10), (Almacen [Comida, Oxigeno])] []) EmptyT EmptyT) 
			(NodeT (S "3" [(Motor 20)] []) EmptyT EmptyT))

--Propósito: Devuelve todos los sectores de la nave.
sectores :: Nave -> [SectorId]
sectores (N EmptyT) = []
sectores (N (NodeT (S id _ _) sl sr)) = id : sectores (N sl) ++ sectores (N sr)

--Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
--el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int
poderDePropulsion (N EmptyT) = 0
poderDePropulsion (N (NodeT (S id c _) sl sr)) = propulsionDeComponentes c + poderDePropulsion (N sl) + poderDePropulsion (N sr)

propulsionDeComponentes :: [Componente] -> Int
propulsionDeComponentes [] = 0
propulsionDeComponentes (x:xs) = if esMotor x then poderDeMotor x + propulsionDeComponentes xs else propulsionDeComponentes xs

esMotor :: Componente -> Bool
esMotor (Motor _) = True
esMotor _ = False

poderDeMotor :: Componente -> Int
poderDeMotor (Motor p) = p
poderDeMotor _ = error "no es un motor"

--Propósito: Devuelve todos los barriles de la nave.
barriles :: Nave -> [Barril]
barriles (N EmptyT) = []
barriles (N (NodeT (S id c _) sl sr)) = barrilesEnComponentes c ++ barriles (N sl) ++ barriles (N sr)

barrilesEnComponentes :: [Componente] -> [Barril]
barrilesEnComponentes [] = []
barrilesEnComponentes (x:xs) = if esAlmacen x then barrilesEnAlmacen x ++ barrilesEnComponentes xs else barrilesEnComponentes xs

esAlmacen :: Componente -> Bool
esAlmacen (Almacen _) = True
esAlmacen _ = False

barrilesEnAlmacen :: Componente -> [Barril]
barrilesEnAlmacen (Almacen xs) = xs
barrilesEnAlmacen _ = error "No es un almacen"

--Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregaASector [] _ _ = []
