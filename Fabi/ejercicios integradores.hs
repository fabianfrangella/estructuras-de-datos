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
duplicarAceitunas (Capa i p) = 
	(Capa (duplicarAceitunasEnIngrediente i) (duplicarAceitunas p)) 

duplicarAceitunasEnIngrediente :: Ingrediente -> Ingrediente
duplicarAceitunasEnIngrediente (Aceitunas c) = (Aceitunas (c * 2))
duplicarAceitunasEnIngrediente i = i

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
hayTesoroEn (x:xs) (Bifurcacion _ mi md) = 
	if esDerecha x 
		then hayTesoroEn xs md 
		else hayTesoroEn xs mi
hayTesoroEn _ (Fin cofre) = False


esDerecha :: Dir -> Bool
esDerecha Der = True
esDerecha _ = False

--Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin _) = []
caminoAlTesoro (Bifurcacion c mi md) = 
	if hayTesoro mi 
		then (Izq:caminoAlTesoro mi)  
		else (Der:caminoAlTesoro md)

--Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _) = []
caminoDeLaRamaMasLarga (Bifurcacion c mi md) = 
	if heightMapa mi > heightMapa md 
		then (Izq:caminoDeLaRamaMasLarga mi) 
		else (Der:caminoDeLaRamaMasLarga md)

heightMapa :: Mapa -> Int
heightMapa (Fin _) = 0
heightMapa (Bifurcacion c mi md) = 
	if isLeaf (Bifurcacion c mi md) 
		then max (heightMapa mi) (heightMapa md) 
		else 1 + max (heightMapa mi) (heightMapa md)

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

nave :: Nave
nave = N (NodeT (S "1" [(Motor 5), (Almacen [Comida, Oxigeno])] ["Fabi"]) 
			(NodeT (S "2" [(Motor 10), (Almacen [Comida, Oxigeno])] []) EmptyT EmptyT) 
			(NodeT (S "3" [(Motor 20)] ["Fabi"]) EmptyT EmptyT))

--Propósito: Devuelve todos los sectores de la nave.
sectores :: Nave -> [SectorId]
sectores (N t) = sectoresT t

sectoresT :: (Tree Sector) -> [SectorId]
sectoresT EmptyT = []
sectoresT (NodeT sector si sd) = 
	(sectorId sector) : (sectoresT si) ++ (sectoresT sd)

sectorId :: Sector -> SectorId
sectorId (S sid _ _) = sid


--Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
--el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int
poderDePropulsion (N t) = poderDePropulsionT t

poderDePropulsionT :: (Tree Sector) -> Int
poderDePropulsionT EmptyT = 0
poderDePropulsionT (NodeT sector si sd) = 
	poderDePropulsionSector sector + 
	poderDePropulsionT si + 
	poderDePropulsionT sd

poderDePropulsionSector :: Sector -> Int
poderDePropulsionSector (S _ xs _) = poderDePropulsionDeComponentes xs

poderDePropulsionDeComponentes :: [Componente] -> Int
poderDePropulsionDeComponentes [] = 0
poderDePropulsionDeComponentes (x:xs) = 
	poderDePropulsionDeComponente x + 
	poderDePropulsionDeComponentes xs

poderDePropulsionDeComponente :: Componente -> Int
poderDePropulsionDeComponente (Motor p) = p 
poderDePropulsionDeComponente _ = 0

--Propósito: Devuelve todos los barriles de la nave.
barriles :: Nave -> [Barril]
barriles (N t) = barrilesT t 

barrilesT :: Tree Sector -> [Barril]
barrilesT EmptyT = []
barrilesT (NodeT sector si sd) = barrilesSector sector ++ barrilesT si ++ barrilesT sd

barrilesSector :: Sector -> [Barril]
barrilesSector (S _ xs _) = barrilesEnListaDeComponentes xs

barrilesEnListaDeComponentes :: [Componente] -> [Barril]
barrilesEnListaDeComponentes [] = []
barrilesEnListaDeComponentes (x:xs) = barrilesEnComponente x ++ barrilesEnListaDeComponentes xs

barrilesEnComponente :: Componente -> [Barril]
barrilesEnComponente (Almacen xs) = xs
barrilesEnComponente _ = []


--Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector [] _ nave = nave
agregarASector xs sid (N t) = (N (agregarASectorT xs sid t))

agregarASectorT :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorT [] _ s = s
agregarASectorT _ _ EmptyT = EmptyT
agregarASectorT xs sid (NodeT s si sd) = (NodeT (agregarComponentes xs sid s) (agregarASectorT xs sid si) (agregarASectorT xs sid sd))

agregarComponentes :: [Componente] -> SectorId -> Sector -> Sector
agregarComponentes xs sid (S ssid c t) =  
	if sid == ssid 
		then (S ssid (xs ++ c) t) 
		else (S ssid c t)

--Propósito: Incorpora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA t xs (N tr) = (N (asignarTripulanteAT t xs tr ))

asignarTripulanteAT :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteAT t xs EmptyT = EmptyT
asignarTripulanteAT t (x:xs) (NodeT s sd si) = (NodeT (asignarTripulanteASector t x s) (asignarTripulanteAT t xs si) (asignarTripulanteAT t xs sd))

asignarTripulanteASector :: Tripulante -> SectorId -> Sector -> Sector
asignarTripulanteASector t sid (S ssid cs ts) = 
	if sid == ssid
		then (S ssid cs (t: ts)) 
		else (S ssid cs ts)

--Propósito: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados tr (N t) = sectoresAsignadosT tr t

sectoresAsignadosT :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosT _ EmptyT = []
sectoresAsignadosT tr (NodeT s si sd) = 
	if estaEnSector tr s 
		then (sectorId s) : (sectoresAsignadosT tr si) ++ (sectoresAsignadosT tr sd)
		else (sectoresAsignadosT tr si) ++ (sectoresAsignadosT tr sd)

estaEnSector :: Tripulante -> Sector -> Bool
estaEnSector t (S sid cs ts) = estaEnListaDeTripulantes t ts

estaEnListaDeTripulantes :: Tripulante -> [Tripulante] -> Bool
estaEnListaDeTripulantes _ [] = False
estaEnListaDeTripulantes t (x:xs) = t == x || estaEnListaDeTripulantes t xs


{-
1. Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean
crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura
que corresponda en cada caso:
2. buenaCaza :: Manada -> Bool

Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la can-
tidad de crías.

3. elAlfa :: Manada -> (Nombre, Int)
Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
cero presas.
4. losQueExploraron :: Territorio -> Manada -> [Nombre]
Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
pasaron por dicho territorio.
5. exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]

Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un terri-
torio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron

dicho territorio. Los territorios no deben repetirse.
6. superioresDelCazador :: Nombre -> Manada -> [Nombre]
Propósito: dado un nombre de cazador y una manada, indica el nombre de todos los
cazadores que tienen como subordinado al cazador dado (directa o indirectamente).
Precondición: hay un cazador con dicho nombre y es único.

Los cazadores poseen nombre, una lista de especies de presas cazadas y 3 lobos a cargo.
Los exploradores poseen nombre, una lista de nombres de territorio explorado (nombres de
bosques, ríos, etc.), y poseen 2 lobos a cargo.
Las crías poseen sólo un nombre y no poseen lobos a cargo.
-}
type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre deriving Show
data Manada = M Lobo deriving Show

manada = M (Cazador "Hunter" ["Conejo", "asd", "asasdd", "asfkadf", "asdfadf"] 
	(Explorador "Explorador 1" ["Canada"] (Cria "Juan") (Cria "Pepe"))
	(Cazador "HunterAlfa" ["Conejo", "asd", "asasdd", "asfkadf", "asdfadf", "asdsa", "ajskfjkaldsf"]
		(Explorador "Explorador 3" ["Canada"] (Cria "Juanzito") (Cria "Pepecito"))
		((Cazador "Subordinado" ["Conejo", "asd", "asasdd", "asfkadf", "asdfadf", "asdsa", "ajskfjkaldsf"]
			(Explorador "Explorador 3" ["Canada"] (Cria "Juanzito") (Cria "Pepecito"))
			(Explorador "Explorador 4" ["Estados Unidos"] (Cria "Carlitos") (Cria "Rubencito"))
		(Cria "Marcos")))
	(Cria "Marcos"))
	(Cria "Marquitos"))

--Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la can-tidad de crías.
buenaCaza :: Manada -> Bool
buenaCaza (M l) = buenaCazaL l

buenaCazaL :: Lobo -> Bool
buenaCazaL l = cantidadDeAlimentoCazado l > cantidadDeCrias l

cantidadDeAlimentoCazado :: Lobo -> Int
cantidadDeAlimentoCazado (Cazador _ xs l1 l2 l3) = length xs + cantidadDeAlimentoCazado l1 + cantidadDeAlimentoCazado l2 + cantidadDeAlimentoCazado l3
cantidadDeAlimentoCazado _ = 0

cantidadDeCrias :: Lobo -> Int
cantidadDeCrias (Cazador _ _ l1 l2 l3) = cantidadDeCrias l1 + cantidadDeCrias l2 + cantidadDeCrias l3
cantidadDeCrias (Explorador _ _ l1 l2) = cantidadDeCrias l1 + cantidadDeCrias l2
cantidadDeCrias (Cria _) = 1

--Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
--con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
--cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
--cero presas.
elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = elAlfaL l

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL (Cazador nom ps l1 l2 l3) = 
	maxPresas [
		(elAlfaL l1),
		(elAlfaL l2),
		(elAlfaL l3),
		(nom, (length ps))]
elAlfaL (Explorador nom _ l1 l2) = 
	maxPresas [(nom, 0),
	(elAlfaL l1), 
	(elAlfaL l2)]
elAlfaL (Cria nom) = (nom, 0)

maxPresa :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
maxPresa l1 l2 = if snd l1 > snd l2 then l1 else l2

maxPresas :: [(Nombre, Int)] -> (Nombre, Int)
maxPresas [] = error "bardeaste con la lista vacia"
maxPresas [x] = x
maxPresas (x:xs) = maxPresa x (maxPresas xs)

cantidadDePresas :: Lobo -> Int
cantidadDePresas (Cazador nom ps _ _ _) = length ps
cantidadDePresas _ = 0

--Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que pasaron por dicho territorio.
losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M l) = losQueExploraronL t l

losQueExploraronL :: Territorio -> Lobo -> [Nombre]
losQueExploraronL t (Cria _) = []
losQueExploraronL t (Cazador _ _ l1 l2 l3) = losQueExploraronL t l1 ++ losQueExploraronL t l2 ++ losQueExploraronL t l3
losQueExploraronL t (Explorador nom ts l1 l2) = 
	if existeTerritorioEnLista t ts 
		then nom : (losQueExploraronL t l1 ++ losQueExploraronL t l2) 
		else (losQueExploraronL t l1 ++ losQueExploraronL t l2)

existeTerritorioEnLista :: Territorio -> [Territorio] -> Bool
existeTerritorioEnLista t [] = False
existeTerritorioEnLista t (x:xs) = t == x || existeTerritorioEnLista t xs


--Propósito: dado un nombre de cazador y una manada, indica el nombre de todos los
--cazadores que tienen como subordinado al cazador dado (directa o indirectamente).
--Precondición: hay un cazador con dicho nombre y es único.
superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador nom (M l) = superioresDelCazadorL nom l

superioresDelCazadorL :: Nombre -> Lobo -> [Nombre]
superioresDelCazadorL nom (Cria _) = []
superioresDelCazadorL nom (Explorador _ _ l1 l2) = superioresDelCazadorL nom l1 ++ superioresDelCazadorL nom l2
superioresDelCazadorL nom (Cazador n xs l1 l2 l3) = 
	if esSubordinado nom (Cazador n xs l1 l2 l3) && n /= nom
		then n : superioresDelCazadorL nom l1 ++ superioresDelCazadorL nom l2 ++ superioresDelCazadorL nom l3
		else superioresDelCazadorL nom l1 ++ superioresDelCazadorL nom l2 ++ superioresDelCazadorL nom l3

esSubordinado :: Nombre -> Lobo -> Bool
esSubordinado nom (Cria _) = False
esSubordinado nom (Explorador _ _ l1 l2) = False
esSubordinado nom (Cazador n xs l1 l2 l3) = 
	nom == n
	|| esSubordinado nom l1 
	|| esSubordinado nom l2 
	|| esSubordinado nom l2

nombreLobo :: Lobo -> Nombre
nombreLobo (Cria n) = n 
nombreLobo (Explorador n _ _ _) = n 
nombreLobo (Cazador n _ _ _ _) = n
{-
Subordinado -> superiores -> [Hunter, HunterAlfa]
-}
{-
--Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
--dicho territorio. Los territorios no deben repetirse.

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M l) = exploradoresPorTerritorioL l

exploradoresPorTerritorioL :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioL (Cria _) = []
exploradoresPorTerritorioL (Cazador _ _ l1 l2 l3) = exploradoresPorTerritorioL l1 ++ exploradoresPorTerritorioL l2 ++ exploradoresPorTerritorioL l3
exploradoresPorTerritorioL (Explorador nom ts l1 l2) = nombrePorTerritorio nom ts : juntarPorTerritorio (exploradoresPorTerritorioL l1) (exploradoresPorTerritorioL l2)

nombrePorTerritorio :: Nombre -> [Territorio] -> [(Territorio, Nombre)]
nombrePorTerritorio nom [] = []
nombrePorTerritorio nom (x:xs) = (x, nom) : nombrePorTerritorio nom xs

juntarPorTerritorio :: [(Territorio, Nombre)] -> [(Territorio, Nombre)] -> [(Territorio, Nombre)]
juntarPorTerritorio [] [] = []
juntarPorTerritorio [] yss = yss
juntarPorTerritorio xss [] = xss
juntarPorTerritorio (x:xs) (y:ys) = 


juntarPorNiveles :: [[a]] -> [[a]] -> [[a]]
juntarPorNiveles [] [] = []
juntarPorNiveles [] yss = yss
juntarPorNiveles xss [] = xss
juntarPorNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarPorNiveles xss yss
-}

{-
[
	("Canada", ["Explorador 1", Explorador 3]),
	("Estados Unidos", ["Explorador 2", Explorador 4])
]
-}