--1. Pizzas
--Tenemos los siguientes tipos de datos:

data Pizza = Prepizza
            | Capa Ingrediente Pizza deriving Show

data Ingrediente = Salsa
            | Queso
            | Jamon
            | Aceitunas Int deriving Show

p1 = Capa Jamon (Capa Queso (Capa Salsa Prepizza))
p2 = Capa Queso (Capa Salsa Prepizza)
p3 = Capa (Aceitunas 10) (Capa Queso (Capa Queso Prepizza))
p4 = Capa (Aceitunas 10) (Capa Queso (Capa (Aceitunas 20) Prepizza))


--Definir las siguientes funciones:
--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing pz) = 1 + cantidadDeCapas pz

--Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) = Capa x (armarPizza xs)

--Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ing pz) = if esJamon ing
                            then sacarJamon pz
                            else Capa ing (sacarJamon pz)

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False

--Dice si una pizza tiene salsa y queso
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ing pz) = (esSalsa ing || esQueso ing) && tieneSoloSalsaYQueso pz

esSalsa :: Ingrediente -> Bool
esSalsa Salsa = True
esSalsa _ = False

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _ = False

--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ing pz) = Capa (duplicar ing) (duplicarAceitunas pz)

duplicar :: Ingrediente -> Ingrediente
duplicar (Aceitunas x) = Aceitunas (x * 2)
duplicar ing = ing

--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
--ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (x:xs) = (cantidadDeCapas x, x) : cantCapasPorPizza xs

--2. Mapa de tesoros (con bifurcaciones)
--Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y
--cada cofre tiene un objeto, que puede ser chatarra o un tesoro.
data Dir = Izq | Der deriving Show

data Objeto = Tesoro | Chatarra deriving Show

data Cofre = Cofre [Objeto] deriving Show

data Mapa = Fin Cofre
            | Bifurcacion Cofre Mapa Mapa deriving Show

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

--Definir las siguientes operaciones:
--Indica si hay un tesoro en alguna parte del mapa.
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnCofre c || hayTesoro m1 || hayTesoro m2

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre o) = hayTesoroEnObjetos o

hayTesoroEnObjetos :: [Objeto] -> Bool
hayTesoroEnObjetos [] = False
hayTesoroEnObjetos (x:xs) = esTesoro x || hayTesoroEnObjetos xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

--Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
--lista vacía de direcciones.

--Creo que funca pero me estoy cagando en la recursión estructural
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] mapa = hayTesoro mapa
hayTesoroEn xs mapa = hayTesoroEnCofre (cofre (caminar xs mapa))

caminar :: [Dir] -> Mapa -> Mapa
caminar [] mapa = mapa
caminar (x:xs) (Bifurcacion c m1 m2) = if esIzquierda x then (caminar xs m1) else (caminar xs m2) 

esIzquierda :: Dir -> Bool
esIzquierda Izq = True
esIzquierda _ = False

cofre :: Mapa -> Cofre
cofre (Fin c) = c
cofre (Bifurcacion c m1 m2) = c

--Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoro m1 
											then Izq : (caminoAlTesoro m1 ++ caminoAlTesoro m2)
											else if hayTesoro m2
												then Der : (caminoAlTesoro m1 ++ caminoAlTesoro m2)
												else (caminoAlTesoro m1 ++ caminoAlTesoro m2)

--Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin c) = []
caminoDeLaRamaMasLarga (Bifurcacion c mi md) = 
	if heightT (mi) > heightT (md) 
		then Izq: (caminoDeLaRamaMasLarga mi)
		else Der: (caminoDeLaRamaMasLarga md)

heightT :: Mapa -> Int
heightT (Fin c) = 0
heightT (Bifurcacion c mi md)= 1 + max (heightT mi) (heightT md)

--Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = []
tesorosPorNivel (Bifurcacion c mi md) = tesoros c : juntarPorNiveles (tesorosPorNivel mi) (tesorosPorNivel md)

tesoros :: Cofre -> [Objeto]
tesoros (Cofre o) = soloTesoros o

soloTesoros :: [Objeto] -> [Objeto]
soloTesoros [] = []
soloTesoros (x:xs) = if esTesoro x
						then x : soloTesoros xs
						else soloTesoros xs

juntarPorNiveles :: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
juntarPorNiveles [] [] = []
juntarPorNiveles [] xss = xss
juntarPorNiveles xss [] = xss
juntarPorNiveles (x:xss) (y:yss) = (x ++ y) : (juntarPorNiveles xss yss)


--Devuelve todos lo caminos en el mapa.
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin c) = [[]]
todosLosCaminos (Bifurcacion c mi md) =
	(agregarATodas Izq (todosLosCaminos mi)) ++
	(agregarATodas Der (todosLosCaminos md))

agregarATodas :: Dir -> [[Dir]] -> [[Dir]]
agregarATodas d [] = []
agregarATodas d (xs:xss) = (d : xs) : (agregarATodas d xss)  

------------------------------------------------------------------------

--3. Nave Espacial
--modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial,
--dividida en sectores, a los cuales podemos asignar tripulantes y componentes. La representación
--es la siguiente:

data Componente = LanzaTorpedos 
				| Motor Int 
				| Almacen [Barril] deriving Show

data Barril = Comida 
			| Oxigeno 
			| Torpedo 
			| Combustible deriving Show

data Sector = S SectorId [Componente] [Tripulante] deriving Show

type SectorId = String 

type Tripulante = String 

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

data Nave = N (Tree Sector) deriving Show

nave = N (NodeT (S "1" [(Motor 5), (Almacen [Comida, Oxigeno])] ["Fabi", "Andy"]) 
			(NodeT (S "2" [(Motor 10), (Almacen [Comida, Oxigeno])] ["Andy", "Juan"]) EmptyT EmptyT) 
			(NodeT (S "3" [(Motor 20)] ["Fabi"]) EmptyT EmptyT))

--Implementar las siguientes funciones utilizando recursión estructural:
--Propósito: Devuelve todos los sectores de la nave.
sectores :: Nave -> [SectorId]
sectores (N t) = sectoresT t

sectoresT :: Tree Sector -> [SectorId]
sectoresT EmptyT = []
sectoresT (NodeT x ti td) = sectorID x : sectoresT ti ++ sectoresT td

sectorID :: Sector -> SectorId
sectorID (S id cs ts) = id

--Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
--el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int
poderDePropulsion (N t) = poderDePropulsionT t

poderDePropulsionT :: Tree Sector -> Int
poderDePropulsionT EmptyT = 0
poderDePropulsionT (NodeT x ti td) = poderDePropulsionS x + poderDePropulsionT ti + poderDePropulsionT td

poderDePropulsionS :: Sector -> Int
poderDePropulsionS (S id cs ts) = poderDePropulsionCS cs

poderDePropulsionCS :: [Componente] -> Int
poderDePropulsionCS [] = 0
poderDePropulsionCS (x:xs) = poderDePropulsionC x + poderDePropulsionCS xs

poderDePropulsionC :: Componente -> Int
poderDePropulsionC (Motor poder) = poder
poderDePropulsionC _ = 0

--Propósito: Devuelve todos los barriles de la nave.
barriles :: Nave -> [Barril]
barriles (N t) = barrilesT t

barrilesT :: Tree Sector -> [Barril]
barrilesT EmptyT = []
barrilesT (NodeT x ti td) = barrilesS x ++ barrilesT ti ++ barrilesT td

barrilesS :: Sector -> [Barril]
barrilesS (S id cs ts) = barrilesCS cs

barrilesCS :: [Componente] -> [Barril]
barrilesCS [] = []
barrilesCS (x:xs) = barrilesC x ++ barrilesCS xs

barrilesC :: Componente -> [Barril]
barrilesC (Almacen bs) = bs
barrilesC _ = []

--Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector xs sid (N t) = agregarASectorT xs sid t

agregarASectorT :: [Componente] -> SectorId -> Tree Sector -> Nave
agregarASectorT xs sid tree= N (agregarASectorT' xs sid tree)

agregarASectorT' :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorT' xs sid EmptyT = EmptyT
agregarASectorT' xs sid (NodeT x ti td) = (NodeT (agregarASectorS xs x sid) (agregarASectorT' xs sid ti) (agregarASectorT' xs sid td))

agregarASectorS :: [Componente] -> Sector -> SectorId -> Sector
agregarASectorS xs (S id cs ts) sid = if id == sid
										then (S id (xs ++ cs) ts)
										else (S id cs ts)

--Propósito: Incorpora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave.	
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA t sids (N ts) = N (asignarTripulanteATS t sids ts)

asignarTripulanteATS :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteATS t sids EmptyT = EmptyT
asignarTripulanteATS t sids (NodeT x ti td) = (NodeT (asignarTripulanteASIDS t sids x) (asignarTripulanteATS t sids ti) (asignarTripulanteATS t sids td))

asignarTripulanteASIDS :: Tripulante -> [SectorId] -> Sector -> Sector
asignarTripulanteASIDS t [] s = s
asignarTripulanteASIDS t (x:xs) s = if x == sectorID s
										then asignarT t s
										else asignarTripulanteASIDS t xs s

asignarT :: Tripulante -> Sector -> Sector
asignarT t (S id cs ts) = (S id cs (t:ts))

--Propósito: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados tp (N t) = sectoresAsignadosT tp t

sectoresAsignadosT :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosT t EmptyT = []
sectoresAsignadosT t (NodeT x ti td) = if tieneAsignacion t x
										then (sectorID x) : (sectoresAsignadosT t ti ++ sectoresAsignadosT t td)
										else sectoresAsignadosT t ti ++ sectoresAsignadosT t td

tieneAsignacion :: Tripulante -> Sector -> Bool
tieneAsignacion t (S id cs ts) = tieneAsignacion' t ts --pertenece

tieneAsignacion' :: Tripulante -> [Tripulante] -> Bool
tieneAsignacion' t [] = False
tieneAsignacion' t (x:xs) = x == t || tieneAsignacion' t xs

--Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes :: Nave -> [Tripulante]
tripulantes (N t) = tripulantesT t

tripulantesT :: Tree Sector -> [Tripulante]
tripulantesT EmptyT = []
tripulantesT (NodeT x ti td) = sinRepetidos (tripulantesS x ++ tripulantesT ti ++ tripulantesT td)

tripulantesS :: Sector -> [Tripulante]
tripulantesS (S id cs ts) = ts

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if estaEn x xs
						then sinRepetidos xs
						else x : sinRepetidos xs

estaEn :: Eq a => a -> [a] -> Bool --pertenece
estaEn t [] = False
estaEn t (x:xs) = t == x || estaEn t xs

--4. Manada de lobos
--Modelaremos una manada de lobos, como un tipo Manada, que es un simple registro compuesto
--de una estructura llamada Lobo, que representa una jerarquía entre estos animales.
--Los diferentes casos de lobos que forman la jerarquía son los siguientes:
--Los cazadores poseen nombre, una lista de especies de presas cazadas y 3 lobos a cargo.
--Los exploradores poseen nombre, una lista de nombres de territorio explorado (nombres de
--bosques, ríos, etc.), y poseen 2 lobos a cargo.
--Las crías poseen sólo un nombre y no poseen lobos a cargo.
--La estructura es la siguiente:

type Presa = String  -- nombre de presa

type Territorio = String  -- nombre de territorio

type Nombre = String  -- nombre de lobo

data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo 
			| Explorador Nombre [Territorio] Lobo Lobo
			| Cria Nombre deriving Show

data Manada = M Lobo deriving Show

manada = M (Cazador "Hunter" ["Conejo", "asd", "asasdd", "asfkadf", "asdfadf"] 
				(Explorador "Explorador 1" ["Canada", "Estados Unidos"] (Cria "Juan") (Cria "Pepe"))
				(Cazador "HunterAlfa" ["Conejo", "asd", "asasdd", "asfkadf", "asdfadf", "asdsa", "ajskfjkaldsf"]
					(Explorador "Explorador 3" ["Canada"] (Cria "Juanzito") (Cria "Pepecito"))
					((Cazador "Subordinado" ["Conejo", "asd", "asasdd", "asfkadf", "asdfadf", "asdsa", "ajskfjkaldsf"]
						(Explorador "Explorador 3" ["Canada"] (Cria "Juanzito") (Cria "Pepecito"))
						(Explorador "Explorador 4" ["Estados Unidos"] (Cria "Carlitos") (Cria "Rubencito"))
					(Cria "Marcos")))
				(Cria "Marcos"))
				(Cria "Marquitos"))

--1. Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean
--crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura
--que corresponda en cada caso:

--Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
buenaCaza :: Manada -> Bool
buenaCaza (M l) = cantPresas l > cantCrias l

cantPresas :: Lobo -> Int
cantPresas (Cria n) = 0
cantPresas (Explorador n tt l1 l2) = cantPresas l1 + cantPresas l2
cantPresas (Cazador n ps l1 l2 l3) = length ps + cantPresas l1 + cantPresas l2 + cantPresas l3

cantCrias :: Lobo -> Int
cantCrias (Cria n) = 1
cantCrias (Explorador n t l1 l2) = cantCrias l1 + cantCrias l2
cantCrias (Cazador n ps l1 l2 l3) = cantCrias l1 + cantCrias l2 + cantCrias l3

--Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
--con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
--cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
--cero presas.

--No sé si está bien porque suma también las presas de los lobos a cargo
elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = elAlfaL l

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL (Cria n) = (n, 0)
elAlfaL (Explorador n t l1 l2) = maxP (elAlfaL l2) (maxP (n, 0) (elAlfaL l1))
elAlfaL (Cazador n ps l1 l2 l3) = maxP (n, length ps) (maxP (elAlfaL l1) (maxP (elAlfaL l2) (elAlfaL l3)))

maxP :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
maxP (x, y) (a, b) = if y > b then (x, y) else (a, b)

--Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
--pasaron por dicho territorio.
losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M l) = losQueExploraronL t l

losQueExploraronL :: Territorio -> Lobo -> [Nombre]
losQueExploraronL t (Cria n) = []
losQueExploraronL t (Explorador n tt l1 l2) = 
	if exploroT t tt 
		then n : (losQueExploraronL t l1 ++ losQueExploraronL t l2)
		else (losQueExploraronL t l1 ++ losQueExploraronL t l2)
losQueExploraronL t (Cazador n ps l1 l2 l3) = 
	(losQueExploraronL t l1 ++ losQueExploraronL t l2 ++ losQueExploraronL t l3)

exploroT :: Territorio -> [Territorio] -> Bool
exploroT t [] = False
exploroT t (x:xs) = t == x || exploroT t xs

--Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio 
--y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
--dicho territorio. Los territorios no deben repetirse.
exploradoresPorTerritorio :: 
   Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M l) = 
	expPorTL l

expPorTL :: Lobo -> [(Territorio, [Nombre])]
expPorTL (Cazador n ps l1 l2 l3) = 
	unirL (expPorTL l1)
	      (unirL (expPorTL l2)
	             (expPorTL l3))
expPorTL (Explorador n ts l1 l2) = 
	unirL 
	  (unirL (agregarTerritorios n ts) 
	  	     (expPorTL l1))
	  (expPorTL l2)
expPorTL (Cria n) = []

-- Propósito: arma una lista de tuplas donde
-- la primera componente es cada territorio
-- y la segunda componente es el nombre dado por parámetro
agregarTerritorios :: Nombre -> [Territorio] -> [(Territorio, [Nombre])]
agregarTerritorios n [] = []
agregarTerritorios n (x:xs) = (x, [n]) : agregarTerritorios n xs

-- Propósito: une dos listas por territorio, 
-- donde para un mismo territorio se juntan las listas de nombres
unirL :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
unirL [] [] = []
unirL xs [] = xs
unirL [] xs = xs
unirL (x:xs) (y:ys) = 
	if fst x == fst y 
		then ((fst x), (snd x) ++ (snd y)) : unirL xs ys
		else ((fst x), (snd x)) : [((fst y), (snd y))] ++ unirL xs ys

--Propósito: dado un nombre de cazador y una manada, indica el nombre de todos los
--cazadores que tienen como subordinado al cazador dado (directa o indirectamente).
--Precondición: hay un cazador con dicho nombre y es único.
superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador n (M l) = superiores n l

superiores :: Nombre -> Lobo -> [Nombre]
superiores cz (Cria n) = []
superiores cz (Explorador n t l1 l2) = 
	superiores cz l1 ++
	superiores cz l2
superiores cz (Cazador n ps l1 l2 l3) = 
	if esSubordinado cz (Cazador n ps l1 l2 l3) && (cz /= n)
		then n :
			(superiores cz l1 ++
			superiores cz l2 ++
			superiores cz l3)
		else (superiores cz l1 ++
			superiores cz l2 ++
			superiores cz l3)

esSubordinado :: Nombre -> Lobo -> Bool
esSubordinado cz (Cria n) = False
esSubordinado cz (Explorador n t l1 l2) = False 
esSubordinado cz (Cazador n ps l1 l2 l3) =
	cz == n ||
	esSubordinado cz l1 ||
	esSubordinado cz l2 ||
	esSubordinado cz l3