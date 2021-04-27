{-
Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:
data Color = Azul | Rojo
data Celda = Bolita Color Celda | CeldaVacia
En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad
de bolitas de ese color en la celda. Por ejemplo, una celda con 2 bolitas azules y 2 rojas, podría
ser la siguiente:
Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
Implementar las siguientes funciones sobre celdas:

sacar :: Color -> Celda -> Celda
Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
Gobstones, esta función es total.
-}

data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

celda1 = Bolita Azul (Bolita Azul (Bolita Rojo CeldaVacia))
azul = Bolita Azul CeldaVacia

--Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia = 0
nroBolitas c (Bolita cl ce) = if sonElMismoColor c cl then 1 + nroBolitas c ce else nroBolitas c ce

sonElMismoColor :: Color -> Color -> Bool
sonElMismoColor Rojo Rojo = True
sonElMismoColor Azul Azul = True
sonElMismoColor x y = False

--Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner cl ce = (Bolita cl ce)

--Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar c CeldaVacia = CeldaVacia
sacar c (Bolita cl ce) = if sonElMismoColor c cl then ce else  (Bolita cl (sacar c ce))

--Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 x y = y
ponerN n c ce = ponerN (n-1) c (Bolita c ce)


{-
data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
Definir las siguientes funciones:
-}

data Objeto = Cacharro | Tesoro deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show

camino1 = Cofre [Cacharro, Cacharro] (Nada (Cofre [Cacharro,Tesoro] Fin))
camino2 = Cofre [Cacharro] (Nada (Cofre [Cacharro, Cacharro] Fin))
camino3 = Nada (Nada (Nada (Cofre [Tesoro] Fin)))
camino4 = Cofre [Tesoro] (Nada Fin)
--Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada c) = hayTesoro c
hayTesoro (Cofre xs c) =  hayTesoroEnListaDeObjetos xs || hayTesoro c

hayTesoroEnListaDeObjetos :: [Objeto] -> Bool
hayTesoroEnListaDeObjetos [] = False
hayTesoroEnListaDeObjetos (x:xs) = esTesoro x || hayTesoroEnListaDeObjetos xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

--Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro. 
--Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
--Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre xs c) = if hayTesoroEnListaDeObjetos xs then 0 else 1 + pasosHastaTesoro c

--Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n c = hayTesoro c && pasosHastaTesoro c == n

--Indica si hay al menos “n” tesoros en el camino.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 _ = True
alMenosNTesoros n c = cantidadDeTesorosEnCamino c >= n

unoSiHayTesoro :: Camino -> Int
unoSiHayTesoro Fin = 0
unoSiHayTesoro (Nada c) = 0
unoSiHayTesoro (Cofre xs c) = if hayTesoroEnListaDeObjetos xs then 1 else 0

cantidadDeTesorosEnCamino :: Camino -> Int
cantidadDeTesorosEnCamino Fin = 0
cantidadDeTesorosEnCamino (Nada c) = cantidadDeTesorosEnCamino c 
cantidadDeTesorosEnCamino (Cofre xs c) = unoSiHayTesoro (Cofre xs c) + cantidadDeTesorosEnCamino c


--recortarCamino :: Int -> Int -> Camino -> Camino
--recortarCamino _ _ Fin = Fin
--recortarCamino n m (Nada c) = recortarCamino (n-1) c
--recortarCamino n m (Cofre _ c) = recortarCamino (n-1) c

--Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. 
--Están incluidos tanto 3 como 5 en el resultado.
--cantTesorosEntre :: Int -> Int -> Camino -> Int
--cantTesorosEntre n m c = if n <= m then cantidadDeTesorosEnCamino (recortarCamino n c) else 0


{-
1. sumarT :: Tree Int -> Int
Dado un árbol binario de enteros devuelve la suma entre sus elementos.
2. sizeT :: Tree a -> Int
Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
en inglés).
3. mapDobleT :: Tree Int -> Tree Int
Dado un árbol de enteros devuelve un árbol con el doble de cada número.
4. perteneceT :: Eq a => a -> Tree a -> Bool
Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
árbol.
5. aparicionesT :: Eq a => a -> Tree a -> Int
Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
iguales a e.
6. leaves :: Tree a -> [a]
Dado un árbol devuelve los elementos que se encuentran en sus hojas.-}
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

arbolEnteros :: Tree Int
arbolEnteros = NodeT 1 
				(NodeT 2 
					(NodeT 3 
						(NodeT 7 EmptyT EmptyT) 
						EmptyT) 
					(NodeT 5 EmptyT EmptyT)) 
				(NodeT 4 
					(NodeT 6 EmptyT EmptyT)
					EmptyT)

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT x tl tr) = x + sumarT tl + sumarT tr

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x tl tr) = 1 + sizeT tl + sizeT tr

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x tl tr) = (NodeT (x * 2) (mapDobleT tl) (mapDobleT tr))

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT = False
perteneceT e (NodeT x tl tr) = e == x || perteneceT e tl || perteneceT e tr 

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT e EmptyT = 0
aparicionesT e (NodeT x tl tr) = if e == x then 1 + aparicionesT e tl + aparicionesT e tr else aparicionesT e tl + aparicionesT e tr

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT e tl tr) = if isLeaf (NodeT e tl tr) then [e] else leaves tl ++ leaves tr

isLeaf :: Tree a -> Bool
isLeaf EmptyT = False
isLeaf (NodeT e tl tr) = isEmptyNode tl && isEmptyNode tr

isEmptyNode :: Tree a -> Bool
isEmptyNode EmptyT = True
isEmptyNode _ = False

{-
7. heightT :: Tree a -> Int
Dado un árbol devuelve su altura.
Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
de niveles del árbol1

. La altura de un árbol vacío es cero y la de una hoja también.

8. mirrorT :: Tree a -> Tree a
Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
en cada nodo del árbol.
9. toList :: Tree a -> [a]
Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
y luego los elementos del hijo derecho.
10. levelN :: Int -> Tree a -> [a]
Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
distancia de la raiz a uno de sus hijos es 1.
Nota: El primer nivel de un árbol (su raíz) es 0.
11. listPerLevel :: Tree a -> [[a]]
Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de
dicho árbol.
12. ramaMasLarga :: Tree a -> [a]
Devuelve los elementos de la rama más larga del árbol
13. todosLosCaminos :: Tree a -> [[a]]
Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.-}

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x tl tr) = if isLeaf (NodeT x tl tr) then max (heightT tl) (heightT tr) else 1 + max (heightT tl) (heightT tr)

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x tl tr) = (NodeT x tr tl)

--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
--Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
--y luego los elementos del hijo derecho.
toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT e tl tr) = toList tl ++ [e] ++ toList tr -- RETOMAR

--toList :: Tree a -> [a]
--toList EmptyT = []
--toList (NodeT x ti td) = (toList ti ++ [x] ++ toList td)


--Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
--nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
--distancia de la raiz a uno de sus hijos es 1.
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN n (NodeT e tl tr) = if n == 0 then [e] else levelN (n - 1) tl ++ levelN (n - 1) tr

--Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de
--dicho árbol.

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel tree = [[elemento tree]] ++ listPerLevelWithoutRoot tree

listPerLevelWithoutRoot :: Tree a -> [[a]]
listPerLevelWithoutRoot EmptyT = []
listPerLevelWithoutRoot (NodeT e tl tr) = [listOfElementsInNextLevel (NodeT e tl tr)] ++ listPerLevelWithoutRoot tl ++ listPerLevelWithoutRoot tr


	{-
	if  
		then [[e]] ++ [listOfElementsInNextLevel (NodeT e tl tr)] ++ listPerLevel tl ++ listPerLevel tr
		else [listOfElementsInNextLevel (NodeT e tl tr)] ++ listPerLevel tl ++ listPerLevel tr
	-}
		
listOfElementsInNextLevel :: Tree a -> [a]
listOfElementsInNextLevel EmptyT = []
listOfElementsInNextLevel (NodeT e EmptyT EmptyT)  = []
listOfElementsInNextLevel (NodeT e tl EmptyT) = [elemento tl]
listOfElementsInNextLevel (NodeT e EmptyT tr) = [elemento tr]
listOfElementsInNextLevel (NodeT e tl tr) = elemento tl : elemento tr : []


elemento :: Tree a -> a
elemento (NodeT e _ _) = e


--Devuelve los elementos de la rama más larga del árbol
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT e tl tr) = if heightT tl > heightT tr then elementosRamaIzquierda tl else elementosRamaDerecha tr

elementosRamaIzquierda :: Tree a -> [a]
elementosRamaIzquierda EmptyT = []
elementosRamaIzquierda (NodeT e tl tr) = e : elementosRamaIzquierda tl

elementosRamaDerecha :: Tree a -> [a]
elementosRamaDerecha EmptyT = []
elementosRamaDerecha (NodeT e tl tr) = e : elementosRamaDerecha tr

--Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []

arbol :: Tree String
arbol = NodeT "Raiz" (NodeT "Izquierdo lvl 1" (NodeT "Izquierdo lvl 2" EmptyT EmptyT) EmptyT) (NodeT "Derecho lvl 1" EmptyT EmptyT) 

