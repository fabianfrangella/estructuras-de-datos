--1. Tipos recursivos simples
--1.1. Celdas con bolitas
--Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:
data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show
--En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad
--de bolitas de ese color en la celda. Por ejemplo, una celda con 2 bolitas azules y 2 rojas, podría
--ser la siguiente:
celda1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
--Implementar las siguientes funciones sobre celdas:
--nroBolitas :: Color -> Celda -> Int
--Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
--existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia = 0
nroBolitas c (Bolita clr cel) = if esMismoColor clr c then 1 + nroBolitas c cel else nroBolitas c cel

esMismoColor :: Color -> Color -> Bool
esMismoColor Rojo Rojo = True
esMismoColor Azul Azul = True
esMismoColor _ _ = False

--poner :: Color -> Celda -> Celda
--Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner c CeldaVacia = Bolita c CeldaVacia
poner c (Bolita clr cel) = Bolita clr (poner c cel)

--sacar :: Color -> Celda -> Celda
--Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
--Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar c CeldaVacia = CeldaVacia
sacar c (Bolita clr cel) = if esMismoColor clr c then cel else Bolita clr (sacar c cel)



--ponerN :: Int -> Color -> Celda -> Celda
--Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 c cel = cel
ponerN 1 c CeldaVacia = poner c CeldaVacia
ponerN n c cel = poner c (ponerN (n-1) c cel)

--1.2. Camino hacia el tesoro
--Tenemos los siguientes tipos de datos
data Objeto = Cacharro 
            | Tesoro deriving Show

data Camino = Fin 
            | Cofre [Objeto] Camino 
            | Nada Camino deriving Show

conTesoro = [Cacharro, Cacharro, Tesoro]
sinTesoro = [Cacharro, Cacharro]

caminito = (Cofre (sinTesoro) (Cofre conTesoro (Cofre sinTesoro (Cofre conTesoro Fin)))) 

--Definir las siguientes funciones:
--hayTesoro :: Camino -> Bool
--Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada c) = False
hayTesoro (Cofre objetos camino) = hayTesoroEnObjetos objetos || hayTesoro camino

hayTesoroEnObjetos :: [Objeto] -> Bool
hayTesoroEnObjetos [] = False
hayTesoroEnObjetos (x:xs) = esTesoro x || hayTesoroEnObjetos xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

--pasosHastaTesoro :: Camino -> Int
--Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
--Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
--Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Cofre objetos camino) = if not (hayTesoroEnObjetos objetos)
                            then 1 + pasosHastaTesoro camino
                            else 0

--Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
--pasos es 5, indica si hay un tesoro en 5 pasos.
--hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n Fin = False
hayTesoroEn n camino = hayTesoroEnObjetos (objetos (caminar n camino))

objetos :: Camino -> [Objeto]
objetos Fin = []
objetos (Nada c) = []
objetos (Cofre objetos camino) = objetos

caminar :: Int -> Camino -> Camino
caminar 0 camino = camino
caminar n camino = darUnPaso (caminar (n-1) camino)

darUnPaso :: Camino -> Camino
darUnPaso Fin = Fin
darUnPaso (Cofre objetos camino) = camino
darUnPaso (Nada camino) = camino

--alMenosNTesoros :: Int -> Camino -> Bool
--Indica si hay al menos “n” tesoros en el camino.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n camino = (cantTesoros camino) >= n

cantTesoros :: Camino -> Int
cantTesoros Fin = 0
cantTesoros (Nada c) = 0
cantTesoros (Cofre objetos camino) = cantTesorosEnObjetos objetos + cantTesoros camino

cantTesorosEnObjetos :: [Objeto] -> Int
cantTesorosEnObjetos [] = 0
cantTesorosEnObjetos (x:xs) = if esTesoro x 
                                then 1 + (cantTesorosEnObjetos xs)
                                else (cantTesorosEnObjetos xs)

--cantTesorosEntre :: Int -> Int -> Camino -> Int
--Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
--el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
--incluidos tanto 3 como 5 en el resultado.
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre x y camino = if x <= y then 
                                cantTesorosEnObjetos (objetos (caminar x camino)) + cantTesorosEntre (x+1) y camino
                                else 0

--2. Tipos arbóreos
--2.1. Árboles binarios
--Dada esta definición para árboles binarios
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

tree :: Tree Int
tree =
    NodeT 10
        (NodeT 20
            (NodeT 30 EmptyT EmptyT)
            (NodeT 30 EmptyT EmptyT))
        (NodeT 20
            (NodeT 50 EmptyT 
                (NodeT 75 EmptyT EmptyT))
            (NodeT 100 EmptyT EmptyT))

--defina las siguientes funciones utilizando recursión estructural según corresponda:
--Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT x ti td) = x + sumarT ti + sumarT td

--Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
--en inglés).
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x ti td) = 1 + sizeT ti + sizeT td

--Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x ti td) = NodeT (x * 2) (mapDobleT ti) (mapDobleT td)

--Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
--árbol.
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT e EmptyT = False
perteneceT e (NodeT x ti td) = x == e || perteneceT e ti || perteneceT e td

--Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
--iguales a e.
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT e EmptyT = 0
aparicionesT e (NodeT x ti td) = if e == x then
                                 1 + aparicionesT e ti + aparicionesT e td
                                 else aparicionesT e ti + aparicionesT e td

--Dado un árbol devuelve los elementos que se encuentran en sus hojas
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x ti td) = if isLeave (NodeT x ti td)
                            then x : (leaves ti ++ leaves td)
                            else (leaves ti ++ leaves td)

isLeave :: Tree a -> Bool
isLeave (NodeT x EmptyT EmptyT) = True
isLeave _ = False

--Dado un árbol devuelve su altura.
--Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
--de niveles del árbol1
-- La altura de un árbol vacío es cero y la de una hoja también.
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x ti td) = 1 + max (heightT ti) (heightT td)

--Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
--en cada nodo del árbol.
mirrorT :: Tree a -> Tree a
mirrorT (NodeT x ti td) = (NodeT x td ti)

--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
--Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
--y luego los elementos del hijo derecho.
toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x ti td) = (toList ti ++ [x] ++ toList td)

--Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
--nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
--distancia de la raiz a uno de sus hijos es 1.
--Nota: El primer nivel de un árbol (su raíz) es 0.
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN 0 (NodeT x ti td) = [x]
levelN n (NodeT x ti td) = levelN (n-1) ti ++ levelN (n-1) td

--Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de
--dicho árbol.
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x ti td) = [x] : juntarPorNiveles (listPerLevel ti)  (listPerLevel td)

juntarPorNiveles :: [[a]] -> [[a]] -> [[a]]
juntarPorNiveles [] [] = []
juntarPorNiveles xss [] = xss
juntarPorNiveles [] yss = yss
juntarPorNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarPorNiveles xss yss


--Devuelve los elementos de la rama más larga del árbol
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x ti td) = if heightT (ti) > heightT (td)
                                    then x : ramaMasLarga ti
                                    else x : ramaMasLarga td

--Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminos (NodeT x ti td) =
    agregarATodas x
    (todosLosCaminos ti ++
    todosLosCaminos td )

agregarATodas :: a -> [[a]] -> [[a]]
agregarATodas a [] = []
agregarATodas a (xs:xss) =
    (a : xs) : (agregarATodas a xss)