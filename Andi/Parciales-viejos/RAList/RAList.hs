import Heap
import Map1
-- Random Access List
-- El objetivo de este parcial es implementar una lista de acceso aleatorio (Random Access List). Es similar a una lista, sólo
-- que es posible acceder a un elemento en base a índice de forma eficiente. Y, en este caso, además es posible acceder al mínimo
-- elemento, también de forma eficiente.
-- La representación que usaremos será la siguiente:

data RAList a = MkR Int (Map Int a) (Heap a) deriving Show

ra = add "d" $ add "c" $ add "a" emptyRAL

-- En dicha representación se observa:
-- Un Int, que representa la próxima posición a ocupar en la lista. Es decir, cuando se agregue un elemento al final, debe
-- agregarse en dicha posición, que luego será incrementada. Cuando la estructura está vacía, el número es 0.
-- Un Map Int a, que representa la relación entre índices (claves) y valores de la estructura.
-- Una Heap a que contiene todos los valores de la estructura.
-- La interfaz a implementar, siendo N la cantidad de elementos, es la siguiente:

-- Propósito: devuelve una lista vacía.
-- Eficiencia: O(1).
emptyRAL :: RAList a
emptyRAL = MkR 0 emptyM emptyH

-- Propósito: indica si la lista está vacía.
-- Eficiencia: O(1).
isEmptyRAL :: RAList a -> Bool
isEmptyRAL (MkR i mi hv) = i == 0

-- Propósito: devuelve la cantidad de elementos.
-- Eficiencia: O(1).
lengthRAL :: RAList a -> Int
lengthRAL (MkR i mi hv) = i

-- Propósito: devuelve el elemento en el índice dado.
-- Precondición: el índice debe existir.
-- Eficiencia: O(log N).
get :: Int -> RAList a -> a
get i (MkR p mi hv) =
    fromJust (lookupM i mi)

-- Propósito: devuelve el mínimo elemento de la lista.
-- Precondición: la lista no está vacía.
-- Eficiencia: O(1).
minRAL :: Ord a => RAList a -> a
minRAL (MkR i mi hv) = findMin hv

-- Propósito: agrega un elemento al final de la lista.
-- Eficiencia: O(log N).
add :: Ord a => a -> RAList a -> RAList a
add e (MkR i mi hv) =
    MkR (i + 1) (assocM (i + 1) e mi) (insertH e hv)

-- Propósito: transforma una RAList en una lista, respetando el orden de los elementos.
-- Eficiencia: O(N log N).
elems :: Ord a => RAList a -> [a]
elems (MkR i mi hv) = toList hv

-- Eficiencia: O(N log N).
toList :: Ord a => Heap a -> [a]
toList h =
    if isEmptyH h
        then []
        else findMin h : toList (deleteMin h)

-- Propósito: elimina el último elemento de la lista.
-- Precondición: la lista no está vacía.
-- Eficiencia: O(N log N).
remove :: Ord a => RAList a -> RAList a
remove (MkR i mi hv) =
    let e = fromJust (lookupM i mi) in
        MkR (i - 1) (deleteM i mi) (deleteH e hv)

deleteH :: Ord a => a -> Heap a -> Heap a
deleteH e h =
    if e == findMin h
        then deleteMin h
        else insertH (findMin h) (deleteH e (deleteMin h))

-- Propósito: reemplaza el elemento en la posición dada.
-- Precondición: el índice debe existir.
-- Eficiencia: O(N log N).
set :: Ord a => Int -> a -> RAList a -> RAList a
set p e (MkR i mi hv) =
    let eABorrar = fromJust (lookupM p mi) in
        MkR i (assocM p e mi) (replaceH e eABorrar hv)

replaceH :: Ord a => a -> a -> Heap a -> Heap a
replaceH e eABorrar hv =
    insertH e (deleteH eABorrar hv)

-- Propósito: agrega un elemento en la posición dada.
-- Precondición: el índice debe estar entre 0 y la longitud de la lista.
-- Observación: cada elemento en una posición posterior a la dada pasa a estar en su posición siguiente.
-- Eficiencia: O(N log N).
-- Sugerencia: definir una subtarea que corra los elementos del Map en una posición a partir de una posición dada. Pasar
-- también como argumento la máxima posición posible.
addAt :: Ord a => Int -> a -> RAList a -> RAList a
addAt p e (MkR i mi hv) =
    MkR (i + 1) (insertarOrdenado p e mi) (insertH e hv)

insertarOrdenado :: Ord a => Int -> a -> Map Int a -> Map Int a
insertarOrdenado i e mi =
    assocM i e (correrTodos (reverse (domM mi)) i mi) 

correrTodos :: Ord a => [Int] -> Int -> Map Int a -> Map Int a
correrTodos [] i mi = mi
correrTodos (x:xs) i mi =
    if i <= x
        then correrTodos xs i (assocM (x + 1) (fromJust(lookupM x mi)) mi)
        else mi

-- Eficiencia: O(1) 
fromJust :: Maybe a -> a
fromJust Nothing = error "Se rompio" 
fromJust (Just a) = a