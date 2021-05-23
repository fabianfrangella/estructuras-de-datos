{-

-}

--Especificar el costo operacional de las siguientes funciones:
head' :: [a] -> a
head' (x:xs) = x

-- Costo: O(1)
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

-- Costo: O(n)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Costo: O(n)
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


-- Costo: O(n²)
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

-- Costo: O(n)
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

-- Costo: O(n²)
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =
	if pertenece x xs
		then sinRepetidos xs
		else x : sinRepetidos xs

-- equivalente a (++)
-- Costo: O(n)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- Costo: O(n²)
concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

-- Costo: O(n)
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

-- Costo: O(n)
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

-- Costo: O(n)
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

-- Costo: O(n)
minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

-- Costo: O(n)
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) =
	if n == x
		then xs
		else x : sacar n xs

-- Costo: O(n²)
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =
	let m = minimo xs
	in m : ordenar (sacar m xs)


{-
	
Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:

emptyS :: Set a
Crea un conjunto vacío.

addS :: Eq a => a -> Set a -> Set a
Dados un elemento y un conjunto, agrega el elemento al conjunto.

belongs :: Eq a => a -> Set a -> Bool
Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.

sizeS :: Eq a => Set a -> Int
Devuelve la cantidad de elementos distintos de un conjunto.


removeS :: Eq a => a -> Set a -> Set a
Borra un elemento del conjunto.

unionS :: Eq a => Set a -> Set a -> Set a
Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.

setToList :: Eq a => Set a -> [a]
Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.

1. Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda
la cantidad de elementos en la estructura.
Nota: la restricción Eq aparece en toda la interfaz se utilice o no en todas las operaciones
de esta implementación, pero para mantener una interfaz común entre distintas posibles
implementaciones estamos obligados a escribir así los tipos.

2. Como usuario del tipo abstracto Set implementar las siguientes funciones:

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
al conjunto.

sinRepetidos :: Eq a => [a] -> [a]
Quita todos los elementos repetidos de la lista dada utilizando un conjunto como es-
tructura auxiliar.

unirTodos :: Eq a => Tree (Set a) -> Set a
Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
del arbol.

Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out). Esto significa
que los últimos elementos agregados a la estructura son los primeros en salir (como en una pila de
platos). Su interfaz es la siguiente:
emptyS :: Stack a
Crea una pila vacía.
isEmptyS :: Stack a -> Bool
Dada una pila indica si está vacía.
push :: a -> Stack a -> Stack a
Dados un elemento y una pila, agrega el elemento a la pila.
top :: Stack a -> a
Dada un pila devuelve el elemento del tope de la pila.
pop :: Stack a -> Stack a
Dada una pila devuelve la pila sin el primer elemento.
lenS :: Stack a -> Int
Dada una pila devuelve la pila sin el primer elemento.
Costo: constante.
1. Como usuario del tipo abstracto Stack implementar las siguientes funciones:
apilar :: [a] -> Stack a
Dada una lista devuelve una pila sin alterar el orden de los elementos.
desapilar :: Stack a -> [a]
Dada una pila devuelve una lista sin alterar el orden de los elementos.
insertarEnPos :: Int -> a -> Stack a -> Stack a
Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
2. Implementar el tipo abstracto Stack utilizando una lista.

-}