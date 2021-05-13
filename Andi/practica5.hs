--1. Cálculo de costos
--Especificar el costo operacional de las siguientes funciones:

--Costo: O(1)
head' :: [a] -> a
head' (x:xs) = x

--Costo: O(1)
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

--Costo: O(n)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--Costo: O(n)
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--Costo: O(n^2)
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

--Costo: O(n)
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

--Costo: O(n^2) ?
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =
    if pertenece x xs
        then sinRepetidos xs
        else x : sinRepetidos xs

-- equivalente a (++)
--Costo: O(n)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

--Costo: O(n)
concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

--Costo: O(n^2) ?
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

--Costo: O(n^2) ?
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

--Costo: O(n^2) ?
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

--Costo: O(n)
minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

--Costo: O(n)
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) =
    if n == x
        then xs
        else x : sacar n xs

--Costo: O(n^2) ?
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =
    let m = minimo xs
        in m : ordenar (sacar m xs)

--2. Set (conjunto)
--Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:

data Set a = ConsS [a]

--Crea un conjunto vacío.
emptyS :: Set a

--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a

--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool

--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int

--Borra un elemento del conjunto.
removeS :: Eq a => a -> Set a -> Set a

--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS :: Eq a => Set a -> Set a -> Set a

--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]

--1. Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda
--la cantidad de elementos en la estructura.
--Nota: la restricción Eq aparece en toda la interfaz se utilice o no en todas las operaciones
--de esta implementación, pero para mantener una interfaz común entre distintas posibles
--implementaciones estamos obligados a escribir así los tipos.
--2. Como usuario del tipo abstracto Set implementar las siguientes funciones:

--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]

--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos :: Eq a => [a] -> [a]

--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.
unirTodos :: Eq a => Tree (Set a) -> Set a

--3. Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En
--otras palabras, al agregar no va a chequear que si el elemento ya se encuentra en la lista, pero
--sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos,
--por ejemplo). Contrastar la eficiencia obtenida en esta implementación con la anterior.