import Set1
--import Set2

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

newS = emptyS
s1 = addS 1 (addS 2 (addS 3 newS))
s2 = addS 3 (addS 4 (addS 5 newS))
s3 = addS 7 (addS 8 (addS 9 newS))

tree =
    NodeT s1
        (NodeT s2 EmptyT EmptyT)
        (NodeT s3 EmptyT EmptyT)

--2. Como usuario del tipo abstracto Set implementar las siguientes funciones:

--Crea un conjunto vacío.
--emptyS :: Set a

--Dados un elemento y un conjunto, agrega el elemento al conjunto.
--addS :: Eq a => a -> Set a -> Set a

--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
--belongs :: Eq a => a -> Set a -> Bool

--Devuelve la cantidad de elementos distintos de un conjunto.
--sizeS :: Eq a => Set a -> Int

--Borra un elemento del conjunto.
--removeS :: Eq a => a -> Set a -> Set a

--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
--unionS :: Eq a => Set a -> Set a -> Set a

--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
--setToList :: Eq a => Set a -> [a]

--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] s = []
losQuePertenecen (x:xs) s =
    if elem x (setToList s)
        then x : losQuePertenecen xs s
        else losQuePertenecen xs s

--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (toSet xs)

-- Costo 1: cuadratica
-- Costo 2: ?
toSet :: Eq a => [a] -> Set a
toSet [] = emptyS
toSet (x:xs) = addS x (toSet xs)

--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT x ti td) = 
    toSet (setToList x ++
    (setToList (unirTodos ti) ++
    setToList (unirTodos td)))