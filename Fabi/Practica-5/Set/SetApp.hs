import Set1

-- O(n2)
--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
--al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] _ = []
losQuePertenecen (x:xs) s = 
    if belongs x s
        then x : losQuePertenecen xs (removeS x s)
        else losQuePertenecen xs s

-- Costo set1 O(n2)
-- Costo set2 O(n)
--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como es-
--tructura auxiliar.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (listToSet xs)

listToSet :: Eq a -> [a] -> Set a
listToSet [] = emptyS
listToSet (x:xs) = addS x (listToSet xs)

--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
--del arbol.
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT x ti td) = 
    toSet (setToList x ++
    (setToList (unirTodos ti) ++
    setToList (unirTodos td)))