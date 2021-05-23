module Set (emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where

-- Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda la cantidad de elementos en la estructura.

data Set = ConsS [] deriving Show

s1 = ConsS [1,2,3,4]
s1 = ConsS [3,4,1,6]

-- O(1)
--Crea un conjunto vacío.
emptyS :: Set a
emptyS = ConsS []

-- O(n)
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a
addS a (ConsS xs) = 
    if belongs a (ConsS xs) 
        then  ConsS xs 
        else ConsS (a:xs)

-- O(n)
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool
belongs a (ConsS xs) = elem a xs

--O(n)
--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (ConsS xs) = length xs

-- O(n)
--Borra un elemento del conjunto.
removeS :: Eq a => a -> Set a -> Set a
removeS a (ConsS xs) = ConsS (removeXS a xs)

removeXS :: Eq a => a -> [a] -> [a]
removeXS e [] = []
removeXS e (x:xs) = 
    if e == x
        then xs
        else x : removeXS e xs

-- O(n)
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS :: Eq a => Set a -> Set a -> Set a
unionS (ConsS xs1) (ConsS xs2) = ConsS (sinRepetidos xs1 ++ xs2)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =
    if elem x xs
        then sinRepetidos xs
        else x : sinRepetidos xs

-- O(1)
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]
setToList (ConsS xs) = xs