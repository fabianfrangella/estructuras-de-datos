module Set1 (
	Set,
    emptyS,
    addS,
    belongs,
    sizeS,
    removeS,
    unionS,
    setToList
 ) where

--2. Set (conjunto)
--Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:

--1. Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda
--la cantidad de elementos en la estructura.
--Nota: la restricción Eq aparece en toda la interfaz se utilice o no en todas las operaciones
--de esta implementación, pero para mantener una interfaz común entre distintas posibles
--implementaciones estamos obligados a escribir así los tipos.

data Set a = S [a] deriving Show

s1 = S [1,2,3]
s2 = S [3,4,5,6]

--Crea un conjunto vacío.
emptyS :: Set a
emptyS = S []

--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a
addS e (S xs) =
    if elem e xs
        then (S xs)
        else (S (e:xs))

--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool
belongs e (S xs) = elem e xs

--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (S xs) = length xs

--Borra un elemento del conjunto.
removeS :: Eq a => a -> Set a -> Set a
removeS e (S xs) = S (removeXS e xs)

removeXS :: Eq a => a -> [a] -> [a]
removeXS e [] = []
removeXS e (x:xs) = 
    if e == x
        then xs
        else x : removeXS e xs

--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs) (S ys) = S (sinRepetidos (xs ++ ys))

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =
    if elem x xs
        then sinRepetidos xs
        else x : sinRepetidos xs

--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]
setToList (S xs) = xs

