-- Ejercicio 2
-- Implementar las siguientes funciones suponiendo que reciben un árbol binario que cumple los
-- invariantes de BST y sin elementos repetidos (despreocuparse por el hecho de que el árbol puede
-- desbalancearse al insertar o borrar elementos).

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

tree :: Tree Int
tree =
    NodeT 100
        (NodeT 80
            (NodeT 75 EmptyT EmptyT)
            (NodeT 90 EmptyT EmptyT))
        (NodeT 150
            (NodeT 120  
                (NodeT 110 EmptyT EmptyT) EmptyT)
            (NodeT 180 EmptyT EmptyT))

bst :: Tree Int
bst =
    NodeT 100
        (NodeT 80 (NodeT 75 EmptyT EmptyT) (NodeT 90 EmptyT EmptyT))
        (NodeT 150 EmptyT EmptyT)

--Propósito: dado un BST dice si el elemento pertenece o no al árbol.
--Costo: O(log N)
belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST e EmptyT = False
belongsBST e (NodeT x ti td) = 
    if e == x
        then True
        else if e < x
            then belongsBST e ti
            else belongsBST e td

--Propósito: dado un BST inserta un elemento en el árbol.
--Costo: O(log N)
insertBST :: Ord a => a -> Tree a -> Tree a
insertBST e EmptyT = (NodeT e EmptyT EmptyT)
insertBST e (NodeT x ti td) = 
    if e == x
        then (NodeT e ti td)
        else if e < x
	   	    then NodeT x (insertBST e ti) td
	   	    else NodeT x ti (insertBST e td)

--Propósito: dado un BST borra un elemento en el árbol.
--Costo: O(log N)
deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST e EmptyT = EmptyT
deleteBST e (NodeT x ti td) = 
    if e == x
        then rearmarBST ti td
        else if e < x
            then NodeT x (deleteBST e ti) td
            else NodeT x ti (deleteBST e td)

--Costo: O(log N)
rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
rearmarBST EmptyT t1 = t1
rearmarBST t1 EmptyT = t1
rearmarBST ti td = NodeT (minBST td) ti (deleteMinBST td)

--Costo: O(log N)
minBST :: Ord a => Tree a -> a
minBST EmptyT = error "no hay minimo"
minBST (NodeT x EmptyT td) = x
minBST (NodeT x ti td) = minBST ti

--Costo: O(log N)
deleteMinBST :: Ord a => Tree a -> Tree a
deleteMinBST EmptyT = error "no hay elementos"
deleteMinBST (NodeT x EmptyT td) = td
deleteMinBST (NodeT x ti td) = NodeT x (deleteMinBST ti) td

-- Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
-- Costo: O(log N)
splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMinBST t = (minBST t, deleteMinBST t)

-- Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
-- Costo: O(log N)
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
splitMaxBST t = (maxBST t, deleteMaxBST t)

--Costo: O(log N)
maxBST :: Ord a => Tree a -> a
maxBST EmptyT = error "no hay maximo"
maxBST (NodeT x ti EmptyT) = x
maxBST (NodeT x ti td) = maxBST td

--Costo: O(log N)
deleteMaxBST :: Ord a => Tree a -> Tree a
deleteMaxBST EmptyT = error "no hay elementos"
deleteMaxBST (NodeT x ti EmptyT) = ti
deleteMaxBST (NodeT x ti td) = NodeT x ti (deleteMaxBST td)

-- Propósito: indica si el árbol cumple con los invariantes de BST.
-- Costo: O(N2)
esBST :: Ord a => Tree a -> Bool
esBST EmptyT = True
esBST (NodeT x ti td) = esMayor x ti && esMenor x td && esBST ti && esBST td

--Costo: O(1)
esMayor :: Ord a => a -> Tree a -> Bool
esMayor e EmptyT = True
esMayor e (NodeT x ti td) = e > x

--Costo: O(1)
esMenor :: Ord a => a -> Tree a -> Bool
esMenor e EmptyT = True
esMenor e (NodeT x ti td) = e < x

-- Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
-- elemento dado.
-- Costo: O(log N)
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA e EmptyT = Nothing
elMaximoMenorA e t = 
    if maxBST t > e
        then elMaximoMenorA e (deleteMaxBST t)
        else Just (maxBST t)

-- Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
-- elemento dado.
-- Costo: O(log N)
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
elMinimoMayorA e EmptyT = Nothing
elMinimoMayorA e t = 
    if minBST t < e
        then elMinimoMayorA e (deleteMinBST t)
        else Just (minBST t)

-- Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
-- nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
-- Costo: O(N2))
balanceado :: Tree a -> Bool
balanceado EmptyT = True
balanceado (NodeT x ti td) = 
    abs(heightT ti - heightT td) <= 1 &&
    balanceado ti &&
    balanceado td

--Costo: O(N)
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x ti td) = 1 + max (heightT ti) (heightT td)