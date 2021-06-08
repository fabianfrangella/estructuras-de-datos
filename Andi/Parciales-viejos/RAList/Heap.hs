module Heap (
    Heap,
    emptyH,
    isEmptyH,
    insertH,
    findMin,
    deleteMin
) where

--1. Priority Queue (cola de prioridad)
--Ejercicio 1
--La siguiente interfaz representa colas de prioridad, llamadas priority queue, en inglés. La misma
--posee operaciones para insertar elementos, y obtener y borrar el mínimo elemento de la estructura.
--Implementarla usando listas, e indicando el costo de cada operación.

data Heap a = PQ [a] deriving Show

pq = PQ [3, 2, 5, 9, 1]

--Propósito: devuelve una priority queue vacía.
emptyH :: Heap a
emptyH = PQ []

--Propósito: indica si la priority queue está vacía.
isEmptyH :: Heap a -> Bool
isEmptyH (PQ xs) = null xs

--Propósito: inserta un elemento en la priority queue.
insertH :: Ord a => a -> Heap a -> Heap a
insertH e (PQ xs) = PQ (e:xs)

--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
findMin :: Ord a => Heap a -> a
findMin (PQ xs) = findMin' xs

findMin' :: Ord a => [a] -> a
findMin' [x] = x
findMin' (x:xs) = min x (findMin' xs)

--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.
deleteMin :: Ord a => Heap a -> Heap a
deleteMin (PQ xs) = PQ (delete (findMin' xs) xs)

delete :: Ord a => a -> [a] -> [a]
delete e [] = []
delete e (x:xs) = 
    if x == e
        then xs
        else x : (delete e xs)

toList :: Ord a => Heap a -> [a]
toList h =
    if isEmptyH h
        then []
        else findMin h : toList (deleteMin h)

--Ejercicio 2
--Implementar la función heapSort :: Ord a => [a] -> [a], que dada una lista la ordena de
--menor a mayor utilizando una Heap como estructura auxiliar. ¿Cuál es su costo?