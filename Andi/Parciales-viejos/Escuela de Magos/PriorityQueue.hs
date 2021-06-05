module PriorityQueue
  (
    PriorityQueue,
    emptyPQ,
    isEmptyPQ,
    insertPQ,
    maxPQ,
    deleteMaxPQ
  ) where

--1. Priority Queue (cola de prioridad)
--Ejercicio 1
--La siguiente interfaz representa colas de prioridad, llamadas priority queue, en inglés. La misma
--posee operaciones para insertar elementos, y obtener y borrar el mínimo elemento de la estructura.
--Implementarla usando listas, e indicando el costo de cada operación.

data PriorityQueue a = PQ [a] deriving Show

pq = PQ [10, 9, 8, 7, 3, 1]

--Propósito: devuelve una priority queue vacía.
emptyPQ :: PriorityQueue a
emptyPQ = PQ []

--Propósito: indica si la priority queue está vacía.
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ xs) = null xs

--Propósito: inserta un elemento en la priority queue.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ e (PQ xs) =
  if elem e xs
    then PQ (replace e xs)
    else PQ (agregar e xs)

-- Private
agregar :: Ord a => a -> [a] -> [a]
agregar e [] = [e]
agregar e (x:xs) = 
  if e < x
    then x : (agregar e xs)
    else e : xs

-- Private
replace :: Ord a => a -> [a] -> [a]
replace e (x:xs) =
  if e == x
    then e : xs
    else x : (replace e xs)

maxPQ :: Ord a => PriorityQueue a -> a
maxPQ (PQ xs) = head xs

deleteMaxPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMaxPQ (PQ xs) = (PQ (tail xs))
