--1. Priority Queue (cola de prioridad)
--Ejercicio 1
--La siguiente interfaz representa colas de prioridad, llamadas priority queue, en inglés. La misma
--posee operaciones para insertar elementos, y obtener y borrar el mínimo elemento de la estructura.
--Implementarla usando listas, e indicando el costo de cada operación.

module PriorityQueue (
    emptyPQ, 
    isEmptyPQ, 
    insertPQ,
    findMinPQ, 
    deleteMinPQ) where

data PriorityQueue a = ConsPQ [a]


pq = PQ [3, 2, 5, 9, 1]

-- O (1)
--Propósito: devuelve una priority queue vacía.
emptyPQ :: PriorityQueue a
emptyPQ = ConsPQ []

-- O(1)
--Propósito: indica si la priority queue está vacía.
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (ConsPQ xs) = null xs

-- O(1)
--Propósito: inserta un elemento en la priority queue.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ e (ConsPQ xs) = (ConsPQ e:xs)

--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (ConsPQ xs) = findMin xs

-- O(n)
findMin :: Ord a => [a] -> a
findMin [x] = x
findMin (x:xs) = min x (findMin xs)

--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (ConsPQ xs) = ConsPQ (delete (findMin xs) xs)

delete :: Ord a => a -> [a] -> [a]
delete (x:xs) e = 
    if x == e
        then xs
        else x : delete xs e
                

--Ejercicio 2
--Implementar la función heapSort :: Ord a => [a] -> [a], que dada una lista la ordena de
--menor a mayor utilizando una Heap como estructura auxiliar. ¿Cuál es su costo?
