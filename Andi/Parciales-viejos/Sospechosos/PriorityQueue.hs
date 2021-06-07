module PriorityQueue (
    PriorityQueue,
    emptyPQ,
    isEmptyPQ,
    insertPQ,
    maxPQ,
    deleteMaxPQ
) where

data PriorityQueue a = PQ [a] deriving Show

pq = PQ [3, 2, 5, 9, 1]

--Propósito: devuelve una priority queue vacía.
emptyPQ :: PriorityQueue a
emptyPQ = PQ []

--Propósito: indica si la priority queue está vacía.
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ xs) = null xs

--Propósito: inserta un elemento en la priority queue.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ e (PQ xs) = PQ (e:xs)

maxPQ :: Ord a => PriorityQueue a -> a
maxPQ (PQ xs) = maximum xs

deleteMaxPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMaxPQ (PQ xs) = PQ (sinMax (maxPQ (PQ xs)) xs)

sinMax :: Ord a => a -> [a] -> [a]
sinMax e [] = []
sinMax e (x:xs) =
    if x == e
        then xs
        else x : sinMax e xs