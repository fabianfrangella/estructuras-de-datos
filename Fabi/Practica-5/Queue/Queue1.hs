module Queue1 (Queue a, emptyQ, isEmptyQ, queue, firstQ, dequeue) where 


data Queue a = Q [a]

--Crea una cola vacía.
--Costo: O(1)
emptyQ :: Queue a
emptyQ = Q []

--Dada una cola indica si la cola está vacía.
--Costo: O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q []) = True
isEmptyQ (Q xs) = False

--Dados un elemento y una cola, agrega ese elemento a la cola.
--Costo: O(n)
queue :: a -> Queue a -> Queue a
queue e (Q xs) = (Q (agregarAlFinal xs e))

--Dada una cola devuelve el primer elemento de la cola.
--Costo: O(1)
firstQ :: Queue a -> a
firstQ (Q []) = error 'no hay first en cola vacia'
firstQ (Q xs) = head xs

--Dada una cola la devuelve sin su primer elemento.
--Costo: O(1)
dequeue :: Queue a -> Queue a
dequeue (Q (x:xs)) = (Q [])
dequeue (Q xs) = (Q (tail xs))

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] n = [n]
agregarAlFinal (x:xs) n = x : agregarAlFinal xs n

-- 1. Implemente el tipo abstracto Queue utilizando listas. Los elementos deben encolarse por el
-- final de la lista y desencolarse por delante.

--2. Implemente ahora la versión que agrega por delante y quita por el final de la lista. Compare
--la eficiencia entre ambas implementaciones.


