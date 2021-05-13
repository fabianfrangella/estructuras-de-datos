module Queue2 (
	Queue,
	emptyQ,
	isEmptyQ,
	queue,
	firstQ,
	dequeue
 ) where

--3. Queue (cola)
--Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). Esto significa
--que los elementos salen en el orden con el que entraron, es decir, el que se agrega primero es el
--primero en salir (como la cola de un banco). Su interfaz es la siguiente:

--2. Implemente ahora la versión que agrega por delante y quita por el final de la lista. Compare
--la eficiencia entre ambas implementaciones.

data Queue a = Q [a] deriving Show

--Crea una cola vacía.
emptyQ :: Queue a
emptyQ = Q []

--Dada una cola indica si la cola está vacía.
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = null xs

--Dados un elemento y una cola, agrega ese elemento a la cola.
queue :: a -> Queue a -> Queue a
queue e (Q xs) = Q (e:xs)

--Dada una cola devuelve el primer elemento de la cola.
firstQ :: Queue a -> a
firstQ (Q xs) = last xs

--Dada una cola la devuelve sin su primer elemento.
dequeue :: Queue a -> Queue a
dequeue (Q xs) = Q (init xs)
