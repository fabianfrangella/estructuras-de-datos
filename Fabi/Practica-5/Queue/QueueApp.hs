--Como usuario del tipo abstracto Queue implementar las siguientes funciones:
import Queue1

{-

--Costo: O(1)
emptyQ :: Queue a

--Dada una cola indica si la cola está vacía.
isEmptyQ :: Queue a -> Bool

--Dados un elemento y una cola, agrega ese elemento a la cola.
queue :: a -> Queue a -> Queue a

--Dada una cola devuelve el primer elemento de la cola.
firstQ :: Queue a -> a

--Dada una cola la devuelve sin su primer elemento.
dequeue :: Queue a -> Queue a

agregarAlFinal :: [a] -> a -> [a]
-}


--Cuenta la cantidad de elementos de la cola.
lengthQ :: Queue a -> Int
lengthQ q = if not isEmptyQ q
                then 1 + lengthQ (dequeue q)
                else lengthQ (dequeue q)

--Dada una cola devuelve la lista con los mismos elementos,
--donde el orden de la lista es el de la cola.
--Nota: chequear que los elementos queden en el orden correcto.
queueToList :: Queue a -> [a]
queueToList q = if not isEmptyQ q
                    then firstQ q : queueToList (dequeue q)
                    else []
--Inserta todos los elementos de la segunda cola en la primera.
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if not isEmptyQ q2
                    then unionQ (queue (firstQ q2) q1) dequeue q2
                    else q1