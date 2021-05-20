
data Queue a = Q [a] [a] deriving Show

--Crea una cola vacía.
emptyQ :: Queue a
emptyQ = Q [] []

--Dada una cola indica si la cola está vacía.
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q fs bs) = null fs

--Dados un elemento y una cola, agrega ese elemento a la cola.
queue :: a -> Queue a -> Queue a
queue e (Q fs xs) = undefined

--Dada una cola devuelve el primer elemento de la cola.
firstQ :: Queue a -> a
firstQ (Q fs bs) = undefined

--Dada una cola la devuelve sin su primer elemento.
dequeue :: Queue a -> Queue a
dequeue (Q fs bs) = undefined
