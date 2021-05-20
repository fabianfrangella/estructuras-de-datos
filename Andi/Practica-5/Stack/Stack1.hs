module Stack1 (
	Stack,
    emptyS,
    isEmptyS,
    push,
    top,
    pop,
    lenS,
    st -- para usarlo de prueba
 ) where

--4. Stack (pila)
--Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out). Esto significa
--que los últimos elementos agregados a la estructura son los primeros en salir (como en una pila de
--platos). Su interfaz es la siguiente:

--2. Implementar el tipo abstracto Stack utilizando una lista.

data Stack a = S [a] Int deriving Show

st :: Stack Int
st = S [1,2,3,4,5] 5

--Crea una pila vacía.
emptyS :: Stack a
emptyS = S [] 0

--Dada una pila indica si está vacía.
isEmptyS :: Stack a -> Bool
isEmptyS (S xs l) = null xs

--Dados un elemento y una pila, agrega el elemento a la pila.
push :: a -> Stack a -> Stack a
push e (S xs l) = S (e:xs) (l+1)

--Dada un pila devuelve el elemento del tope de la pila.
top :: Stack a -> a
top (S xs l) = head xs

--Dada una pila devuelve la pila sin el primer elemento.
pop :: Stack a -> Stack a
pop (S xs l) = S (tail xs) (l-1)

--Dada una pila devuelve su tamaño.
--Costo: constante
lenS :: Stack a -> Int
lenS (S xs l) = l