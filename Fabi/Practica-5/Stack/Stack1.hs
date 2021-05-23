{-Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out). Esto significa
que los últimos elementos agregados a la estructura son los primeros en salir (como en una pila de
platos). Su interfaz es la siguiente:-}

module Stack (emptyS, isEmptyS, push, pop, lenS) where

data Stack = ConsS [] int deriving Show

-- O(1)
--Crea una pila vacía.
emptyS :: Stack a
emptyS = ConsS [] 0

-- O(n)
--Dada una pila indica si está vacía.
isEmptyS :: Stack a -> Bool
isEmptyS (ConsS xs _) = null xs

-- O(1)
--Dados un elemento y una pila, agrega el elemento a la pila.
push :: a -> Stack a -> Stack a
push a (ConsS xs l) = ConsS (a:xs) (l+1)

-- O(1)
--Dada un pila devuelve el elemento del tope de la pila.
top :: Stack a -> a
top (ConsS xs _) = head xs

-- O(1)
--Dada una pila devuelve la pila sin el primer elemento.
pop :: Stack a -> Stack a
pop (ConsS xs l) = ConsS (tail xs) (l-1)

-- O(1)
--Dada una pila devuelve su tamaño.
--Costo: constante.
lenS :: Stack a -> Int
lenS (ConsS xs l) = l


