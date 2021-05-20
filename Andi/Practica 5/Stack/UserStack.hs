import Stack1

--1. Como usuario del tipo abstracto Stack implementar las siguientes funciones:

--Crea una pila vacía.
--emptyS :: Stack a

--Dada una pila indica si está vacía.
--isEmptyS :: Stack a -> Bool

--Dados un elemento y una pila, agrega el elemento a la pila.
--push :: a -> Stack a -> Stack a

--Dada un pila devuelve el elemento del tope de la pila.
--top :: Stack a -> a

--Dada una pila devuelve la pila sin el primer elemento.
--pop :: Stack a -> Stack a

--lenS :: Stack a -> Int

--Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a
apilar [] = emptyS
apilar (x:xs) = push x (apilar xs)

--Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar :: Stack a -> [a]
desapilar s =
    if isEmptyS s
        then []
        else top s : desapilar (pop s)

--Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
--posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos n e st =
    if n == 0
        then push e st
        else insertarEnPos (n-1) e (pop st)