import Stack1 

-- O (n)
--Como usuario del tipo abstracto Stack implementar las siguientes funciones:
--Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a
apilar [] = emptyS
apilar (x:xs) = push x (apilar xs)

-- O(n2)
--Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar :: Stack a -> [a]
desapilar s = 
    if isEmptyS s
        then []
        else top s : desapilar (pop s) 

-- O(n)
--Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
--posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos n e st =
    if n == 0
        then push e st
        else insertarEnPos (n-1) e (pop st)
