--2. Como usuario del tipo abstracto Set implementar las siguientes funciones:

--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]

--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos :: Eq a => [a] -> [a]

--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.
unirTodos :: Eq a => Tree (Set a) -> Set a

--3. Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En
--otras palabras, al agregar no va a chequear que si el elemento ya se encuentra en la lista, pero
--sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos,
--por ejemplo). Contrastar la eficiencia obtenida en esta implementación con la anterior.