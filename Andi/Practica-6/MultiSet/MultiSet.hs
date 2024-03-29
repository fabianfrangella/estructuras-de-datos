--3. MultiSet (multiconjunto)
--Ejercicio 6
--
--Un MultiSet (multiconjunto) es un tipo abstracto de datos similar a un Set (conjunto). A dife-
--rencia del último, cada elemento posee una cantidad de apariciones, que llamaremos ocurrencias
--del elemento en el multiset. Su interfaz es la siguiente:
--
--emptyMS :: MultiSet a
--Propósito: denota un multiconjunto vacío.
--
--addMS :: Ord a => a -> MultiSet a -> MultiSet a
--Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
--multiconjunto.
--
--ocurrencesMS :: Ord a => a -> MultiSet a -> Int
--Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
--elemento en el multiconjunto.
--
--unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a (opcional)
--Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
--ambos multiconjuntos.
--
--intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a (opcional)
--Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
--multiconjuntos tienen en común.
--
--multiSetToList :: MultiSet a -> [(a, Int)]
--Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
--su cantidad de ocurrencias.
--
--1. Implementar el tipo abstracto MultiSet utilizando como representación un Map. Indicar los
--ordenes de complejidad en peor caso de cada función de la interfaz.
--
--2. Reimplementar como usuario de MultiSet la función ocurrencias de ejercicios anteriores,
--que dado un string cuenta la cantidad de ocurrencias de cada caracter en el string. En este
--caso el resultado será un multiconjunto de caracteres.