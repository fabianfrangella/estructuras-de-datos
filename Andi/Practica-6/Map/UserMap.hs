import Map1
--import Map2
--import Map3

-- Interfaz
-- emptyM :: Map k v
-- assocM :: Eq k => k -> v -> Map k v -> Map k v
-- lookupM :: Eq k => k -> Map k v -> Maybe v
-- deleteM :: Eq k => k -> Map k v -> Map k v
-- keys :: Map k v -> [k]

--Implementar como usuario del tipo abstracto Map las siguientes funciones:

--1. valuesM :: Eq k => Map k v -> [Maybe v]
--Propósito: obtiene los valores asociados a cada clave del map.
--
--2. todasAsociadas :: Eq k => [k] -> Map k v -> Bool
--Propósito: indica si en el map se encuenrtan todas las claves dadas.
--
--3. listToMap :: Eq k => [(k, v)] -> Map k v
--Propósito: convierte una lista de pares clave valor en un map.
--
--4. mapToList :: Eq k => Map k v -> [(k, v)]
--Propósito: convierte un map en una lista de pares clave valor.
--
--5. agruparEq :: Eq k => [(k, v)] -> Map k [v]
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
--la misma clave.
--
--6. incrementar :: Eq k => [k] -> Map k Int -> Map k Int
--Propósito: dada una lista de claves de tipo k y un mapa que va de k a int, le suma uno a
--cada número asociado con dichas claves.
--
--7. mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
--una clave del primero existe en el segundo, es reemplazada por la del primero.
--Indicar los ordenes de complejidad en peor caso de cada función implementada.

--Ejercicio 5
--Implemente estas otras funciones como usuario de Map:
--indexar :: [a] -> Map Int a
--Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
--su posición en la lista.
--
--ocurrencias :: String -> Map Char Int
--Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
--en el string, y los valores la cantidad de veces que aparecen en el mismo.
--Indicar los ordenes de complejidad en peor caso de cada función de la interfaz y del usuario.