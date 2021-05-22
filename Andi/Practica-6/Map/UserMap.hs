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

--Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = values (keys m) m

values :: Eq k => [k] -> Map k v -> [Maybe v]
values [] m = []
values (x:xs) m = lookupM x m : (values xs m)

--Propósito: indica si en el map se encuenrtan todas las claves dadas.
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] m = True
todasAsociadas (x:xs) m = elem x (keys m) && todasAsociadas xs m

--Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap (x:xs) = assocM (fst x) (snd x) (listToMap xs)

--Propósito: convierte un map en una lista de pares clave valor.
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m = toList (keys m) m

toList :: Eq k => [k] -> Map k v -> [(k, v)]
toList [] m = []
toList (x:xs) m = (x, valor (lookupM x m)) : toList xs m

valor :: Maybe v -> v
valor Nothing = error "no se obtener un valor"
valor (Just x) = x

--Picante
--5. agruparEq :: Eq k => [(k, v)] -> Map k [v]
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
--la misma clave.

--Propósito: dada una lista de claves de tipo k y un mapa que va de k a int, le suma uno a
--cada número asociado con dichas claves.
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar [] m = m
incrementar (x:xs) m =
    if elem x (keys m)
        then assocM x (valor (lookupM x m) + 1) (incrementar xs m) 
        else assocM x (valor (lookupM x m)) (incrementar xs m) 

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