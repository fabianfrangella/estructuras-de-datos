module Map1
  (
    Map,
    emptyM,
    assocM,
    lookupM,
    deleteM,
    keys
  ) where

--2. Map (diccionario)

--Ejercicio 3
--La interfaz del tipo abstracto Map es la siguiente:

--1. Como una lista de pares-clave valor sin claves repetidas 
data Map k v = M [(k, v)] deriving Show

tel = M [("juan", 44441231), ("pedro", 1231312)] 

--Propósito: devuelve un map vacío
emptyM :: Map k v
emptyM = M []

--Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M xs) = M (replace k v xs)

replace :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
replace k v [] = [(k,v)]
replace k v (x:xs) =
    if k == fst x
        then (k,v) : xs
        else x : replace k v xs

--Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M xs) = find k xs

find :: Eq k => k -> [(k, v)] -> Maybe v
find k [] = Nothing
find k (x:xs) =
    if k == fst x
        then Just (snd x)
        else find k xs

--Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M xs) = M (delete k xs)

delete :: Eq k => k -> [(k, v)] -> [(k, v)]
delete k [] = error "no esite"
delete k (x:xs) =
    if k == fst x
        then xs
        else x : delete k xs

--Propósito: devuelve las claves del map.
keys :: Map k v -> [k]
keys (M xs) = keys' xs

keys' :: [(k, v)] -> [k]
keys' [] = []
keys' (x:xs) = fst x : keys' xs
