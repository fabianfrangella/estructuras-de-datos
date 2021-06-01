--Ejercicio 3
--La interfaz del tipo abstracto Map es la siguiente:

module Map (emptyM, assocM, lookupM, deleteM, keys) where

-- Invariante de representacion: no existen pares con "k" repetido
data Map k v = M [(k,v)] deriving Show

tel = M [("juan", 44441231), ("pedro", 1231312)]
p1 = M [(1, "juan"), (2, "pepe"), (3, "mauro")]
p2 = M [(3, "andy"), (5, "fafa"), (6, "fafafafa")]

notas :: Map String Int
notas = M [("juan", 9), ("andy", 8), ("fabi", 9), ("fede", 8)]
--------------------------------------------------------------------

-- O(1)
--Propósito: devuelve un map vacío
emptyM :: Map k v
emptyM = M []


-- O(n)
--Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M xs)) = M (replace k v xs)

-- O(n)
replace :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
replace k v [] = [(k,v)]
replace k v (x:xs) = 
    if k == fst x
        then (k, v) : xs
        else x : replace k v xs

-- O(n)
--Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M xs) = lookUpL k xs

-- O(n)
lookUpL :: Eq k => k -> [(k,v)] -> Maybe v
lookUpL _ [] = Nothing
lookUpL k (x:xs) = 
    if k == fst x
        then Just (snd x)
        else lookUpL k xs

-- O(n)
--Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M xs) = M (delete k xs)

-- O(n)
delete :: Eq k => k -> [(k,v)] => [(k,v)]
delete k [] = []
delete k (x:xs) = 
    if fst x == k
        then xs
        else x : delete k xs

-- O(n)
--Propósito: devuelve las claves del map.
keys :: Map k v -> [k]
keys (M xs) = keysL xs

-- O(n)
keysL :: [(k,v)] -> [k]
keysL [] = []
keysL (x:xs) = fst x : keysL xs

--Ejercicio 4

--Implemente las siguientes variantes del tipo Map, indicando los costos obtenidos para cada operación:

--1. Como una lista de pares-clave valor sin claves repetidas
--2. Como una lista de pares-clave valor con claves repetidas
--3. Como dos listas, una de claves y otra de valores, donde la clave ubicada en la posición i está
--asociada al valor en la misma posición, pero de la otra lista.
