--2. Map (diccionario)

--Ejercicio 3
--La interfaz del tipo abstracto Map es la siguiente:

--2. Como una lista de pares-clave valor con claves repetidas

data Map k v = M [(k, v)] deriving Show

tel = M [("juan", 44441231), ("pedro", 1231312), ("juan", 11111)] 

--Propósito: devuelve un map vacío
emptyM :: Map k v
emptyM = M []

--Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M xs) = M ((k,v) : xs)

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
delete k [] = []
delete k (x:xs) =
    if k == fst x
        then delete k xs
        else x : delete k xs

--Propósito: devuelve las claves del map.
keys :: Eq k => Map k v -> [k]
keys (M xs) = sinRepetidos (keys' xs)

keys' :: Eq k => [(k, v)] -> [k]
keys' [] = []
keys' (x:xs) = fst x : keys' xs

sinRepetidos :: Eq k => [k] -> [k]
sinRepetidos [] = []
sinRepetidos (k:ks) =
  if elem k ks
     then sinRepetidos ks
     else k : sinRepetidos ks

--Ejercicio 4

--Implemente las siguientes variantes del tipo Map, indicando los costos obtenidos para cada operación:

--3. Como dos listas, una de claves y otra de valores, donde la clave ubicada en la posición i está
--asociada al valor en la misma posición, pero de la otra lista.