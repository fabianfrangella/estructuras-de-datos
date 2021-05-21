--2. Map (diccionario)

--Ejercicio 3
--La interfaz del tipo abstracto Map es la siguiente:

--3. Como dos listas, una de claves y otra de valores, donde la clave ubicada en la posición i está
--asociada al valor en la misma posición, pero de la otra lista.

data Map k v = M [k] [v] deriving Show

tel = M ["juan", "pedro"] [43221122, 12313322]

--Propósito: devuelve un map vacío
emptyM :: Map k v
emptyM = M [] []

--Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M ks vs) = armarMKV (replace k v ks vs)

armarMKV :: ([k], [v]) -> Map k v
armarMKV pkvs = M (fst pkvs) (snd pkvs) 

replace :: Eq k => k -> v -> [k] -> [v] -> ([k], [v])
replace k v [] [] = ([k], [v])
replace k v (k':ks) (v':vs) =
    if k == k'
        then ((k:ks), (v:vs))  
        else agregar k' v' (replace k v ks vs)

agregar :: Eq k => k -> v -> ([k], [v]) -> ([k], [v]) 
agregar k v (ks, vs) = ((k:ks), (v:vs))

--Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M ks vs) = find k ks vs

find :: Eq k => k -> [k] -> [v] -> Maybe v
find k [] [] = Nothing
find k (k':ks) (v':vs) =
    if k == k'
        then Just v' 
        else find k ks vs

--Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M ks vs) = armarMKV (delete k ks vs)

delete :: Eq k => k-> [k] -> [v] -> ([k], [v])
delete k [] [] = error "no esite"
delete k (k':ks) (v':vs) =
    if k == k'
        then (ks, vs)
        else agregar k' v' (delete k ks vs)

--Propósito: devuelve las claves del map.
keys :: Eq k => Map k v -> [k]
keys (M ks vs) = ks

----Ejercicio 4

--Implemente las siguientes variantes del tipo Map, indicando los costos obtenidos para cada operación: