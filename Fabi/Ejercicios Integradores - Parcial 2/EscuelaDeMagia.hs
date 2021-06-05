module EscuelaDeMagia (
    estaVacia, fundarEscuela, registrar, magos, hechizosDe, leFaltanAprender, egresarUno, enseñar
) where

import Map
import PriorityQueue
import Set


--Existe un tipo abstracto llamado Mago, ya implementado, cuya interfaz se adjunta en el anexo de interfaces. Un mago puede
--aprende hechizos, e informarnos su nombre y qué hechizos conoce.
--El tipo Hechizo es sinónimo de String, y se corresponde con el nombre de un hechizo.
--El tipo Nombre es sinónimo de String, y se corresponde con el nombre de un mago.
--Podemos suponer que dos magos son iguales si poseen el mismo nombre, y un mago es más poderoso que otro si conoce más
--hechizos (lo que permite ordenarlos por la cantidad de hechizos que saben).
--En la escuela no existen dos magos con el mismo nombre.


-- Invariantes de representación
-- No existen dos magos con el mismo nombre 
data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)

--Dicho esto, la representación que utilizaremos será la siguiente (que no es posible modificar ):
--data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)
--Esta representación utiliza:
--Un Set de hechizos que la escuela ha enseñado a lo largo de su historia.
--Un Map que asocia magos con su respectivo nombre.
--Una estructura llamada PriorityQueue que posee a todos los magos de la escuela y permite obtenerlos de forma eficiente
--de mayor a menor en base a la cantidad de hechizos que saben.

--Propósito: Devuelve una escuela vacía.
--Eficiencia: O(1)
fundarEscuela :: EscuelaDeMagia
fundarEscuela = EDM emptyS emptyM emptyPQ


--Propósito: Indica si la escuela está vacía.
--Eficiencia: O(1)
estaVacia :: EscuelaDeMagia -> Bool
estaVacia (EDM set map pq) = isEmptyPq pq

--Propósito: Incorpora un mago a la escuela (si ya existe no hace nada).
--Eficiencia: O(log M)
registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
registrar n (EDM set map pq) = 
    let mago = crearM n 
    in if existeMagoEnEscuela
        then EDM set map pq
        else EDM (addS mago set) (assocM n mago map) (insertPQ mago pq)

existeMagoEnEscuela :: Mago -> EscuelaDeMagia -> Bool
existeMagoEnEscuela m (EDM set map pq) = isJust (lookupM m map)

--Propósito: Devuelve los nombres de los magos registrados en la escuela.
--Eficiencia: O(M)
magos :: EscuelaDeMagia -> [Nombre]
magos (EDM set map pq) = domM map

--Propósito: Devuelve los hechizos que conoce un mago dado.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(log M)
hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
hechizosDe n (EDM set map pq) = hechizos fromJust (lookupM n map)

--Propósito: Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(log M)
leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
leFaltanAprender n (EDM set map pq) = sizeS set - sizeS (hechizosDe n (EDM set map pq))

--Propósito: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
--Precondición: Hay al menos un mago.
--Eficiencia: O(log M)
egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
egresarUno (EDM set map pq) = 
    let mago = maxPQ pq
    in (mago, EDM set (deleteM (nombre mago)) (deleteMaxPQ pq)) 

--Propósito: Enseña un hechizo a un mago existente, y si el hechizo no existe en la escuela es incorporado a la misma.
--Nota: No importa si el mago ya conoce el hechizo dado.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(M log M + log H)
enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
enseñar h n (EDM set map pq) = 
    let mago = lookupM mago map
    in EDM (addS h) (assocM (nombre mago) (aprender h mago) map) (insertPQ mago pq)


{-
Anexo de interfaces
Mago, siendo H la cantidad de hechizos que sabe:
crearM :: Nombre -> Mago O(1)
nombre :: Mago -> Nombre O(1)
aprender :: Hechizo -> Mago -> Mago O(log H)
hechizos :: Mago -> Set Hechizo O(1)

Set, siendo N la cantidad de elementos del conjunto:
emptyS :: Set a O(1)
addS :: Ord a => a -> Set a -> Set a O(log N)
belongsS :: Ord a => a -> Set a -> Bool O(log N)
unionS :: Ord a => Set a -> Set a -> Set a O(N log N)
sizeS :: Set a -> Int O(1)

PriorityQueue, siendo M la cantidad de elementos en la estructura:
emptyPQ :: PriorityQueue a O(1)
isEmptyPQ :: PriorityQueue a -> Bool O(1)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a O(log M)
maxPQ :: PriorityQueue a -> a O(1)
deleteMaxPQ :: Ord a => PriorityQueue a -> PriorityQueue a O(log M)

Map, siendo K la cantidad de claves distintas en el map:
emptyM :: Map k v O(1)
assocM :: Ord k => k -> v -> Map k v -> Map k v O(log K)
lookupM :: Ord k => k -> Map k v -> Maybe v O(log K)
deleteM :: Ord k => k -> Map k v -> Map k v O(log K)
domM :: Map k v -> [k] O(K)
-}
    
-- Helper functions
isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust Nothing = False

fromJust :: Maybe a -> a
fromjust (Just a) = a
fromJust Nothing = error "Nothing no tiene valor"