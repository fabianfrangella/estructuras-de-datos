import EscuelaDeMagia
import Set
import PriorityQueue
import Map
import Mago

-- Interfaz EDM estaVacia, fundarEscuela, registrar, magos, hechizosDe, leFaltanAprender, egresarUno, enseñar

--Propósito: Retorna todos los hechizos aprendidos por los magos.
--Eficiencia: O(M ∗ (log M + H log H))
hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
hechizosAprendidos edm = hechizosAprendidos' (magos edm) edm

hechizosAprendidos' :: [Mago] -> EscuelaDeMagia -> Set Hechizo
hechizosAprendidos' [] edm = emptyS
hechizosAprendidos' (x:xs) = hechizosDe x edm : hechizosAprendidos' xs edm

--Propósito: Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.
--Eficiencia: O(log M)
hayUnExperto :: EscuelaDeMagia -> Bool
hayUnExperto edm = 
    let experto = fst (egresarUno edm)
    in esExperto experto edm

--Propósito: Devuelve un par con la lista de magos que saben todos los hechizos dados por la escuela y la escuela sin dichos magos.
--Eficiencia: O(M log M)
egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
egresarExpertos edm = 
    let par = egresarUno edm
    in if esExperto (fst par) edm
        then ([fst par] ++ fst (egresarExpertos (snd par)), snd par)
        else ([], edm)

esExperto :: Mago -> EscuelaDeMagia -> Bool
esExperto m edm = leFaltanAprender (nombre m) edm == 0

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