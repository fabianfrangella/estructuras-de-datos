module Nave (
    Nave,
    construir,
    ingresarT,
    sectoresAsignados,
    datosDeSector,
    tripulantesN,
    agregarASector,
    asignarASector
) where

import Sector

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

-- Invariantes
-- a) Dar invariantes de representación válidos según la descripción de la estructura.
-- Implementación
-- Implementar la siguiente interfaz de Nave, utilizando la representación y los costos dados, calculando los costos de cada
-- subtarea, y siendo T la cantidad de tripulantes y S la cantidad de sectores:

-- Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
-- Eficiencia: O(S)
construir :: [SectorId] -> Nave
construir ss = N (construir' ss) emptyM emptyH

-- Eficiencia: O(S)
construir' :: [SectorId] -> Map SectorId Sector
construir' [] = emptyM
construir' (s:ss) =
    assocM s (crearS s) (construir' ss)

-- Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
-- Eficiencia: O(log T)
ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT n r (N ms mt mht) =
    let tripulante = crearT n r in
        N ms (assocM n tripulante mt) (insertH tripulante mht)

-- Propósito: Devuelve los sectores asignados a un tripulante.
-- Precondición: Existe un tripulante con dicho nombre.
-- Eficiencia: O(log M)
sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados n (N ms mt mht) =
    sectoresAsignados' n mt

sectoresAsignados' :: Nombre -> Map Nombre Sector Id -> Set SectorId
sectoresAsignados' n mp =
    sectoresT (fromJust (lookupM n mp))

-- Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
-- Precondición: Existe un sector con dicho id.
-- Eficiencia: O(log S)
datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
datosDeSector sid (N ms mt mht) = 
    let sector = fromJust (lookupM sid ms) in
        ((tripulantesS sector), (componentesS sector))

-- Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
-- Eficiencia: O(T * log T)
tripulantesN :: Nave -> [Tripulante]
tripulantesN (N ms mt mht) =
    toList mht

toList :: MaxHeap Tripulante -> [Tripulante]
toList mht =
    if isEmptyH mht
        then []
        else maxH mht : toList (deleteMaxH mht)

-- Propósito: Asigna una lista de componentes a un sector de la nave.
-- Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs sid (N ms mt mht) = 
    N (agregarASector' cs sid ms) mt mht

agregarASector' :: [Componente] -> SectorId -> Map SectorId Sector -> Map SectorId Sector
agregarASector' cs sid ms =
    case (lookupM sid ms) of
        Nothing -> error "No existe el Id"
        Just s -> assocM sid (agregarComponentes s cs) ms

agregarComponentes :: Sector -> [Componente] -> Sector
agregarComponentes s [] = s
agregarComponentes s (c:cs) =
    agregarC c (agregarComponentes s cs)
 
-- Propósito: Asigna un sector a un tripulante.
-- Nota: No importa si el tripulante ya tiene asignado dicho sector.
-- Precondición: El tripulante y el sector existen.
-- Eficiencia: O(log S + log T + T log T)
asignarASector :: Nombre -> SectorId -> Nave -> Nave
asignarASector n sid (N ms mt mht) =
    let t fromJust (lookupM n mt) in
        let s = fromJust (lookupM sid ms) in
            N (assocM sid (agregarT n s) ms) (assocM n (assignarS sid t) mt) (updateMHT t mht)

updateMHT :: Tripulante -> MaxHeap Tripulante -> MaxHeap Tripulante
updateMHT t mht =
    if t == maxH mht
        then insertH t (deleteMaxH mht)
        else insertH maxH (updateMHT t (deleteMaxH mht))

-- El de clase:

-- asignarSector :: Nombre -> SectorId -> Nave -> Nave
-- asignarSector n sid (N ms mt mh) =
-- 	N ms (actualizarMT n sid mt) (actualizarMH n sid mh)

-- -- O(log N)
-- actualizarMT n sid mt =
-- 	case lookupM n mt of
-- 		Just t -> assocM n (asignarS sid t) mt

-- -- O(N . log N)
-- actualizarMH n sid mh =
-- 	insertH (asignarS sid (buscarN n mh)) (quitarN n mh)

-- -- O(N . log N)
-- buscarN n mh =
-- 	if isEmptyH mh
-- 	   then error "tenia que existir"
-- 	   else if nombre (maxH mh) == n
-- 	   	       then maxH mh
-- 	   	       else buscarN n (deleteMaxH mh)

-- -- O(N . log N)
-- quitarN n mh = 
-- 	if isEmptyH mh
-- 	   then error "tenia que existir"
-- 	   else if nombre (maxH mh) == n
-- 	   	       then deleteMaxH mh
-- 	   	       else insertH (maxH mh) (quitarN n (deleteMaxH mh))