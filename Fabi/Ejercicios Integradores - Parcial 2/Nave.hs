module Nave () where

import Map
import MaxHeap
import Sector
import Tripulante

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible


--Un Map que relaciona para cada SectorId su sector correspondiente.
--Otro Map que relaciona para cada Nombre de tripulante el tripulante con dicho nombre.
--Una MaxHeap que incluye a todos los tripulantes de la nave, cuyo criterio de ordenado es por rango de los tripulantes.

-- Invariantes
-- Todos los tripulantes en el Map de tripulantes también están en el MaxHeap y viceversa
data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

--El tipo Sector es un tipo abstracto, y representa al sector de una nave, el cual contiene componentes y tripulantes asignados.
--El tipo Tripulante es un tipo abstracto, y representa a un tripulante dentro de la nave, el cual tiene un nombre, un rango
--y sectores asignados.
--El tipo SectorId es sinónimo de String, e identifica al sector de forma unívoca.
--Los tipos Nombre y Rango son sinónimos de String. Todos los nombres de tripulantes son únicos.
--Un sector está vacío cuando no tiene tripulantes, y la nave está vacía si no tiene ningún tripulante.
--Puede haber tripulantes sin sectores asignados.


construir :: [SectorId] -> Nave
--Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
--Eficiencia: O(S)
construir ssid = N (construirSectores ssid) emptyM emptyH

-- Eficiencia: O(S)
construirSectores :: [SectorId] -> Map SectorId Sector
construirSectores [] = emptyM
construirSectores (id: ids) = assocM id (crearS id) (construirSectores ids)


ingresarT :: Nombre -> Rango -> Nave -> Nave
--Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
--Eficiencia: O(log T)
ingresarT n r (N s mt mh) = 
    let t = crearT n r in
         N s (assocM n t mt) (insertH t mh)

sectoresAsignados :: Nombre -> Nave -> Set SectorId
--Propósito: Devuelve los sectores asignados a un tripulante.
--Precondición: Existe un tripulante con dicho nombre.
--Eficiencia: O(log M)
sectoresAsignados nom (N _ m _) = sectoresT (lookupM nom m)
        

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
--Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
--Precondición: Existe un sector con dicho id.
--Eficiencia: O(log S)
datosDeSector sid (N ms mt mh) = 
    let sector = lookupM sid ms in
        ((tripulantesS sector), componentesS sector)

tripulantesN :: Nave -> [Tripulante]
--Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
--Eficiencia: O(log T)
tripulantesN (N ms mt mh) = 
    if isEmptyH mh
        then []
        else maxH mh ++ tripulantesN N ms mt (deleteMaxH mh)

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
--Propósito: Asigna una lista de componentes a un sector de la nave.
--Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
agregarASector cs sid (N ms mt mh) = 
    N (assocM sectorId (agregarASector' (lookupM sectorId ms) ms)) mt mh

-- Eficiencia O(C)
agregarASector' :: Sector -> [Componente] -> Sector
agregarASector' s [] = s
agregarASector' s (c:cs) = agregarASector' (agregarC c s) cs


asignarASector :: Nombre -> SectorId -> Nave -> Nave
--Propósito: Asigna un sector a un tripulante.
--Nota: No importa si el tripulante ya tiene asignado dicho sector.
--Precondición: El tripulante y el sector existen.
--Eficiencia: O(log S + log T + T log T)
asignarASector nom sid (N ms mt mh) = N ms (asignarASectorM nom sid mt) (asignarASectorMH nom sid mh) 

-- Eficiencia O(log n)
asignarASectorM :: Nombre -> SectorId -> Map Nombre Tripulante -> Map Nombre Tripulante
asignarASectorM nom sid mt = 
    let tripulante = lookupM nom mt in
        assocM nom (asignarS sid  tripulante) 

asignarASectorMH :: Nombre -> SectorId -> MaxHeap Tripulante -> MaxHeap Tripulante
asignarASectorMH nom sid mh = 
    if nom == nombre (maxH mh)
        then insertH (asignarS sid tripulante) (deleteMaxH mh)
        else insertH (maxH mh) (asignarASectorMH nom sid (deleteMaxH mh))


