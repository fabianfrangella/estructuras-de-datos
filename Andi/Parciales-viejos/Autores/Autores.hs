module Autores 
    (
        Organizador,
        Persona,
        Checksum,
        nuevo,
        agregarPrograma,
        todosLosProgramas,
        autoresDe,
        programasDe,
        programaronJuntas,
        nroProgramasDePersona
    )
where

import Map1
import Set

type Checksum = Int
type Persona = String

data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum)) deriving Show

-- Invariantes de representación:
-- La asociación del primer map tiene que ser consistente con la del segundo. Ejemplo:
    -- Si en el checksum x tengo un set con "juan", en el segundo, para "juan" debe aparecer mínimo el x en el set

np :: Set Persona
np = addS "pepito" (addS "juansis" emptyS)

chss :: Map Checksum (Set Persona)
chss = assocM 1 (addS "pepe" (addS "andi" emptyS)) emptyM

pps :: Map Persona (Set Checksum)
pps = assocM "pepe" (addS 1 emptyS) (assocM "andi" (addS 1 emptyS) emptyM)

o = MkO chss pps

-- Propósito: Un organizador vacío.
-- Eficiencia: O(1)
nuevo :: Organizador
nuevo = MkO emptyM emptyM

-- Propósito: Agrega al organizador un programa con el Checksum indicado; 
-- el conjunto es el conjunto de personas autores de dicho programa.
-- Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, 
-- y el Set de personas no está vacío.

-- Eficiencia: no hay ninguna garantía de eficiencia.
-- Eficiencia: (log C + P * log P)
agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (MkO chs ps) ch pps = 
    MkO (assocM ch pps chs) (updatePs ch pps ps)

-- Eficiencia: (P * log P)
updatePs :: Checksum -> Set Persona -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
updatePs ch pps mp = updatePs' ch (set2list pps) mp 

-- Eficiencia: (P * log P)
updatePs' :: Checksum -> [Persona] -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
updatePs' ch [] mp = mp
updatePs' ch (x:xs) mp =
    assocM x (addOrUpdate ch x mp) (updatePs' ch xs mp)

-- Eficiencia: O(log P)
addOrUpdate :: Checksum -> Persona -> Map Persona (Set Checksum) -> Set Checksum
addOrUpdate ch p mp =
    case lookupM p mp of
        Nothing -> addS ch emptyS
        Just ss -> addS ch ss

-- Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
-- Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
todosLosProgramas :: Organizador -> [Checksum]
todosLosProgramas (MkO mc mp) = domM mc

-- Propósito: denota el conjunto de autores que aparecen en un programa determinado.
-- Precondición: el Checksum debe corresponder a un programa del organizador.
-- Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
autoresDe :: Organizador -> Checksum -> Set Persona
autoresDe (MkO mc mp) ch = fromJust (lookupM ch mc)

-- Propósito: denota el conjunto de programas en los que participó una determinada persona.
-- Precondición: la persona debe existir en el organizador.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
programasDe :: Organizador -> Persona -> Set Checksum
programasDe (MkO mc mp) p = fromJust (lookupM p mp)

-- Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
-- Precondición: las personas deben ser distintas.
-- Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
-- programas del organizador, y C la cantidad total de programas.
programaronJuntas :: Organizador -> Persona -> Persona -> Bool
programaronJuntas o p1 p2 = not (isEmptyS (softwaresEnComun o p1 p2))

-- Eficiencia: O(log P + C log C)
-- Private
softwaresEnComun :: Organizador -> Persona -> Persona -> Set Checksum
softwaresEnComun o p1 p2 = intersection (programasDe o p1) (programasDe o p2)

-- Propósito: dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad de personas del organizador.
nroProgramasDePersona :: Organizador -> Persona -> Int
nroProgramasDePersona o p = sizeS (programasDe o p)

-- Eficiencia: O(1) 
fromJust :: Maybe a -> a
fromJust Nothing = error "Se rompio" 
fromJust (Just a) = a