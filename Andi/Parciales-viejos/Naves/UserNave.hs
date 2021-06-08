import Nave

-- Usuario
-- Implementar las siguientes funciones como usuario del tipo Nave, 
-- indicando la eficiencia obtenida para cada operación:

-- Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
-- Eficiencia: O(N * log N)
sectores :: Nave -> Set SectorId
sectores n =
    sectores' (tripulantesN n) n

-- Eficiencia: O(N * log N)
sectores' :: [Tripulante] -> Nave -> Set SectorId
sectores' [] n = emptyS
sectores' (t:ts) n =
    unionS (sectoresAsignados (nombre t) n) (sectores' ts n)

-- Propósito: Devuelve los tripulantes que no poseen sectores asignados.
-- Eficiencia: O(N * log N)
sinSectoresAsignados :: Nave ->[Tripulante]
sinSectoresAsignados n =
    sinSectoresAsignados' (tripulantesN n) n

-- Eficiencia: O(N) 
sinSectoresAsignados' :: [Tripulante] -> Nave -> [Tripulante]
sinSectoresAsignados' [] n = []
sinSectoresAsignados' (t:ts) n =
    if sizeS (sectoresT t) == 0
        then t : sinSectoresAsignados' ts n
        else sinSectoresAsignados' ts n 

-- Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
barriles :: Nave -> [Barril]
barriles n = barriles' (setToList (sectores) n) n

-- Eficiencia: O(N2 * log N) ?
barriles' :: [SectorId] -> Nave -> [Barril]
barriles' [] = []
barriles' (s:ss) = 
    soloBarriles ((snd (datosDeSector s n)) ++ barriles' ss n)

-- Eficiencia: O(N)
soloBarriles :: [Componente] -> [Barril]
soloBarriles [] = []
soloBarriles (c:cs) =
    if esBarril c
        then c : soloBarriles' cs
        else soloBarriles' cs

-- Eficiencia: O(1)
esBarril :: Componente -> Bool
esBarril Barril = True
esBarril _ = False    

-- Bonus

data Sector = S SectorId [Componente] (Set Nombre)
