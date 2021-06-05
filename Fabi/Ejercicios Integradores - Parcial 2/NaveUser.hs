{-
Usuario
Implementar las siguientes funciones como usuario del tipo Nave, indicando la eficiencia obtenida para cada operación:

-}
import Nave

-- Eficiencia O (N . N Log N)
--Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
sectores :: Nave -> Set SectorId
sectores n = sectoresT (tripulantesN n) n

sectoresT :: [Tripulante] -> Nave -> Set SectorId
sectoresT [] _ = []
sectoresT (x:xs) = (sectoresAsignados (nombre x) n) unionS (sectoresT xs n)

-- Eficiencia O(n.2)
--Propósito: Devuelve los tripulantes que no poseen sectores asignados.
sinSectoresAsignados :: Nave -> [Tripulante]
sinSectoresAsignados n = sinSectoresAsignadosT (tripulantesN n) n

-- Eficiencia O (n.2)
sinSectoresAsignadosT :: [Tripulante] -> Nave -> [Tripulante]
sinSectoresAsignadosT [] _ = []
sinSectoresAsignadosT(x:xs) n = 
    if tieneSectorAsignado x
        then sinSectoresAsignadosT xs n
        else x ++ sinSectoresAsignadosT xs n

-- Eficiencia O (n)
tieneSectorAsignado :: Tripulante -> Bool
tieneSectorAsignado t = 
    length (setToList (sectoresT t)) > 0

-- Eficiencia O (n.2)
--Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
barriles :: Nave -> [Barril]
barriles n = barrilesS (setToList (sectores n))

-- Eficiencia O (n.2)
barrilesSS :: [Sector] -> [Componente]
barrilesSS [] = []
barrilesSS (x:xs) = barrilesS x ++ barrilesSS xs

-- Eficiencia O (n)
barrilesS :: Sector -> [Componente]
barrilesS s = barrilesC componenteS s

-- Eficiencia O (n)
barrilesC :: [Componente] -> [Componente]
barrilesC [] = []
barrilesC (x:xs) = 
    if esAlmacen x 
        then barrilesA x
        else barrilesC xs

-- Eficiencia O (1)
esAlmacen :: Componente -> Bool
esAlmacen Almacen _ = True
esAlmacen _ = False 
 

        