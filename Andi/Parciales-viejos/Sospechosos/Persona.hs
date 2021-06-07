module Persona (
    Persona,
    Nombre,
    Evidencia,
    crearP,
    nombre,
    evidencia,
    cantEvidencia,
    agregarEvidencia
) where

type Nombre = String
type Evidencia = String

data Persona = P Nombre [Evidencia] Int deriving Show

instance Eq Persona where
	(==) (P n1 evs1 c1) (P n2 evs2 c2) = n1 == n2

instance Ord Persona where
	(<=) (P n1 evs1 c1) (P n2 evs2 c2) = c1 <= c2

crearP :: Nombre -> Persona
crearP n = P n [] 0

nombre :: Persona -> Nombre
nombre (P n evs c) = n

evidencia :: Persona -> [Evidencia]
evidencia (P n evs c) = evs

cantEvidencia :: Persona -> Int
cantEvidencia (P n evs c) = c

agregarEvidencia :: Evidencia -> Persona -> Persona
agregarEvidencia e (P n evs c) = P n (e:evs) (c + 1)
