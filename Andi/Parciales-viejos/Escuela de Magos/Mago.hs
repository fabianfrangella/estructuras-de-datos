
module Mago (
	Mago,
    Hechizo,
    Nombre,
    crearM,
    nombre,
    aprender,
    hechizos
 ) where

import Set

type Hechizo = String
type Nombre = String

data Mago = M Nombre (Set Hechizo) deriving Show

instance Eq Mago where
	(==) (M n1 hs1) (M n2 hs2) = n1 == n2

instance Ord Mago where
	(<=) (M n1 hs1) (M n2 hs2) = (sizeS hs1) <= (sizeS hs2)

crearM :: Nombre -> Mago
crearM n = M n emptyS

nombre :: Mago -> Nombre
nombre (M n hs) = n

aprender :: Hechizo -> Mago -> Mago
aprender h (M n hs) = M n (addS h hs)

hechizos :: Mago -> Set Hechizo
hechizos (M n hs) = hs