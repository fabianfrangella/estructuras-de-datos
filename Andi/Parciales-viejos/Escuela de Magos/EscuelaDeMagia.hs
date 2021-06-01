module EscuelaDeMagia (
	EscuelaDeMagia,
    fundarEscuela,
    estaVacia,
    registrar,
    magos,
    hechizosDe,
    leFaltanAprender,
    egresarUno,
    ensenar,
    esc
 ) where

import Set
import PriorityQueue
import Map1
import Mago

-- Escuela de Magia
-- El objetivo de este examen es modelar una escuela de magos. Para ello, definiremos un tipo abstracto llamado EscuelaDeMagia.
-- Y damos por hecho que:
-- Existe un tipo abstracto llamado Mago, ya implementado, cuya interfaz se adjunta en el anexo de interfaces. Un mago puede
-- aprende hechizos, e informarnos su nombre y qué hechizos conoce.
-- El tipo Hechizo es sinónimo de String, y se corresponde con el nombre de un hechizo.
-- El tipo Nombre es sinónimo de String, y se corresponde con el nombre de un mago.
-- Podemos suponer que dos magos son iguales si poseen el mismo nombre, y un mago es más poderoso que otro si conoce más
-- hechizos (lo que permite ordenarlos por la cantidad de hechizos que saben).
-- En la escuela no existen dos magos con el mismo nombre.
-- Representación
-- Dicho esto, la representación que utilizaremos será la siguiente (que no es posible modificar ):

data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago) deriving Show

esc = (registrar "juan" (registrar "pepe" fundarEscuela))

-- Esta representación utiliza:
-- Un Set de hechizos que la escuela ha enseñado a lo largo de su historia.
-- Un Map que asocia magos con su respectivo nombre.
-- Una estructura llamada PriorityQueue que posee a todos los magos de la escuela y permite obtenerlos de forma eficiente
-- de mayor a menor en base a la cantidad de hechizos que saben.

-- Ejercicios
-- Invariantes
-- a) Dar invariantes de representación válidos según la descripción de la estructura.
-- Implementación
-- Implementar la siguiente interfaz de EscuelaDeMagia, utilizando la representación y los costos dados, calculando los costos
-- de cada subtarea, y siendo M la cantidad de magos y H la cantidad de hechizos:

-- Propósito: Devuelve una escuela vacía.
-- Eficiencia: O(1)
fundarEscuela :: EscuelaDeMagia
fundarEscuela = EDM emptyS emptyM emptyPQ

-- Propósito: Indica si la escuela está vacía.
-- Eficiencia: O(1)
estaVacia :: EscuelaDeMagia -> Bool
estaVacia (EDM h mgs mgspq) = isEmptyPQ mgspq

-- Propósito: Incorpora un mago a la escuela (si ya existe no hace nada).
-- Eficiencia: O(log M)
registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
registrar n (EDM h mgs mgspq) = 
    let mago = crearM n in
        EDM h (assocM n mago mgs) (insertPQ mago mgspq)

-- Propósito: Devuelve los nombres de los magos registrados en la escuela.
-- Eficiencia: O(M)
magos :: EscuelaDeMagia -> [Nombre]
magos (EDM h mgs mgspq) = domM mgs

-- Propósito: Devuelve los hechizos que conoce un mago dado.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(log M)
hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
hechizosDe n (EDM h mgs mgspq) =
    let mago = lookupM n mgs in
        hechizos (fromJust mago)

-- Propósito: Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(log M)
leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
leFaltanAprender n esc = sizeS (todosLosHechizos esc) - sizeS (hechizosDe n esc) 

-- Propósito: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
-- Precondición: Hay al menos un mago.
-- Eficiencia: O(log M)
egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
egresarUno (EDM h mgs mgspq) = 
    let mago = maxPQ mgspq in
        (mago, EDM h (deleteM (nombre mago) mgs) (deleteMaxPQ mgspq)) 

-- Propósito: Enseña un hechizo a un mago existente, y si el hechizo no existe en la escuela es incorporado a la misma.
-- Nota: No importa si el mago ya conoce el hechizo dado.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(M log M + log H)
ensenar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
ensenar h n (EDM hs mgs mgspq) = 
    let _mago = mago n mgs in
        let magoConNuevoHechizo = aprender h _mago in
            EDM (addS h hs) (assocM n magoConNuevoHechizo mgs) (insertPQ magoConNuevoHechizo mgspq)


--------------
-- Privates --
--------------

-- Eficiencia: O(1) 
fromJust :: Maybe a -> a
fromJust Nothing = error "Se rompio" 
fromJust (Just a) = a

-- Precondición: Existe el mago
-- Propósito: Devuelve el mago con dicho nombre
-- Eficiencia: O(log M)
mago :: Nombre -> Map Nombre Mago -> Mago
mago n magosM = fromJust (lookupM n magosM)

-- Eficiencia: O(1)
todosLosHechizos :: EscuelaDeMagia -> Set Hechizo
todosLosHechizos (EDM h mgs mgspq) = h