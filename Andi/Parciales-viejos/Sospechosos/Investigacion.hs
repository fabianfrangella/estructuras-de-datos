import PriorityQueue
import Map1
import Persona

-- Los sospechosos de siempre
-- El objetivo de este examen es modelar una investigación criminal. No nos importará el hecho en cuestión, más sí la información
-- relevante acerca de las personas relacionadas al crimen. Para ello, definiremos un tipo abstracto llamado Investigacion, donde
-- damos por hecho que:
-- Existe un tipo abstracto llamado Persona, ya implementado, cuya interfaz se adjunta en el anexo de interfaces. Una persona
-- posee un nombre, y una lista de evidencia en su contra, además de la cantidad. Adicionalmente, su interfaz permite ingresar
-- una evidencia.
-- El tipo Evidencia es sinónimo de String, y se corresponde con el nombre de una evidencia.
-- El tipo Nombre es sinónimo de String, y se corresponde con el nombre de una persona.
-- Podemos suponer que dos personas son iguales si poseen el mismo nombre, y una persona es más sospechosa que otra si
-- posee más evidencia en su contra (lo que permite ordenarlos por la cantidad de evidencia).
-- En la investigación no existen dos personas ni evidencias con el mismo nombre.
-- Representación
-- Dicho esto, la representación que utilizaremos será la siguiente (que no es posible modificar ):

data Investigacion = ConsI (Map Nombre Persona)
                    (Map Evidencia [Nombre])
                    (PriorityQueue Persona)
                    Int deriving Show

-- Esta representación utiliza:
-- Un Map que relaciona a personas con su nombre.
-- Un Map que relaciona una lista de nombres con una evidencia en su contra que las mismas comparten.
-- Una PriorityQueue que posee a todas las personas de la investigación, y que permite obtenerlas 
-- de forma eficiente de mayor a menor en base a la cantidad de evidencia en su contra.
-- Un Int que indica la cantidad de evidencia diferente en la investigación.

-- i = ingresarPersonas ["andi", "pepe"] comenzarInvestigacion
andi = crearP "andi"
pepe = crearP "pepe"
m1 = assocM "andi" andi (assocM "pepe" pepe emptyM)
me1 = assocM "evidencia 1" [] emptyM
pqp = insertPQ andi (insertPQ pepe emptyPQ)
i = ConsI m1 me1 pqp 1

-- Ejercicios
-- Invariantes
-- a) Dar invariantes de representación válidos según la descripción de la estructura.
-- Puntaje: 0.75


-- Implementación
-- Implementar la siguiente interfaz de Investigacion, utilizando la representación y los costos dados y calculando los costos
-- de cada subtarea definida:

-- Propósito: crea una investigación sin datos.
-- Eficiencia: O(1)
-- Puntaje: 0.25
comenzarInvestigacion :: Investigacion
comenzarInvestigacion = ConsI emptyM emptyM emptyPQ 0

-- Propósito: devuelve la cantidad de eviencia ingresada.
-- Eficiencia: O(1)
-- Puntaje: 0.25
cantEvidenciaIngresada :: Investigacion -> Int
cantEvidenciaIngresada (ConsI mn me pqp e) = e

-- Propósito: devuelve la evidencia ingresada.
-- Eficiencia: O(N)
-- Puntaje: 0.5
evidenciaIngresada :: Investigacion -> [Evidencia]
evidenciaIngresada (ConsI mn me pqp e) = domM me

-- Propósito: devuelve los nombres de personas ingresadas.
-- Eficiencia: O(N)
-- Puntaje: 0.5
nombresIngresados :: Investigacion -> [Nombre]
nombresIngresados (ConsI mn me pqp e) = domM mn

-- Propósito: indica si la investigación posee al menos una persona con 5 evidencias en su contra.
-- Eficiencia: O(1)
-- Puntaje: 0.5
casoCerrado :: Investigacion -> Bool
casoCerrado (ConsI mn me pqp e) =
    cantEvidencia (maxPQ pqp) >= 5

-- Propósito: indica si esa persona tiene al menos una evidencia en su contra.
-- Nota: la persona puede no existir.
-- Eficiencia: O(log N)
-- Puntaje: 0.5
esSospechoso :: Nombre -> Investigacion -> Bool
esSospechoso n (ConsI mn me pqp e) =
    case lookupM n mn of
        Nothing -> False --error "No existe una persona con ese nombre"
        Just p -> cantEvidencia p > 0

-- Propósito: devuelve a las personas con cero evidencia en su contra.
-- Eficiencia: O(N log N)
-- Puntaje: 0.5
posiblesInocentes :: Investigacion -> [Persona]
posiblesInocentes (ConsI mn me pqp e) = posiblesInocentes' mn (domM mn)

posiblesInocentes' :: Map Nombre Persona -> [Nombre] -> [Persona]
posiblesInocentes' mn [] = []
posiblesInocentes' mn (n:ns) =
    let p = fromJust (lookupM n mn) in
        if cantEvidencia p == 0
            then p : posiblesInocentes' mn ns
            else posiblesInocentes' mn ns

-- Propósito: ingresa a personas nuevas a la investigación (mediante sus nombres), sin evidencia en su contra.
-- Precondición: las personas no existen en la investigación y no hay nombres repetidos.
-- Eficiencia: O(N log N)
-- Puntaje: 1.75
ingresarPersonas :: [Nombre] -> Investigacion -> Investigacion
ingresarPersonas ns (ConsI mn me pqp e) = 
    ConsI (agregarAMap ns mn) me (agregarAPq ns pqp) e

-- Eficiencia: O(N log N)
agregarAMap :: [Nombre] -> Map Nombre Persona -> Map Nombre Persona
agregarAMap [] mn = mn
agregarAMap (n:ns) mn = 
    assocM n (crearP n) (agregarAMap ns mn)

-- Eficiencia: O(N log N)
agregarAPq :: [Nombre] -> PriorityQueue Persona -> PriorityQueue Persona
agregarAPq [] pqp = pqp
agregarAPq (n:ns) pqp = insertPQ (crearP n) (agregarAPq ns pqp)

-- Propósito: asocia una evidencia a una persona dada.
-- Precondición: la evidencia aún no está asociada a esa persona.
-- Nota: la persona y la evidencia existen, pero NO están asociadas.
-- Eficiencia: O(N log N)
-- Puntaje: 1.75
ingresarEvidencia :: Evidencia -> Nombre -> Investigacion -> Investigacion
ingresarEvidencia e n (ConsI mn me pqp ce) =
    let p = agregarEvidencia e (fromJust (lookupM n mn)) in
        ConsI (assocM n p mn) (agregarEvidenciaAMap e n me) (updatePQP p n pqp) ce

-- Eficiencia: O(log N)
agregarEvidenciaAMap :: Evidencia -> Nombre -> Map Evidencia [Nombre] -> Map Evidencia [Nombre]
agregarEvidenciaAMap e n me = assocM e (n : fromJust (lookupM e me)) me

-- Eficiencia: O(N log N)
updatePQP :: Persona -> Nombre -> PriorityQueue Persona -> PriorityQueue Persona
updatePQP p n pqp = insertPQ p (delete n pqp)

-- Eficiencia: O(N log N)
delete :: Nombre -> PriorityQueue Persona -> PriorityQueue Persona
delete n pqp =
    if n == nombre (maxPQ pqp)
        then deleteMaxPQ pqp
        else insertPQ (maxPQ pqp) (delete n (deleteMaxPQ pqp))

-- Eficiencia: O(1) 
fromJust :: Maybe a -> a
fromJust Nothing = error "Se rompio" 
fromJust (Just a) = a

