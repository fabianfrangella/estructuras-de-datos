import Investigacion

-- Propósito: Comienza una investigación con una lista de nombres sin evidencia.
-- Puntaje: 0.5
-- Eficiencia: O(N log N)
comenzarConPersonas :: [Nombre] -> Investigacion
comenzarConPersonas ns =
    ingresarPersonas ns comenzarInvestigacion

-- Propósito: Indica si las personas en la investigación son todas inocentes.
-- Puntaje: 0.75
-- Eficiencia: O(N log N)
todosInocentes :: Investigacion -> Bool
todosInocentes i =
    length (posiblesInocentes i) == length (nombresIngresados i)

-- Propósito: Indica si la evidencia en la lista es suficiente para cerrar el caso.
-- Puntaje: 1
terminaCerrado :: [(Evidencia, Nombre)] -> Investigacion -> Bool
terminaCerrado [] i = casoCerrado i
terminaCerrado (x:xs) i =
    terminaCerrado xs (ingresarEvidencia (fst x) (snd x) i) 
