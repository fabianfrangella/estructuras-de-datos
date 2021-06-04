import Autores
import Set


-- Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personas
-- programaron juntas.
-- Eficiencia: O(log P + C log C)
programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
programasEnComun p1 p2 o = intersection (programasDe o p1) (programasDe o p2)

-- Propósito: denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.
-- Eficiencia: O(log P + C)
esUnGranHacker :: Organizador -> Persona -> Bool
esUnGranHacker o p = nroProgramasDePersona o p == length (todosLosProgramas o)