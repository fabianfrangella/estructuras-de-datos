import EscuelaDeMagia
import Set
import Mago

-- Usuario
-- Implementar las siguientes funciones como usuario del tipo EscuelaDeMagia:
-- Propósito: Retorna todos los hechizos aprendidos por los magos.
-- Eficiencia: O(M ∗ (log M + H log H))
hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
hechizosAprendidos esc = hechizos' (magos esc) esc

-- Eficiencia: O(M ∗ (log M + H log H))
hechizos' :: [Nombre] -> EscuelaDeMagia -> Set Hechizo
hechizos' [] esc = emptyS
hechizos' (x:xs) esc =
    unionS (hechizosDe x esc) (hechizos' xs esc)
      
-- Propósito: Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.
-- Eficiencia: O(log M)
hayUnExperto :: EscuelaDeMagia -> Bool
hayUnExperto esc = esExperto (fst (egresarUno esc)) esc

esExperto :: Mago -> EscuelaDeMagia -> Bool
esExperto m esc = leFaltanAprender (nombre m) esc == 0

-- Propósito: Devuelve un par con la lista de magos que saben todos los hechizos dados por la escuela 
--y la escuela sin dichos magos.
-- Eficiencia: O(M log M)
-- TODO: Ta medio roto
egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
egresarExpertos esc = 
    if hayUnExperto esc
        then 
            let par = egresarUno esc in
                (fst par : fst (egresarExpertos (snd par)), snd par)
        else ([], esc)