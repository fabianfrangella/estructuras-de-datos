--1. sumatoria :: [Int] -> Int
--Dada una lista de enteros devuelve la suma de todos sus elementos
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--2. longitud :: [a] -> Int
--Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
--de elementos que posee.
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
--3. sucesores :: [Int] -> [Int]
--Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = (x + 1) : sucesores xs
--4. conjuncion :: [Bool] -> Bool
--Dada una lista de booleanos devuelve T rue si todos sus elementos son T rue.
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x:xs) = x && conjuncion xs
--5. disyuncion :: [Bool] -> Bool
--Dada una lista de booleanos devuelve T rue si alguno de sus elementos es T rue.
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs
--6. aplanar :: [[a]] -> [a]
--Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs
--7. pertenece :: Eq a => a -> [a] -> Bool
--Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual
--a e.
pertenece :: Eq a => a -> [a] -> Bool
pertenece x [] = False
pertenece y (x:xs) = y == x || pertenece y xs
--8. apariciones :: Eq a => a -> [a] -> Int
--Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a -> [a] -> Int
apariciones y [] = 0
apariciones y (x:xs) = if x == y then 1 + apariciones y xs else 0 + apariciones y xs

--9. losMenoresA :: Int -> [Int] -> [Int]
--Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA y [] = []
losMenoresA y (x:xs) = if x < y then x : losMenoresA y xs else losMenoresA y xs
--10. lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
--Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
--de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA y [] = []
lasDeLongitudMayorA y (x:xs) = if longitud x > y then x : lasDeLongitudMayorA y xs else lasDeLongitudMayorA y xs
--11. agregarAlFinal :: [a] -> a -> [a]
--Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
--lista.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] y = [y]
agregarAlFinal (x:xs) y = x : agregarAlFinal xs y

--12. concatenar :: [a] -> [a] -> [a]
--Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
--elementos de la segunda a continuación. Definida en Haskell como ++.
concatenar :: [a] -> [a] -> [a]
concatenar [] [] = []
concatenar xs [] = xs
concatenar [] xs = xs
concatenar (x:xs) ys = x : concatenar xs ys

--13. reversa :: [a] -> [a]
--Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
--en Haskell como reverse.
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x
--14. zipMaximos :: [Int] -> [Int] -> [Int]
--Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
--máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
--las listas no necesariamente tienen la misma longitud.
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] [] = []
zipMaximos [x] [] = [x]
zipMaximos [] [x] = [x]
zipMaximos (x:xs) (y:ys) = max x y : zipMaximos xs ys
--15. elMinimo :: Ord a => [a] -> a
--Dada una lista devuelve el mínimo
elMinimo :: Ord a => [a] -> a
elMinimo [] = error "Lista vacia"
elMinimo [x] = x
elMinimo (x:xs) = min x (elMinimo xs)

--2. Recursión sobre números
--Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique
--lo contrario:
--1. factorial :: Int -> Int
--Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
--llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)
--2. cuentaRegresiva :: Int -> [Int]
--Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
--n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva x = x : cuentaRegresiva (x - 1)
--3. repetir :: Int -> a -> [a]
--Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e = e : repetir (n - 1) e
--4. losPrimeros :: Int -> [a] -> [a]
--Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
--Si la lista es vacía, devuelve una lista vacía.
losPrimeros :: Int -> [a] -> [a]
losPrimeros n [] = []
losPrimeros 0 [] = []
losPrimeros 1 (x:xs) = [x]
losPrimeros n (x:xs) = x : losPrimeros (n - 1) xs
--5. sinLosPrimeros :: Int -> [a] -> [a]
--Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
--recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs = xs
sinLosPrimeros n [] = []
sinLosPrimeros n (x:xs) =  if n > 0 then sinLosPrimeros (n - 1) xs else x : sinLosPrimeros (n - 1) xs

--3. Registros
--1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
--siguientes funciones:
data Persona = P String Int deriving Show
juancito = P "juan" 10
pedro = P "pedro" 20
mario = P "mario" 30

--Devuelve la edad de una persona (Practica 1)
edad :: Persona -> Int
edad (P name age) = age

personas = juancito : pedro : mario : []
--mayoresA :: Int -> [Persona] -> [Persona]
--Dados una edad y una lista de personas devuelve todas las personas que son mayores
--a esa edad.
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (x:xs) = if edad x > n then x : mayoresA n xs else mayoresA n xs

--promedioEdad :: [Persona] -> Int
--Dada una lista de personas devuelve el promedio de edad entre esas personas. Precon-
--dición: la lista al menos posee una persona.

sumatoriaEdad :: [Persona] -> Int
sumatoriaEdad [] = 0
sumatoriaEdad (x:xs) = edad x + sumatoriaEdad xs 

promedioEdad :: [Persona] -> Int
promedioEdad xs = div (sumatoriaEdad xs) (longitud xs)
--elMasViejo :: [Persona] -> Persona
--Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la
--lista al menos posee una persona.
elMasViejo :: [Persona] -> Persona
elMasViejo [x] = x
elMasViejo (x:xs) = if edad x > edad (elMasViejo xs) then x else elMasViejo xs
--2. Modificaremos la representación de Entreador y Pokemon de la práctica anterior de la si-
--guiente manera:
--
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int deriving Show
data Entrenador = ConsEntrenador String [Pokemon] deriving Show

poke1 = ConsPokemon Agua 30
poke2 = ConsPokemon Agua 50
poke3 = ConsPokemon Planta 40
poke4 = ConsPokemon Fuego 75

pepe = ConsEntrenador "pepe" [poke1, poke2, poke3, poke4]
pepin = ConsEntrenador "pepin" [poke1, poke2, poke3]

--Practica 1
tipoSuperaA :: TipoDePokemon -> TipoDePokemon -> Bool
tipoSuperaA Agua Fuego = True
tipoSuperaA Fuego Planta = True
tipoSuperaA Planta Agua = True
tipoSuperaA _ _ = False

unoSiEsDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Int
unoSiEsDelMismoTipo Fuego Fuego = 1
unoSiEsDelMismoTipo Agua Agua = 1
unoSiEsDelMismoTipo Planta Planta = 1
unoSiEsDelMismoTipo _ _ = 0

tipoPokemon :: Pokemon -> TipoDePokemon
tipoPokemon (ConsPokemon t e) = t

superaA :: Pokemon -> Pokemon -> Bool
superaA x y = tipoSuperaA (tipoPokemon x) (tipoPokemon y)
--Como puede observarse, ahora los entrenadores tienen una cantidad de Pokemon arbitraria.
--Definir en base a esa representación las siguientes funciones:
--cantPokemones :: Entrenador -> Int
--Devuelve la cantidad de pokémon que posee el entrenador.
cantPokemones :: Entrenador -> Int
cantPokemones (ConsEntrenador n pokemones) = longitud pokemones
--cantPokemonesDe :: TipoDePokemon -> Entrenador -> Int
--Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.

cantidadPokemonesDeTipo :: TipoDePokemon -> [Pokemon] -> Int
cantidadPokemonesDeTipo t [] = 0
cantidadPokemonesDeTipo t (x:xs) = (unoSiEsDelMismoTipo t (tipoPokemon x)) + (cantidadPokemonesDeTipo t xs)

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (ConsEntrenador n pokemones) = pokemones

cantPokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonesDe t e = cantidadPokemonesDeTipo t (pokemonesDe e)

--losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
--Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
--a los Pokemon del segundo entrenador.

--losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
--losQueLeGanan t e
--esMaestroPokemon :: Entrenador -> Bool
--Dado un entrenador, devuelve True si posee al menos un pokémon de cada tipo posible.
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon e = (cantPokemonesDe Fuego e) > 0 && (cantPokemonesDe Agua e) > 0 && (cantPokemonesDe Planta e) > 0


--3. El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
--de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
--una lista de personas con diferente rol. La definición es la siguiente:
data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = ConsProyecto String deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = ConsEmpresa [Rol] deriving Show

erp = ConsProyecto "Sistema de ERP"
trader = ConsProyecto "Trader"
andi = Developer Junior trader
fabi = Developer Senior trader
juan = Developer Senior trader
gaston = Management Senior trader
gaby = Developer SemiSenior erp
maniglie = ConsEmpresa [andi, fabi, juan, gaston, gaby]

--Definir las siguientes funciones sobre el tipo Empresa:
--proyectos :: Empresa -> [Proyecto]
--Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repe-
--tidos.

proyectos :: Empresa -> [Proyecto]
proyectos e = proyectosSinDuplicados (todosLosProyectos (roles e))

roles :: Empresa -> [Rol]
roles (ConsEmpresa r) = r

todosLosProyectos :: [Rol] -> [Proyecto]
todosLosProyectos [] = []
todosLosProyectos (x:xs) = proyectoDeRol x : todosLosProyectos xs

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer _ p) = p
proyectoDeRol (Management _ p) = p

proyectosSinDuplicados :: [Proyecto] -> [Proyecto]
proyectosSinDuplicados [] = []
proyectosSinDuplicados [x] = [x]
proyectosSinDuplicados (x:xs) = if nombreProyecto x == nombreProyecto (head(xs))
                                    then proyectosSinDuplicados xs
                                    else x : proyectosSinDuplicados xs

nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto nombre) = nombre

--losDevSenior :: Empresa -> [Proyecto] -> Int
--Dada una empresa indica la cantidad de desarrolladores senior que posee.
--losDevSenior :: Empresa -> Int
losDevSenior :: Empresa -> Int
losDevSenior e = seniorEnRoles (roles e)

seniorEnRoles :: [Rol] -> Int
seniorEnRoles [] = 0
seniorEnRoles (x:xs) = unoSiEsSenior x + seniorEnRoles xs

unoSiEsSenior :: Rol -> Int
unoSiEsSenior (Developer Senior _ ) = 1
unoSiEsSenior (Management Senior _ ) = 1
unoSiEsSenior (Developer _ _ ) = 0
unoSiEsSenior (Management _ _ ) = 0

--cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
--Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.

--Asumo que no puede haber dos proyectos con el mismo nombre para no usar Eq en Proyecto
esElMismoProyecto :: Proyecto -> Proyecto -> Bool
esElMismoProyecto p1 p2 = nombreProyecto p1 == nombreProyecto p2

trabajaEnProyecto :: Proyecto -> Rol -> Bool
trabajaEnProyecto p r = nombreProyecto (proyectoDeRol r) == (nombreProyecto p)

unoSiTrabajaEnProyecto :: Proyecto -> Rol -> Int
unoSiTrabajaEnProyecto p r = if trabajaEnProyecto p r then 1 else 0

cantQueTrabajanEnUnProyecto :: Proyecto -> [Rol] -> Int
cantQueTrabajanEnUnProyecto p [] = 0
cantQueTrabajanEnUnProyecto p (x:xs) = unoSiTrabajaEnProyecto p x + cantQueTrabajanEnUnProyecto p xs

cantQueTrabajanEnUnProyectoDeEmpresa :: Proyecto -> Empresa -> Int
cantQueTrabajanEnUnProyectoDeEmpresa p e = cantQueTrabajanEnUnProyecto p (roles e)

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn [] e = 0
cantQueTrabajanEn (x:xs) e = cantQueTrabajanEnUnProyectoDeEmpresa x e + cantQueTrabajanEn xs e

--asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
--Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
--cantidad de personas involucradas.
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto e = totalesPorProyecto (proyectos e) (roles e)

totalesPorProyecto :: [Proyecto] -> [Rol] -> [(Proyecto, Int)]
totalesPorProyecto [] [] = []
totalesPorProyecto [] _ = []
totalesPorProyecto _ [] = []
totalesPorProyecto (x:xs) ys = (x, cantQueTrabajanEnUnProyecto x ys) : totalesPorProyecto xs ys