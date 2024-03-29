{- 
1. Recursión sobre listas
Defina las siguientes funciones utilizando recursión estructural sobre listas, salvo que se indique
lo contrario:
1. sumatoria :: [Int] -> Int
Dada una lista de enteros devuelve la suma de todos sus elementos.
2. longitud :: [a] -> Int
Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
de elementos que posee.
3. sucesores :: [Int] -> [Int]
Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
4. conjuncion :: [Bool] -> Bool
Dada una lista de booleanos devuelve T rue si todos sus elementos son T rue.
5. disyuncion :: [Bool] -> Bool
Dada una lista de booleanos devuelve T rue si alguno de sus elementos es T rue.
6. aplanar :: [[a]] -> [a]
Dada una lista de listas, devuelve una única lista con todos sus elementos.
7. pertenece :: Eq a => a -> [a] -> Bool
Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual
a e.
8. apariciones :: Eq a => a -> [a] -> Int
Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
9. losMenoresA :: Int -> [Int] -> [Int]
Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
10. lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
de n elementos.
11. agregarAlFinal :: [a] -> a -> [a]
Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
lista.
12. concatenar :: [a] -> [a] -> [a]
Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
elementos de la segunda a continuación. Definida en Haskell como ++.
13. reversa :: [a] -> [a]
Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
en Haskell como reverse.
14. zipMaximos :: [Int] -> [Int] -> [Int]
Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
las listas no necesariamente tienen la misma longitud.
15. elMinimo :: Ord a => [a] -> a
Dada una lista devuelve el mínimo
-}

-- Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria(x:xs) = x + sumatoria xs

-- Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad de elementos que posee.
longitud :: [a] -> Int
longitud [] = 0
longitud(x:xs) = 1 + longitud xs

-- Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x: xs) = succ x : sucesores xs

-- Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x:xs) = x && conjuncion xs

-- Dada una lista de booleanos devuelve True si alguno de sus elementos es T rue.
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs

-- Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x: xs) = x ++ aplanar xs

-- Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = x == e || pertenece e xs

-- Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) = if e == x then 1 + apariciones e xs else apariciones e xs

--Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n [] = []
losMenoresA n (x:xs) = if n > x then x : losMenoresA n xs else losMenoresA n xs

--Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (x:xs) = if longitud x > n then x : lasDeLongitudMayorA n xs else lasDeLongitudMayorA n xs

--Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] n = [n]
agregarAlFinal (x:xs) n = x : agregarAlFinal xs n

--Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los elementos de la segunda a continuación. Definida en Haskell como ++.
concatenar :: [a] -> [a] -> [a]
concatenar [] xs = xs
concatenar (x:xs1) xs = x : (concatenar xs1 xs)

--Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida en Haskell como reverse.
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = [x] ++ reversa xs

--Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
--máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
--las listas no necesariamente tienen la misma longitud.
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] [] = []
zipMaximos (x:xs) [] = []
zipMaximos [] (x:xs) = []
zipMaximos (x1: xs1) (x2: xs2) = if x1 > x2 then x1 : zipMaximos xs1 xs2 else x2 : zipMaximos xs1 xs2

--Dada una lista devuelve el mínimo
elMinimo :: Ord a => [a] -> a
elMinimo [] = error "No se puede pedir el minimo de una lista vacia"
elMinimo [x] = x
elMinimo (x:xs) = min x (elMinimo xs)
{-
Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique
lo contrario:
1. factorial :: Int -> Int
Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
2. cuentaRegresiva :: Int -> [Int]
Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
3. repetir :: Int -> a -> [a]
Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
4. losPrimeros :: Int -> [a] -> [a]
Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
Si la lista es vacía, devuelve una lista vacía.
5. sinLosPrimeros :: Int -> [a] -> [a]
Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
recibida. Si n es cero, devuelve la lista completa.
-}

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

--Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir 1 e = [e]
repetir n e = e : repetir (n-1) e

--Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs. Si la lista es vacía, devuelve una lista vacía.

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 xs = []
losPrimeros 1 (x:xs) = [x]
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

--Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros _ [] = []
sinLosPrimeros n [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros(n-1) xs

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar (x:xs) = ponerOrdenado x (ordenar xs)

ponerOrdenado :: Int -> [Int] -> [Int]
ponerOrdenado n [] = [n]
ponerOrdenado n (x:xs) = if n > x then x : ponerOrdenado n xs else n : x : xs  

{-
Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
siguientes funciones:
mayoresA :: Int -> [Persona] -> [Persona]
Dados una edad y una lista de personas devuelve todas las personas que son mayores
a esa edad.
promedioEdad :: [Persona] -> Int

Dada una lista de personas devuelve el promedio de edad entre esas personas. Precon-
dición: la lista al menos posee una persona.

elMasViejo :: [Persona] -> Persona
Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la
lista al menos posee una persona.
-}

data Persona = Pers String Int deriving (Show)

carlos = Pers "Carlos" 18
jorge = Pers "Jorge" 24
jose = Pers "Jose" 52
maria = Pers "Maria" 16

personas :: [Persona]
personas = [carlos,jorge,maria]

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n (x:xs) = if esMayorDe n x then x : mayoresA n xs else mayoresA n xs

esMayorDe :: Int -> Persona -> Bool
esMayorDe n p = edad p > n

edad :: Persona -> Int
edad (Pers _ e) = e 

promedioEdad :: [Persona] -> Int
promedioEdad xs = div (sumatoria (edades xs)) (length xs)

edades :: [Persona] -> [Int]
edades [] = []
edades (x:xs) = edad x : edades xs

esMayorQue :: Persona -> Persona -> Bool
esMayorQue x y = edad x > edad y

elMayor :: Persona -> Persona -> Persona
elMayor a b = if esMayorQue a b then a else b

elMasViejo :: [Persona] -> Persona
elMasViejo [a] = a
elMasViejo [a,b] = elMayor a b
elMasViejo (x:xs) = elMasViejo (elMayor x (head xs) : tail xs)

{-
Modificaremos la representación de Entreador y Pokemon de la práctica anterior de la si-
guiente manera:

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]
Como puede observarse, ahora los entrenadores tienen una cantidad de Pokemon arbitraria.
Definir en base a esa representación las siguientes funciones:
cantPokemones :: Entrenador -> Int
Devuelve la cantidad de pokémon que posee el entrenador.
cantPokemonesDe :: TipoDePokemon -> Entrenador -> Int
Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
a los Pokemon del segundo entrenador.
esMaestroPokemon :: Entrenador -> Bool
Dado un entrenador, devuelve True si posee al menos un pokémon de cada tipo posible.
-}

data TipoDePokemon = Agua | Fuego | Planta deriving (Show)
data Pokemon = ConsPokemon TipoDePokemon Int deriving (Show)
data Entrenador = ConsEntrenador String [Pokemon] deriving (Show)

charizard = ConsPokemon Fuego 80
bulbasour = ConsPokemon Agua 60
plantita = ConsPokemon Planta 40
aguita = ConsPokemon Agua 20

pokemones = [bulbasour, plantita, aguita]
ash = ConsEntrenador "Ash Ketchup" [charizard, plantita, bulbasour, aguita, plantita]

brook = ConsEntrenador "Ash Ketchup" [aguita, bulbasour]


cantPokemones :: Entrenador -> Int
cantPokemones (ConsEntrenador _ xs) = length xs

cantPokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonesDe t (ConsEntrenador _ xs) = cantPokemonesDeTipo t xs

sonElMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonElMismoTipo Agua Agua = True
sonElMismoTipo Planta Planta = True
sonElMismoTipo Fuego Fuego = True
sonElMismoTipo x y = False

unoSiSonElMismoTipo :: TipoDePokemon -> TipoDePokemon -> Int
unoSiSonElMismoTipo Agua Agua = 1
unoSiSonElMismoTipo Planta Planta = 1
unoSiSonElMismoTipo Fuego Fuego = 1
unoSiSonElMismoTipo x y = 0

cantPokemonesDeTipo :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonesDeTipo t [] = 0
cantPokemonesDeTipo t xs = unoSiSonElMismoTipo t (tipoDe (head xs)) + cantPokemonesDeTipo t (tail xs)

tipoDe :: Pokemon -> TipoDePokemon
tipoDe (ConsPokemon t _) = t

--Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían a los Pokemon del segundo entrenador.
--losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int


--Dado un entrenador, devuelve True si posee al menos un pokémon de cada tipo posible.
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ xs) = hayUnoDeCadaTipo xs

hayUnoDeCadaTipo :: [Pokemon] -> Bool
hayUnoDeCadaTipo [] = False
hayUnoDeCadaTipo [a] = False
hayUnoDeCadaTipo [a,b] = False
hayUnoDeCadaTipo xs = existeTipoEn Fuego xs && existeTipoEn Agua xs && existeTipoEn Planta xs

tiposDe :: [Pokemon] -> [TipoDePokemon]
tiposDe [] = []
tiposDe (x: xs) = tipoDe x : tiposDe xs

existeTipoEn :: TipoDePokemon -> [Pokemon] -> Bool
existeTipoEn t [] = False
existeTipoEn t [a] = sonElMismoTipo t (tipoDe a)
existeTipoEn t (x:xs) = sonElMismoTipo t (tipoDe x) || existeTipoEn t xs

--Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían a los Pokemon del segundo entrenador.
losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan t e1 e2 = cantidadQueLeGananATodos (losPokemonesDeTipoDe t e1) (pokemonesDe e2)

cantidadQueLeGananATodos :: [Pokemon] -> [Pokemon] -> Int
cantidadQueLeGananATodos [] _ = 0
cantidadQueLeGananATodos xs [] = longitud xs
cantidadQueLeGananATodos (x1:xs1) (x2:xs2) = if leGanaATodos x1 (x2:xs2) then 1 + cantidadQueLeGananATodos xs1 (x2:xs2) else cantidadQueLeGananATodos xs1 (x2:xs2)

leGanaATodos :: Pokemon -> [Pokemon] -> Bool
leGanaATodos p [] = True
leGanaATodos p (x:xs) = leGana p x && leGanaATodos p xs

losPokemonesDeTipoDe :: TipoDePokemon -> Entrenador -> [Pokemon]
losPokemonesDeTipoDe t e = losDeTipo t (pokemonesDe e)

losDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
losDeTipo _ [] = []
losDeTipo t (x:xs) = if sonElMismoTipo t (tipoDe x) then x : losDeTipo t xs else losDeTipo t xs

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (ConsEntrenador _ xs) = xs

leGana :: Pokemon -> Pokemon -> Bool
leGana p1 p2 = esTipoSuperior (tipoDe p1) (tipoDe p2)

esTipoSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoSuperior Agua Fuego = True
esTipoSuperior Fuego Planta = True
esTipoSuperior Planta Agua = True
esTipoSuperior x y = False

----------------- LOS QUE LE GANAN BIS --------------------------

losQueLeGanan' :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan' t e1 e2 = if sonTodosDeTipo (elTipoInferiorDe t) (pokemonesDe e2) then cantidadDeTipo t (pokemonesDe e1) else 0

sonTodosDeTipo :: TipoDePokemon -> [Pokemon] -> Bool
sonTodosDeTipo _ [] = True
sonTodosDeTipo t (x:xs) = sonElMismoTipo t (tipoDe x) && sonTodosDeTipo t xs

cantidadDeTipo :: TipoDePokemon -> [Pokemon] -> Int
cantidadDeTipo _ [] = 0
cantidadDeTipo t (x:xs) = if sonElMismoTipo t (tipoDe x) then 1 + cantidadDeTipo t xs else cantidadDeTipo t xs

elTipoInferiorDe :: TipoDePokemon -> TipoDePokemon
elTipoInferiorDe Agua = Fuego
elTipoInferiorDe Planta = Agua
elTipoInferiorDe Fuego = Planta

{-
El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
una lista de personas con diferente rol. La definición es la siguiente:
data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]
Definir las siguientes funciones sobre el tipo Empresa:
proyectos :: Empresa -> [Proyecto]

Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repe-
tidos.

losDevSenior :: Empresa -> [Proyecto] -> Int
Dada una empresa indica la cantidad de desarrolladores senior que posee.
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
cantidad de personas involucradas.
-}

data Seniority = Junior | SemiSenior | Senior deriving (Show)
data Proyecto = ConsProyecto String deriving (Show)
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving (Show)
data Empresa = ConsEmpresa [Rol] deriving (Show)

discas = ConsProyecto "Discas"
caterpillar = ConsProyecto "Caterpillar"
quiena = ConsProyecto "Quiena"
accenture = ConsProyecto "Accenture"

tutu = ConsProyecto "Tutu"

andy = Developer Senior discas
juan = Developer Senior discas
fabi = Developer Senior discas
fede = Developer Junior quiena
gonza = Developer SemiSenior caterpillar
gaston = Management Senior discas
gabi = Developer Junior accenture

losPibes = [andy,juan,fabi,fede,gonza,gaston, gabi]

proyectosList = [discas,quiena]

maniglie = ConsEmpresa losPibes

--Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa []) = []
proyectos (ConsEmpresa xs) = proyectosSinRepetidos (proyectosPorRoles xs)

empleados :: Empresa -> [Rol]
empleados (ConsEmpresa xs) = xs 

proyectosSinRepetidos :: [Proyecto] -> [Proyecto]
proyectosSinRepetidos [] = []
proyectosSinRepetidos (x:xs) = if (existeProyectoEnLista x xs) then proyectosSinRepetidos xs else x : proyectosSinRepetidos xs

proyectosPorRoles :: [Rol] -> [Proyecto]
proyectosPorRoles [] = []
proyectosPorRoles (x:xs) = proyecto x : proyectosPorRoles xs

proyecto :: Rol -> Proyecto
proyecto (Developer _ p) = p
proyecto (Management _ p) = p

nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto nom) = nom

existeProyectoEnLista :: Proyecto -> [Proyecto] -> Bool
existeProyectoEnLista _ [] = False
existeProyectoEnLista p (x:xs) = sonElMismoProyecto p x || existeProyectoEnLista p xs

sonElMismoProyecto :: Proyecto -> Proyecto -> Bool
sonElMismoProyecto p1 p2 = nombreProyecto p1 == nombreProyecto p2

-- Dada una empresa indica la cantidad de desarrolladores senior que posee.
losDevSenior :: Empresa -> Int
losDevSenior (ConsEmpresa []) = 0
losDevSenior e = length (losDevSeniorPorRoles (empleados e))

losDevSeniorPorRoles :: [Rol] -> [Rol]
losDevSeniorPorRoles [] = []
losDevSeniorPorRoles (x:xs) = if esDevSenior x then x : losDevSeniorPorRoles xs else losDevSeniorPorRoles xs

esDevSenior :: Rol -> Bool
esDevSenior (Developer Senior _) = True
esDevSenior x = False

--Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn [] _ = 0
cantQueTrabajanEn (x:xs) e = length (trabajanEn (empleados e) x) + cantQueTrabajanEn xs e

trabajanEn :: [Rol] -> Proyecto -> [Rol]
trabajanEn [] _ = []
trabajanEn (x:xs) p = if trabajaEn x p then x : trabajanEn xs p else trabajanEn xs p

trabajaEn :: Rol -> Proyecto -> Bool
trabajaEn r p = sonElMismoProyecto (proyecto r) p

--Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su cantidad de personas involucradas.
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto e = asignadosPorProyectoPorListaDeProyectos (proyectos e) e

asignadosPorProyectoPorListaDeProyectos :: [Proyecto] -> Empresa -> [(Proyecto, Int)]
asignadosPorProyectoPorListaDeProyectos [] _ = []
asignadosPorProyectoPorListaDeProyectos (x:xs) e = asignadosEnProyecto x e : asignadosPorProyectoPorListaDeProyectos xs e

asignadosEnProyecto :: Proyecto -> Empresa -> (Proyecto, Int)
asignadosEnProyecto p e = (p, length (trabajanEn (empleados e) p))