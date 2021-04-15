
jorge = P "Jorge" 35
carlos = P "Carlos" 30

charizard = Poke Fuego 80
bulbasour = Poke Agua 60
plantita = Poke Planta 40
aguita = Poke Agua 20

ash = Entr "Ash Ketchup" charizard bulbasour
brook = Entr "Brook" plantita aguita
-- Enteros
sucesor :: Int -> Int
sucesor x = x+1

sumar :: Int -> Int -> Int
sumar x y = x+y

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x 0 = error "No se puede dividir por 0"
divisionYResto x y = (div x y, mod x y)

maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = if x > y then x else y

-- Enums

-- Data definitions
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving (Show, Enum)

{-
a) primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
Devuelve un par donde la primera componente es el primer día de la semana, y la
segunda componente es el último día de la semana.
b) empiezaConM :: DiaDeSemana -> Bool
Dado un dia de la semana indica si comienza con la letra M.
c) vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
Dado dos dias de semana, indica si el primero viene después que el segundo.
d) estaEnElMedio :: DiaDeSemana -> Bool
Dado un dia de la semana indica si no es ni el primer ni el ultimo dia.
-}

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM x = False

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Lunes Domingo = True
vieneDespues Martes Lunes = True
vieneDespues Miercoles Martes = True
vieneDespues Jueves Miercoles = True
vieneDespues Viernes Jueves = True
vieneDespues Sabado Viernes = True
vieneDespues Domingo Sabado = True
vieneDespues x y = False

vieneDespuesConSucc :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespuesConSucc x y = sonElMismoDia (succ y) x

sonElMismoDia :: DiaDeSemana -> DiaDeSemana -> Bool
sonElMismoDia Lunes Lunes = True
sonElMismoDia Martes Martes = True
sonElMismoDia Miercoles Miercoles = True
sonElMismoDia Jueves Jueves = True
sonElMismoDia Viernes Viernes = True
sonElMismoDia Sabado Sabado = True
sonElMismoDia Domingo Domingo = True
sonElMismoDia x y = False

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio x = True

data Dir = Norte | Sur | Este | Oeste deriving (Show)

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "No hay direccion siguiente de Oeste"

negar :: Bool -> Bool
negar True = False
negar False = True

--Dados dos booleanos, si el primero es True y el segundo es False, devuelve F alse, sino
--devuelve True.
implica :: Bool -> Bool -> Bool
implica True False = False
implica x y = True

and :: Bool -> Bool -> Bool
and True True = True
and x y = False

or :: Bool -> Bool -> Bool
or True False = True
or True True = True
or False True = True
or False False = False

-- Registros

{-
Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
siguientes funciones:

nombre :: Persona -> String
Devuelve el nombre de una persona
edad :: Persona -> Int
Devuelve la edad de una persona
crecer :: Persona -> Persona
Aumenta en uno la edad de la persona.
cambioDeNombre :: String -> Persona -> Persona
Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
nuevo nombre.
esMayorQueLaOtra :: Persona -> Persona -> Bool
Dadas dos personas indica si la primera es mayor que la segunda.
laQueEsMayor :: Persona -> Persona -> Persona
Dadas dos personas devuelve a la persona que sea mayor.
-}
--			   nombre edad
data Persona = P String Int deriving (Show)

nombre :: Persona -> String
nombre (P n e) = n

edad :: Persona -> Int
edad (P n e) = e

crecer :: Persona -> Persona
crecer (P n e) = P n (e + 1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nom (P n e) = P nom e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2 

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2 then p1 else p2

{-
Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
porcentaje de energía; y Entrenador, como un nombre y dos pokemones. Luego definir las
siguientes funciones:

superaA :: Pokemon -> Pokemon -> Bool
Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.

cantidadDePokemonesDe :: TipoDePokemon -> Entrenador -> Int
Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.

juntarPokemones :: (Entrenador, Entrenador) -> [Pokemon]
Dado un par de entrenadores, devuelve a sus pokemones en una lista.
-}

data TipoDePokemon = Agua | Fuego | Planta deriving (Show)

-- 				  tipo            porcentaje de energia
data Pokemon = Poke TipoDePokemon Int deriving (Show)

data Entrenador = Entr String Pokemon Pokemon deriving (Show)

superaA :: Pokemon -> Pokemon -> Bool
superaA x y = esTipoSuperior (tipo x) (tipo y)

esTipoSuperior :: TipoDePokemon -> TipoDePokemon -> TipoDePokemon
esTipoSuperior Agua Fuego = True
esTipoSuperior Fuego Planta = True
esTipoSuperior Planta Agua = True
esTipoSuperior x y = False

tipo :: Pokemon -> TipoDePokemon
tipo (Poke t _ ) = t

sonElMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonElMismoTipo Agua Agua = True
sonElMismoTipo Planta Planta = True
sonElMismoTipo Fuego Fuego = True
sonElMismoTipo x y = False

primerPokemon :: Entrenador -> Pokemon
primerPokemon (Entr _ p1 _) = p1

segundoPokemon :: Entrenador -> Pokemon
segundoPokemon (Entr _ _ p2) = p2

unoSiEsElMismoTipo :: TipoDePokemon -> TipoDePokemon -> Int
unoSiEsElMismoTipo Agua Agua = 1
unoSiEsElMismoTipo Fuego Fuego = 1
unoSiEsElMismoTipo Planta Planta = 1
unoSiEsElMismoTipo x y = 0

cantidadDePokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonesDe t e = unoSiEsElMismoTipo (tipo (primerPokemon e)) t + unoSiEsElMismoTipo (tipo (segundoPokemon e)) t

juntarPokemones :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemones (e1, e2) = pokemonesDe e1 ++ pokemonesDe e2

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (Entr _ p1 p2) = [p1, p2]
{-
1. Defina las siguientes funciones polimórficas:
a) loMismo :: a -> a
Dado un elemento de algún tipo devuelve ese mismo elemento.
b) siempreSiete :: a -> Int
Dado un elemento de algún tipo devuelve el número 7.
c) swap :: (a,b) -> (b, a)
Dadas una tupla, invierte sus componentes.
¿Por qué existen dos variables de tipo diferentes?
2. Responda la siguiente pregunta: ¿Por qué estas funciones son polimórficas?
-}

loMismo :: a -> a
loMismo b = b

siempreSiete :: a -> Int
siempreSiete a = 7

-- ¿Por qué existen dos variables de tipo diferentes?
-- Porque los valores de las tuplas pueden ser de distinto tipo
swap :: (a,b) -> (b, a)
swap (a, b) = (b, a)

-- 2. Responda la siguiente pregunta: ¿Por qué estas funciones son polimórficas?
-- Porque la logica es indiferente a los tipos, ya sea el tipo que expresa o tipos que reciba por parametro

{-
1. Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas (no
utilizar las funciones que ya vienen con Haskell):
2. estaVacia :: [a] -> Bool
Dada una lista de elementos, si es vacía devuelve T rue, sino devuelve F alse.
Definida en Haskell como null.
3. elPrimero :: [a] -> a
Dada una lista devuelve su primer elemento.
Definida en Haskell como head.
Nota: tener en cuenta que el constructor de listas es :
4. sinElPrimero :: [a] -> [a]
Dada una lista devuelve esa lista menos el primer elemento.
Definida en Haskell como tail.
Nota: tener en cuenta que el constructor de listas es :
5. splitHead :: [a] -> (a, [a])
Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
lista, y la segunda componente es esa lista pero sin el primero.
Nota: tener en cuenta que el constructor de listas es :
-}

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
elPrimero (a:_) = a
elPrimero [] = error "No se puede obtener el primero de una lista vacia"

sinElPrimero :: [a] -> [a]
sinElPrimero (a:xs) = xs 
sinElPrimero [] = error "No se le puede sacar el primero a una lista vacia"

splitHead :: [a] -> (a, [a])
splitHead (a:xs) = (a, xs)
splitHead [] = error "No se puede splitear una lista vacia"
