sucesor :: Int -> Int
sucesor x = x + 1

sumar :: Int -> Int -> Int
sumar x y = x + y

divisionYResto :: Int -> Int -> (Int, Int)
{-Dado dos números, devuelve un par donde la primera componente es la división del
primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
provista por Haskell. -}
divisionYResto x y = (div x y, mod x y)

maxDelPar :: (Int,Int) -> Int
--Dado un par de números devuelve el mayor de estos.
maxDelPar (x, y) = if x > y then x else y

--Definir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. Luego implementar
--las siguientes funciones:

data Dir = Norte | Sur | Este | Oeste 
    deriving (Show)

--a) opuesto :: Dir -> Dir
--Dada una dirección devuelve su opuesta.
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

--b) iguales :: Dir -> Dir -> Dir
--Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True

--c) siguiente :: Dir -> Dir
--Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
--la siguiente dirección a Oeste. ¿Posee una precondición esta función? ¿Es una función
--total o parcial? ¿Por qué?
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste

--2. Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
--Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
--es domingo. Luego implementar las siguientes funciones:
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving (Show, Eq)
--a) primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
--Devuelve un par donde la primera componente es el primer día de la semana, y la
--segunda componente es el último día de la semana.
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

--b) empiezaConM :: DiaDeSemana -> Bool
--Dado un dia de la semana indica si comienza con la letra M.
empiezaConM :: DiaDeSemana -> Bool
empiezaConM x = take 1 (show x) == "M"

--c) vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
--Dado dos dias de semana, indica si el primero viene después que el segundo.
diaSiguiente :: DiaDeSemana -> DiaDeSemana
diaSiguiente Lunes = Martes
diaSiguiente Martes = Miercoles
diaSiguiente Miercoles = Jueves
diaSiguiente Jueves = Viernes
diaSiguiente Viernes = Sabado
diaSiguiente Sabado = Domingo
diaSiguiente Domingo = Lunes

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues x y = diaSiguiente y == x
--d) estaEnElMedio :: DiaDeSemana -> Bool
--Dado un dia de la semana indica si no es ni el primer ni el ultimo dia.
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio x = not (fst primeroYUltimoDia == x || snd primeroYUltimoDia == x)

--3. Los booleanos también son un tipo de enumerativo. Un booleano es True o False. Defina
--las siguientes funciones utilizando pattern matching (no usar las funciones sobre booleanos
--ya definidas en Haskell):
--a) negar :: Bool -> Bool
--Dado un booleano, si es T rue devuelve F alse, y si es F alse devuelve T rue.
--En Haskell ya está definida como not.
negar :: Bool -> Bool
negar True = False
negar False = True
--b) implica :: Bool -> Bool -> Bool
--Dados dos booleanos, si el primero es T rue y el segundo es F alse, devuelve F alse, sino
--devuelve T rue.
--Nota: no viene implementada en Haskell.
implica :: Bool -> Bool -> Bool
implica True False = False
implica x y = True
--c) and :: Bool -> Bool -> Bool
--Dados dos booleanos si ambos son T rue devuelve T rue, sino devuelve F alse.
--En Haskell ya está definida como &&.
and :: Bool -> Bool -> Bool
and True True = True
and x y = False
--d) or :: Bool -> Bool -> Bool
--Dados dos booleanos si alguno de ellos es T rue devuelve T rue, sino devuelve F alse.
--En Haskell ya está definida como ||.
or :: Bool -> Bool -> Bool
or True x = True
or False False = False

--1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
--siguientes funciones:
data Persona = P String Int deriving Show
juan = P "Juan" 30
pedro = P "Pedro" 25

--nombre :: Persona -> String
--Devuelve el nombre de una persona
nombre :: Persona -> String
nombre (P name age) = name
--edad :: Persona -> Int
--Devuelve la edad de una persona
edad :: Persona -> Int
edad (P name age) = age
--crecer :: Persona -> Persona
--Aumenta en uno la edad de la persona.
crecer :: Persona -> Persona
crecer (P n e) = P n (e + 1)
--cambioDeNombre :: String -> Persona -> Persona
--Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
--nuevo nombre.
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre x (P n e) = P x e 
--esMayorQueLaOtra :: Persona -> Persona -> Bool
--Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P n1 e1) (P n2 e2) = e1 > e2 
--laQueEsMayor :: Persona -> Persona -> Persona
--Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor x y = if esMayorQueLaOtra x y then x else y

--2. Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
--porcentaje de energía; y Entrenador, como un nombre y dos pokemones. Luego definir las
--siguientes funciones:
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = UnPokemon TipoDePokemon Int deriving Show
data Entrenador = UnEntrenador String Pokemon Pokemon deriving Show

poke1 = UnPokemon Agua 30
poke2 = UnPokemon Fuego 75
poke3 = UnPokemon Planta 80
poke4 = UnPokemon Planta 85
trainer = UnEntrenador "Pepe" poke1 poke2
sarasa = UnEntrenador "Pepin" poke3 poke4
--superaA :: Pokemon -> Pokemon -> Bool
--Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
--supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
tipoSuperaA :: TipoDePokemon -> TipoDePokemon -> Bool
tipoSuperaA Agua Fuego = True
tipoSuperaA Fuego Planta = True
tipoSuperaA Planta Agua = True
tipoSuperaA _ _ = False

tipoPokemon :: Pokemon -> TipoDePokemon
tipoPokemon (UnPokemon t e) = t

superaA :: Pokemon -> Pokemon -> Bool
superaA x y = tipoSuperaA (tipoPokemon x) (tipoPokemon y)

--cantidadDePokemonesDe :: TipoDePokemon -> Entrenador -> Int
--Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
primerPokemonDe :: Entrenador -> Pokemon
primerPokemonDe (UnEntrenador n p1 p2) = p1

segundoPokemonDe :: Entrenador -> Pokemon
segundoPokemonDe (UnEntrenador n p1 p2) = p2

unoSiEsDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Int
unoSiEsDelMismoTipo Fuego Fuego = 1
unoSiEsDelMismoTipo Agua Agua = 1
unoSiEsDelMismoTipo Planta Planta = 1
unoSiEsDelMismoTipo _ _ = 0

cantidadDePokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonesDe tipo ent = (unoSiEsDelMismoTipo tipo (tipoPokemon(primerPokemonDe ent))) + (unoSiEsDelMismoTipo tipo (tipoPokemon(segundoPokemonDe ent))) 

--juntarPokemones :: (Entrenador, Entrenador) -> [Pokemon]
--Dado un par de entrenadores, devuelve a sus pokemones en una lista.
pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (UnEntrenador n p1 p2) = [p1, p2]

juntarPokemones :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemones (x, y) = (pokemonesDe x) ++ (pokemonesDe y)

--1. Defina las siguientes funciones polimórficas:
--a) loMismo :: a -> a
--Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo :: a -> a
loMismo x = x
--b) siempreSiete :: a -> Int
--Dado un elemento de algún tipo devuelve el número 7.
siempreSiete :: a -> Int
siempreSiete x = 7
--c) swap :: (a,b) -> (b, a)
--Dadas una tupla, invierte sus componentes.
swap :: (a,b) -> (b, a)
swap (x, y) = (y, x)
--Por qué existen dos variables de tipo diferentes?
--2. Responda la siguiente pregunta: ¿Por qué estas funciones son polimórficas?

--5. Pattern matching sobre listas
--1. Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas (no
--utilizar las funciones que ya vienen con Haskell):
--2. estaVacia :: [a] -> Bool
--Dada una lista de elementos, si es vacía devuelve T rue, sino devuelve F alse.
--Definida en Haskell como null.
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia (x:xs) = False
--3. elPrimero :: [a] -> a
--Dada una lista devuelve su primer elemento.
--Definida en Haskell como head.
--Nota: tener en cuenta que el constructor de listas es :
elPrimero :: [a] -> a
elPrimero (x:xs) = x
--4. sinElPrimero :: [a] -> [a]
--Dada una lista devuelve esa lista menos el primer elemento.
--Definida en Haskell como tail.
--Nota: tener en cuenta que el constructor de listas es :
sinElPrimero :: [a] -> [a]
sinElPrimero (x:xs) = xs
--5. splitHead :: [a] -> (a, [a])
--Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
--lista, y la segunda componente es esa lista pero sin el primero.
--Nota: tener en cuenta que el constructor de listas es :
splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x,xs)
