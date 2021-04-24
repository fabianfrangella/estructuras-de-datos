--1. Tipos recursivos simples
--1.1. Celdas con bolitas
--Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:
data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show
--En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad
--de bolitas de ese color en la celda. Por ejemplo, una celda con 2 bolitas azules y 2 rojas, podría
--ser la siguiente:
celda1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
--Implementar las siguientes funciones sobre celdas:
--nroBolitas :: Color -> Celda -> Int
--Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
--existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia = 0
nroBolitas c (Bolita clr cel) = if esMismoColor clr c then 1 + nroBolitas c cel else nroBolitas c cel

esMismoColor :: Color -> Color -> Bool
esMismoColor Rojo Rojo = True
esMismoColor Azul Azul = True
esMismoColor _ _ = False

--poner :: Color -> Celda -> Celda
--Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner c CeldaVacia = Bolita c CeldaVacia
poner c (Bolita clr cel) = Bolita clr (poner c cel)

--sacar :: Color -> Celda -> Celda
--Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
--Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar c CeldaVacia = CeldaVacia
sacar c (Bolita clr cel) = if esMismoColor clr c then cel else Bolita clr (sacar c cel)



--ponerN :: Int -> Color -> Celda -> Celda
--Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 c cel = cel
ponerN 1 c CeldaVacia = poner c CeldaVacia
ponerN n c cel = poner c (ponerN (n-1) c cel)

--1.2. Camino hacia el tesoro
--Tenemos los siguientes tipos de datos
data Objeto = Cacharro 
            | Tesoro deriving Show

data Camino = Fin 
            | Cofre [Objeto] Camino 
            | Nada Camino deriving Show

objetos = [Cacharro, Cacharro, Tesoro]
sinTesoro = [Cacharro, Cacharro]

caminito = (Cofre (sinTesoro) (Cofre objetos (Cofre sinTesoro (Cofre objetos Fin)))) 

--Definir las siguientes funciones:
--hayTesoro :: Camino -> Bool
--Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada c) = False
hayTesoro (Cofre objetos camino) = hayTesoroEnObjetos objetos || hayTesoro camino

hayTesoroEnObjetos :: [Objeto] -> Bool
hayTesoroEnObjetos [] = False
hayTesoroEnObjetos (x:xs) = esTesoro x || hayTesoroEnObjetos xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

--pasosHastaTesoro :: Camino -> Int
--Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
--Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
--Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Cofre objetos camino) = if not (hayTesoroEnObjetos objetos)
                            then 1 + pasosHastaTesoro camino
                            else 0

--Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
--pasos es 5, indica si hay un tesoro en 5 pasos.
--hayTesoroEn :: Int -> Camino -> Bool

--TODO: esto así no messi
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n Fin = False
hayTesoroEn n (Cofre objetos camino) = pasosHastaTesoro (Cofre objetos camino) == n 
                                        || hayTesoroEn n camino


caminar :: Int -> Camino -> Camino
caminar 0 camino = camino
caminar n camino = darUnPaso (caminar (n-1) camino)

darUnPaso :: Camino -> Camino
darUnPaso Fin = Fin
darUnPaso (Cofre objetos camino) = camino
darUnPaso (Nada camino) = camino

--alMenosNTesoros :: Int -> Camino -> Bool
--Indica si hay al menos “n” tesoros en el camino.
--cantTesorosEntre :: Int -> Int -> Camino -> Int
--Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
--el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
--incluidos tanto 3 como 5 en el resultado.