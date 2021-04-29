--1. Pizzas
--Tenemos los siguientes tipos de datos:

data Pizza = Prepizza
            | Capa Ingrediente Pizza deriving Show

data Ingrediente = Salsa
            | Queso
            | Jamon
            | Aceitunas Int deriving Show

p1 = Capa Jamon (Capa Queso (Capa Salsa Prepizza))
p2 = Capa Queso (Capa Salsa Prepizza)
p3 = Capa (Aceitunas 10) (Capa Queso (Capa Queso Prepizza))
p4 = Capa (Aceitunas 10) (Capa Queso (Capa (Aceitunas 20) Prepizza))


--Definir las siguientes funciones:
--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing pz) = 1 + cantidadDeCapas pz

--Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) = Capa x (armarPizza xs)

--Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ing pz) = if esJamon ing
                            then sacarJamon pz
                            else Capa ing (sacarJamon pz)

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False

--Dice si una pizza tiene salsa y queso
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ing pz) = (esSalsa ing || esQueso ing) && tieneSoloSalsaYQueso pz

esSalsa :: Ingrediente -> Bool
esSalsa Salsa = True
esSalsa _ = False

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _ = False

--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ing pz) = Capa (duplicar ing) (duplicarAceitunas pz)

duplicar :: Ingrediente -> Ingrediente
duplicar (Aceitunas x) = Aceitunas (x * 2)
duplicar ing = ing

--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
--ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (x:xs) = (cantidadDeCapas x, x) : cantCapasPorPizza xs

--2. Mapa de tesoros (con bifurcaciones)
--Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y
--cada cofre tiene un objeto, que puede ser chatarra o un tesoro.
data Dir = Izq | Der deriving Show

data Objeto = Tesoro | Chatarra deriving Show

data Cofre = Cofre [Objeto] deriving Show

data Mapa = Fin Cofre
            | Bifurcacion Cofre Mapa Mapa deriving Show

mapaTesoro = Bifurcacion (Cofre []) 
				(Bifurcacion (Cofre []) 
					(Bifurcacion (Cofre []) 
						(Fin (Cofre [])) 
						(Fin (Cofre [Tesoro])))
					(Fin (Cofre [])))
				(Fin (Cofre []))

mapaSinTesoro = Bifurcacion (Cofre []) 
				(Bifurcacion (Cofre [Chatarra]) 
					(Fin (Cofre []))
					(Fin (Cofre [])))
				(Fin (Cofre []))

mapaLargo = Bifurcacion (Cofre [Tesoro]) 
				(Bifurcacion (Cofre []) 
					(Bifurcacion (Cofre []) 
						(Bifurcacion (Cofre []) 
							(Fin(Cofre []))
							(Bifurcacion (Cofre []) 
								(Fin(Cofre []))
								(Fin(Cofre [])))) 
						(Fin (Cofre [Tesoro])))
					(Fin (Cofre [])))
				(Fin (Cofre [Tesoro]))

--Definir las siguientes operaciones:
--Indica si hay un tesoro en alguna parte del mapa.
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnCofre c || hayTesoro m1 || hayTesoro m2

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre o) = hayTesoroEnObjetos o

hayTesoroEnObjetos :: [Objeto] -> Bool
hayTesoroEnObjetos [] = False
hayTesoroEnObjetos (x:xs) = esTesoro x || hayTesoroEnObjetos xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

--2. hayTesoroEn :: [Dir] -> Mapa -> Bool
--Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
--lista vacía de direcciones.
--3. caminoAlTesoro :: Mapa -> [Dir]
--Indica el camino al tesoro. Precondición: existe un tesoro y es único.
--4. caminoDeLaRamaMasLarga :: Mapa -> [Dir]
--Indica el camino de la rama más larga.
--5. tesorosPorNivel :: Mapa -> [[Objeto]]
--Devuelve los tesoros separados por nivel en el árbol.
--6. todosLosCaminos :: Mapa -> [[Dir]]
--Devuelve todos lo caminos en el mapa.