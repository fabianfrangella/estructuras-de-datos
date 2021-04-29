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