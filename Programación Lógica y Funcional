--Actividad 1 Determina el resultado de un número x elevado a una potencia n (6 pts)

potencia :: Integer -> Integer -> Integer
potencia m 0 = 1
potencia m n = m*(potencia m (n-1))

--Ejemplo: potencia 2 4


--2. Determina si un número n se encuentra en un rango determinado (6 pts)

contiene :: Int->Bool
contiene n = n `elem` [1,2..10]

--Ejemplo: contiene 2

--3 una cantidad de segundos, devuelve la cantidad de horas, minutos y segundos equivalente

segundosAHora :: Integer -> (Integer,Integer,Integer)
segundosAHora s = (horas, minutos, segundos)
				where 
				horas = div s 3600
				ss = mod s 3600
				minutos = div ss 60
				segundos = mod ss 60

--Ejemplo: segundosAHora 5678

--4. Determine el mayor de 4 enteros (6 pts)

--2. Determina si un número n se encuentra en un rango determinado (6 pts)
maxCuatro :: Int -> Int -> Int -> Int ->Int
maxCuatro w x y z = max w (max x (max y z))

--Ejemplo: maxCuatro 2 4 6 7

--5. Calcula la suma de una lista (arreglo) de elementos. (6 pts)

sumar::[Int]->Int
sumar [ ] = 0
sumar (x:xs) = x + sumar(xs)

--Ejemplo: sumar [2,5,6]

--6. Determina si un elemento dado está contenido en una lista. Devuelve verdadero o falso.
(8 pts)

contiene :: Int->Bool
contiene n = n `elem` [1,2,3,4,5,6]

--Ejemplo contiene 5

--7. Determina si dada una lista, ésta se encuentra ordenada. Se debe devolver verdadero o falso. (9 pts)

lista_ordenada::Ord a=>[a]->Bool
lista_ordenada [] = True
lista_ordenada [_] = True
lista_ordenada (x:y:xs) = (x<=y) && lista_ordenada (y:xs)

--Ejemplo lista_ordenada [1,2,3,4,5,4]

--8. Dadas dos listas, determine si son iguales. Devolver verdadeo o falso. (9 pts)

igualLista:: Eq a => [a]->[a]->Bool
igualLista l1 l2 = l1 == l2

--Ejemplo: igualLista [2,3,4] [2,6,4]

--9 Realizar un función recursiva que retorne como salida el resultado de la suma 1 + 3 + 5 + 7 + 9 + N (10 pts)

sumaImpares :: Integer -> Integer
sumaImpares n = sum [x | x <- [1,3..n]]

--Ejemplo: sumaImpares 3 

--10. Realizar una función que reciba una lista y devuelva empleando recursividad otra
lista de los elementos pares. (10 pts)

filtraPares xs = [ x | x <- xs, odd x == False]

--Ejemplo: filtraPares [1,2,3,4,5,6]


--11. Realiza una función en Haskell que permita cargar calcular la unión, intersección y
diferencia de dos conjuntos datos. Para esto puede hacer uso de la librería “Data.set”
(12 pts.)


import qualified Data.Set as Set

lista1 = ['a','u','d','r','e','t']
lista2 = ['u','o','r','j','n','k']

set1 = Set.fromList lista1
set2 = Set.fromList lista2

union = Set.union set1 set2
intersec = Set.intersection set1 set2
dif = Set.difference set1 set2

--Para realizar las operaciones de los dos conjuntos solo hay que llamar a la funcion


--12. Realiza una funcón que permita definir un mapa de datos y permita encontrar un
valor a partir de su clave. Para esto puede hacer uso de la librería “Data.map” (12 pts.)

import Data.Char
import qualified Data.Map as Map

phoneBook =
    [("betty", "555-2938")
    ,("betty", "342-2492")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("patsy", "943-2929")
    ,("patsy", "827-9162")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ,("penny", "555-2111")
    ]

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
    where add number1 number2 = number1 ++ ", " ++ number2

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs
main = do
    print $ Map.lookup "patsy" $ phoneBookToMap phoneBook
    print $ Map.lookup "wendy" $ phoneBookToMap phoneBook
    print $ Map.lookup "patsy" $ phoneBookToMap' phoneBook


