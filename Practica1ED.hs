																			--Conceptos básicos

sucesor :: Int -> Int
sucesor x = x + 1

sumar :: Int -> Int -> Int
sumar x y = x + y 

maximo :: Int -> Int -> Int
maximo x y = max x y

negar :: Bool -> Bool
negar True = False
negar  False = True

andLogico :: Bool -> Bool -> Bool
andLogico True True = True
andLogico _ _  = False

orLogico :: Bool -> Bool -> Bool
orLogico False False = False
orLogico _ _ = True

primera :: (Int,Int) -> Int
primera (x, _) = x

segunda :: (Int,Int) -> Int
segunda (_, y) = y

sumaPar :: (Int,Int) -> Int
sumaPar (x, y) = x + y

maxDelPar :: (Int,Int) -> Int
maxDelPar (x, y) = maximo x y

loMismo :: a -> a
loMismo x = x

siempreSiete 	:: a -> Int
siempreSiete _ = 7

duplicar :: a -> (a,a)
duplicar x = (x, x) 

singleton :: a -> [a]
singleton x = [x]

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False 

head' :: [a] -> a
head' [] = error "Lista vacia"
head' (x : xs) = x

tail' :: [a] -> [a]
tail' [] = error "Lista vacia"
tail' (x : xs) = xs

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x : xs) = x + sumatoria xs

longitud :: [a] -> Int
longitud [] = 0
longitud (x : xs) = 1 + longitud xs

mapSucesor :: [Int] -> [Int]
mapSucesor [] = []
mapSucesor (x : xs) = sucesor x : mapSucesor xs

mapMaxDelPar :: [(Int, Int)] -> [Int]
mapMaxDelPar [] = []
mapMaxDelPar ((x,  y) : ps) = maximo x y : mapMaxDelPar ps

todoVerdad :: [Bool] -> Bool
todoVerdad [] = True
todoVerdad (x : xs) = x && todoVerdad xs

algunaVerdad :: [Bool] -> Bool
algunaVerdad [] = False
algunaVerdad (x : xs) = x || algunaVerdad xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece y [] = False
pertenece y (x : xs) = esIgual x y || pertenece y xs

unoSi :: Eq a => Bool -> a -> a -> Int
unoSi cond x y = if cond 
					then 1
					else 0

apariciones :: Eq a => a -> [a] -> Int
apariciones y [] = 0
apariciones y (x : xs) = unoSi (esIgual x y) x y + apariciones y xs

esMenor :: Int -> Int -> Bool
esMenor x y = x < y

filtrarMenoresA :: Int -> [Int] -> [Int]
filtrarMenoresA _ [] = []
filtrarMenoresA n (x : xs) = if esMenor x n
									then  filtrarMenoresA n xs
									else x : filtrarMenoresA n xs

esIgual :: Eq a => a -> a -> Bool
esIgual x y = x == y

filtrarElemento :: Eq a => a -> [a] -> [a]
filtrarElemento _ [] = [] 									
filtrarElemento y (x : xs) = if esIgual x y
									then filtrarElemento y xs
									else x : filtrarElemento y xs 	 					

mapLongitudes :: [[a]] -> [Int]
mapLongitudes [] = []
mapLongitudes (ls : lss) = longitud ls : mapLongitudes lss									

tieneMasDeN :: Int -> [a] -> Bool
tieneMasDeN n xs = longitud xs > n

longitudMayorA :: Int -> [[a]] -> [[a]]
longitudMayorA _ [] = []
longitudMayorA n (ls : lss) =  if tieneMasDeN n ls
									then ls : longitudMayorA n lss
									else longitudMayorA n lss

intercalar :: a -> [a] -> [a]
intercalar y [] = []
intercalar y (x : xs) = if longitud xs == 1
								then x : y : xs
								else x : y : intercalar y xs

--snoc :: [a] -> a -> [a]
--snoc xs x = xs ++ [x]
snoc :: [a] -> a -> [a]
snoc [] y = [y]
snoc (x : xs) y = x : snoc xs y

--append :: [a] -> [a] -> [a]
--append xs ys = xs ++ ys
append :: [a] -> [a] -> [a]
append xs [] = xs
append [] ys = ys
append xs (y : ys) = append (snoc xs y) ys

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (ls : lss) = ls ++ aplanar lss

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] [] = []
zipMaximos [] xs = []
zipMaximos ys [] = []
zipMaximos (y : ys) (x : xs) = maximo x y : zipMaximos ys xs

zipSort :: [Int] -> [Int] -> [(Int, Int)]
zipSort [] [] = []
zipSort [] xs = []
zipSort ys [] = []
zipSort (y : ys) (x : xs) =(min x y, max x y) : zipSort ys xs

promedio :: [Int] -> Int 
promedio [] = error "Lista vacia"
promedio xs = sumatoria xs `div` longitud xs 

minimo :: Ord a => [a] -> a
minimo [] = error "Lista vacia"
minimo [x] = x
minimo (x : xs) = min x (minimo xs)

predecesor :: Int -> Int
predecesor n = n - 1

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (predecesor n)

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n <= 0
						then []
						else n : cuentaRegresiva (predecesor n)

cuenta :: Int -> Int -> [Int]
cuenta n m = if n <= m
					then n : cuenta (sucesor n) m
					else []

contarHasta ::  Int -> [Int]
contarHasta n = cuenta 1 n 

replicarN :: Int -> a -> [a]
replicarN 0 _ = []
replicarN n x = if n > 0
					then x : replicarN (predecesor n) x
					else []

takeN :: Int -> [a] -> [a]
takeN n [] = [] 
takeN n (x : xs) = if n > 0
						then x : takeN (predecesor n) xs
						else []

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN _ [] = []
dropN n list@(x : xs) = if longitud list <= n 
							then []
							else dropN (predecesor n) xs      	

splitN :: Int -> [a] -> ([a], [a])
splitN n xs = (takeN n xs, dropN n xs)											


maximum' :: Ord a => [a] -> a
maximum' [] = error "Lista vacia"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

splitMin :: Ord a => [a] -> (a, [a])
splitMin [] = error "Lista vacia"
splitMin [x] = (x, [])
splitMin xs = (minimo xs, filtrarElemento (minimo xs) xs)

ordenar :: Ord a => [a] -> [a]
ordenar [] = []   
ordenar [x] = [x]
ordenar xs = minimo xs : ordenar (filtrarElemento (minimo xs) xs) 

interseccion :: Eq a => [a] -> [a] -> [a]
interseccion _ [] = []
interseccion [] _ = []
interseccion xs (y : ys) = if pertenece y xs
								then y : interseccion xs ys
								else  interseccion xs ys


diferencia :: Eq a => [a] -> [a] -> [a]
diferencia xs [] = xs
diferencia [] _ = []
diferencia (x : xs) ys = if pertenece x ys
								then diferencia (filtrarElemento x xs) ys
								else x : diferencia (filtrarElemento x xs) ys 								

particionPorSigno :: [Int] -> ([Int], [Int])
particionPorSigno xs = (positivos xs, negativos xs)

positivos :: [Int] -> [Int]
positivos xs = filtrarMenoresA 0 xs

negativos ::  [Int] -> [Int]
negativos xs = diferencia xs (positivos xs)

subtails :: [a] -> [[a]]
subtails [] = error "Lista vacia"
subtails [x] = [[x], []]
subtails xs = xs : subtails (tail xs)
	
agrupar :: Eq a => [a] -> [[a]]
--Dada una lista xs devuelve una lista de listas donde cada sublista contiene elementos conti-
--guos iguales de xs. Ejemplo: agrupar "AABCCC"  ["AA","B","CC"]
agrupar [] = []
agrupar [x] = [[x]]
agrupar (x : y : xs) = agruparSi 

esPrefijo :: Eq a => [a] -> [a] -> Bool
esPrefijo [] _ = True
esPrefijo _ [] = False 
esPrefijo (x : xs) (y : ys) = esIgual x y && esPrefijo xs ys 


esSufijo :: Eq a => [a] -> [a] -> Bool
esSufijo [] _ = True
esSufijo xs ys = esMismaLista xs (dropN (longitud ys - longitud xs) ys)

esMismaLista :: Eq a => [a] -> [a] -> Bool
esMismaLista [] [] = True
esMismaLista xs [] = False
esMismaLista [] ys = False
esMismaLista (x : xs) (y : ys) = esIgual x y && esMismaLista xs ys 

--circulo 
--cruz
--triangulo
--cuadrado

