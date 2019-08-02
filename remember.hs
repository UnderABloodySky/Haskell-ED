doubleMe x = 2 * x 

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x <= 100
						then doubleMe x
						else x

boomBang xs = [if x < 10 then "Boom" else "Bang" | x <- xs, odd x ] 						

allDiferentsTo xs n0 n1 n2 = [x | x <- xs, x /= n0,x /= n1, x /= n2  ]  

removeLowCase st = [c | c <- st, c `elem` ['A' .. 'Z']]

removeUpperCase st = [c | c <- st, not (c `elem` ['A' .. 'Z'])]

--let triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]

triangles = [(a,b,c) | c <- [1 .. 10], b <- [1 .. 10], a <- [1 .. 10], a ^ 2 + b ^ 2 == c ^ 2 && a + b + c == 24] 

factorial xs = product xs

capital :: String -> String
capital "" = "¡Una cadena vacía!"
capital pepe@(x:_) = "La primera letra de " ++ pepe ++ " es " ++ [x]	

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
	| bmi <= 18.5 = "Tienes infrapeso ¿Eres emo?"
	| bmi <= 25.0 = "Supuestamente eres normal... Espero que seas feo."
	| bmi <= 30.0 = "¡Estás gordo! Pierde algo de peso gordito."
	| otherwise = "¡Enhorabuena, eres una ballena!"
	where bmi = weight / height ^ 2

initials :: String -> String -> String -> String
initials firstname middlename lastname = [f] ++ ". " ++ [m] ++ ". " ++ [l] ++ "." 
	where  (f : _) = firstname
	       (m : _) = middlename  
	       (l : _) = lastname  

		  
head' :: [a] -> a
head' [] = error "Lista vacia"
head' (x : _ ) = x

head'' :: [a] -> a
head'' xs = case xs of
				[] 		 -> error "Lista vacia"
				(x : xs) -> x		  

isEmpty :: [a] -> Bool
isEmpty xs = case xs of
				[] 		 -> True
				_ 		 -> False		  				


maximum' :: Ord a => [a] -> a
maximum' [] = error "lista vacia"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs) 				


replicate' :: (Num i , Eq i) => i -> a -> [a]
replicate' n x
	| n == 0 = []
	| otherwise = x : replicate' (n-1) x 


replicate'' :: (Ord n, Num n, Eq n) => n -> a -> [a]
replicate'' 0 x = []
replicate'' n x = if n > 0
					then x : replicate'' (n-1) x  
					else []

takeN :: Num n => n -> [a] -> [a]
takeN 0 xs = []
takeN _ [] = []	
takeN n (x : xs) = x : takeN (n-1) xs 

takeN' :: (Num n, Eq n) => n -> [a] -> [a]
takeN' n list@(x : xs)
	| n == 0 	 	= []
	| otherwise 	= x : takeN (n-1) xs 

quicksort :: (Ord a) => [a] -> [a]
 
