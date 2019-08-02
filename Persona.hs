data Persona = Person String Int deriving (Show, Eq)

nombre :: Persona -> String
nombre (Person name _) = name

edad :: Persona -> Int
edad (Person _ age) = age

crecer :: Persona -> Persona
crecer (Person name age) = Person name (succ age)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre  newname (Person _ age) = Person newname age   

esMenorQueLaOtra :: Persona -> Persona -> Bool
esMenorQueLaOtra (Person _ age0) (Person _ age1) = age0 < age1

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (p : ps) = if edad p > n
						then p : mayoresA n ps
						else mayoresA n ps 

promedioEdad :: [Persona] -> Int
promedioEdad ps = div (sum (edades ps)) (length ps)

edades :: [Persona]	-> [Int]
edades [] = []
edades (p : ps) = edad p : edades ps

--elMasViejo ::  [Persona] -> Persona 

potencia :: Int -> Int -> Int
potencia n 0 = 1
potencia n m = n * potencia n (m -1)

