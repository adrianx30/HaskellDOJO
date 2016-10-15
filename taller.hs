---Punto 1---
 
--Paso1
--Funcion1
--Entrada: Número entero 
--Salida: Lista invertida
--Ejemplo 1234 -> [4,3,2,1]

toDigitsRev :: Int -> [Int]
toDigitsRev n 
    |n<=0 = []
    |otherwise = [(n `mod` 10)]++toDigitsRev(div n 10)
--FinFuncion 1

toDigits :: Int -> [Int]
toDigits = map (read . (:[])) . show


---Punto 2---

--Funcion1
--Entrada:  
--Salida: 
--Ejemplo 
intListLength :: [Int] -> Int
intListLength (x:[]) = 1
intListLength (x:za) = 1 + intListLength za

pares :: [Int] -> [Int]
pares [] = []
pares (x:za:papa) = x*2 : za : pares(papa)

impares :: [Int] -> [Int]
impares [] = []
impares (x:[]) = [x]
impares (x:za:papa) = x : za*2 : impares(papa)

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther n
	|(intListLength n) `mod` 2 == 0 = pares n 
	|otherwise = impares n


---Punto 3---

--Funcion1
--Entrada:  
--Salida: 
--Ejemplo 

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits (x:yuca)
	|x<10 = x + sumDigits yuca
	|otherwise = (x `div` 10) + (x `mod` 10) + sumDigits yuca
	

---Punto 4---

--Funcion1
--Entrada:  
--Salida: 
--Ejemplo 

puntoCuatro :: Int -> Bool
puntoCuatro 0 = False
puntoCuatro n
	|((sumDigits(doubleEveryOther (toDigits n))) `mod` 10) == 0 = True
	|otherwise = False
