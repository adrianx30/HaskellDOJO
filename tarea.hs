x :: Int
x=5

lucky :: (Integral a) => a -> String
lucky 7 = "El siete de la suerte :)"
lucky x = "Hoy no es tu dia de suerte :( "

factorial :: Integer -> Integer
factorial 0=1
factorial x=x * factorial (x-1)

sumatoria :: Integer -> Integer
sumatoria 0 = 0
suma	toria x = x + sumatoria(x-1)

sumEveryTwo :: [Integer]->[Integer]
sumEveryTwo [] = []
sumEveryTwo (x:[]) = [x]
sumEveryTwo (x:y:zs) = (x+y):sumEveryTwo zs

intListLength :: [Int] -> Int
intListLength (x:[]) = 1
intListLength (x:za) = 1 + intListLength za

pesoTest :: Int -> [Char]
pesoTest peso
	|peso<=40 = "Estas muy flaco"
	|peso<=60 = "Estas bien de peso"
	|peso<=80 = "Estas obeso"
	|otherwise = "Te vas a estallar marica"

mayor :: Int -> Int -> Int
mayor a b 
	|a>b = a
	|a<b = b
	|a==b = 0

duplicarPares xs=[x*2| x<-xs, (mod x 2)==0]

multiplosDeTres :: [Int] -> [Int]
multiplosDeTres xs=[x| x<-xs,(mod x 3)==0]


