--Questão 1

aux1 :: Int -> Int
aux1 x = div x 100

aux2 :: Int -> Int 
aux2 x = div (x - (aux1 x * 100)) 10

aux3 :: Int -> Int
aux3 x = x - ((aux1 x * 100) + (aux2 x * 10))

questao1 :: Int -> Int
questao1 x = aux1 x + aux2 x + aux3 x

--Questão2

aux02 :: Float -> Float
aux02 p = p / 18

questao2 :: Float -> Float -> Float
questao2 a p = a / aux02 p 

--Questão 3

questao3 :: (Float, Float, Float) -> Float
questao3 (c,l,a) = (((l*a)*2) + ((c*a)*2)) / 1.5

--Questão 4

questao4a :: (Int, Int, Float) -> (Int, Int, Float) -> Float
questao4a (a,b,c) (x,y,z) = (c+z) / 2

questao4b :: (Int, Int, Float) -> (Int, Int, Float) -> (Int, Int, Float) -> Float
questao4b (a,b,c) (x,y,z) (m,n,o) = fromIntegral (b+y+n) / 3

--Questão 5

questao5a :: (Char, Int, Int) -> Int
questao5a (a,b,c) = b*c

questao5b :: (Char, Int, Int) -> (Char, Int, Int) -> (Int, Char, Char)
questao5b (a,b,c) (m,n,o) = (b+n,a,m)

questao5c :: (Char, Int, Int) -> (Char, Int, Int) -> (Char, Int, Int) -> Int
questao5c (a,b,c) (m,n,o) (g,h,j) = questao5a (a,b,c) + questao5a (m,n,o) + questao5a (g,h,j)

 