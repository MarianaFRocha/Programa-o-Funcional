questao1 :: Float -> Float
questao1 y = (y*32) + 1.8

questao2 :: Float -> Float -> Float
questao2 b h = b*h

questao3 :: Float -> Float
questao3 r = 2*3.14*r

questao4 :: Int -> Int
questao4 n |mod n 2 == 0 = 0
           |mod n 2 == 1 = n 

questao5 :: Int -> Int
questao5 x = (x^2) - (3*x) + 4

questao6 :: Int -> Int
questao6 n = n^2