funcao1 :: Int -> Int
funcao1 0 = 0
funcao1 x = mod x 10 + funcao1 (div x 10)

funcao2 :: Int -> Int -> Int
funcao2 x n |x >= n = funcao2 (x-n) n
            |otherwise = x
			
funcao3 :: Int -> Int -> Int 
funcao3 x n |x>=n = 1+(funcao3 (x-n) n)
            |otherwise = 0 
			
funcao4 :: [Int] -> Int -> Int
funcao4 (x:xs) n |n == 0 = x
                 |otherwise = funcao4 xs (n-1)
				 
funcao5 :: String -> String
funcao5 [] = []
funcao5 (x:xs) |x == ' ' = '_':(funcao5 xs)
               |otherwise = x:(funcao5 xs)
			   
funcao6 :: [Int] -> [Int] -> [Int]
funcao6 [] (y:ys) = (y:ys)
funcao6 (x:xs) (y:ys) = x:(funcao6 xs (y:ys))

funcao7 :: Int -> Int -> [Int]
funcao7 m n |m == n = [n]
            |otherwise = m:(funcao7 (m-1) n)
			
aux8 :: Int -> Int -> [Int]
aux8 x 0 = []
aux8 x y|y>=0 && mod x y == 0 = y:(aux8 x (y-1))
		|otherwise = aux8 x (y-1)
		
funcao8 :: Int -> [Int]
funcao8 x = aux8 x x
		   
aux :: [Int] -> Int -> Int
aux [] n = 0
aux (x:xs) n |n == x = 0 + aux xs n
             |otherwise = x + aux xs n		  
		   
perfeito :: Int -> Bool
perfeito x |aux (funcao8 x) x == x = True
           |otherwise = False 

