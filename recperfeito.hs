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