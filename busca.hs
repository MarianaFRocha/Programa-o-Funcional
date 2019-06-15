blbool :: [Int] -> Int -> Bool
blbool [] n = False
blbool (x:xs) n |x == n = True
                |otherwise = blbool xs n
				
blinear :: [Int] -> Int -> Int
blinear (x:xs) n|x == n = 0
                |otherwise = 1 + (blinear xs n)
				
--Por Recursão

blb :: [Int] -> Int -> Bool
blb xs n = length [x|x<-xs, x == n] > 0

bl :: [Int] -> Int -> Int
bl xs m = head [x|x<-[0..(length xs)], xs!!x == m]

--Busca Binaria 

bbbool :: [Int] -> Int -> Bool
bbbool [] n = False 
bbbool xs y|xs!!n > y = bbbool (sndhalf) y
           |xs!!n == y = True
		   |xs!!n < y = bbbool (fsthalf) y
		   where n = div (length xs) 2
		         fsthalf = [xs!!i|i<-[0..(n-1)]]
		         sndhalf = [xs!!i|i<-[n..((length xs)-1)]]
				 
bb :: [Int] -> Int -> Int -> Int -> Int
bb xs y i f|xs!!n < y = bb xs y (i+1) f
           |xs!!n == y = n
		   |xs!!n > y  = bb xs y i (f-1)
		   where n = div (length xs) 2
		   
bbf :: [Int] -> Int -> Int
bbf xs y = bb xs y 0 ((length xs)-1)