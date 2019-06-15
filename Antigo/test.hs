qsort :: [Int] -> [Int]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort[y|y<-xs,y<x] ++ [x] ++ qsort[y|y<-xs,y>x] 

bolha :: [Int] -> [Int]
bolha [] = []
bolha [x] = [x]
bolha (x:xs) |x>head xs = (head xs):(bolha (x:tail xs))
             |otherwise = x:(bolha xs)
			 
aplicabolha :: [Int] -> Int -> [Int]
aplicabolha xs n | n>0 = aplicabolha ys (n-1)
                 |otherwise = xs
				 where ys = bolha xs
				 
bolhafinal :: [Int] -> [Int]
bolhafinal xs = aplicabolha xs (length xs)


buscalbool :: [Int] -> Int -> Bool
buscalbool [] n = False
buscalbool (x:xs) n |x == n = True
                    |otherwise = buscalbool xs n
					
buscal :: [Int] -> Int -> Int -> Int
buscal [] y i = -1
buscal xs y i|head xs == y = i
             |otherwise = buscal (tail xs) y (i+1)

linearfinal :: [Int] -> Int -> Int
linearfinal xs y = buscal xs y 0

buscabinaria :: [Int] -> Int -> Int -> Int -> Int
buscabinaria xs y i f |i>f = -1
                      |(xs!!n)==y=n
                      |(xs!!n)<y = buscabinaria xs y (i+1) f
					  |(xs!!n)>y = buscabinaria xs y i (f-1)
					  where n = div (i+f) 2
					 
binariafinal :: [Int] -> Int -> Int
binariafinal xs y = buscabinaria xs y 0 ((length xs)-1)

boolbin :: [Int] -> Int -> Bool
boolbin [] y = False
boolbin xs y |(xs!!n)>y = boolbin (fst_half xs) y
             |(xs!!n)==y=True
			 |(xs!!n)<y = boolbin (snd_half xs) y
			 where n = div (length xs) 2
			       fst_half xs = [xs!!i|i<-[0..(n-1)]]
			       snd_half xs = [xs!!i|i<-[(n+1)..((length xs)-1)]]

				   


