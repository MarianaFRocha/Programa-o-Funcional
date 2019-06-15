menor :: [Int] -> Int
menor [x] = x
menor (x:xs) |x > (head xs) = menor xs
             |otherwise = menor (x:(tail xs))
			 
remove :: Int -> [Int] -> [Int]
remove m [] = []
remove m (x:xs) |x == m = xs
                |otherwise = x:(remove m xs)
				
selection_sort :: [Int] -> [Int]
selection_sort [] = []
selection_sort [x] =[x]
selection_sort xs = m:(selection_sort (remove m xs))
                  where m = menor xs

--				  
				  
insere :: [Int] -> Int -> [Int]
insere [] m = [m]
insere (x:xs) m|m<x = m:(x:xs)
               |otherwise = x:(insere xs m) 
			   
insertion_sort :: [Int] -> [Int]
insertion_sort [] = [] 
insertion_sort [x] = [x]
insertion_sort (x:xs) = insere (insertion_sort xs) x

--

qsort :: [Int] -> [Int]
qsort [] = [] 
qsort [x] = [x]
qsort (x:xs) = qsort [y|y<-xs, y<x] ++ [x] ++ qsort [y|y<-xs, y>=x]

--

bolha :: [Int] -> [Int]
bolha [] = [] 
bolha (a:[]) = [a]
bolha (a:b:xs) |a<b = a:(bolha (b:xs))
               |otherwise = b:(bolha (a:xs))
			   
aplicabolha :: [Int] -> Int -> [Int]
aplicabolha xs n |n>0 = aplicabolha (bolha xs) (n-1)
                 |otherwise = xs
				 
bolha_sort :: [Int] -> [Int]
bolha_sort xs = aplicabolha xs (length xs)
 
--

auxpreco :: [(String, Float, Int)] -> (String, Float, Int) -> [(String, Float, Int)]
auxpreco [] y = [y]
auxpreco (x:xs) y |m<b = y:(x:xs)
                  |otherwise = x:(auxpreco xs y)
				  where (a,b,c) = x
				        (l,m,n) = y
						
preco :: [(String, Float, Int)] -> [(String, Float, Int)]
preco [] = [] 
preco [x] = [x]
preco (x:xs) = auxpreco (preco xs) x

pop :: [(String, Float, Int)] -> [(String, Float, Int)]
pop [] = []
pop [x] = [x]
pop (x:xs) = pop [(l,m,n)|(l,m,n)<-xs, n<c] ++ [x] ++ pop [(l,m,n)|(l,m,n)<-xs, n>=c]
            where (a,b,c) = x