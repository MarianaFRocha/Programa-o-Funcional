menor ::   [Integer] -> Integer
menor [x] = x
menor (x:xs) | x>head xs = menor xs
             | otherwise = menor (x:tail xs)
			 
remove :: Integer -> [Integer] -> [Integer]
remove m [x]|m==x=[]
            |otherwise = [x]
remove m [] =[]
remove m (x:xs)|m==x=xs
               |otherwise = x:(remove m xs)
			   
bolha :: [Int] -> [Int]
bolha [] = []
bolha [x] = [x] 
bolha (x:xs) | x>head xs = head xs :(bolha(x:tail xs))