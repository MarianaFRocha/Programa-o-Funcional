-- Questão 1

quest1a :: [(String, Float, Char, Int)] -> Int -> [(String, Float, Char, Int)]
quest1a [] n = []
quest1a (x:xs) n|d == n = x:(quest1a xs n)
                |otherwise = quest1a xs n
				where (a,b,c,d) = x
				
quest1b :: [(String, Float, Char, Int)] -> [(String, Float, Char, Int)]
quest1b [] = []
quest1b [x] = [x]
quest1b (x:xs) = quest1b [(a,b,c,d)|(a,b,c,d)<-xs, b>=f] ++ [x] ++ quest1b [(a,b,c,d)|(a,b,c,d)<-xs, b<f]
               where (e,f,g,h) = x
			   
--quest1ca :: [(String, Float, Char, Int)] -> Float -> Int -> Int -> Int
--quest1ca xs y i f |b==y = n
--                  |b>n = quest1ca xs y i (f-1)
--                  |otherwise = quest1ca xs y (i+1) f
--				   where n = (div (i+f) 2) 
--				   (a,b,c,d) = (xs!!n)
					             
								 
--quest1c :: [(String, Float, Char, Int)] -> Float -> Int
--quest1c xs = quest1ca xs y 0 ((length xs)-1) 

aux2 :: [(String, String)] -> String -> Int
aux2 [] n = 0
aux2 (x:xs) n |(fst x) == n = 1+(aux2 xs n)
              |otherwise = aux2 xs n
			 
aux3 :: [(String, String)] -> (String, String) -> [(String, String)]
aux3 [] n = []
aux3 (x:xs) n |x==n = aux3 xs n
              |otherwise = x:(aux3 xs n) 

	 
			 
quest4 :: [(String, String)] -> [(String, Int)]
quest4 [] = []
quest4 (x:xs) = (fst x, ((aux2 xs (fst x))+1)): quest4 (aux3 xs x)

				 