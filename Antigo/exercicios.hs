--Exercicios Propostos - pag. 83

maior :: [Int] -> Int
maior [a] = a
maior (x:xs) |x > (maior xs) = x 
                |otherwise = maior xs
				
posicao :: [Int] -> Int -> Int
posicao [] n = 0
posicao (x:xs) n |n==x=1
				 |otherwise = 1+ (posicao xs n)
				
questao1::[Int] -> (Int,Int)
questao1 xs = (maior xs, posicao (xs) (maior xs))