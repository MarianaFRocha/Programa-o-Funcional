--Ordenaçao

meu_qsort :: [Integer] -> [Integer]
meu_qsort [] = []
meu_qsort [x] =[x]
meu_qsort (x:xs) = meu_qsort [y| y<-xs, y<x] ++ [x] ++ meu_qsort [y|y<-xs , y>x]

bolha :: [Int] -> [Int]
bolha [] = []
bolha [x] = [x] 
bolha (x:xs) | x>head xs = head xs :(bolha(x:tail xs))
             | otherwise = x :(bolha xs)

aplicabolha :: [Int] -> Int -> [Int]
aplicabolha xs n |n>0 = aplicabolha ys (n-1)
                 |otherwise = xs
				 where ys = bolha xs

bolhasort :: [Int] -> [Int]
bolhasort xs = aplicabolha xs (length xs)

--questao1

funcao1 :: [Int] -> Bool
funcao1 [x] = True
funcao1 [] = True
funcao1 (x:xs)|x<head xs = funcao1 xs
              |otherwise = False

--questao2

funcaoqsort :: [(Int, String)] -> [(Int, String)]
funcaoqsort [] = []
funcaoqsort [x] = [x]
funcaoqsort (x:xs) = funcaoqsort [y|y<-xs, fst y < fst x] ++ [x] ++ funcaoqsort [y| y<-xs , fst y > fst x]

funcaoaux :: [(Int, String)] -> [(Int, String)]
funcaoaux xs = [x|x<-(funcaoqsort xs), fst x >= 1, fst x <= 50]

questao2 :: [(Int, String)] -> [String]
questao2 xs = [ snd x | x<-(funcaoaux xs)]

--questao3

bolha3 :: [Int] -> [Int]
bolha3 [] = [] 
bolha3 [x] = [x]
bolha3 (x:xs) |x > head xs = head xs:( bolha3 (x:tail xs))
			 |otherwise = x:(bolha3 xs)

contador :: [Int] -> Int -> Int
contador [x] n = n
contador (x:xs) n |x > head xs = contador (x:(tail xs)) (n+1)
                  |otherwise = contador xs n
			 
aplica :: [Int] -> Int -> Int -> Int
aplica xs i n |i>0 = aplica ys (i-1) a
              |otherwise = n 
			   where a = contador xs n
			         ys = bolha3 xs
					 
funcaofinal :: [Int] -> Int
funcaofinal xs = aplica xs (length xs) 0
