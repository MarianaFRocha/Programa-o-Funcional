-- Questão1

quest1 :: [Int] -> Bool
quest1 [] = True
quest1 [x] = True
quest1 (x:xs) |x <= (head xs) = quest1 xs
              |otherwise = False
			  
-- Questão2

insere :: [(Int, String)] -> (Int, String) -> [(Int, String)]
insere [] n = [n]
insere (x:xs) m|a<c = m:(x:xs)
               |otherwise = x:(insere xs m)
			   where (a,b) = m
			         (c,d) = x
isort :: [(Int, String)] -> [(Int, String)]
isort [] = [] 
isort [x] = [x]
isort (x:xs) = insere (isort xs) x

quest2 :: [(Int, String)] -> [String]
quest2 xs = [b|(a,b)<-ys]
          where ys = isort xs
		  
-- Questão 4

bolha :: [Int] -> [Int] 
bolha [] = []
bolha [x] = [x]
bolha (x:xs)|x>(head xs) = (head xs):(bolha (x:(tail xs)))
            |otherwise = x:(bolha xs)
			
qsort :: [Int] -> [Int]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort [y|y<-xs, y<=x] ++ [x] ++ qsort [y|y<-xs, y>x]

aplicbolha :: [Int] -> [Int] -> Int
aplicbolha [] [] = 0
aplicbolha xs ys |xs==ys = 0
                 |otherwise = 1+(aplicbolha (bolha xs) ys)

quest4 :: [Int] -> Int
quest4 xs = aplicbolha xs (qsort xs)				 
                  