soma :: (Int, Int, Int) -> Int
soma (a,b,c) = a*3600 + b*60 + c 

questao1 :: [(Int, Int, Int)] -> [Int]
questao1 xs = [soma x|x<-xs]