questao1 :: [Integer] -> Integer
questao1 xs = [x|x<-xs, fst==x]

questao2 :: [Integer] -> [Integer] -> [Integer]
questao2 xs ys = [x++y|x<-xs && y<-ys]