questao2a :: [(Integer, Char)] -> [(Integer, Char)]
questao2a xs = [x|x<-xs, fst x > 40 && snd x == 'F']

questao2b :: [(Int, Char)] -> [Int]
questao2b xs = [length x|x<-xs, fst x > 40]

--questao2c :: [(Float, Char)] -> [Float]
--questao2c xs = [sum x|x<-xs, fst x > 0 ] 