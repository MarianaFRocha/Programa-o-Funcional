--prova3 2012

--questao1
--a

distancia :: (Float, Float) -> (Float ,Float) -> Float
distancia (x,y) (a,b) = sqrt (((x-y)^2) + ((a-b)^2))

--Questão3
--a

qst2a :: [(String, Int, Int)] -> [(String, Int, Int)]
qst2a [] = []
qst2a [x] = [x]
qst2a (x:xs) = qst2a [y|y<-xs, snd y < snd x] ++ [x] ++ qst2a [y|y<-xs, snd y > snd x]

 