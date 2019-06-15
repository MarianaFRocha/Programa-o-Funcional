-- Número 1

-- função que calcula a distancia entre dois pontos
exe1a (x1,y1) (x2,y2) = sqrt(((x1-x2)^2)+((y1-y2)^2))


-- Número 3

-- função auxiliar para pegar elementos da tupla
pega1 (a,b,c) = a
pega2 (a,b,c) = b
pega3 (a,b,c) = c

-- letra 

exe3a [] = []
exe3a [a] = [a]
exe3a (x:xs) = exe3a [i|i<-xs, pega2 i < pega2 x]++[x]++ exe3a [i|i<-xs, pega2 i > pega2 x] 

exe3a' (x:xs) = pega1 (last (exe3a(x:xs)))

-- letrab

exe3b [] y = "Naum Encontrado"
exe3b xs y | pega2 (xs!!n) > y = exe3b fst_half y
	   | pega2 (xs!!n) == y = pega1 (xs!!n)
	   | otherwise = exe3b snd_half y
	   where n = div (length xs) 2
		 fst_half = [xs !! i | i<-[0..(n-1)]]
		 snd_half = [xs !! i | i<-[(n+1)..((length xs)-1)]]

-- letra d

exe3d [] y = "Naum Encontrado"
exe3d xs y | pega3 (xs!!n) > y = exe3c fst_half y
	   | pega3 (xs!!n) == y = pega1 (xs!!n)
	   | otherwise = exe3c snd_half y
	   where n = div (length xs) 2
		 fst_half = [xs !! i | i<-[0..(n-1)]]
		 snd_half = [xs !! i | i<-[(n+1)..((length xs)-1)]]


-- função auxiliar para ordenar lista

qsort [] = []
qsort [a] = [a]
qsort (x:xs) = qsort [i|i<-xs,i<x]++[x]++qsort [i|i<-xs,i>x]

-- Número 4

--letra a
exe4a xs = [i|i<-(qsort xs), mod i 2 == 0]++[i|i<-(qsort xs), mod i 2 /= 0]

-- Número 5

-- letra 
exe5a xs = qsort [sum i | i<-xs]

-- Função auxiliar que retorna o menor elemento da lista
-- obs.: esta função faz a mesma coisa que a função minimum

menor [a] = a
menor (x:xs)| x < head xs = menor (x: tail xs)
	    | otherwise = menor xs

--letra b
exe5b xs = menor [menor i | i <- xs]

-- letra d
exe5d xs x = [i|i<-xs, sum i > x]



