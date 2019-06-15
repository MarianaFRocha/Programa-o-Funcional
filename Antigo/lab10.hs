--Busca e Ordenação

boolbin :: [Int] -> Int -> Bool
boolbin [] y = False
boolbin xs y |(xs!!n)>y = boolbin (fst_half xs) y
             |(xs!!n)==y=True
			 |(xs!!n)<y = boolbin (snd_half xs) y
			 where n = div (length xs) 2
			       fst_half xs = [xs!!i|i<-[0..(n-1)]]
			       snd_half xs = [xs!!i|i<-[(n+1)..((length xs)-1)]]
						   
busca_binaria :: [Int] -> Int -> Int -> Int -> Int
busca_binaria xs y i f |i>f = -1
                       |(xs!!n) == y = n
					   |(xs!!n) > y = busca_binaria xs y i (f-1)
					   |(xs!!n) < y = busca_binaria xs y (i+1) f
					   where n = div (i+f) 2

busca_binaria_final :: [Int] -> Int -> Int
busca_binaria_final xs y = busca_binaria xs y 0 ((length xs)-1)

busca_linear_bool :: [Int] -> Int -> Bool
busca_linear_bool [] y = False
busca_linear_bool (x:xs) y|x==y=True
                          |otherwise = busca_linear_bool xs y
						  
buscal :: [Int] -> Int -> Int -> Int
buscal [] y i = -1
buscal xs y i|head xs == y = i
             |otherwise = buscal (tail xs) y (i+1)

linearfinal :: [Int] -> Int -> Int
linearfinal xs y = buscal xs y 0

--Questão
--a
funcao1 :: [(Float, Float, String, Float)] -> [(Float, Float, String, Float)]
funcao1 [] = []
funcao1 [x] = [x]
funcao1 (x:xs) = funcao1[(e,f,g,h)|(e,f,g,h)<-xs, h<d] ++ [x] ++ funcao1[(e,f,g,h)|(e,f,g,h)<-xs, h>d]
                        where (a,b,c,d) = x
						
--b
funcao2 :: [(Float, Float, String, Float)] -> Float -> Int -> Int -> [(Float, Float, String, Float)]
funcao2 xs y i f|n==0=[]
                |d==y=[(a,b,c,d)]
				|d>y=funcao2 xs y i (n-1)
				|d<y=funcao2 xs y (n+1) f
				where n = div (f+i) 2
				      (a,b,c,d)=(xs!!n)
					  
funcao2final :: [(Float, Float, String, Float)] -> Float -> [(Float, Float, String, Float)]
funcao2final xs y = funcao2 xs y 0 ((length xs)-1)

--c
