

questao1 :: Integer -> [Integer] -> [Integer]
questao1 a [] = [a]
questao1 a (x:xs)|x==a=x:(xs)
                 |otherwise = x:(questao1 a xs)
				 
questao2 :: Integer -> [Integer] -> Integer 
questao2 n [] = error "acabou a lista"
questao2 n (x:xs)|n==1=x
                 |otherwise = questao2 (n-1) xs

questao3 :: String -> String
questao3 []=[] 
questao3 (x:xs)|x==' '='_':(questao3 xs)
               |otherwise = x:(questao3 xs)

questao4 :: [Integer] -> [Integer] -> [Integer]
questao4 [] (y:ys) = (y:ys)
questao4 (x:xs) (y:ys) = x:(questao4 xs (y:ys))

--questao5 :: Integer -> [Integer] -> [Integer]
--questao5 a [] = []
--questao5 a (x:xs)|a==0=x
--                 |otherwise = x:(questao5 (a-1)xs)

questao6 :: Integer -> Integer -> [Integer]
questao6 1 x = [x]
questao6 a x = x:(questao6 (a-1) x)

--aux1 :: Integer -> Integer -> [Integer]
--aux1 m n |m==n=m
--         |otherwise = m:(aux1(m-1)n)

--aux2 :: Integer -> Integer -> [Integer]
--aux2 m n |m==n=m
--         |otherwise = m:(aux2 (m+1) n)
	
--questao7 :: Integer -> Integer -> [Integer]
--questao7 m n |m>n = aux1 m n
 --            |m<n = aux2 m n