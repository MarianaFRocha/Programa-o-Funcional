aux1 :: Int -> Int -> Int -> Int -> Bool
aux1 a b c d |a <= 8 && b <= 8 && c <= 8 && d <= 8 = True 
             |otherwise = False
questao1a :: (Int, Int) -> (Int, Int) -> Int
questao1a (a,b) (c,d)|aux1 a b c d == True = ((b-d)+1)*2
                     |otherwise = -1
                       
questao1b :: Int -> Int
questao1b n = n*4*2 

questao2a :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Bool
questao2a ((a,b) , (c,d)) ((x,y), (m,n))|a==x && b==y && c==m && d==n = True
                                        |otherwise = False
										
questao2b :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
questao2b ((a,b) , (c,d)) ((x,y), (m,n)) = ((a + x,b + y), (c + m,d + n )) 

questao2c :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
questao2c ((a,b) , (c,d)) ((x,y), (m,n)) = ((a*x + b*m, a*y + b*n), (c*x + d*m, c*y + d*n))										        

questao2d ::((Int, Int), (Int, Int)) -> Int
questao2d ((a,b) , (c,d)) = (a*d) - (b*c) 

questao3 :: (Int, Int) -> (Int, Int) -> Bool
questao3 (a,b) (c,d)| b == d = True
                    |otherwise = False
					
questao3b :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
questao3b (a,b) (c,d) (x,y)|x == a || x == b || x == c || x == d || y == a || y == b || y == c || y == d = True
                           |otherwise = False
						   
questao4 :: (Int, Int, Int, Int) -> Char
questao4 (a,b,c,d) |a == 0 = 'A'
                   |b == 0 = 'B'
				   |c == 0 = 'C'
				   |d == 0 = 'D'
		
-- Incompleto 			
questao4b :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
questao4b (a,b,c,d) (x,z,y,w)| a == 0 && z == 0 && b == x = True 
                             | a == 0 && y == 0 && c == x = True
							 | a == 0 && w == 0 && d == x = True
							 | b == 0 && y == 0 && c == z = True
							 | b == 0 && w == 0 && d == z = True
							 | c == 0 && w == 0 && d == y = True
							 | otherwise = False