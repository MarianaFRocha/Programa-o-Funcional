questao1 :: Int -> Bool
questao1 x |mod x 2 == 0 = True
           |otherwise = False
		   
questao2 :: Float -> Float -> Float -> Float
questao2 a b c = (-b + sqrt (b^2 - 4*a*c)) / 2*a

questao3 :: Float -> Float -> Float -> Float
questao3 a b c = (-b - sqrt (b^2 - 4*a*c)) / 2*a

questao4 :: Float -> Float -> Float -> Float -> Float -> Float
questao4 a b c d e = (c*b) + ((((a-b)+e)*(c+d))/2)

questao5 :: (Float, Float) -> Bool
questao5 (x,y)|sqrt (x^2 + y^2) > 2 = False
              |otherwise = True 

questao6 :: Float -> Float 
questao6 a = (a*pi)/180

