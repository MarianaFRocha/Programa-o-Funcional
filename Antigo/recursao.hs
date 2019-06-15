meulenght :: [Integer] -> Int
meulenght [] = 0
meulenght (x:xs) = 1 + meulenght xs

meusum :: [Integer] -> Integer
meusum [] = 0 
meusum (x:xs) = x + meusum xs

meuinsert :: Integer -> [Integer] -> [Integer]
meuinsert a [] = [a]
meuinsert a (x:xs) | a < x = a :(x:xs)
                   | otherwise = x:(meuinsert a xs)
				   
--ok