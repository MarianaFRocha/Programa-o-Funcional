--Listas 02/03/2013
questao1 :: [Integer] -> [Integer]
questao1 n = [x|[1..n]->x, mod n x == 0

digitos :: [Strins] -> [Char]
digitos x = [x|xs -> x, x >= '0' && x=<'9']

primeiro :: [(Integer, Integer)] -> [Integer]
primeiro xs = [fst x| x<-xs]
segundo :: [(Integer, Integer)] -> [Integer]
segundo xs = [snd x| x<-xs]
total :: [(Integer, Integer)] -> [Integer]
total x = primeiro x ++ segundo x

homem :: [(Integer, Char)] -> [(Integer, Char)]
homem x = [x | xs->x, snd x == 'h']

fatorial :: Integer -> Integer 
fatorial 1 = 1
fatorial n = n*fatorial(n-1)

resto :: Intger -> Integer -> Integer
resto x n | x >= n = resto(x-n)n
          | otherwise = n

idade :: [(Integer, Char)] -> [(Integer, Char)]
idade x = [x| xs->x, fst x > 40] 

--Haskell é uma droga 