minhapot :: Int -> Int -> Int
minhapot x 0 = 1
minhapot x n = x*minhapot x(n-1)

fatorial :: Integer -> Integer 
fatorial 1 = 1
fatorial n = n*fatorial(n-1)

contesp :: String -> Int
contesp xs = length [x|x <- xs, x==' ']

idade :: [(Integer, Char)] -> [(Integer, Char)]
idade xs = [x|x<-xs, fst x > 40] 

primeiro :: [(Integer, Integer)] -> [Integer]
primeiro xs = [fst x| x<-xs]
segundo :: [(Integer, Integer)] -> [Integer]
segundo xs = [snd x| x<-xs]
total :: [(Integer, Integer)] -> [Integer]
total x = primeiro x ++ segundo x

homem :: [(Integer, Char)] -> [(Integer, Char)]
homem xs = [x |x<-xs, snd x == 'h']