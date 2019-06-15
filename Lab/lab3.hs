quest1 :: [Int] -> Bool
quest1 [] = True
quest1 [x] = True
quest1 (x:xs) |x <= (head xs) = quest1 xs
	      |otherwise = False
	    
		    
quest2 :: [(Int, String)] -> [(Int, String)]
quest2 [] = []
quest2 [x] = [x]
quest2 (x:xs) = quest2 [(a,b)|(a,b)<-xs, a<c] ++ [x] ++ quest2 [(a,b)|(a,b)<-xs,a>=c]
  where (c,d) = x
	
quest2f :: [(Int, String)] -> [String]
quest2f xs = [b|(a,b)<-(quest2 xs)]

quest3 :: String -> String
quest3 [] = []
quest3 (x:xs)  |(x>='A' && x<='Z') || (x>='a'&& x<='z') = x:(quest3 xs) 
	       |otherwise = []
	       
quest3f :: String -> String
quest3f [] = []
quest3f (x:xs) | (x>='A' && x<='Z') || (x>='a'&&x<='z') = quest3f xs
	       |otherwise = xs
	       
q3f :: [String] -> [String]
q3f [] = []
q3f (x:xs) |x == "" = q3f xs
	   |otherwise = x:(q3f xs)
	       
q3final :: String -> [String]
q3final [] = []
q3final xs = q3f (quest3 xs :(q3final (quest3f xs)))


q3b :: [String] -> [String] -> [String]
q3b [] [] = []
q3b [x] [] = []
q3b [] xs = xs
q3b ys (x:xs)|(head ys)==x = q3b ys xs
	     |otherwise = x:(q3b ys xs)
		 
q3b2 :: [String] -> [String] -> [String]
q3b2 [] [] = []
q3b2 [] xs = xs
q3b2 ys [] = []
q3b2 ys xs = (q3b2 (tail ys) (q3b ys xs))
		 


		




