minidivisor::Int->[Int]
minidivisor n =[x|x<-[1..n], mod n x ==0, x/=n] 

perfeito::Int->Bool
perfeito n = sum (minidivisor n ) ==n

