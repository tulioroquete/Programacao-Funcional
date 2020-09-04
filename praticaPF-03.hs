
--ex2
type Ponto = (Float, Float)
dist :: Ponto -> Ponto -> Float
dist (x1, y1) (x2, y2) = sqrt (x*x + y*y)
	where	x = x1 - x2
		y = y1 - y2

--ex4 
fatorial ::Int -> Int
fatorial x 
	| x==0 = 1
	|otherwise = x * fatorial(x-1)

fatorialPadrao:: Int->Int
fatorialPadrao 0 = 1
fatorialPadrao x = x * fatorialPadrao(x-1)

--ex5
fibonacci ::Int->Int
fibonacci x
	|x == 0 = 0
	|x == 1 = 1
	|otherwise = fibonacci(x-2) + fibonacci(x-1)

--ex6 
triangular :: Int->Int
triangular 0 = 0
triangular 1 = 1
triangular x = triangular(x-1) + x


--ex 8
type Par = (Int,Int)
potencia2 :: Par -> Int
potencia2 (2,0) = 1
potencia2 (2,1) = 2
potencia2 (2,n) = potencia(2,n-1)*2 

--ex12
mdcg ::(Int,Int) -> Int
mdcg(m,n) 
	|n==0 = m
	|otherwise = mdcg(n,(mod m n))

mdc::(Int,Int) -> Int
mdc(m,0) = m
mdc(m,n) = mdc(n,(mod m n))

--ex13 
binomialg::(Int,Int)->Int
binomialg(n,k)  
	|k==0 =1
	|k==n =1
	|otherwise = binomialg(n-1,k) + binomialg(n-1,k-1)

binomial::(Int,Int)->Int
binomial (n,0) = 1
binomial(n,k)= if(k==n)
		then 1	
		else binomial(n-1,k)+ binomial(n-1,k-1)

--ex14
[5,4..1]
['a','c'..'e']
[1,4..16]
zip[1,-2..(-11)] [1,5..17]

--ex15
lista a b | a == b = [a]
	|a>b = []
	|otherwise = [a..b]


