--ex2
quadrado x y = [a^2 | a<-[x..y]]

--ex3
seleciona_impares l1 = [n | n <- l1, odd n]

--ex4
tabuada x = [x*y | y<-[1..10]]


--ex5
verifica:: Int -> Int
verifica ano | mod ano 4 == 0 && (mod ano 100 /= 0 || mod ano 400 == 0) = 1
             | otherwise = 0

bissextos [] = []
bissextos (x:xs) 
	|verifica x == 1 = [x] ++ bissextos xs
	|otherwise = bissextos xs

--ex6
sublistas l1 = [n | n<- concat l1]

--ex8
npares [] = 0
npares (x:xs) | mod x 2==0 = 1+ npares xs
	|otherwise = npares xs

--ex9
produtorio [] = 1
produtorio (x:xs) = x * produtorio xs 

--ex10
comprime [] = []
comprime (x:xs) = x ++ comprime xs

