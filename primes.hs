primeFactors n = 1:[x | x <- 2:[3,5..upper], mod n x == 0] ++ [n]
				 where upper = ceiling (sqrt (fromIntegral n))

isPrime n = length (primeFactors n) == 2

firstNPrimes n = findPrimes 2 n []

findPrimes number stop xs
	| length xs == stop		= xs
	| number ==	2			= findPrimes 3 stop [2]
	| otherwise				=
		if isPrime number
			then findPrimes (number + 2) stop (number:xs)
			else findPrimes (number + 2) stop xs