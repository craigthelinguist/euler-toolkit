

-- Return largest prime factor of a number.
largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = f starting n
  where
    starting  = ceiling (sqrt (fromIntegral n))
    f curr n  = if curr <= 0
                  then -1
                else if (2 `divides` curr)
                  then f (curr-1) n
                else if isPrime curr && curr `divides` n
                  then curr
                else f (curr-2) n

-- Return the prime factors of a number.
primeFactors :: Integer -> [Integer]
primeFactors n
  | n < 2     = []
  | otherwise = [x | x <- (specialCases n) ++ [3,5..upper], isPrime x, x `divides` n]
    where
      specialCases n  = if isPrime n && n /= 2
                          then [2,n]
                        else if isPrime n
                          then [n]
                        else [2]
      upper           = ceiling ( (toRational n) / 2)

-- Return true if a divides b (that is k*a = b for some k).
divides :: Integer -> Integer -> Bool
divides a b = b `mod` a == 0

-- Return True if the input is prime, or false otherwise.
isPrime :: Integer -> Bool
isPrime n
  | n == 2    = True
  | n == 1    = False
  | otherwise = isPrime' n 2
  where isPrime' :: Integer -> Integer -> Bool
        isPrime' n curr
          | curr == 2 = if 2 `divides` n
                          then False
                          else isPrime' n 3
          | otherwise = if curr > (ceiling (sqrt (fromIntegral n)))
                          then True
                        else if curr `divides` n
                          then False
                        else
                          isPrime' n (curr+2)

-- Find the first n primes.
sieve :: Integer -> [Integer]
sieve n = reverse (sieve' 2 n [])
  where sieve' :: Integer -> Integer -> [Integer] -> [Integer]
        sieve' curr stop primes
          | toInteger (length primes) == n  = primes
          | curr == 2             = sieve' 3 stop (2:primes)
          | otherwise             = if isPrime curr
                                    then sieve' (curr+2) stop (curr:primes)
                                    else sieve' (curr+2) stop primes
