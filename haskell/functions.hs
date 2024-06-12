import Data.List (nub)

binomial :: Int -> Int -> Int
binomial n k
    | k == 0 || k == n = 1
    | k > n = 0
    | otherwise = binomial (n - 1) (k - 1) + binomial (n - 1) k

binomial2 :: Int -> Int -> Int
binomial2 n k 
    | k == 0 || k == n = 1
    | k > n = 0
    | otherwise = pascal !! fromIntegral n !! fromIntegral k
    where
        pascal = iterate nextRow [1]
        nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
    where
        (left, right) = splitAt (length xs `div` 2) xs


de :: Int -> Int -> (Int, Int, Int)
de a b
    | b == 0 = (a, 1, 0)
    | otherwise =
        let (z, x1, y1) = de b (a `mod` b)
            x = y1
            y = x1 - (a `div` b) * y1
        in (z, x, y)

factorize :: Int -> Int -> [Int] 
factorize d n 
    | d * d > n = [n]
    | n `mod` d == 0 = d : factorize d (n `div` d)
    | otherwise = factorize (d + 1) n

primeFactors :: Int -> [Int]
primeFactors n    
    | n <= 1 = []
    | otherwise = factorize 2 n

totient :: Int -> Int
totient n
    | n <= 1 = 0
    | otherwise = length [x | x <- [1..n], coprime x n]
        where coprime a b = gcd a b == 1

totient2 :: Int -> Int
totient2 n 
    | n <= 1 = 0
    | otherwise = round $ fromIntegral n * product [1 - 1/fromIntegral p | p <- nub $ primeFactors n] 
        -- nub gets rid of duplicates

primes :: Int -> [Int]
primes n = sieve [2..n]
    where
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]
        sieve [] = []

main :: IO ()
main = do
    print (binomial 10 3)
    print (binomial2 10 3)

    let unsortedList = [5, 3, 8, 6, 2, 7, 4, 1]
    let sortedList = mergeSort unsortedList
    print sortedList  

    let a = 56
    let b = 15
    let (z, x, y) = de a b
    putStrLn $ "z: " ++ show z
    putStrLn $ "x: " ++ show x
    putStrLn $ "y: " ++ show y

    print (primeFactors 50)

    print (primes 50)

    print (totient 100)
    
    print (totient2 100)