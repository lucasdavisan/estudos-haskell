module Cap2 where

ex1a :: [Int]
ex1a = [11^exp | exp <- [0..6]]

ex1b :: [Int]
ex1b = [n | n <- [1..39], mod n 4 /= 0]

ex1c :: [String]
ex1c = ["A" ++ ch ++ "BB" | ch <- ["a", "b", "c", "d", "e", "f", "g"]]

ex1d :: [Int]
ex1d = [n | n <- [5, 8..41], n /= 14, n/= 23, n /= 35]

ex1e :: [Float]
ex1e = [1 / 2^n | n <- [0..5]]

ex1f :: [Int]
ex1f = [9 * n + 1 | n <- [0..7]]

ex1g :: [Int]
ex1g = [n | n <- [2, 4..30], n /= 6, n /= 14, n /= 20, n /= 26]

ex1h :: [Char]
ex1h = [n | n <- ['@','A'..'L'], n /= 'B', n /= 'F', n /= 'H', n /= 'I', n /= 'K']

ex2 :: [Char] -> Bool
ex2 str = mod (length str) 2 == 0

ex3 :: [String] -> [String]
ex3 str = reverse str

ex4 :: [String] -> [Int]
ex4 input = [length (input !! n) | n <- [0..4], mod (length (input !! n)) 2 /= 0]

ex5 :: [String] -> String
ex5 str = last (reverse str)

ex6 :: String -> Bool
ex6 str = reverse str == str

ex7 :: Int -> (Int, Int, Int, Int)
ex7 num = (num * 2, num * 3, num * 4, num * 5)