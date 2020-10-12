module Cap4 where

-- ex4.1
mdDouble :: [Double] -> Double
mdDouble [] = 0
mdDouble str = (foldl (+) 0 str) / (fromIntegral $ length str)

-- ex4.2
ehPalindromo :: String -> Bool
ehPalindromo str = reverse str == str

filtrarPalindromo :: [String] -> [String]
filtrarPalindromo str = filter (ehPalindromo) str

-- ex4.3
ehPar :: Int -> Bool
ehPar n = even n

ehImpar :: Int -> Bool
ehImpar n = odd n

filtrarPares :: [Int] -> [Int]
filtrarPares str = filter (ehPar) str

filtrarImpares :: [Int] -> [Int]
filtrarImpares str = filter (ehImpar) str

-- ex4.4
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], mod n x == 0]

ehPrimo :: Int -> Bool
ehPrimo n = 
  2 == length (divisores n)

filtrarPrimos :: [Int] -> [Int]
filtrarPrimos str = filter (ehPrimo) str

-- ex4.5
ehMultiplo4 :: Int -> Bool
ehMultiplo4 n = (mod) n 4 == 0

dobraTodos :: [Int] -> [Int]
dobraTodos str = map (*2) (filter (ehMultiplo4) str) 

-- ex4.6
reverso :: String -> String
reverso str = reverse str

func :: (String -> String) -> String -> String
func f s = s ++ f s

-- ex4.7
data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo deriving (Show, Eq)

ehTerca :: Dia -> Bool
ehTerca dia = dia == Terca

filtrarTercas :: [Dia] -> [Dia]
filtrarTercas str = filter (ehTerca) str

-- ex4.8
data Cambio = Real | Dolar deriving (Show, Eq)
data Dinheiro = Dinheiro {valor :: Double, correncia :: Cambio} deriving (Show, Eq)

converterParaDolar :: Dinheiro -> Dinheiro
converterParaDolar (Dinheiro v Real) = Dinheiro (v * 0.18) Dolar
converterParaDolar (Dinheiro v _) = Dinheiro v Dolar

converterTodosDolar :: [Dinheiro] -> [Dinheiro]
converterTodosDolar str = map (converterParaDolar) str

converterParaReal :: Dinheiro -> Dinheiro
converterParaReal (Dinheiro v Dolar) = Dinheiro (v * 5.53) Real
converterParaReal (Dinheiro v _) = Dinheiro v Real

converterTodosReal :: [Dinheiro] -> [Dinheiro]
converterTodosReal str = map (converterParaReal) str

ehDolar :: Dinheiro -> Bool
ehDolar (Dinheiro v Dolar) = True
ehDolar (Dinheiro v _) = False

filtrarTodosDolares :: [Dinheiro] -> [Dinheiro]
filtrarTodosDolares str = filter (ehDolar) str

contarDolares :: [Dinheiro] -> Int
contarDolares str = length (filter (ehDolar) str)

-- ex4.9

-- ex4.10
-- TODO