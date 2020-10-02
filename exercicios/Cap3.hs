module Cap3 where

-- ex3.1
{--*
data Pergunta = Sim | Nao deriving Show

pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

pergList :: [Pergunta] -> [Int]
pergList pergs = [pergNum n | n <- pergs]

and' :: Pergunta -> Pergunta -> Pergunta
and' Sim Sim = Sim
and' Nao Nao = Nao
and' Nao Sim = Nao
and' Sim Nao = Nao

or' :: Pergunta -> Pergunta -> Pergunta
or' Sim Sim = Sim
or' Nao Nao = Nao
or' Nao Sim = Sim
or' Sim Nao = Sim

not' :: Pergunta -> Pergunta
not' Sim = Nao
not' Nao = Sim
*--}

-- ex3.2
data Temperatura = Celsius | Fahrenheit | Kelvin

converterCelsius :: Double -> Temperatura -> Double
converterCelsius x Fahrenheit = (x - 32) / 1.8
converterCelsius x Kelvin = x - 273.15
converterCelsius x _ = x

converterKelvin :: Double -> Temperatura -> Double
converterKelvin x Celsius = x + 273.15
converterKelvin x Fahrenheit = (x - 32) / 1.8 + 273.15
converterKelvin x _ = x

converterFahrenheit :: Double -> Temperatura -> Double
converterFahrenheit x Celsius = (x * 1.8) + 32
converterFahrenheit x Kelvin = (x - 273.15) * 1.8 + 32
converterFahrenheit x _ = x

-- ex3.3
data Jogada = Pedra | Papel | Tesoura deriving Show
data Resultado = Venceu | Empatou deriving Show

vencedorPartida :: Jogada -> Jogada -> String
vencedorPartida Pedra Papel = "O " ++ show Papel ++ " " ++ show Venceu
vencedorPartida Pedra Tesoura = "A " ++ show Pedra ++ " " ++ show Venceu
vencedorPartida Papel Pedra = "A " ++ show Papel ++ " " ++ show Venceu
vencedorPartida Papel Tesoura = "A " ++ show Tesoura ++ " " ++ show Venceu
vencedorPartida Tesoura Pedra = "A " ++ show Pedra ++ " " ++ show Venceu
vencedorPartida Tesoura Papel = "A " ++ show Tesoura ++ " " ++ show Venceu
vencedorPartida _ _ = show Empatou

-- ex3.4
removerVogais :: String -> String
removerVogais str = [n | n <- str, n /= 'a', n /= 'e', n /= 'i', n /= 'o', n /= 'u', n /= 'A', n /= 'E', n /= 'I', n /= 'O', n /= 'U'] 

-- ex3.5
data UnidadeImperial = Inch | Yard | Foot

converterMetros :: UnidadeImperial -> Double -> Double
converterMetros Inch x = x * 0.0254
converterMetros Yard x = x * 0.9144
converterMetros Foot x = x * 0.3048

converterImperial :: Double -> UnidadeImperial -> Double
converterImperial x Inch = x / 0.0254
converterImperial x Yard = x / 0.9144
converterImperial x Foot = x / 0.3048

-- ex3.6
data Mes = Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro deriving (Show, Eq, Ord)
data Hemisferio = Norte | Sul
data Estacao = Inverno | Primavera | Verao | Outono deriving (Show, Eq, Ord)

checaFim :: Mes -> Int
checaFim Janeiro = 31
checaFim Fevereiro = 28
checaFim Marco = 31
checaFim Abril = 30
checaFim Maio = 31
checaFim Junho = 30
checaFim Julho = 31
checaFim Agosto = 31
checaFim Setembro = 30
checaFim Outubro = 31
checaFim Novembro = 30
checaFim Dezembro = 31

prox :: Mes -> Mes
prox Janeiro = Fevereiro
prox Fevereiro = Marco
prox Marco = Abril
prox Abril = Maio
prox Maio = Junho
prox Junho = Julho
prox Julho = Agosto
prox Agosto = Setembro
prox Setembro = Outubro
prox Outubro = Novembro
prox Novembro = Dezembro
prox Dezembro = Janeiro

estacao :: Mes -> Hemisferio -> Estacao
estacao Dezembro Norte = Inverno
estacao Janeiro Norte = Inverno
estacao Fevereiro Norte = Inverno
estacao Marco Norte = Inverno
estacao Abril Norte = Primavera
estacao Maio Norte = Primavera
estacao Junho Norte = Primavera
estacao Julho Norte = Verao
estacao Agosto Norte = Verao
estacao Setembro Norte = Verao
estacao Outubro Norte = Outono
estacao Novembro Norte = Outono

estacao Dezembro Sul = Verao
estacao Janeiro Sul = Verao
estacao Fevereiro Sul = Verao
estacao Marco Sul = Verao
estacao Abril Sul = Outono
estacao Maio Sul = Outono
estacao Junho Sul = Outono
estacao Julho Sul = Inverno
estacao Agosto Sul = Inverno
estacao Setembro Sul = Inverno
estacao Outubro Sul = Primavera
estacao Novembro Sul = Primavera

-- ex3.7
ehPalindromo :: String -> Bool
ehPalindromo str = str == reverse str

-- ex3.8
ex8 :: [Int] -> [Int]
ex8 x = reverse[n | n <- x, (mod) n 2 /= 0 && (mod) n 7 /= 0, n > 0]

-- ex3.9
ex9 :: String -> String -> String -> (String, String, String)
ex9 x y z = (reverse x, reverse y, reverse z)

-- ex3.10
-- TODO

-- ex3.11
data Binario = Zero | Um deriving Show
data Funcao = Soma2 | Maior | Menor | Mult2

aplicar :: Funcao -> Binario -> Binario -> Binario
aplicar Soma2 Zero Zero = Zero
aplicar Soma2 Um Um = Zero
aplicar Soma2 Zero Um = Um
aplicar Soma2 Um Zero = Um

aplicar Maior Um Zero = Um
aplicar Maior Zero Um = Zero
aplicar Maior Zero Zero = Zero
aplicar Maior Um Um = Zero

aplicar Menor Um Zero = Zero
aplicar Menor Zero Um = Um
aplicar Menor Zero Zero = Zero
aplicar Menor Um Um = Zero

aplicar Mult2 Um Um = Um
aplicar Mult2 Zero Zero = Zero
aplicar Mult2 Zero Um = Zero
aplicar Mult2 Um Zero = Zero

-- ex3.12
convBinarioDecimal :: Binario -> Int
convBinarioDecimal Um = 1
convBinarioDecimal Zero = 0

binList :: [Binario] -> [Int]
binList str = [convBinarioDecimal(aplicar Soma2 n Um) | n <- str]

-- ex3.13
-- TODO

-- ex3.14
data Valido = Sim String | Nao deriving Show

isNomeValido :: String -> Valido
isNomeValido "" = Nao
isNomeValido str = Sim str

-- ex3.15
-- TODO

-- ex3.16
{--*
data Numero = Numero Double | Erro String deriving Show

dividir :: Numero -> Numero -> Numero
dividir _ (Numero 0) = Erro "Impossivel dividir por zero."
dividir  (Numero n1) (Numero n2) = Numero (n1 / n2)
*--}

-- ex3.17
data Cripto = Mensagem String | Cifrado String | Erro deriving Show

encriptar :: Cripto -> Cripto
encriptar (Mensagem "") = Erro
encriptar (Mensagem str) = Cifrado [succ x | x <- str]

decriptar :: Cripto -> Cripto
decriptar (Cifrado "") = Erro
decriptar (Cifrado str) = Mensagem [pred x | x <- str]

-- ex3.18
encriptarTodos :: [Cripto] -> [Cripto]
encriptarTodos vetStr = [encriptar x | x <- vetStr]

-- ex3.19
data Cambio = Euro | Real | Dollar deriving Show
data Moeda = Moeda {val :: Double, cur :: Cambio} deriving Show

convMoeda :: Moeda -> Cambio -> Moeda
convMoeda (Moeda v Euro) Real = Moeda (v * 6.35) Real 
convMoeda (Moeda v Euro) Dollar = Moeda (v * 1.18) Dollar
convMoeda (Moeda v Real) Euro = Moeda (v * 0.16) Euro
convMoeda (Moeda v Real) Dollar = Moeda (v * 0.19) Dollar
convMoeda (Moeda v Dollar) Euro = Moeda (v * 0.85) Euro
convMoeda (Moeda v Dollar) Real = Moeda (v * 5.39) Real

-- ex3.20
converterTodosReal :: [Moeda] -> [Moeda]
converterTodosReal str = [convMoeda x Real | x <- str]

-- ex3.21
maxMoeda :: [Moeda] -> Double
maxMoeda str = maximum [val x | x <- str]
