import Data.Char

esMin :: Char -> Bool 

--esMin x = isLower x
esMin 'ñ' = False
esMin x = (toUpper x) /= x


letANat :: Char -> Int

letANat n = (ord n) - 97


natALet :: Int -> Char

--natALet x | x <= 25 && x >= 0 = (chr (x + 97)) 
--natALet x | otherwise = natALet (mod x 26)
natALet x = chr ( (mod x 26) + 97 )

desplazar :: Int -> Char -> Char

desplazar n l | not (esMin l) = l
desplazar n l | esMin l = natALet ((letANat l) + (mod n 26))
--desplazar n l | (letANat l) + n >= 25 = natALet ((letANat l) + (mod n 26))
--desplazar n l | (letANat l) + n < 25 && n > 0 = natALet ((letANat l) + n)

{- 
natALet y Desplazar
Esto funciona porque el resto es siempre positivo
Por lo tanto, mod K J con K < 0 = J - (mod |K| J)
-}

cantMinusc :: String -> Integer

cantMinusc [] = 0
cantMinusc x | not (esMin (head x)) = cantMinusc (tail x)
cantMinusc x | esMin (head x) = 1 + cantMinusc (tail x)


contar :: Char -> String -> Integer


contar char [] = 0
contar char lista | head (lista) == char = 1 + (contar char (tail lista))
contar char lista | head (lista) /= char = contar char (tail lista)


codificar :: Int -> String -> String 

codificar x [] = []
codificar x (hmsj:tmsj) | not(esMin hmsj) = hmsj : codificar x tmsj
codificar x (hmsj:tmsj) | esMin hmsj = (desplazar x hmsj) : codificar x tmsj


decodificar :: Int -> String -> String

decodificar x [] = []
decodificar x (hmsj:tmsj) | not(esMin hmsj) = hmsj : decodificar x tmsj
decodificar x (hmsj:tmsj) | esMin hmsj = (desplazar (-x)  hmsj) : decodificar x tmsj


--frec :: String -> [Float]

frec msj = reverse (aux 25 msj) 
	where aux x msj = contar x msj : aux (x-1) msj