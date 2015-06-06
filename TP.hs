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

--frec msj devuelve la lista de los promedios
frec :: String -> [Float]
frec msj = reverse (aux 25 msj) 

{- Esta función auxiliar recorre los valores que representan
 los 26 caracteres del alfabeto en minúscula y calcula el porcentaje
-}
aux 0 msj = [porcentaje 0 msj]
aux x msj = porcentaje x msj : (aux (x-1) msj)

--Esta es la función que hace el calculo del porcentaje
porcentaje char msj | cantMinusc msj == 0 = 0
porcentaje char msj | otherwise = fromIntegral(contar (natALet char) msj) / fromIntegral(cantMinusc msj)

rotar :: Int -> [a] -> [a]
rotar 0 msj = msj
rotar n (hmsj:tmsj) = rotar (n-1) (tmsj ++ [hmsj])

--Chi2. Asumo que ambas listas tienen que ser del mismo tamaño.
chi2 :: [Float] -> [Float] -> Float
chi2 [] _ = 0
chi2 _ [] = 0
chi2 (x:tx) (y:ty) = ((x-y)^2 ) / y + (chi2 tx ty)

{- 7)
a) Es fácil, es solamente llamar la función frec con el texto.
b) Acá es donde se pone picante.
-}