-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Zahir Saad
--  Carnet: 21-10600
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--  La IA utilizada fue Deepseek principalmente para verificar la nomenclatura 
--  y sintaxis del codigo de Haskell asi como para saber el correcto uso de 
--  ciertas funciones. 
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1

limpiar :: String -> String
limpiar [] = []
limpiar (c:cs)
    | isAlphaNum c = toLower c : limpiar cs
    | otherwise    = limpiar cs

inverso :: String -> String
inverso "" = ""
inverso n = (last n) : inverso (init n)

esPalindromo :: String -> Bool
esPalindromo frase = (limpiar frase) == (inverso (limpiar frase))


-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
--
-- Mi solución recorre la lista una sola vez porque en cada paso recursivo 
-- procesa únicamente el primer elemento de la lista (la cabeza y:ys) y 
-- actualiza el máximo y el mínimo acumulados. No hay llamadas a funciones 
-- como 'head' o 'tail' que fuercen a recorrer la lista de nuevo.

-- Función auxiliar con acumuladores (maxActual, minActual)
buscarExtremos :: Ord a => [a] -> a -> a -> (a, a)
buscarExtremos [] maxAct minAct = (maxAct, minAct)
buscarExtremos (y:ys) maxAct minAct = 
    let nuevoMax = if y > maxAct then y else maxAct
        nuevoMin = if y < minAct then y else minAct
    in buscarExtremos ys nuevoMax nuevoMin

mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (x:xs) = Just (buscarExtremos xs x x)

-- A3

leerHora :: String -> Either String (Int, Int)
leerHora [a, b, ':', c, d]
    | not (isDigit a && isDigit b && isDigit c && isDigit d) = Left "formato invalido"
    | digitToInt a * 10 + digitToInt b >= 24 = Left "hora fuera de rango"
    | digitToInt c * 10 + digitToInt d >= 60 = Left "minutos fuera de rango"
    | otherwise = Right (digitToInt a * 10 + digitToInt b,digitToInt c * 10 + digitToInt d)

leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int
duracion h1 h2 = 
    case leerHora h1 of
        Left err1 -> Left err1 
        
        Right (h1_h, h1_m) -> 
            case leerHora h2 of
                Left err2 -> Left err2 
                
                Right (h2_h, h2_m) -> 
                    let minutos1 = h1_h * 60 + h1_m
                        minutos2 = h2_h * 60 + h2_m
                    in if minutos2 >= minutos1
                       then Right (minutos2 - minutos1) -- Mismo día
                       else Right ((24 * 60) - minutos1 + minutos2) -- Día siguiente

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral umbral xs =
  let (suma, cuenta) = foldr paso (0.0, 0) xs
      paso x (s, n)
        | x > umbral = (s + x, n + 1)
        | otherwise  = (s, n)
  in if cuenta == 0
       then Nothing
       else Just (suma / fromIntegral cuenta) --fromIntegral es para transformar "cuenta" de entero a double

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo (f, c) =
  [ (f + df, c + dc)
  | (df, dc) <- [(-2,-1), (-2,1), (-1,-2), (-1,2),
                 (1,-2),  (1,2),  (2,-1),  (2,1)]
  , 1 <= f + df, f + df <= 8
  , 1 <= c + dc, c + dc <= 8
  ]

enDosSaltos :: (Int, Int) -> Int
enDosSaltos pos =
  length (foldr agregar [] destinos)
  where
    destinos = [ q
               | p <- movimientosCaballo pos
               , q <- movimientosCaballo p
               ]

    agregar q acc
      | q `elem` acc = acc
      | otherwise    = q : acc

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap f = foldr (\x acc -> f x : acc) []

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter p = foldr (\x acc -> if p x then x : acc else acc) []

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl f z xs = foldr (\x g -> \acc -> g (f acc x)) id xs z  --id es la función identidad (\x -> x)
