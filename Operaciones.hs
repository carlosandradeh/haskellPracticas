--Andrade Hernández Carlos. Práctica 1 Estructuras Discretas


--Suma
suma :: (Int -> Int -> Int)
suma a b = a + b 

--Resta 
resta :: Int -> Int -> Int
resta a b = a - b 

--Multiplicación 
multiplicacion :: Float -> Float -> Float
multiplicacion a b = a * b 

--División 
division :: Float -> Float -> Float
division a b = a / b 

--Función potencia
potencia :: Int -> Int -> Int
potencia a b = a ^ b    

--Máximo 
maximo :: Float -> Float -> Float -> Float 
maximo a b c = if a > b
                then if a > c
                     then a
                     else c 
                else if b > c
                     then b
                     else c 

--Comparador 
comparador :: Float -> Float -> Int
comparador a b = if a > b
                  then 1
                 else if a < b
                  then -1
                 else 0
                 
        
--Distancia entre dos puntos
distanciaPuntos :: Float -> Float -> Float -> Float -> Float
distanciaPuntos x1 y1 x2 y2 = sqrt(((x2-x1)^2)+((y2-y1)^2))



--Hipotenusa
hipotenusa :: Float -> Float -> Float 
hipotenusa b h = sqrt(((b)^2)+((h)^2))

--Pendiente
pendiente :: (Float -> Float -> Float -> Float -> Float)
pendiente x1 y1 x2 y2 = (y2 - y1)/(x2 - x1)
                 
                  
                 
                




                     