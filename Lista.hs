--Andrade Hernández Carlos

--LONGITUD
longitud :: [a] -> Int 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--Suma de los numeros
sumaNumeros:: Num a => [a] -> a
sumaNumeros [] = 0
sumaNumeros (x:xs) = x + sumaNumeros xs

--Maximo
maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (x:xs) = if x > maximo xs
                then x
                else maximo xs
                
--Indices
indiceDe :: Int -> [a] -> a
indiceDe q []= error "La lista es vacía"
indiceDe 0(x:xs) = x 
indiceDe q (x:xs) = if q > longitud(x:xs) ||  q < 0
                    then error "El indice no coincide con la lista"
                    else indiceDe(q-1)xs
--Insertar elemento
insertarElemento :: a -> [a] -> Bool -> [a]
insertarElemento e xs True = (e:xs)
insertarElemento e xs False = xs ++ [e]

--reversa
reversa:: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

--palindromo
esPalindromo:: Eq a =>[a] -> Bool
esPalindromo [] = True
esPalindromo xs = xs == reversa xs

--contenido para las listas, para ver si un elemento está en una lista 
contenido :: Eq a => a -> [a] -> Bool
contenido elem [] = False
contenido elem (x:xs) = if elem == x 
                        then True
                        else contenido elem xs
--AConjunto para eliminar los repetidos en una lista
aConjunto :: Eq a => [a] -> [a]
aConjunto [] = []
aConjunto (x:xs) = if contenido x xs
                   then aConjunto xs 
                   else (x:(aConjunto xs))

--interseccion 
--Esta hace que la interseccion se quede en una lista 
interseccionA :: Eq a => [a] -> [a] -> [a]
interseccionA [] xs = []
interseccionA (x:xs) ys = if contenido x ys
                         then (x:(interseccionA xs ys))
                         else interseccionA xs ys
--Esta hace que se cree una interseccion sin repetidos                         
interseccion :: Eq a => [a] -> [a] -> [a]
interseccion xs ys = interseccionA (aConjunto xs) (aConjunto ys)

--UNION
unionA :: Eq a => [a] -> [a] -> [a]
unionA [] xs = xs
unionA (x:xs) ys = if contenido x ys 
                   then unionA xs ys 
                   else (x:(unionA xs ys))
union :: Eq a => [a] -> [a] -> [a]
union xs ys = aConjunto(unionA xs ys) 

--Diferencia simétrica 
--diferencia 
diferencia :: Eq a => [a] -> [a] -> [a]
diferencia [] xs = []
diferencia (x:xs) ys = if contenido x ys
                       then diferencia xs ys
                       else (x:(diferencia xs ys))
                       
--Diferencia Simetrica
diferenciaSimetrica :: Eq a => [a] -> [a] -> [a]
diferenciaSimetrica xs ys = union (diferencia xs ys) (diferencia ys xs)


--Producto cruz x=[1,2,3],y=[4,5,6] | (x,y)
productoCruz :: [a] -> [a] -> [(a,a)]
productoCruz [] [] = []
productoCruz xs [] = []
productoCruz xs ys = [(x,y) | x <- xs , y <- ys]

--Punto extra
--Divisores
divisible :: Int -> Int -> Bool
divisible x y = (mod x y ) == 0 

--Divisores
divisores :: Int -> [Int]
divisores x = [y | y <- [1..x], divisible x y ]

--CONJUNCION ^
conjuncion :: Bool -> Bool -> Bool
conjuncion True True = True
conjuncion p q = False

--DISYUNCION
disyuncion :: Bool -> Bool -> Bool
disyuncion True q = True
disyuncion p True = True
disyuncion p q = False

--IMPLICACION 
implicacion :: Bool -> Bool -> Bool
implicacion False q = True
implicacion p True = True 
implicacion p q = False

--BICONDICIONAL 
dobleImplica :: Bool -> Bool -> Bool
dobleImplica True True = True
dobleImplica False False = True
dobleImplica p q = False


                             
