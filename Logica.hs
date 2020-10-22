--Andrade Hernández Carlos
--Estructuras Discretas
data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Prop Var
	|Neg Formula
	|Formula :&: Formula
	|Formula :|: Formula
	|Formula :=>: Formula
	|Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

--Negacion
negar :: Formula -> Formula
negar (Prop xs) = Neg(Prop xs)
negar (Neg xs) = xs
negar (xs :|: ys) = (negar xs) :&: (negar ys)
negar (xs :&: ys) = (negar xs) :|: (negar ys)
negar (xs :=>: ys) = (xs) :&: (negar ys)
negar (xs :<=>: ys) = (xs :&: (negar ys)) :|: (ys :&: (negar xs))

--Da la lista de variables de una formula
varListAux :: Formula -> [Var]
varListAux (Prop x ) = [x]
varListAux (Neg xs) = varListAux xs
varListAux (xs :&: ys) = (varListAux xs) ++ (varListAux ys)
varListAux (xs :|: ys) = (varListAux xs) ++ (varListAux ys)
varListAux (xs :=>: ys) = (varListAux xs) ++ (varListAux ys)
varListAux (xs :<=>: ys) = (varListAux xs) ++ (varListAux ys)

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
--VArlist chida
varList :: Formula -> [Var]
varList xs = aConjunto(varListAux(xs))

--Equivalencias
equivalencia :: Formula -> Formula
equivalencia (Prop xs) = Prop xs
equivalencia (Neg xs) = Neg xs
equivalencia (xs :&: ys ) = (equivalencia(xs) :&: equivalencia(ys))
equivalencia (xs :|: ys ) = (equivalencia(xs) :|: equivalencia(ys))
equivalencia (xs :=>: ys ) = (negar(xs) :|: ys)
equivalencia (xs :<=>: ys ) = ((negar(xs):|: ys) :&: (negar(ys) :|: xs))

--Interpretacion
--Buscar VAriables
buscarVar ::Var -> [(Var,Bool)] -> Bool
buscarVar x [] = error "No todas las var estan definidas"
buscarVar x ((y,b):ys) = if x == y
			  then b
			  else buscarVar x ys

interp ::Formula -> [(Var,Bool)] -> Bool
interp (Prop x) xs = buscarVar x xs
interp (Neg xs) ys = not( interp xs ys)
interp (xs :|: ys) zs = (interp xs zs) || (interp ys zs)
interp (xs :&: ys) zs = (interp xs zs) && (interp ys zs)
--Sinterp (xs :=>: ys) zs =  interp(Neg(xs) :|: ys)




--COMBINACIONES
--1.-longitud
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

combina :: [[Bool]] -> Bool -> [[Bool]]
combina [] b = []
combina (x:xs) b = (b:x):(combina xs b)

combinacionesAux :: Int -> [[Bool]] -> [[Bool]]
combinacionesAux 0 xs = xs
combinacionesAux n xs = combinacionesAux (n-1) ((combina xs False)++ (combina xs True))

asignarValor :: [Var] -> [Bool] -> [(Var, Bool)]
asignarValor (x:xs) (y:ys) = (x,y):asignarValor xs ys
asignarValor [] xs = []

asignarInterp :: [Var] -> [[Bool]] -> [[(Var,Bool)]]
asignarInterp xs (y:ys) = (asignarValor xs y: asignarInterp xs ys)
asignarInterp xs [] = []

combinaciones :: Formula -> [[(Var, Bool)]]
combinaciones xs = asignarInterp (varList xs) (combinacionesAux((longitud(varList xs))) [[False],[True]])

--Tabla de verdad
tablaVerdadAux :: Formula -> [[(Var,Bool)]] -> [([(Var,Bool)], Bool)]
tablaVerdadAux xs [] = []
tablaVerdadAux xs (y:ys) = ((y),(interp xs y)):(tablaVerdadAux xs ys)
tablaVerdad :: Formula -> [([(Var, Bool)], Bool)]
tablaVerdad xs = tablaVerdadAux xs (combinaciones xs)
