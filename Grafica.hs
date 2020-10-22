type Grafica = ([Integer],[(Integer,Integer,Float)])

--Funcion incidencias (Cunántas aristas van hacia él)
incidencias :: Grafica -> Integer -> [Integer]
incidencias (xs,[]) n = []
incidencias (xs,((a,b,w): ys)) n = if n == b
                                 then [a] ++ incidencias (xs, ys) n
                                 else incidencias(xs, ys) n


--Funcion adyacencias (Arista a otra arista)
adyacencias :: Grafica -> Integer -> [Integer]
adyacencias (xs,[]) n = []
adyacencias (xs,((a,b,w): ys)) n = if n == a
                                 then [b] ++ adyacencias (xs, ys) n
                                 else adyacencias(xs, ys) n

--Función para ver si un elemento esta contenido en un conjunto
contenido :: Eq a => a -> [a] -> Bool
contenido elem [] = False
contenido elem (x:xs) = if elem == x
                        then True
                        else contenido elem xs                                

--Ingrado Los que inciden en el
ingrado :: Grafica -> Integer -> Integer 
ingrado (xs,[]) n = 0
ingrado (xs,((a,b,w): ys)) n = if n == b
                               then 1 + (ingrado (xs,ys) n)
                               else ingrado (xs,ys) n
                                  
--Exgrado Los que salen de el
trayectorias :: Grafica -> Integer -> Integer
trayectorias (xs,[]) n = 0 
trayectorias (xs,((a,b,w): ys)) n = if n == a
                                    then 1 + (trayectorias (xs,ys) n)   
                                    else trayectorias (xs,ys) n
--por si Luis ocupa el metodo "exgrado" en lugar de "trayectorias"
exgrado :: Grafica -> Integer -> Integer
exgrado (xs,[]) n = 0 
exgrado (xs,((a,b,w): ys)) n = if n == a
                                    then 1 + (exgrado (xs,ys) n)   
                                    else exgrado (xs,ys) n                                    
--Dominio 
dominio :: Grafica -> [Integer]
dominio (xs,[]) = []
dominio (xs,((a,b,w): ys)) = [a] ++ dominio(xs, ys)

--imagen
imagen :: Grafica -> [Integer]
imagen (xs,[]) = []
imagen (xs,((a,b,w): ys)) = [b] ++ imagen(xs, ys)


--LONGITUD
longitud :: [a] -> Int 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--AConjunto para eliminar los repetidos en una lista
aConjunto :: Eq a => [a] -> [a]
aConjunto [] = []
aConjunto (x:xs) = if contenido x xs
                   then aConjunto xs
                   else (x:(aConjunto xs))
--Para ver si es funcion
esFuncion :: Grafica -> Bool
esFuncion (xs,[]) = False
esFuncion g = if longitud (aConjunto(dominio g)) /= longitud(dominio g)
            then False
            else True

--Contenida auxiliar 
estaContenida :: (Integer, Integer) -> Grafica -> Bool
estaContenida (z,y) (xs, []) = False
estaContenida (z,y) (xs,((a,b,w): ys)) = if z == a && y == b
                                         then True
                                         else estaContenida (z,y) (xs,ys)

--  Es reflexiva (x,x)
esReflexiva :: Grafica -> Bool
esReflexiva ([],ys) = True
esReflexiva (x:xs, ys) = if estaContenida (x,x) (xs,ys) 
                         then esReflexiva (xs,ys)
                         else False 

--Funcion para quitar tuplas                           
quitar :: (Integer, Integer, Float) -> [(Integer, Integer, Float)] -> [(Integer, Integer, Float)]
quitar (x,y,w) ys= [(z,u,k)| (z,u,k) <- ys, (z,u,k) /= (x,y,w)]

-- Es Simetrica 
esSimetrica :: Grafica -> Bool
esSimetrica (xs,[]) = True
esSimetrica (xs,((a,b,w): ys)) = if (estaContenida (b,a) (xs,ys))
                                 then esSimetrica (xs, (quitar (b,a,w) ys))
                                 else False

--Composicion de una grafica

composicionaux :: Grafica -> Grafica -> [(Integer, Integer, Float)]
composicionaux (xs,ys) (zs,us) = [(a,y,c+z)| (a,b,c) <- us, (x,y,z) <- ys, b==x ]

todosVertices :: [(Integer, Integer, Float)] -> [Integer]
todosVertices xs = ([a| (a,b,c) <- xs] ++ [b| (a,b,c) <-xs])

composicion :: Grafica -> Grafica -> Grafica
composicion (xs,ys) (zs,us) = ( aConjunto( todosVertices (composicionaux (xs,ys) (zs,us)))  ,  composicionaux (xs,ys) (zs,us) )


--Antisimetrica 
esAntisimetrica :: Grafica -> Bool
esAntisimetrica (xs, []) = True
esAntisimetrica (xs,((a,b,w): ys)) = if not (estaContenida (b,a) (xs,ys)) || b == a
                                     then esAntisimetrica (xs,(quitar (b,a,w) ys))
                                     else False

--Potencia
potencia :: Grafica -> Integer -> Grafica                                     
potencia x 0 = x
potencia x n = potencia (composicion x x) (n-1)

--UNION
unionA :: Eq a => [a] -> [a] -> [a]
unionA [] xs = xs
unionA (x:xs) ys = if contenido x ys 
                   then unionA xs ys 
                   else (x:(unionA xs ys))
union :: Eq a => [a] -> [a] -> [a]
union xs ys = aConjunto(unionA xs ys) 

--cerradura SImetrica 
cerrSimetrica :: Grafica -> Grafica
cerrSimetrica ([], ys) = ([],[])
cerrSimetrica (xs,((a,b,w): ys)) = if esSimetrica (xs,((a,b,w): ys)) == False
                                   then (xs, union ((a,b,w):ys) [(b,a,w)| (a,b,w) <- ((a,b,w):ys)]) --UNion del mismo conjnto ++ LIsta de comprehensión que solo acepta (b,a,w)
                                   else (xs,((a,b,w): ys))

--Cerradura REflexiva
cerrReflexiva :: Grafica -> Grafica
cerrReflexiva ([], ys) = ([],[])
cerrReflexiva (xs,((a,b,w): ys)) = if esReflexiva (xs,((a,b,w): ys)) == False
                                   then (xs, union ((a,b,w): ys) [(a,a,w)| a <- xs]) --UNion del mismo conjnto ++ LIsta de comprehensión que solo acepta (a,a,w)
                                   else (xs,((a,b,w): ys))                           
--Cerradura transitiva 
--esTransitiva :: Grafica -> Bool
--esTransitiva (xs, []) = True
--esTransitiva (xs,((a,b,w): ys)) = if
--cerrTransitiva :: Grafica -> Grafica
--cerrTransitiva ([],ys) = ([],[])
--cerrTransitiva (xs,((a,b,w): ys))  = if 