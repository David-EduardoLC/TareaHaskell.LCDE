--Tarea

-- Ejercicio 1. Definir la función promedio3 tal que (promedio3 x y z) es
-- la promedio aritmética de los números x, y y z.
promedio3 x y z = (x+y+z)/3

-- Ejercicio 2. Definir la función sumaMonedas tal que
-- (sumaMonedas a b c d e) es la suma de las monedas  correspondientes a
sumaMonedas a b c d e= (a*1)+(b*2)+(c*5)+(d*10)+(e*20) 

-- Ejercicio 3. Definir la función volumenEsfera tal que
-- (volumenEsfera r) es el volumen de la esfera de radio r
volumenEsfera r = 4/3*pi*r^3

-- Ejercicio 4. Definir la función areaDeCoronaCircular tal que
-- (areaDeCoronaCircular r1 r2) es el área de una corona circula
areaDeCoronaCircular r1 r2 = pi*(r2^2-r1^2)

-- Ejercicio 5. Definir la función ultimaCifra tal que (ultimaCifra x)
ultimaCifra n= rem n 10

-- Ejercicio 6. Definir la función maxTres tal que (maxTres x y z)
maxTres x y z = max x (max y z)

-- Ejercicio 7. Definir la función rota1 tal que (rota1 xs) es la lista
-- obtenida poniendo el primer elemento de xs al final de la lista.
rota1 []=[]
rota1 xs=tail xs++[head xs] 

-- Ejercicio 8. Definir la función rota tal que (rota n xs) es la lista
-- obtenida poniendo los n primeros elementos de xs al final de la
-- lista.
rota n[]=[]
rota n xs = drop n xs ++ take n xs

-- Ejercicio 9. Definir la función rango tal que (rango xs) es la
-- lista formada por el menor y mayor elemento de xs.
rango[]=[]
rango xs=[maximum xs, minimum xs]

-- Ejercicio 10. Definir la función palindromo tal que (palindromo xs) se
-- verifica si xs es un palíndromo; es decir, es lo mismo leer xs de
-- izquierda a derecha que de derecha a izquierda.
palindromo xs= xs == reverse xs

-- Ejercicio 11. Definir la función interior tal que (interior xs) es la
-- lista obtenida eliminando los extremos de la lista xs.
interior xs= init(tail xs)

-- Ejercicio 13. Definir la función segmento tal que (segmento m n xs) es
-- la lista de los elementos de xs comprendidos entre las posiciones m y
-- n.
segmento m n []=[]
segmento m n xs = take(n-m+1)(drop(m-1)xs)

-- Ejercicio 14. Definir la función extremos tal que (extremos n xs) es
-- la lista formada por los n primeros elementos de xs y los n finales
-- elementos de xs.
extremos n [] = []
extremos n xs = take n xs ++ drop  (length xs-n)xs

-- Ejercicio 15. Definir la función mediano tal que (mediano x y z) es el
-- número mediano de los tres números x, y y z.
mediano x y z =x+y+z-maximum[x,y,z]-minimum[x,y,z]

-- Ejercicio 16. Definir la función tresIguales tal que
-- (tresIguales x y z) se verifica si los elementos x, y y z son
-- iguales.
tresiguales x y z 
  |x==y&&y==z=True
  |otherwise=False

-- Ejercicio 17. Definir la función tresDiferentes tal que
-- (tresDiferentes x y z) se verifica si los elementos x, y y z son
-- distintos.
tresdiferentes x y z
 |x/=y&&y/=z&&x/=z=True
 |otherwise = False 

-- Ejercicio 18. Definir la función cuatroIguales tal que
-- (cuatroIguales x y z u) se verifica si los elementos x, y, z y u son
-- iguales.
cuatroiguales v x y z 
  |v==x&&x==y&&y==z&&v==z=True
  |otherwise=False

--Guardas y patrones

-- Ejercicio 1. Definir la función
-- divisionSegura :: Double -> Double -> Double
-- tal que (divisionSegura x y) es x/y si y no es cero y 9999 en caso
-- contrario. 
divisionsegura x y 
 |y==0=9999.0
 |otherwise = x/y

-- Ejercicio 2 La disyunción excluyente xor de dos fórmulas se
-- verifica si una es verdadera y la otra es falsa.
xor1 True True = False
xor1 True False = True
xor1 False True = True
xor1 False False= False

-- Ejercicio 3. Las dimensiones de los rectángulos puede representarse
-- por pares; por ejemplo, (5,3) representa a un rectángulo de base 5 y
-- altura 3.
mayorRectangulo (b1, h1) (b2, h2)
    | b1 * h1 >= b2 * h2 = (b1, h1)
    | otherwise  = (b2, h2)

-- Ejercicio 4. Definir la función
-- intercambia :: (a,b) -> (b,a)
-- tal que (intercambia p) es el punto obtenido intercambiando las
-- coordenadas del punto p.
intercambia (x,y)=(y,x)

-- Ejercicio 5. Definir la función
-- distancia :: (Double,Double) -> (Double,Double) -> Double
-- tal que (distancia p1 p2) es la distancia entre los puntos p1 y
-- p2
distancia p1 p2 = sqrt ((fst p2 - fst p1) ** 2 + (snd p2 - snd p1) ** 2)

-- Ejercicio 6. Definir una función
-- ciclo :: [a] -> [a]
-- tal que (ciclo xs) es la lista obtenida permutando cíclicamente los
-- elementos de la lista xs, pasando el último elemento al principio de
-- la lista.
ciclo []=[]
ciclo xs = last xs : init xs

-- Ejercicio 7. Definir la función
-- numeroMayor :: (Num a, Ord a) => a -> a -> a
-- tal que (numeroMayor x y) es el mayor número de dos cifras que puede
-- construirse con los dígitos x e y
numeroMayor x y 
 |x>=y = x*10+y
 |otherwise = y*10+x

-- Ejercicio 8. Definir la función
-- numeroDeRaices :: (Floating t, Ord t) => t -> t -> t -> Int
-- tal que (numeroDeRaices a b c) es el número de raíces reales de la
-- ecuación a*x^2 + b*x + c = 0.
numeroDeRaices a b c
 | discriminante > 0 = 2
 | discriminante == 0 = 1
 | otherwise = 0
  where
    discriminante = b^2 - 4 * a * c

-- Ejercicio 9. Definir la función
-- raices :: Double -> Double -> Double -> [Double]
-- tal que (raices a b c) es la lista de las raíces reales de la
-- ecuación ax^2 + bx + c = 0.
raices a b c
 |discriminante2 > 0 = [(-b + sqrt discriminante2) / (2 * a), (-b - sqrt discriminante2) / (2 * a)]
 | discriminante2 == 0 = [(-b) / (2 * a)]
 | otherwise = []
  where
    discriminante2 = b^2 - 4 * a * c


-- Ejercicio 10. En geometría, la fórmula de Herón, descubierta por
-- Herón de Alejandría, dice que el área de un triángulo cuyo lados
-- miden a, b y c es la raíz cuadrada de s(s-a)(s-b)(s-c) donde s es el
-- semiperímetro
-- s = (a+b+c)/2
--
-- Definir la función
-- area :: Double -> Double -> Double -> Double
-- tal que (area a b c) es el área del triángulo de lados a, b y c. Por
area a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2

-- Ejercicio 11. Los intervalos cerrados se pueden representar mediante
-- una lista de dos números (el primero es el extremo inferior del
-- intervalo y el segundo el superior).
--
-- Definir la función
--interseccion :: Ord a => [a] -> [a] -> [a]
-- tal que (interseccion i1 i2) es la intersección de los intervalos i1 e
-- i2.
interseccion2 [] _ = []
interseccion2 _ [] = []
interseccion2 [a1, b1] [a2, b2]
 | maxA <= minB = [maxA, minB]
 | otherwise = []
  where
    maxA = max a1 a2
    minB = min b1 b2

-- Ejercicio 12. Los triángulos aritméticos se forman como sigue
--1
--2 3
--4 5 6
--7 8 9 10
--11 12 13 14 15
--16 17 18 19 20 21
--
-- Definir la función
--linea :: Integer -> [Integer]
-- tal que (linea n) es la línea n-ésima de los triángulos
-- aritméticos
linea n = [inicio..fin]
  where
    inicio = (n * (n - 1)) `div` 2 + 1
    fin = inicio + n - 1

---------Recursividad

-- Ejercicio 1. Definir por recursión la función
--potencia :: Integer -> Integer -> Integer
-- tal que (potencia x n) es x elevado al número natural n.
potencia _ 0 = 1
potencia x n = x * potencia x (n-1)

-- Ejercicio 2. Dados dos números naturales, a y b, es posible
-- calcular su máximo común divisor mediante el Algoritmo de
-- Euclides. Este algoritmo se puede resumir en la siguiente fórmula:
--mcd(a,b) = a, si b = 0
--         = mcd (b, a módulo b), si b > 0
--
-- Definir la función
--mcd :: Integer -> Integer -> Integer
-- tal que (mcd a b) es el máximo común divisor de a y b calculado
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

-- Ejercicio 3, Definir por recursión la función
--pertenece :: Eq a => a -> [a] -> Bool
-- tal que (pertenece x xs) se verifica si x pertenece a la lista xs.
pertenece _ [] = False
pertenece a (x:xs)
 |a==x=True
 |otherwise = pertenece a xs


-- Ejercicio 4. Definir por recursión la función
--tomar :: Int -> [a] -> [a]
-- tal que (tomar n xs) es la lista de los n primeros elementos de
-- xs.
tomar1 _ [] = []
tomar1 0 _ = []  
tomar1 n (x:xs) = x : tomar1 (n-1) xs

-- Ejercicio 5. Definir, por comprensión, la función
--digitosC :: Integer -> [Integer]
-- tal que (digitosC n) es la lista de los dígitos del número n.
digitosC 0 = []
digitosC n = digitosC (div n 10)++[mod n 10]

-- Ejercicio 6. Definir, por recursión, la función
-- sumaDigitosR :: Integer -> Integer
-- tal que (sumaDigitosR n) es la suma de los dígitos de n.
sumaDigitosR 0 = 0
sumaDigitosR n = (mod n 10) + sumaDigitosR (div n 10)

-- Ejercicio 2.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenación rápida se selecciona el primer elemento x de xs, se divide
-- los restantes en los menores o iguales que x y en los mayores que x,
-- se ordena cada una de las dos partes y se unen los resultados.
ordenaRapida [] = []  
ordenaRapida (x:xs) = 
  ordenaRapida [y | y <- xs, y <= x]  
  ++ [x] ++
  ordenaRapida [y | y <- xs, y > x] 


--Nuevos tipos de Datos
--1.- Crea un nuevo tipo Estudiate con los siguientes atributos
-- Nombre, Apellido, Edad, Número de control 
-- Genera una lista de un mínimo de 10 estudiantes en donde obtendras

-- Lista ordenada de los estudiantes de acuedo a la edad
-- Obtener al estudiante menor, mayor
-- Obtener el promedio de edades.


data Estudiante = Estudiante {
    nombre      :: String,
    apellido    :: String,
    edad        :: Int,
    numControl  :: String
} deriving (Show)

estudiantes = [
  Estudiante "Roberto" "Fernandez" 18 "011",
Estudiante "Camila" "Ramirez" 20 "012",
Estudiante "Diego" "Hernandez" 21 "013",
Estudiante "Paula" "Vega" 22 "014",
Estudiante "Fernando" "Ortiz" 19 "015",
Estudiante "Mariana" "Lopez" 20 "016",
Estudiante "Jorge" "Gonzalez" 18 "017",
Estudiante "Sofia" "Martinez" 21 "018",
Estudiante "Andres" "Rios" 19 "019",
Estudiante "Valeria" "Torres" 22 "020"
 ]


-- Estudiantes por edad 
quickSort [] = []
quickSort (p:xs) = quickSort [y | y <- xs, edad y <= edad p] ++ [p] ++ quickSort [y | y <- xs, edad y > edad p]

-- Estudiante menor edad
estudianteMenor = head . quickSort

--Estudiante mayor edad
estudianteMayor = last . quickSort

--Promedio de edades
promedioEdades estudiantes = fromIntegral (sum (map edad estudiantes)) / fromIntegral (length estudiantes)


--Arboles
--Crea un nuevo tipo de dato árbol
--Funciones

data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a) deriving (Show,Eq)

generarNodo :: a->Arbol a
generarNodo x = Nodo x Hoja Hoja

--1.- Insertar desde un arreglo 

insertar x Hoja=generarNodo x 
insertar x (Nodo a izq der)
 |x<a =(Nodo a (insertar x izq)der)
 |x>a=(Nodo a izq (insertar x der))
 |otherwise = Nodo a izq der 

--2.- Buscar un elemento en un árbol

buscarNodo x Hoja = False
buscarNodo x (Nodo a izq der)
  | x == a = True
  |x<a=buscarNodo x izq
  |otherwise = buscarNodo x der

--3.- Recorridos (Inorden, posorden, preorden)

inOrden Hoja=[]
inOrden (Nodo a izq der)=inOrden izq ++ [a] ++ inOrden der


preOrden Hoja=[]
preOrden (Nodo a izq der)=[a]++preOrden izq ++preOrden der

posOrden Hoja=[]
posOrden(Nodo a izq der)=posOrden izq++posOrden der ++ [a]
