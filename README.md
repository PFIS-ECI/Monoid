
Clone el repositorio 

```bash
$ git clone https://github.com/PFIS-ECI/Monoid.git
```

Cambie el directorio actual al directorio del repositorio
```bash
$ cd Monoid
```

Para utilizar `ghci` utilice el comando
```bash
$ stack ghci
```

# Monoid

La clase Monoid en Haskell se utiliza para representar monoides:

```haskell
class Monoid a where
  mempty :: a              -- elemento neutro o identidad del monoide
  mappend :: a -> a -> a   -- operación binaria
  mconcat :: [a] -> a      -- reduce la lista utilizando
                           -- `mappend` entre sus elementos
  
(<>) :: Monoid a => a -> a -> a
x <> y = mappend x y
```

donde `mempty` representa la unidad de la operación `<>`.

Un monoide debe cumplir las siguientes leyes:

```haskell
x <> mempty = x                 -- Identidad derecha
mempty <> x = x                 -- Identidad izquierda

x <> (y <> z) == (x <> y) <> z  -- Asociatividad
```
Para todo elemento `x, y, z :: a`. Es tarea del programador que
crea la instancia revisar que se cumplen las propiedades.

## Monoide String

Las cadenas de caracteres, con la concatenación y la cadena vacía
es un ejemplo de un monoide. 

```haskell
> mempty :: String
...
> "hello" <> mempty
...
> mempty <> "hello"
...
> "hello" <> (" " <> "haskell")
...
> ("hello" <> " ") <> "haskell"
...
> mconcat ["hello"," ","haskell"]
...
```

## Monoide Lista

Un `String` en Haskell es una lista de caracteres (`[Char]`). En general, las
listas, con la concatenación y la lista vacía, independientemente del tipo de datos que contienen, son instancia de la clase Monoid:

```haskell
instance Monoid [a] where
    mempty = []
    mappend = (++)
```

Intente en ghci
```
> [1,2,3] <> mempty <> [4,5,6]
...
> mconcat [[True,False,True],mempty,[True,False]]
...
```

### Listas por comprensión

En Haskell se pueden definir listas por comprensión, de manera similar a 
los conjuntos por comprensión. La siguiente expresión crea la lista de los
`x` al cuadrado, para los `x` entre 1 y 10 que sean pares (`even x`).

```haskell
> [ x*x | x <- [1 .. 10], even x ]
...
```

Las expresiones de la forma `x <- ...` se denominan generadores y las
expresiones de la forma `c x :: Bool`, se denominan _filtros_. Las expresiones
de la forma `[a..b]`i, donde el tipo de `a` y `b` son instancia de la clase
`Enum`, se denominan rangos. Los rangos permiten un segundo elemento que determina el paso `[a,b..c]`

```haskell
> [0,2..10]
...
```

Las listas por comprensión pueden implementarse utilizando la clase Monoid, 
por ejemplo, la expresión `[ x*x | x <- [1 .. 10], even x ]` es equivalente a: 

```haskell
> mconcat (map (\x -> if even x then [x*x] else mempty) [1..10])
...
```

Las listas por comprensión pueden utilizar cualquier cantidad de generadores y
filtros.

```haskell
> [ (p,x) | p <- [True,False], x <-[0..5]]
...
```

Note que esta expresión es equivalente a la siguiente expresión:

```haskell
> mconcat (map (\p -> map (\x -> [(p,x)]) [0..5]) [True,False]) 
```

Haskell al ser un lenguaje perezoso, permite listas infinitas, por ejemplo:

```haskell
> take 20 [0..]
...
> take 20 [0,2..]
...
```

La función `zip` junta dos listas:

```haskell
> zip [1..] ['a'..'z']
...
```

Se puede utilizar la pereza, junto con la función `zip` para definir la lista 
de los números de Fibonnacci.

Note lo siguiente:
```
            fib = 0:1:1:2:3:5:...
       tail fib = 1:1:2:3:5:8:...
---------------------------------------
fib +' tail fib = 1:2:3:5:8:13:.....
```

```haskell
> let fib = 0:1:[ n+m | (n,m) <- zip fib (tail fib)]
...
```

## Monoides Booleanos

Las siguientes son posibles instancias de la clase Monoid para el tipo de datos 
`Bool`:

* `(Bool,True,&&)`
* `(Bool,False,||)`
* `(Bool,Equiv,===)`

Intente definir la siguiente instancia en el archivo `src/Monoid.hs`
```haskell
instance Monoid Bool where
   mempty = True
   mappend = (&&)
```
y cárguelo en ghci. Intente las siguientes expresiones:

```haskell
> True <> True <> True
...
> True <> False <> True
...
```

Intente definir la siguiente instancia en el archivo `src/Monoid.hs`
```haskell
instance Monoid Bool where
   mempty = False
   mappend = (||)
```
y cárguelo en ghci.

* Que error aparece?

En Haskell un tipo de datos solo puede tener una instancia de clase. Para
solucionar este problema se utiliza el comando `newtype`: 

```haskell
newtype All = All { getAll :: Bool } 
``` 

define un nuevo tipo de datos `All` que es equivalente a `Bool` pero sin
instancias definidas. El comando `newtype` no sobrecarga el código ejecutable,
una vez que pasa la verificación de tipos y decide las instancias a utilizar lo
elimina en el código generado.

```haskell
instance Monoid All where
    mempty = All True
    mappend (All p) (All q) = All (p && q)
```
Se puede definir también:
```haskell
newtype Any = Any { getAny :: Bool } 

instance Monoid Any where
    mempty = Any False
    mappend (Any p) (Any q) = Any (p || q)
```

Los tipos `All` y `Any` y las instancias de la clase Monoid se encuentran definidas en el módulo `Data.Monoid`.

Ejercicio:

1. Defina un nuevo tipo de datos `Equiv` y cree la instancia de Monoid donde el operador `mappend` es el operador de equivalencia.
2. Evalue en ghci la expresión `Equiv True <> Equiv False <> Equiv False`.

## Monoides Numéricos

Al igual que los booleanos, los números tienen más de una posible instancias para la clase Monoid.

```haskell
newtype Sum = Sum { getSum :: a } 

instance Num a => Monoid (Sum a) where
   mempty = Sum 0
   mappend (Sum x) (Sum y) = Sum (x + y)
```

El monoide producto (`Product`) se define de manera similar.

```haskell
newtype Product = Product { getProduct :: a } 

instance Num a => Monoid (Product a) where
   mempty = Product 1
   mappend (Product x) (Product y) = Product (x * y)
```

Ejercicio:

Defina las funciones `sumar`/`multiplicar` que suma/multiplica los elementos de 
una lista utilizando los monoides de `Sum` y `Product` 

```haskell
sumar :: Num a => [a] -> a
sumar xs = ...

multiplicar :: Num a => [a] -> a
multiplicar xs = ...
```

## Monoides genericos

Hay instancias genericas de monoides como las tuplas:

```haskell
instance (Monoid a,Monoid b) => Monoid (a,b) where
    mempty = (mempty,mempty)
    mappend (a,b) (a',b') = (a `mappend` a',b `mappend` b')
```

Ejemplo
```haskell
> mconcat [ (Sum x,Product x) | x <- [1..10] ]
...
```

# Functores

Los functores se pueden entender como una generalización de una función 
`f :: a -> b` cuando el(los) parámetro(s) se encuentra(n) dentro de un
tipo de datos.

## Listas
```haskell
mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (x:xs) = f x : map f xs
```

```haskell
> mapList (^2) [1..10]
...
```

## Maybe
```haskell
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just x) = Just (f x)
```

```haskell
> mapMaybe (^2) (Just 10)
...
```

## Arbol

Dada la definición de árbol:
```haskell
data Tree a 
    = Empty
    | Branch (Tree a) a (Tree a)
    deriving (Show,Eq)
```

defina la función:
```haskell
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty = ...
mapTree f (Branch left x right) = ...
```

```haskell
> mapTree (^2) (Branch (Branch Empty 2 Empty) 1 (Branch Empty 3 Empty))
...
```

Observando los tipos de datos anteriores:

```haskell
mapList  :: (a -> b) -> [a]     -> [b]
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapTree  :: (a -> b) -> Tree a  -> Tree b
```

se puede generalizar este concepto con la tipo de clase functor:
```haskell
class Functor (c :: * -> *) where
   fmap :: (a -> b) -> c a -> c b 
```

Los functores deben cumplir las siguientes leyes:
```
fmap id = id                     -- (1) 
fmap (g . f) = fmap g . fmap f   -- (2)
```

Las instancias para lista y `Maybe` están definidas de antemano en Haskell y son
equivalentes a las siguientes:

```haskell
instance Functor [] where
   fmap = mapList

instance Functor Maybe where
   fmap = mapMaybe
```

Defina la instancia para `Tree`:
```haskell
instance Functor Tree where
   fmap = mapTree
```

Haskell define el operador `(<$>)` como `fmap`:

```haskell
(<$>) :: Functor c => (a -> b) -> c a -> c b
f <$> cx = fmap f cx
```

La función que eleva los elementos al cuadrado de un tipo de datos que sea
un functor:

```haskell
square :: (Num a,Functor c) => c a -> c a
square cx = (^2) <$> cx 
```

Intente en ghci:
```haskell
> square (^2) [1..10]
...
> square (^2) (Just 10)
...
> square (^2) (Branch (Branch Empty 2 Empty) 1 (Branch Empty 3 Empty))
...
```

de hecho, se puede pensar que `fmap` toma una función `f:: a -> b` y
la convierte en una función `fmap f` de tipo `c a -> c b`:

```
  f::          a -> b
        
                 | fmap f
                 V
        
  fmap f ::  c a -> c b
```

# Functor Aplicativo

Que pasa si la función esta contenida dentro de un tipo de datos?

```Haskell
> :t (+) <$> (Just 3)
(+) <$> Just 3 :: Num a => Maybe (a -> a)
```

Note que:
```
  (+) <$> (Just 3) 
= { fmap }
  Just ((+) 3)
= { Notación }
  Just ((\x y -> x + y) 3)
= { Beta }
  Just (\y -> 3 + y)
= { Notación }
  Just (3 +)
```

Como se puede pasar el(los) argumento(s) a la función contenida en el tipo de
datos?

Se quiere tener un operador `(<*>)` de manera que:
```haskell
> (+) <$> Just 3 <*> Just 5
Just 8
```

Si se revisa de que tipo es `(<*>)`:
```haskell
> :t (<*>)
(<*>) :: Applicative c => c (a -> b) -> c a -> c b
```

La clase Applicative en Haskell se define
```haskell
class Functor f => Applicative (c :: * -> *) where
  pure :: a -> c a
  (<*>) :: c (a -> b) -> c a -> c b
```

La función `pure` se utiliza para introducir un elemento en el functor, por
ejemplo la instancia de Maybe es:

```haskell
instance Applicative Maybe where
    pure                  = Just
    (Just f) <*> (Just x) = Just (f x)
    _        <*> _        = Nothing
```

Las leyes que debe cumplir un functor aplicativo son:

```
pure id <*> v = v                                  -- Identidad
pure f <*> pure x = pure (f x)                     -- Homomorfismo
cf <*> pure y = pure ($ y) <*> cf                  -- Intercambio
pure (.) <*> cf <*> cg <*> cx = cf <*> (cg <*> cx) -- Composition
```

Una implementación para listas:
```haskell
instance Applicative [] where
    pure  x   = [x]
    fs <*> gs = map (\f -> f <$> gs) fs
    _        <*> _        = Nothing
```

Ejercicio, haga a los árboles una instancia de Applicative.

# Leyendo DATOS de un archivo CSV

Cassava es una librería para leer datos en formato [csv](https://en.wikipedia.org/wiki/Comma-separated_values).

En este ejercicio se utiliza la librería [cassava](https://hackage.haskell.org/package/cassava) para leer los datos del archivo [empleados.csv](empleados.csv).

Para convertir los datos desde el formato CSV a un tipo de datos particular, el
tipo debe instanciar la clase `FromNamedRecord`, para lo cual se utilizan
functores applicativos. Tome como guía el ejemplo
[cassava](https://hackage.haskell.org/package/cassava) para completar la
instancia de la clase `FromNamedRecord` en el archivo `src/Empleado.hs`.

Compile y ejecute el programa compilado con `stack`. Debe aparecer el listado
de los empleados leídos del archivo.

Implemente la función `nomina` del archivo `src/Datos.hs` utilizando monoides
y quite el comentario de la línea:

```haskell
  -- printf "Nomina: %10.2f\n" (nomina empleados)
```
para que imprima el valor de la nómina. Compile y ejecute el nuevo programa.

Aunque el promedio no es un monoide, se puede llevar la suma de los elementos
elementos con el monoide `Sum` y simultáneamente se puede llevar la cantidad de
elementos con un monoide; para obtener el promedio, al final del proceso de
todos los datos, si hay mas de un elemento, se divide la suma entre el número de
elementos.

Implemente la función `salarioPromedio` utilizando monoides y
quite el comentario de la línea 
```haskell
  -- printf "Salario promedio: %10.2f\n" (salarioPromedio empleados)
```
para imprimir el resultado. Compile y ejecute el nuevo programa.
