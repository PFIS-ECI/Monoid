module Datos
    ( totalizar
    ) where

import Text.Printf
import Empleado
import qualified Data.Vector as V
import Data.Monoid
import Monoid

nomina :: [Empleado] -> Double
nomina = error "Not implemented"

salarioPromedio :: [Empleado] -> Double
salarioPromedio = error "Not implemented"

salarioDe :: Nombre -> Maybe Salario
salarioDe = error "Not implemented"

totalizar :: IO ()
totalizar = do
  empleados <- leerEmpleados
  mapM_ print empleados
  -- printf "Nomina: %10.2f\n" (nomina empleados)
  -- printf "Salario promedio: %10.2f\n" (salarioPromedio empleados)
