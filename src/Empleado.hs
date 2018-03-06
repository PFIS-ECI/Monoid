{-# LANGUAGE OverloadedStrings #-}

module Empleado where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

type Nombre = String
type Edad = Int
type Salario = Double

data Empleado = Empleado { nombre :: Nombre, edad :: Edad, salario :: Salario }
              deriving (Show)

instance FromNamedRecord Empleado where
    parseNamedRecord e = error "Not implemented" 

leerEmpleados :: IO [Empleado]
leerEmpleados = do
  csvData <- BL.readFile "empleados.csv"
  case decodeByName csvData of
       Left err -> error err
       Right (_, v) -> return $ V.toList v
