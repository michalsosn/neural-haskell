module Main where

import Control.Monad hiding (msum)
import Options.Applicative

import MainIAD1
import MainIAD2
import MainIAD3

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parser :: Parser (IO ())
parser = subparser $
       command "Labs"           (parseLinear         `withInfo` "Klasyfikacja z laboratorium")
    <> command "Transformation" (parseTransformation `withInfo` "Transformacja")
    <> command "Approximation"  (parseApproximation  `withInfo` "Aproksymacja")
    <> command "Classification" (parseClassification `withInfo` "Klasyfikacja z trzema klasami")
    <> command "Kohonen"        (parseKohonen        `withInfo` "Grupowanie z siecią Kohenena")
    <> command "NeuralGas"      (parseNeuralGas      `withInfo` "Grupowanie z gazem neuronowym")
    <> command "KMeans"         (parseKMeans         `withInfo` "Grupowanie z algorytmem k-średnich")
    <> command "Compression"    (parseCompression    `withInfo` "Kompresja obrazu")
    <> command "RadialApprox"   (parseRadialApprox   `withInfo` "RBF - Aproksymacja")

parserInfo :: ParserInfo (IO ())
parserInfo = parser `withInfo` "Myslenie - rozwiązanie pierwszego zadania z IAD"

main :: IO ()
main = join $ execParser parserInfo
