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
       command "Labs"           (parseLinear         `withInfo` "Dummy classification")
    <> command "Transformation" (parseTransformation `withInfo` "Transformation")
    <> command "Approximation"  (parseApproximation  `withInfo` "Approximation")
    <> command "Classification" (parseClassification `withInfo` "Classification with three classes")
    <> command "Kohonen"        (parseKohonen        `withInfo` "Kohenen network clustering")
    <> command "NeuralGas"      (parseNeuralGas      `withInfo` "Neural gas clustering")
    <> command "KMeans"         (parseKMeans         `withInfo` "K-mean clustering")
    <> command "Compression"    (parseCompression    `withInfo` "Image compression")
    <> command "RadialApprox"   (parseRadialApprox   `withInfo` "Approximation with RBF")
    <> command "RadialClass"    (parseRadialClass    `withInfo` "Classification with RBF")
    <> command "RadialDescentApprox" (parseRadialDescentApprox `withInfo` "Approximation with RBF (descent version)")
    <> command "RadialDescentClass"  (parseRadialDescentClass  `withInfo` "Classification with RBF (descent version)")

parserInfo :: ParserInfo (IO ())
parserInfo = parser `withInfo` "Myslenie - neural networks in Haskell"

main :: IO ()
main = join $ execParser parserInfo
