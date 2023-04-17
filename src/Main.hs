module Main (main) where

import Data.Maybe (fromMaybe)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Interp (Conf(name), initial)
import Dibujos.Ejemplo (ejemploConf)
import Dibujos.Feo (feoConf) -- correr como "./main Feo"
import Dibujos.Grilla (grillaConf)
import Dibujos.Fractal (fractalConf)
import Dibujos.AnimateRandom (animateRandomConf)
import Dibujos.Escher (escherConf)

-- Lista de configuraciones de los dibujos
configs :: [Conf]
configs = [
    ejemploConf, 
    feoConf, 
    grillaConf, 
    animateRandomConf, 
    fractalConf,
    escherConf]

-- Dibuja el dibujo n
initial' :: [Conf] -> String -> IO ()
initial' [] n = do
    putStrLn $ "No hay un dibujo llamado " ++ n
initial' (c : cs) n = 
    if n == name c then
        initial c 400
    else
        initial' cs n

lista :: [Conf] -> [String]
lista [] = []
lista (c : cs) = name c : lista cs

main :: IO ()
main = do
    args <- getArgs
    if head args == "--lista" then do
        putStrLn $ unlines $ lista configs
        print("Please enter the ConfigName here")
        n <- getLine
        initial' configs n
    else
        initial' configs $ head args
