module Interp (
    interp,
    Conf(..),
    interpConf,
    initial
) where

import Graphics.Gloss(Picture, Display(InWindow), makeColorI, color, pictures, translate, white, display)
import Dibujo (Dibujo, foldDib)
import FloatingPic (FloatingPic, Output, grid)


-- Interpretación de un dibujo
-- formulas sacadas del enunciado
-- a -> Vector -> Vector -> Vector -> Picture -> (Dibujo a) -> Vector -> Vector -> Vector -> Picture
interp :: Output a -> Output (Dibujo a)
interp = undefined
-- interp (Figura f) fig = figura fig
-- interp (Rotar d) fig = rotar (interp d fig)
-- interp (Espejar d) fig = reflejaX (interp d fig)
-- interp (Rot45 d) fig = rotate (pi / 4) (interp d fig)
-- interp (Apilar x y d1 d2) fig = apilar x y (interp d1 fig) (interp d2 fig)
-- interp (Juntar x y d1 d2) fig = juntar x y (interp d1 fig) (interp d2 fig)
-- interp (Encimar d1 d2) fig = encimar (interp d1 fig) (interp d2 fig)

-- Configuración de la interpretación
data Conf = Conf {
        name :: String,
        pic :: FloatingPic
    }

interpConf :: Conf -> Float -> Float -> Picture 
interpConf (Conf _ p) x y = p (0, 0) (x,0) (0,y)

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: Conf -> Float -> IO ()
initial cfg size = do
    let n = name cfg
        win = InWindow n (ceiling size, ceiling size) (0, 0)
    display win white $ withGrid (interpConf cfg size size) size size
  where withGrid p x y = translate (-size/2) (-size/2) $ pictures [p, color grey $ grid (ceiling $ size / 10) (0, 0) x 10]
        grey = makeColorI 120 120 120 120