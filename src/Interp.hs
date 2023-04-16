module Interp
  ( interp,
    Conf (..),
    interpConf,
    initial,
  )
where

import Dibujo (Dibujo, apilar, encimar, espejar, foldDib, juntar, rot45, rotar)
import FloatingPic (FloatingPic, Output, grid, half)
import Graphics.Gloss (Display (InWindow), Picture, color, animate, makeColorI, pictures, translate, white)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss.Data.Vector (mulSV)

-- Interpretación de un dibujo
-- formulas sacadas del enunciado
interp :: Output a -> Output (Dibujo a)
interp fun = foldDib
  fun
  interpRotar
  interpEspejar
  interpRot45
  interpApilar
  interpJuntar
  interpEncimar
  interpEscalar

-- funcion que interpreta rotar
-- rotar :: OutputBas
interpRotar :: Output FloatingPic
interpRotar fun x w h = fun (x V.+ w) h (V.negate w)

-- funcion que interpreta espejar
-- espejar :: OutputBas
interpEspejar :: Output FloatingPic
interpEspejar fun x w = fun (x V.+ w) (V.negate w)

-- funcion que interpreta rot45
-- rot45 :: OutputBas
interpRot45 :: Output FloatingPic
interpRot45 fun x w h = fun (x V.+ half (w V.+ h)) (half (w V.+ h)) (half (h V.- w))

-- funcion que interpreta apilar
-- apilar :: Float -> Float -> FloatingBas -> OutputBas
interpApilar :: Float -> Float -> FloatingPic -> Output FloatingPic
interpApilar n m f g x w h = pictures [f (x V.+ h_aux) w (r V.* h), g x w h_aux]
  where
    r_aux = n / (m + n)
    r = m / (m + n)
    h_aux = r_aux V.* h

-- funcion que interpreta juntar
-- juntar :: Float -> Float -> FloatingBas -> OutputBas
interpJuntar :: Float -> Float -> FloatingPic -> Output FloatingPic
interpJuntar n m f g x w h = pictures [f x w_aux h, g (x V.+ w_aux) (r_aux V.* w) h]
  where
    r_aux = n / (m + n)
    r = m / (m + n)
    w_aux = r V.* w

-- funcion que interpreta encimar
-- encimar :: FloatingBas -> OutputBas
interpEncimar :: FloatingPic -> Output FloatingPic
interpEncimar f g x w h = pictures [f x w h, g x w h]

-- funcion que interpreta escalar
-- escalar :: Float -> FloatingBas -> OutputBas
interpEscalar :: Float -> Float -> Output FloatingPic
interpEscalar n m fun x w h = fun x' (n V.* w) (m V.* h)
  where x' = x V.+ (((n - 1) / 2) `mulSV` V.negate w)

-- Configuración de la interpretación
data Conf = Conf
  { name :: String,
    pic :: Float -> FloatingPic
  }

interpConf :: Conf -> Float -> Float -> Float -> Picture
interpConf (Conf _ p) x y n = p n (0, 0) (x, 0) (0, y)

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: Conf -> Float -> IO ()
initial cfg size = do
  let n = name cfg
      win = InWindow n (ceiling size, ceiling size) (0, 0)
  animate win white $ withGrid (interpConf cfg size size) size size
  where
    withGrid p x y n = translate (-size / 2) (-size / 2) $ pictures [p n, color grey $ grid (ceiling $ size / 10) (0, 0) x 10]
    grey = makeColorI 120 120 120 120
