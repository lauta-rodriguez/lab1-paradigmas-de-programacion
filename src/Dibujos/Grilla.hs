module Dibujos.Grilla (grillaConf) where

import Dibujo
  ( Dibujo,
    cuarteto,
    figura,
  )
import FloatingPic (Output)
import Graphics.Gloss (scale, text, translate)
import Interp (Conf (..), interp)

data FiguraData = Point Int Int deriving (Eq, Show)

grilla :: (Int, Int) -> (Int, Int) -> Dibujo FiguraData
grilla (rs, re) (cs, ce) = do
  let startHalf s e = (s, div (e - 1 + s) 2)
      halfEnd s e = (div (e + 1 + s) 2, e)
      nr = startHalf rs re
      sr = halfEnd rs re
      wc = startHalf cs ce
      ec = halfEnd cs ce
  if rs == re && cs == ce
    then figura (Point rs cs)
    else cuarteto (grilla nr wc) (grilla nr ec) (grilla sr wc) (grilla sr ec)

draw :: Dibujo FiguraData
draw = grilla (0, 7) (0, 7)

-- interpBas :: FiguraData -> Basic -> Picture
interpBas :: Output FiguraData
interpBas fig (x1, x2) (y1, y2) (z1, z2) = case fig of
  (Point a b) -> translate x y (richText a b)
  where
    richText a b = scale 0.1 0.1 (text ("(" ++ show a ++ "," ++ show b ++ ")"))
    x = x1 + 10
    y = x2 + z2 - 25

grillaConf :: Conf
grillaConf =
  Conf
    { name = "Grilla",
      pic = \_ -> interp interpBas draw
    }
    