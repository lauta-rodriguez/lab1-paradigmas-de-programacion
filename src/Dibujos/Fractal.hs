module Dibujos.Fractal (fractalConf) where
import Graphics.Gloss (polygon)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (
        Dibujo,
        figura,
        escalar,
        (///),
        (.-.),
    )
import FloatingPic (Output, half, zero)
import Interp (Conf(..), interp)

data FiguraData = Triangle deriving (Eq, Show)

sierpinski :: Int -> Dibujo FiguraData
sierpinski 0 = figura Triangle
sierpinski n = upSierpinski .-. (downSierpinski /// downSierpinski)
    where
        upSierpinski = escalar 0.5 1 (sierpinski (n - 1))
        downSierpinski = sierpinski (n - 1)

draw :: Dibujo FiguraData
draw = sierpinski 7

-- interpBas :: FiguraData -> Basic -> Picture
interpBas :: Output FiguraData
interpBas fig x w h = case fig of
    Triangle -> triangle
    where
        triangle = polygon $ map (x V.+) [zero, w, half w V.+ h, zero]

fractalConf :: Conf
fractalConf = Conf {
    name = "Fractal",
    pic = \_ -> interp interpBas draw
}
