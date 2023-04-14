module Dibujos.AnimateRandom (
    interpBas,
    animateRandomConf
) where
    
import Graphics.Gloss (polygon, makeColorI, color)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (Dibujo, figura, juntar, apilar, rotar, r180, r270, escalar, encimar4, ciclar)
import FloatingPic (Output, half, zero)
import Interp (Conf(..), interp)

type Basica = ()

test :: Float -> Dibujo Basica
test f = escalar (pos x) (pos y) $ rotar' get
    where
        bas = figura ()
        rnd n = fromIntegral $ round f `mod` n
        x = rnd 5
        y = rnd 4
        get = case rnd 9 of 
            0 -> bas
            1 -> juntar 1 1 bas bas
            2 -> apilar 1 1 bas bas
            3 -> rotar bas
            4 -> r180 bas
            5 -> r270 bas
            6 -> escalar 0.5 0.5 bas
            7 -> encimar4 bas
            8 -> ciclar bas
        pos n = n / (x + y) + 0.1
        rotar' = case rnd 4 of
            0 -> id
            1 -> rotar
            2 -> r180
            3 -> r270

interpBas :: Float -> Output Basica
interpBas n () a b c = color color' $ polygon [a, a V.+ c, a V.+ b, a]
  where
      color' = makeColorI n' 0 0 255
      n' = mod (round n * 50) 256

animateRandomConf :: Conf
animateRandomConf = Conf {
    name = "AnimateRandom",
    pic = \f -> interp (interpBas f) (test f)
}
