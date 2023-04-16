module Dibujos.Escher (escherConf) where

import Dibujo
  ( Dibujo,
    apilar,
    cuarteto,
    encimar,
    encimar4,
    espejar,
    figura,
    juntar,
    r180,
    r270,
    rot45,
    rotar,
  )
import FloatingPic (Output, half, vacia, zero)
import Graphics.Gloss (blank, line, pictures, polygon, scale, text, translate, white)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Interp (Conf (..), interp)

type Escher = Bool

dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT p = encimar p (encimar p' p'')
  where
    p' = espejar (rot45 p)
    p'' = r270 p'

dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU p =
  encimar
    (encimar p' (rotar p'))
    (encimar (r180 p') (r270 p'))
  where
    p' = espejar (rot45 p)

esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 1 p = cuarteto (figura False) (figura False) (figura False) (dibujoU p)
esquina n p = cuarteto (esquina (n - 1) p) (lado (n - 1) p) (rotar (lado (n - 1) p)) (dibujoU p)

lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 1 p = cuarteto (figura False) (figura False) (rotar (dibujoT p)) (dibujoT p)
lado n p = cuarteto (lado (n - 1) p) (lado (n - 1) p) (rotar (dibujoT p)) (dibujoT p)

noneto p q r s t u v w x =
  apilar
    2
    1
    (juntar 2 1 p (juntar 1 1 q r))
    (apilar 1 1 (juntar 2 1 s (juntar 1 1 t u)) (juntar 2 1 v (juntar 1 1 w x)))

escher :: Int -> Escher -> Dibujo Escher
escher n f = noneto p q r s t u v w x
  where
    p = esquina n (figura f)
    q = lado n (figura f)
    r = r270 (esquina n (figura f))
    s = rotar (lado n (figura f))
    t = dibujoU (figura f)
    u = r270 (lado n (figura f))
    v = rotar (esquina n (figura f))
    w = r180 (lado n (figura f))
    x = r180 (esquina n (figura f))

-- Funcion que interpreta la figura
interpBas :: Output Escher
interpBas fig a b c =
  if fig then pictures [line $ triangulo a b c, cara a b c] else vacia a b c
  where
    triangulo a b c = map (a V.+) [zero, c, b, zero]
    cara a b c = polygon $ triangulo (a V.+ half c) (half b) (half c)

escherConf :: Conf
escherConf =
  Conf
    { name = "Escher",
      pic = \_ -> interp interpBas (escher 5 True)
    }