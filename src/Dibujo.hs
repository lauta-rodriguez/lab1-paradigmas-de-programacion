{-# LANGUAGE LambdaCase #-}

module Dibujo
  ( Dibujo,
    figura,
    rotar,
    espejar,
    rot45,
    apilar,
    juntar,
    encimar,
    r180,
    r270,
    (.-.),
    (///),
    (^^^),
    cuarteto,
    encimar4,
    ciclar,
    foldDib,
    mapDib,
    figuras,
  )
where

import FloatingPic (Output, FloatingPic, half)
import Graphics.Gloss (pictures)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

{-
Gramática de las figuras:
<Fig> ::= Figura <Bas> | Rotar <Fig> | Espejar <Fig> | Rot45 <Fig>
    | Apilar <Float> <Float> <Fig> <Fig>
    | Juntar <Float> <Float> <Fig> <Fig>
    | Encimar <Fig> <Fig>
-}

data Dibujo a
  = Figura a
  | Rotar (Dibujo a)
  | Espejar (Dibujo a)
  | Rot45 (Dibujo a)
  | Apilar Float Float (Dibujo a) (Dibujo a)
  | Juntar Float Float (Dibujo a) (Dibujo a)
  | Encimar (Dibujo a) (Dibujo a)
  deriving (Eq, Show)

-- Agreguen los tipos y definan estas funciones
-- type Punto = (Float, Float)
-- type Basica = Punto -> Punto -> Punto

-- type Figura =  Rectangulo | Triangulo

-- triangulo :: Basica -> Picture
-- triangulo a b c = polygon [a, b, c]

-- rectangulo :: Basica -> Picture
-- rectangulo a b c = polygon [a, b, a + c, b + c]

-- Construcción de dibujo. Abstraen los constructores.
figura :: a -> Dibujo a
figura = Figura

rotar :: Output FloatingPic
rotar fun x w h = fun (x V.+ w) h (V.negate w)

espejar :: Output FloatingPic
espejar fun x w h = fun (x V.+ w) (V.negate w) h

rot45 :: Output FloatingPic
rot45 fun x w h = fun (x V.+ half (w V.+ h)) (half (w V.+ h)) (half (h V.- w))

apilar :: Float -> Float -> FloatingPic -> Output FloatingPic
apilar n m f g x w h = pictures [f (x V.+ h_aux) w (r V.* h), g x w h_aux]
  where r_aux = n / (m + n)
        r = m / (m + n)
        h_aux = r_aux V.* h

juntar :: Float -> Float -> FloatingPic -> Output FloatingPic
juntar n m f g x w h = pictures [f x w_aux h, g (x V.+ w_aux) (r_aux V.* w) h]
  where r_aux = n / (m + n)
        r = m / (m + n)
        w_aux = r V.* w

encimar :: FloatingPic -> Output FloatingPic
encimar f g x w h = pictures [f x w h, g x w h]

-- Composición n-veces de una función con sí misma. Componer 0 veces
-- es la función constante, componer 1 vez es aplicar la función 1 vez, etc.
-- Componer negativamente es un error!
comp :: (a -> a) -> Int -> a -> a
comp f n a
  | n < 0 = error "Componer negativamente es un error!"
  | n == 0 = a
  | otherwise = f (comp f (n - 1) a)

-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 = Rotar . Rotar

r270 :: Dibujo a -> Dibujo a
r270 = Rotar . r180

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = Apilar 1.0 1.0

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = Juntar 1.0 1.0

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = Encimar

-- Dadas cuatro figuras las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto d1 d2 d3 d4 = (d1 /// d2) .-. (d3 /// d4)

-- Una figura repetida con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 d = (d ^^^ (Rotar d)) ^^^ ((r180 d) ^^^ (r270 d))

-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar d = cuarteto d (Rotar d) (r180 d) (r270 d)

-- Estructura general para la semántica (a no asustarse). Ayuda:
-- pensar en foldr y las definiciones de Floatro a la lógica
foldDib ::
  (a -> b) ->
  (b -> b) ->
  (b -> b) ->
  (b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (b -> b -> b) ->
  Dibujo a ->
  b
foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dibujo = case dibujo of
  Figura a -> fFigura a
  Rotar dib -> fRotar (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib)
  Espejar dib -> fEspejar (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib)
  Rot45 dib -> fRot45 (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib)
  Apilar f1 f2 dib1 dib2 -> fApilar f1 f2 (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib1) (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib2)
  Juntar f1 f2 dib1 dib2 -> fJuntar f1 f2 (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib1) (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib2)
  Encimar dib1 dib2 -> fEncimar (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib1) (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib2)

-- Demostrar que `mapDib figura = id`
mapDib :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
mapDib f fig = foldDib f Rotar Espejar Rot45 Apilar Juntar Encimar fig

-- Junta todas las figuras básicas de un dibujo.
figuras :: Dibujo a -> [a]
figuras = foldDib (: []) id id id (\_ _ -> (++)) (\_ _ -> (++)) (++)
