module Pred (
  Pred,
  cambiar, anyDib, allDib, orP, andP
) where

import Dibujo (Dibujo, mapDib, figura)

-- `Pred a` define un predicado sobre figuras básicas. Por ejemplo,
-- `(== Triangulo)` es un `Pred TriOCuat` que devuelve `True` cuando la
-- figura es `Triangulo`.
type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
-- Por ejemplo, `cambiar (== Triangulo) (\x -> Rotar (Figura x))` rota
-- todos los triángulos.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar t fun dib = mapDib (\a -> if t a then fun a else figura a) dib 

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib t dib = foldDib (\a -> t a) (||) (||) (||) (||) (||) (||) dib 

-- Todas las básicas satisfacen el predicado.
allFig :: Pred a -> Dibujo a -> Bool
allDib t dib = foldDib (\a -> t a) (&&) (&&) (&&) (&&) (&&) (&&) dib

-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP p1 p2 = \a -> p1 a && p2 a

-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP p1 p2 = \a -> p1 a || p2 a
