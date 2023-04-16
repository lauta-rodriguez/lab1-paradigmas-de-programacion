{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pred
  ( Pred,
    cambiar,
    anyDib,
    allDib,
    orP,
    andP,
  )
where

import Dibujo
  ( Dibujo,
    ciclar,
    cuarteto,
    encimar4,
    figura,
    foldDib,
    mapDib,
    (.-.),
    (///),
    (^^^),
  )

-- `Pred a` define un predicado sobre figuras básicas. Por ejemplo,
-- `(== Triangulo)` es un `Pred TriOCuat` que devuelve `True` cuando la
-- figura es `Triangulo`.
type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
-- Por ejemplo, `cambiar (== Triangulo) (\x -> Rotar (Figura x))` rota
-- todos los triángulos.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar pred mod = mapDib (\a -> if pred a then mod a else figura a)

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib pred = foldDib
  pred
  id
  id
  id
  (\_ _ b1 b2 -> b1 || b2)
  (\_ _ b1 b2 -> b1 || b2)
  (||)
  (\_ _ res -> res)

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib pred = foldDib
    pred
    id
    id
    id
    (\_ _ b1 b2 -> b1 && b2)
    (\_ _ b1 b2 -> b1 && b2)
    (&&)
    (\_ _ b -> b)

-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP p1 p2 a = p1 a && p2 a

-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP p1 p2 a = p1 a || p2 a
