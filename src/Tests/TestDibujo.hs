module Tests.TestDibujo where

import Dibujo
  ( Dibujo,
    apilar,
    cuarteto,
    encimar,
    encimar4,
    espejar,
    figura,
    foldDib,
    juntar,
    escalar,
    r180,
    r270,
    rot45,
    rotar,
    (.-.),
    (///),
    (^^^),
  )
import Test.HUnit (Test (..), assertEqual)

data TriORect = Triangulo | Rectangulo deriving (Eq, Show)

testfigura :: Test
testfigura = 
  TestCase $ assertEqual 
    "figura" 
      (show $ figura 5)
      "Figura 5"

testrotar :: Test
testrotar =
  TestCase $ assertEqual
    "rotar"
      (show $ rotar (figura 5))
      "Rotar (Figura 5)"

testespejar :: Test
testespejar =
  TestCase $ assertEqual
    "espejar"
      (show $ espejar (figura 5))
      "Espejar (Figura 5)"

testrot45 :: Test
testrot45 =
  TestCase $ assertEqual
    "rot45"
      (show $ rot45 (figura 5))
      "Rot45 (Figura 5)"

testapilar :: Test
testapilar =
  TestCase $ assertEqual
    "apilar"
      (show $ apilar 2.0 3.0 (figura 1) (figura 2))
      "Apilar 2.0 3.0 (Figura 1) (Figura 2)"

testjuntar :: Test
testjuntar =
  TestCase $ assertEqual
    "juntar"
      (show $ juntar 2.0 3.0 (figura 1) (figura 2))
      "Juntar 2.0 3.0 (Figura 1) (Figura 2)"

testencimar :: Test
testencimar =
  TestCase $ assertEqual
    "encimar"
      (show $ encimar (figura 1) (figura 2))
      "Encimar (Figura 1) (Figura 2)"

testR180 :: Test
testR180 =
  TestCase $ assertEqual
    "r180"
      (show $ r180 (figura 5))
      "Rotar (Rotar (Figura 5))"

testR270 :: Test
testR270 =
  TestCase $ assertEqual
    "r270"
      (show $ r270 (figura 5))
      "Rotar (Rotar (Rotar (Figura 5)))"

testapilarOp :: Test
testapilarOp =
  TestCase $ assertEqual
    "(.-.)"
      (show $ figura 1 .-. figura 2)
      "Apilar 1.0 1.0 (Figura 1) (Figura 2)"

testjuntarOp :: Test
testjuntarOp =
  TestCase $ assertEqual
    "(///)"
      (show $ figura 1 /// figura 2)
      "Juntar 1.0 1.0 (Figura 1) (Figura 2)"

testencimarOp :: Test
testencimarOp =
  TestCase $ assertEqual
    "(^^^)"
      (show $ figura 1 ^^^ figura 2)
      "Encimar (Figura 1) (Figura 2)"

testCuarteto :: Test
testCuarteto =
  TestCase $ assertEqual
    "cuarteto"
      (show $ 
        cuarteto (figura 1) (figura 2) (figura 3) (figura 4))
      "Apilar 1.0 1.0 (Juntar 1.0 1.0 (Figura 1) (Figura 2)) (Juntar 1.0 1.0 (Figura 3) (Figura 4))"

testencimar4 :: Test
testencimar4 =
  TestCase $ assertEqual
    "encimar4"
      (show $ encimar4 (figura 1))                                                                          
      "Encimar (Encimar (Encimar (Figura 1) (Rotar (Figura 1))) (Rotar (Rotar (Figura 1)))) (Rotar (Rotar (Rotar (Figura 1))))"

testescalar :: Test
testescalar =
  TestCase $ assertEqual
    "escalar"
      (show $ escalar 2.0 2.0 (figura 1))
      "Escalar 2.0 2.0 (Figura 1)"

-- cuenta la cantidad de figuras que hay en un dibujo
testfoldDib :: Test
testfoldDib =
  TestCase $ assertEqual
    "foldDib"
    3
      (foldDib
        (const 1)
        id
        id
        id
        (\_ _ x y -> x + y)
        (\_ _ x y -> x + y)
        (+)
        (\_ _ x -> x)
        (encimar 
          (juntar 1.0 1.0 (figura Triangulo) (figura Rectangulo)) 
          (rotar (figura Rectangulo))
        )
      )
