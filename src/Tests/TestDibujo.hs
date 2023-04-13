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

testfigura :: Test                           
testfigura = TestCase $ assertEqual "figura" (figura 5) (figura 5)

testrotar :: Test
testrotar = TestCase $ assertEqual "rotar" (rotar (figura 5)) (rotar (figura 5))

testespejar :: Test
testespejar = TestCase $ assertEqual "espejar" (espejar (figura 5)) (espejar (figura 5))

testrot45 :: Test
testrot45 = TestCase $ assertEqual "rot45" (rot45 (figura 5)) (rot45 (figura 5))

testapilar :: Test
testapilar = TestCase $ assertEqual "apilar" (apilar 2.0 3.0 (figura 1) (figura 2)) (apilar 2.0 3.0 (figura 1) (figura 2))

testjuntar :: Test
testjuntar = TestCase $ assertEqual "juntar" (juntar 2.0 3.0 (figura 1) (figura 2)) (juntar 2.0 3.0 (figura 1) (figura 2))

testencimar :: Test
testencimar = TestCase $ assertEqual "encimar" (encimar (figura 1) (figura 2)) (encimar (figura 1) (figura 2))

testR180 :: Test
testR180 = TestCase $ assertEqual "r180" (rotar (rotar (figura 5))) (r180 (figura 5))

testR270 :: Test
testR270 = TestCase $ assertEqual "r270" (rotar (rotar (rotar (figura 5)))) (r270 (figura 5))

testapilarOp :: Test
testapilarOp = TestCase $ assertEqual "(.-.)" (apilar 1.0 1.0 (figura 1) (figura 2)) ((figura 1) .-. (figura 2))

testjuntarOp :: Test
testjuntarOp = TestCase $ assertEqual "(///)" (juntar 1.0 1.0 (figura 1) (figura 2)) ((figura 1) /// (figura 2))

testencimarOp :: Test
testencimarOp = TestCase $ assertEqual "(^^^)" (encimar (figura 1) (figura 2)) ((figura 1) ^^^ (figura 2))

testCuarteto :: Test
testCuarteto =
  TestCase $
    assertEqual
      "cuarteto"
      (apilar 1.0 1.0 ((figura 1) /// (figura 2)) ((figura 3) /// (figura 4)))
      (cuarteto (figura 1) (figura 2) (figura 3) (figura 4))

testencimar4 :: Test
testencimar4 =
  TestCase $
    assertEqual
      "encimar4"
      (encimar4 (figura 1))
      (encimar (encimar (encimar (figura 1) (rotar (figura 1))) (rotar (rotar (figura 1)))) (rotar (rotar (rotar (figura 1)))))

testescalar :: Test
testescalar = TestCase $ assertEqual "escalar" (escalar 2.0 2.0 (figura 1)) (escalar 2.0 2.0 (figura 1))
