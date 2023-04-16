module Tests.TestPred where

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
    r180,
    r270,
    rot45,
    rotar,
    (.-.),
    (///),
    (^^^),
  )
import Pred (Pred, allDib, andP, anyDib, cambiar, orP)
import Test.HUnit (Test (..), assertEqual)

testalldibTrue :: Test
testalldibTrue =
  TestCase $ assertEqual
    "allDib"
      (allDib (== 1) (apilar 1 1 (figura 1) (figura 1)))
      True

testalldibFalse :: Test
testalldibFalse =
  TestCase $ assertEqual
    "allDib"
      (allDib (== 1) (apilar 1 1 (figura 1) (figura 2)))
      False

testanydibTrue :: Test
testanydibTrue =
  TestCase $ assertEqual
    "anyDib"
      (anyDib (== 1) (apilar 1 1 (figura 1) (figura 2)))
      True

testanydibFalse :: Test
testanydibFalse =
  TestCase $ assertEqual
    "anyDib"
      (anyDib (== 1) (apilar 1 1 (figura 2) (figura 2)))
      False

testcambiar :: Test
testcambiar =
  TestCase $ assertEqual
    "cambiar"
      (cambiar (== 1) (const (figura 2)) (figura 1))
      (figura 2)

testandp :: Test
testandp =
  TestCase $ assertEqual
    "andP"
      (andP (== 1) (== 2) 1)
      False

testorp :: Test
testorp =
  TestCase $ assertEqual
    "orP"
      (orP (== 1) (== 2) 1)
      True
