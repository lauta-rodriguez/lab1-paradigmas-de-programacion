module Main where

import Test.HUnit
import Tests.TestDibujo
import Tests.TestPred

main :: IO ()
main = do
  let tests =
        TestList
          [ TestLabel "testfigura" testfigura,
            TestLabel "testrotar" testrotar,
            TestLabel "testespejar" testespejar,
            TestLabel "testrot45" testrot45,
            TestLabel "testapilar" testapilar,
            TestLabel "testjuntar" testjuntar,
            TestLabel "testencimar" testencimar,
            TestLabel "testR180" testR180,
            TestLabel "testR270" testR270,
            TestLabel "testapilarOp" testapilarOp,
            TestLabel "testjuntarOp" testjuntarOp,
            TestLabel "testencimarOp" testencimarOp,
            TestLabel "testCuarteto" testCuarteto,
            TestLabel "testencimar4" testencimar4,
            TestLabel "testescalar" testescalar,
            TestLabel "testfoldDib" testfoldDib,
            TestLabel "testmapDib" testmapDib,
            TestLabel "testalldibTrue" testalldibTrue,
            TestLabel "testalldibFalse" testalldibFalse,
            TestLabel "testanydibTrue" testanydibTrue,
            TestLabel "testanydibFalse" testanydibFalse,
            TestLabel "testcambiar" testcambiar,
            TestLabel "testandp" testandp,
            TestLabel "testorp" testorp
          ]

  runTestTT tests
  return ()
