module Main where

import Test.HUnit
import Tests.TestDibujo (testCuarteto, testR180, testR270, testapilar, testapilarOp, testencimar, testencimar4, testencimarOp, testescalar, testespejar, testfigura, testjuntar, testjuntarOp, testrot45, testrotar)
import Tests.TestPred (testalldib, testandp, testanydib, testcambiar, testorp)

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
            TestLabel "testalldib" testalldib,
            TestLabel "testanydib" testanydib,
            TestLabel "testcambiar" testcambiar,
            TestLabel "testandp" testandp,
            TestLabel "testorp" testorp
          ]

  runTestTT tests
  return ()
