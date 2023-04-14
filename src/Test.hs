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
            TestLabel "testescalar" testescalar,
            TestLabel "testR180" testR180,
            TestLabel "testR270" testR270,
            TestLabel "testapilarOp" testapilarOp,
            TestLabel "testjuntarOp" testjuntarOp,
            TestLabel "testencimarOp" testencimarOp,
            TestLabel "testCuarteto" testCuarteto,
            TestLabel "testencimar4" testencimar4,
            TestLabel "testalldib" testalldib,
            TestLabel "testanydib" testanydib,
            TestLabel "testcambiar" testcambiar,
            TestLabel "testandp" testandp,
            TestLabel "testorp" testorp
          ]
  results <- runTestTT tests
  let counts = showCounts results
  putStrLn ""
  putStrLn "Results:"
  putStrLn "--------"
  mapM_ (putStrLn . formatResult) (zip (testLabels tests) (testResults results))
  putStrLn "--------"

testLabels :: Test -> [String]
testLabels (TestList xs) = concatMap testLabels xs
testLabels (TestLabel s _) = [s]
testLabels _ = []

testResults :: Counts -> [String]
testResults counts =
  replicate (errors counts) "Failed"
    ++ replicate (failures counts) "Failed"
    ++ replicate (total - errors counts - failures counts) "Passed"
  where
    total = cases counts

formatResult :: (String, String) -> String
formatResult (label, result) = label ++ ": " ++ result