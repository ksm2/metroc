module Commands.Test (test) where

import Builder.AST
import Builder.IOUtils
import Builder.Instance (Instance, callFunc, withInstance)
import Builder.Runtime (withRuntime)
import Chalk
import MetroLang.AST as MetroAST
import System.Directory

surroundSpaces :: String -> String
surroundSpaces str = " " ++ str ++ " "

printTestName :: Color -> [Char] -> String -> IO ()
printTestName color label testName =
  do
    background color (surroundSpaces label)
    putStr " "
    boldLn testName

countTestCases :: (MetroAST.Identifier, MetroAST.TestBody) -> Int
countTestCases (_, MetroAST.TestBody stmts) = length stmts

repeatStr :: Char -> Int -> String
repeatStr _ 0 = ""
repeatStr c n = c : repeatStr c (n - 1)

putStatisticLine :: String -> Int -> Int -> IO ()
putStatisticLine label failed passed =
  do
    putBold label
    putBold ":"
    putStr $ repeatStr ' ' $ 12 - (length label)
    if failed > 0
      then do
        putColored Red $ (show failed) ++ " failed"
        putStr ", "
      else do
        return ()
    putColored Green $ (show passed) ++ " passed"
    putStr ", "
    putStrLn $ (show $ failed + passed) ++ " total"

printStatistics :: [(MetroAST.Identifier, MetroAST.TestBody)] -> IO ()
printStatistics tests =
  do
    countTestSuites <- return $ length tests
    countTests <- return $ foldl (+) 0 $ map countTestCases tests
    putStatisticLine "Test Suites" 0 countTestSuites
    putStatisticLine "Tests" 0 countTests

findTests :: MetroAST.Module -> [(MetroAST.Identifier, MetroAST.TestBody)]
findTests (MetroAST.Mod []) = []
findTests (MetroAST.Mod ((MetroAST.Test i b) : xs)) = (i, b) : findTests (MetroAST.Mod xs)
findTests (MetroAST.Mod (_ : xs)) = findTests (MetroAST.Mod xs)

printRuns :: [(MetroAST.Identifier, MetroAST.TestBody)] -> IO ()
printRuns = mapM_ printRun

printRun :: (MetroAST.Identifier, MetroAST.TestBody) -> IO ()
printRun (testName, _) = printTestName Yellow "RUNS" testName

runTests :: Instance -> [(MetroAST.Identifier, MetroAST.TestBody)] -> IO ()
runTests inst = mapM_ $ runTest inst

runTest :: Instance -> (MetroAST.Identifier, MetroAST.TestBody) -> IO ()
runTest inst (testName, (MetroAST.TestBody stmts)) =
  do
    runTestCases inst stmts
    clearLine
    printTestName Green "PASS" testName

runTestCases :: Instance -> [MetroAST.TestStmt] -> IO ()
runTestCases inst = mapM_ $ runTestCase inst

runTestCase :: Instance -> MetroAST.TestStmt -> IO ()
runTestCase inst (MetroAST.ItStmt testCase _) =
  callFunc inst testCase

test :: [String] -> IO ()
test args =
  do
    relativeGlobs <- return $ if null args then ["tests/**/*.metro"] else args
    globs <- mapM makeAbsolute relativeGlobs
    cwd <- getCurrentDirectory
    contents <- listDirectoryRecursive cwd
    matches <- return $ matchGlobs globs contents
    inputStrs <- mapM readFile matches
    ast <- return $ metroToAST True $ zip matches inputStrs
    wat <- return $ astToWAT True "" ast
    tests <- return $ findTests ast

    printRuns tests
    moveUp (length tests)

    withRuntime $ \runtime ->
      withInstance runtime wat $ \wasmInstance ->
        runTests wasmInstance tests

    putStrLn ""
    printStatistics tests
