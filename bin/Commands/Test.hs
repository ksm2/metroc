module Commands.Test (test) where

import Builder.AST
import Builder.IOUtils
import Builder.Instance (Instance, callFuncErr, withInstance)
import Builder.Runtime (withRuntime)
import Chalk
import Control.Monad
import MetroLang.AST as MetroAST
import MetroLang.Utils
import System.Directory
import System.IO
import System.IO.Temp
import Tty

data TestResult = Pass | Fail String deriving (Show)

data TestSuiteResult = TestSuiteResult
  { suiteName :: String,
    passedCases :: Int,
    totalCases :: Int
  }

surroundSpaces :: String -> String
surroundSpaces str = " " ++ str ++ " "

printTestName :: Color -> String -> String -> String -> IO ()
printTestName color label testName reason =
  do
    background color (surroundSpaces label)
    putStr " "
    putBold testName
    putStr "  "
    putColored Yellow (head ((lines reason) ++ [""]))
    putStrLn ""

printTestPassed :: String -> IO ()
printTestPassed testName =
  do
    tty <- isTty
    if tty
      then printTestName Green "PASS" testName ""
      else putStrLn $ "Passed: " ++ testName

printTestFailed :: String -> String -> IO ()
printTestFailed testName reason =
  do
    tty <- isTty
    if tty
      then printTestName Red "FAIL" testName reason
      else putStrLn $ "Failed: " ++ testName ++ "\n" ++ (indent "  " reason)

isTestSuitePassed :: TestSuiteResult -> Bool
isTestSuitePassed suite = (passedCases suite) == (totalCases suite)

repeatStr :: Char -> Int -> String
repeatStr _ 0 = ""
repeatStr c n = c : repeatStr c (n - 1)

putStatisticLine :: String -> Int -> Int -> IO ()
putStatisticLine label passed total =
  do
    failed <- return $ total - passed
    putBold label
    putBold ":"
    putStr $ repeatStr ' ' $ 12 - (length label)
    if failed > 0
      then do
        putColoredBold Red $ (show failed) ++ " failed"
        putStr ", "
      else do
        return ()
    if passed > 0
      then do
        putColoredBold Green $ (show passed) ++ " passed"
        putStr ", "
      else do
        return ()
    putStrLn $ (show $ total) ++ " total"

printStatistics :: [TestSuiteResult] -> IO ()
printStatistics tests =
  do
    countTestSuites <- return $ length tests
    countPassedTestSuites <- return $ length $ filter isTestSuitePassed tests
    countTests <- return $ foldl (+) 0 $ map totalCases tests
    countPassedTests <- return $ foldl (+) 0 $ map passedCases tests
    putStatisticLine "Test Suites" countPassedTestSuites countTestSuites
    putStatisticLine "Tests" countPassedTests countTests

findTests :: MetroAST.Module -> [(MetroAST.Identifier, MetroAST.TestBody)]
findTests (MetroAST.Mod []) = []
findTests (MetroAST.Mod ((MetroAST.Test i b) : xs)) = (i, b) : findTests (MetroAST.Mod xs)
findTests (MetroAST.Mod (_ : xs)) = findTests (MetroAST.Mod xs)

printRuns :: [(MetroAST.Identifier, MetroAST.TestBody)] -> IO ()
printRuns = mapM_ printRun

printRun :: (MetroAST.Identifier, MetroAST.TestBody) -> IO ()
printRun (testName, _) = printTestName Yellow "RUNS" testName ""

readWhileNotEOF :: Handle -> IO String
readWhileNotEOF stderrHandle =
  do
    isHandleEOF <- hIsEOF stderrHandle
    if isHandleEOF
      then return ""
      else do
        line <- hGetLine stderrHandle
        next <- readWhileNotEOF stderrHandle
        return $ line ++ "\n" ++ next

runTestSuites :: Instance -> Handle -> [(MetroAST.Identifier, MetroAST.TestBody)] -> IO [TestSuiteResult]
runTestSuites inst stderrHandle = mapM $ runTestSuite inst stderrHandle

runTestSuite :: Instance -> Handle -> (MetroAST.Identifier, MetroAST.TestBody) -> IO TestSuiteResult
runTestSuite inst stderrHandle (suiteName, (MetroAST.TestBody stmts)) =
  do
    passedCases <- runTestCases inst stmts
    totalCases <- return $ length stmts
    isSuitePassed <- return $ passedCases == totalCases
    stderrText <- readWhileNotEOF stderrHandle
    clearLine
    case isSuitePassed of
      True -> printTestPassed suiteName
      False -> printTestFailed suiteName stderrText
    return $ TestSuiteResult {suiteName, passedCases, totalCases}

runTestCases :: Instance -> [MetroAST.TestStmt] -> IO Int
runTestCases inst = foldM (combineResult (runTestCase inst)) 0

combineResult :: (MetroAST.TestStmt -> IO TestResult) -> Int -> MetroAST.TestStmt -> IO Int
combineResult cb passedSoFar stmt =
  do
    result <- cb stmt
    case result of
      Pass -> return $ passedSoFar + 1
      Fail _ -> return passedSoFar

runTestCase :: Instance -> MetroAST.TestStmt -> IO TestResult
runTestCase inst (MetroAST.ItStmt testCase _) =
  do
    maybeError <- callFuncErr inst testCase
    case maybeError of
      Just err -> return $ Fail err
      Nothing -> return Pass

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

    stderrPath <- emptySystemTempFile "metro"
    stderrHandle <- openFile stderrPath ReadMode

    ifTty $ do
      printRuns tests
      moveUp (length tests)

    testSuiteResults <- withRuntime $ \runtime ->
      withInstance runtime stderrPath wat $ \wasmInstance ->
        runTestSuites wasmInstance stderrHandle tests

    removeFile stderrPath

    putStrLn ""
    printStatistics testSuiteResults
