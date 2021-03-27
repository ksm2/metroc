module Commands.Test (test) where

import Builder.AST
import Builder.IOUtils
import Builder.Instance (Instance, callFuncErr, withInstance)
import Builder.Runtime (withRuntime)
import Chalk
import Control.Monad
import MetroLang.Lang.Model as MetroAST
import MetroLang.Location
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
    putColored Yellow (head (lines reason ++ [""]))
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
      else putStrLn $ "Failed: " ++ testName ++ "\n" ++ indent "  " reason

isTestSuitePassed :: TestSuiteResult -> Bool
isTestSuitePassed suite = passedCases suite == totalCases suite

repeatStr :: Char -> Int -> String
repeatStr _ 0 = ""
repeatStr c n = c : repeatStr c (n - 1)

putStatisticLine :: String -> Int -> Int -> IO ()
putStatisticLine label passed total =
  do
    let failed = total - passed
    putBold label
    putBold ":"
    putStr $ repeatStr ' ' $ 12 - length label
    when (failed > 0) $
      do
        putColoredBold Red $ show failed ++ " failed"
        putStr ", "
    when (passed > 0) $
      do
        putColoredBold Green $ show passed ++ " passed"
        putStr ", "
    putStrLn $ show total ++ " total"

printStatistics :: [TestSuiteResult] -> IO ()
printStatistics tests =
  do
    let countTestSuites = length tests
    let countPassedTestSuites = length $ filter isTestSuitePassed tests
    let countTests = sum $ map totalCases tests
    let countPassedTests = sum $ map passedCases tests
    putStatisticLine "Test Suites" countPassedTestSuites countTestSuites
    putStatisticLine "Tests" countPassedTests countTests

findTests :: MetroAST.Module -> [(MetroAST.Identifier, [MetroAST.TestStatement])]
findTests (MetroAST.Module []) = []
findTests (MetroAST.Module (MetroAST.TestDeclaration i b : xs)) = (i, b) : findTests (MetroAST.Module xs)
findTests (MetroAST.Module (_ : xs)) = findTests (MetroAST.Module xs)

printRuns :: [(MetroAST.Identifier, [MetroAST.TestStatement])] -> IO ()
printRuns = mapM_ printRun

printRun :: (MetroAST.Identifier, [MetroAST.TestStatement]) -> IO ()
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

runTestSuites :: Instance -> Handle -> [(MetroAST.Identifier, [MetroAST.TestStatement])] -> IO [TestSuiteResult]
runTestSuites inst stderrHandle = mapM $ runTestSuite inst stderrHandle

runTestSuite :: Instance -> Handle -> (MetroAST.Identifier, [MetroAST.TestStatement]) -> IO TestSuiteResult
runTestSuite inst stderrHandle (suiteName, stmts) =
  do
    passedCases <- runTestCases inst stmts
    let totalCases = length stmts
    let isSuitePassed = passedCases == totalCases
    stderrText <- readWhileNotEOF stderrHandle
    clearLine
    if isSuitePassed then printTestPassed suiteName else printTestFailed suiteName stderrText
    return $ TestSuiteResult {suiteName, passedCases, totalCases}

runTestCases :: Instance -> [MetroAST.TestStatement] -> IO Int
runTestCases inst = foldM (combineResult (runTestCase inst)) 0

combineResult :: (MetroAST.TestStatement -> IO TestResult) -> Int -> MetroAST.TestStatement -> IO Int
combineResult cb passedSoFar stmt =
  do
    result <- cb stmt
    case result of
      Pass -> return $ passedSoFar + 1
      Fail _ -> return passedSoFar

runTestCase :: Instance -> MetroAST.TestStatement -> IO TestResult
runTestCase inst (MetroAST.TestStatement testCase _) =
  do
    maybeError <- callFuncErr inst testCase
    case maybeError of
      Just err -> return $ Fail err
      Nothing -> return Pass

test :: [String] -> IO ()
test args =
  do
    let relativeGlobs = if null args then ["tests/**/*.metro"] else args
    globs <- mapM makeAbsolute relativeGlobs
    cwd <- getCurrentDirectory
    contents <- listDirectoryRecursive cwd
    let matches = matchGlobs globs contents
    let sources = map Source matches
    inputStrs <- mapM readFile matches
    let inputs = zip sources inputStrs
    let ast = metroToAST True inputs
    let wat = astToWAT True "" inputs ast
    let tests = findTests ast

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
