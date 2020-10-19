module Main where

import System.Console.ANSI

mylength :: [a] -> Integer
mylength []       = 0
mylength (x:xs)   = 1 + mylength xs

-- data Color = Red | Green | Blue | Black | White
data PersonType = Person Name Address
type Name = String
data Address = None | Addr String Int String

instance Show Address where
    show (Addr street hn hnAddition) = street ++ " " ++ show hn ++ hnAddition

instance Show PersonType where
    show (Person name address) = name ++ " lives at " ++ show address

putGreenStr a = do
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
    putStr a
    setSGR []

putGreenStrLn a = do
    putGreenStr a
    putStrLn ""

-- | main, do nothing
main :: IO ()
main = do
    let var1 = 12
    let var2 = 3
    putStrLn "The Division of the Two Numbers is:"
    print (var1 / var2)
    print (mylength "123")

    print Green

    let p = Person "Konstantin" $ Addr "Rokin" 136 "B"
    print p

    putGreenStrLn "Hello World!"


