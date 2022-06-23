module MyLib (someFunc, otherfunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

otherfunc :: Int -> Int
otherfunc i = i * i
