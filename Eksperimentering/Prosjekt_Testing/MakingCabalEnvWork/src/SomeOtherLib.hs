module SomeOtherLib (someOtherFunc) where

someOtherFunc :: String -> Int -> String
someOtherFunc s = (++ " " ++ s) . show