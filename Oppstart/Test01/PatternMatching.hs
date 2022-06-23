myAnd :: Bool -> Bool -> Bool
myAnd True x = x
myAnd False _ = False

myOr :: Bool -> Bool -> Bool
myOr True _ = True
myOr False x = x

myNot :: Bool -> Bool
myNot True = False
myNot False = True

complicated :: Bool -> Bool -> Bool -> Bool -> Bool
complicated a b c d = myAnd a $ myNot $ myAnd b $ myOr c d

main :: IO ()
main = do
  putStrLn "What's your name"
  input <- getLine
  let answer = "Hello " ++ input
  putStrLn answer
