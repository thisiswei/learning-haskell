import Control.Monad
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3 * n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n 
  | n < 0 = 0
  | n `mod` 17 == 2 = -43
  | otherwise       = n + 3

data Thing = Shoe
           | Ship
           | SealingWax
           | Ball
           | Fuck
    deriving Show

data IntList = Empty | Cons Int IntList 
  deriving Show

addOneToAll :: IntList -> IntList
addOneToAll Empty = Empty
addOneToAll (Cons x xs) = Cons (x + 1) (addOneToAll xs)

myIntList = Cons 2 (Cons (-3) (Cons 4 Empty))

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

dostuff2 [] = 0
dostuff2 [_] = 0
dostuff2 (x1:x2:_) = x1 + x2

greaterThan100 [] = []
greaterThan100 (x:xs) = if x > 100 then x:greaterThan100 xs
                                   else greaterThan100 xs

greaterThan100Two = filter (> 100)

-- (b -> c) -> (a -> b) -> (a -> c)

foobar :: [Integer] -> Integer
foobar = sum . map (\x -> 7*x + 2) . filter (>3)

foldll _ z [] = []
foldll f z (x:xs) = foldl f (f z x) xs

data Person = Person {firstName :: String,
                     lastName  :: String,
                     age       :: Int,
                     height    :: Float}
                     deriving (Show)
                       
isPalindromes = unlines. map (\x -> if pali x then "palindrome"
                                              else "not pal")
                       . lines
                       where pali x = x == reverse x




--main = print [foo (-3), foo 0, foo 1, foo 36, foo 38]
--main = print (addOneToAll myIntList)

--main = mapIntList (\ x -> x*x) (Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty))))

main = do
  colors <- forM [1, 2, 3, 4] (\a -> do 
      putStrLn $ "whcich color do you associate wit number " ++ show a ++ "?"
      color <- getLine
      return color)
  putStrLn "the color that associated with 1, 2, 3, 4 are: "
  mapM putStrLn colors

