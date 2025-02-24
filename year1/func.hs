-- problem 1: write a fucntion named mySignum :: Int -> Int that returns:
-- -1 for negative numbers
-- 0 for zero
-- 1 for positive numbers

mySignum :: Int -> Int
mySignum x
  | x > 0     =  1
  | x < 0     = -1
  | otherwise =  0

-- problem 2: implement factorial :: Integer -> Integer

factorial :: Integer -> Integer
factorial 1 = 1
factorial x = x * factorial (x - 1)

-- problem 3: implement fib :: Integer -> Integer

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- problem 4: implement myMap :: (a _. b) -> [a] -> [b]

myMap :: (a -> b) -> [a] -> [b]
myMap _    []     = []
myMap func (x:xs) = func x : myMap func xs

-- problemm 5: implemnt myFilter :: (a -> Bool) -> [a] -> [a]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _    []     = []
myFilter func (x:xs)
  | func x           = x : myFilter func xs
  | otherwise        =  myFilter func xs

-- problem 6: implement myFoldl :: (b -> a -> b) -> b -> a -> b

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _    acc []     = acc
myFoldl func acc (x:xs) =  myFoldl func acc xs `func` x


-- probllem 7: implement myFoldr :: (a -> b -> b) -> b -> a -> b

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _    acc []         = acc
myFoldr func acc (x:xs)     = x `func` (myFoldr func acc xs)

-- problem 8: implement mySum :: [a] -> a

mySum :: Num a  =>  [a] -> a
mySum []     = 0
mySum (x:xs) = foldl (+) x xs

-- problem 9: implement myPythagoreanTriples :: Int -> Int -> Int -> Int

myPythagoreanTriples :: Int ->  [(Int, Int, Int)]
myPythagoreanTriples limit = [(a, b, c) | a <- [1..limit], b <- [1..limit], c <- [1..limit],  a^2 + b^2 - c^2 == 0]


 
