import Data.List
import System.IO

-- Opdracht 3.2.1 - sum_of_squares

sum_of_squares :: [Float] -> Float
sum_of_squares [] = 0
sum_of_squares (x:xs) = x^2 + sum_of_squares xs

-- Opdracht 3.2.2 - repeat

my_repeat :: Int -> Int -> [Int]
my_repeat _ 0 = []
my_repeat x t = x : my_repeat x (t - 1)

-- Opdracht 3.2.3 - reverse

my_reverse :: [Int] -> [Int]
my_reverse [] = []
my_reverse (x:xs) = my_reverse xs ++ [x]

-- Opdracht 3.2.4 - Tail recursion

fib_acc :: Int -> Int -> Int -> Int
fib_acc 0 a b = a
fib_acc 1 a b = b
fib_acc x a b = fib_acc (x - 1) b (a + b)

fib_rec :: Int -> Int
fib_rec x = fib_acc x 0 1

-- Opdracht 3.3.1

apply_everything :: [a -> a] -> a -> a
apply_everything [] y = y
apply_everything (x:xs) y = apply_everything xs (x y)

-- Opdracht 3.3.2

-- Mod does not work with doubles
filter_even_on_input :: [(Integer -> Integer)] -> Integer -> [(Integer -> Integer)]
filter_even_on_input [] y = []
filter_even_on_input (x:xs) y
    | x y `mod` 2 == 0 = x : filter_even_on_input xs y
    | otherwise = filter_even_on_input xs y

-- Opdracht 4.1.1

reduce :: (a -> a -> a) -> [a] -> a
reduce l x = foldl1 l x

reduce_4_1_1 = reduce (\x y -> x + 2*y) [1, 2, 4]

-- Opdracht 4.1.2

fac :: Int -> Int
fac x = reduce (*) [1..x]

-- Opdracht 4.1.3

my_foldr :: (a -> a -> a) -> a -> [a] -> a
my_foldr f x xs = reduce (f) (reverse xs ++ [x])

-- Opdracht 4.1.4

my_zip_with :: (a -> b -> c) -> [a] -> [b] -> [c]
my_zip_with _ _ [] = []
my_zip_with _ [] _ = []
my_zip_with f (x:xs) (y:ys) = [f x y] ++ my_zip_with f xs ys

my_zip :: [a] -> [b] -> [(a, b)]
my_zip x y = my_zip_with (\x y -> (x, y)) x y

-- Opdracht 5.1.1

data Peano = Zero | Double Peano | Double_Plus_One Peano
    deriving Show

uitkomst :: Peano -> Int
uitkomst Zero = 0
uitkomst (Double_Plus_One x) = uitkomst x * 2 + 1
uitkomst (Double x) = uitkomst x * 2

d x = Double x
p x = Double_Plus_One x

zero = Zero
one = p zero
two = d one
three = p one
four = d two
five = p two
six = d three
seven =  p three
eight = d four
nine = p four
ten = d five
eleven = p five
twelve = d six
thirteen = p six
fourteen = d seven
fifteen = p seven
sixteen = d eight
seventeen = p eight
eightteen = d nine
nineteen = p nine
twenty = d ten