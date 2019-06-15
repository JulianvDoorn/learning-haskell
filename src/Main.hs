import Data.List
import System.IO
import Data.Maybe
import Data.Char

-- Opdracht 3.1.1 - functies [PORTFOLIO]

-- wave: Pure functie, maar niet functioneel
-- wait_for_password: Niet-pure functie
-- set_width: Niet-pure functie
-- make_sense: Pure functie en functioneel
-- latin: Pure functie en functioneel
-- latinese: Ik verwacht dat regex niet puur is, maar het wel referentieel
--           transparent is. Dus puur? Wel functioneel
-- say_it_in_latin: Niet-pure functie

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

-- Opdracht 3.3.2 [PORTFOLIO]

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

-- Opdracht 4.1.3 [PORTFOLIO]

my_foldr :: (a -> a -> a) -> a -> [a] -> a
my_foldr f x xs = reduce (\x y -> f y x) (reverse xs ++ [x])

-- Opdracht 4.1.4 [PORTFOLIO]

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

-- Opdracht 5.2.1 [PORTFOLIO]
-- Poor man's git

type Hash = Int

class IsHash a where
    isHash :: a -> Hash -> Bool

data Rev = Rev { desc :: String, hash :: Hash }
    deriving Show

instance IsHash Rev where
    isHash self h = hash self == h

updateRev :: Rev -> Maybe String -> Maybe Int -> Rev
updateRev self d h = Rev (fromMaybe (desc self) d) (fromMaybe (hash self) h)

data History = History { rev :: Rev, children :: [History], thread :: Maybe History }
    deriving Show

addChild :: History -> History -> History
addChild self appended = History (rev appended) (children appended) (Just self)

removeChild :: History -> Hash -> History
removeChild self h =    
    History (rev self)
    (filter (\x -> not (isHash x h)) (children self))
    (thread self)

findCorrectChild :: History -> [History] -> Hash -> Maybe History
findCorrectChild self [] _ = Just self
findCorrectChild self (x:xs) h
    | isHash x h == True = Just $ addToThread x $ removeChild self h
    | otherwise = findCorrectChild self xs h

descend :: History -> Hash -> History
descend self x = fromMaybe self (findCorrectChild self (children self) x)

ascend :: History -> History
ascend self = fromMaybe self $ thread self

commit :: History -> Rev -> History
commit self r = do
    let newHistory = addChild self (History r [] Nothing)
    descend newHistory (hash $ rev newHistory)

updateHistory :: History -> (Rev -> Rev) -> History
updateHistory self action = History (action (rev self)) (children self) (thread self)

removeThread :: History -> History
removeThread self = History (rev self) (children self) Nothing

addToThread :: History -> History -> History
addToThread self parent = History (rev self) (children self) (Just parent)

root :: History -> History
root self = 
    case thread self of
        Just s -> root $ fromMaybe s $ thread s
        Nothing -> self

head :: History -> Rev
head self = rev self

instance IsHash History where
    isHash self h = hash (rev self) == h

h = History (Rev "Initial commit" 1) [] Nothing
h2 = commit h (Rev "Added README.md" 2)

complex = root ((History (Rev "Initial commit" 1) [] Nothing)
        `commit` (Rev "Added README.md" 2)
        `commit` (Rev "Actual work" 4))
        `commit` (Rev "Fork, as READMEs are for wimps!" 3)        

-- updateHistory h2 (\x -> updateRev x (Just "Altered space and time!") Nothing)