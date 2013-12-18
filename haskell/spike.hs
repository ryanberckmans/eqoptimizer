import Data.List
import Data.Ord

{-
	eqoptimizer trial #1

	Simplified eqoptimizer to kick off haskell impl:

	Equipment choices will be modelled as [[int],[int]], representing a list of two equipment slots, each with a list of integers as the possible eq items.

	Character preferences such as base hp, min total hp, are excluded entirely.

	The eq solution is a list of the max integers from each of the two eq slots. The solved "stat" is the sum of the chosen integers.
-}

possible_eq :: [[Int]]
possible_eq = [[1,3,7],[3,4,9]]

optimize_eq :: [[Int]] -> [Int]
optimize_eq eq = [maximum x | x <- eq]

eq_stat :: [Int] -> Int
eq_stat eq_set = sum eq_set

eqoptimizer1 = do
	putStrLn "eqoptimizer trial #1"
	putStrLn $ "possible_eq: " ++ show possible_eq
	putStrLn $ "optimized_set: " ++ show (optimize_eq possible_eq)
	putStrLn $ "eq_stat: " ++ show (eq_stat(optimize_eq possible_eq))

{-
	eqoptimizer trial #2

	As in #1, but equipment is now a data type with a name and hp amount
-}

data Item1 = Item1 {
	itemName :: String,
	itemHp :: Int
	} deriving (Show)

possible_eq2 :: [[Item1]]
possible_eq2 = [
	[Item1 "Diamond orb of Tyche" 45, Item1 "Pulsating Ruby" 30],
	[Item1 "A bracelet of strange silvery mist" 37, Item1 "A blue-steel bracer from bloodstone" 44]]

optimize_eq2 :: [[Item1]] -> [Item1]
optimize_eq2 eqs = [maximumBy (comparing itemHp) eq | eq <- eqs]

set_hp :: [Item1] -> Int
set_hp eqs = sum [itemHp eq | eq <- eqs]

eqoptimizer2 = do
	putStrLn "eqoptimizer trial #2"
	putStrLn $ "possible_eq: " ++ show possible_eq2
	let optimized_eq = optimize_eq2 possible_eq2
	putStrLn $ "optimized_set: " ++ show optimized_eq
	putStrLn $ "set_hp: " ++ show (set_hp optimized_eq)

{-
	eqoptimizer trial #3

	As in #2, but add mana to Item and score each set using a weights for hp and mana
-}

data Item2 = Item2 {
	itemName2 :: String,
	itemHp2 :: Int,
	itemMana :: Int
} deriving (Show)

data Weights = Weights {
	hpWeight :: Int,
	manaWeight :: Int
} deriving (Show)

score_item :: Weights -> Item2 -> Int
score_item weights item = (hpWeight weights) * (itemHp2 item) + (manaWeight weights) * (itemMana item)

score_set :: Weights -> [Item2] -> Int
score_set weights items = sum (map (score_item weights) items)

possible_eq3 :: [[Item2]]
possible_eq3 = [
	[Item2 "Diamond orb of Tyche" 45 0, Item2 "Rod of dragonhide" 30 20],
	[Item2 "A bracelet of strange silvery mist" 37 20, Item2 "A blue-steel bracer from bloodstone" 48 0]]

-- Given a list of each location's items (a sublist), return all permutations of the eq, ie the cartesian product of all the sublists.
-- I've since found out that this is available with "sequence". In fact, there's no reason for this type signature
-- to specificy Item2, nor for the function to be named eq_permutations. It's an anonymous cartesian product of n lists.
eq_permutations :: [[Item2]] -> [[Item2]]
eq_permutations [] = []
eq_permutations (eqs_this_location:[]) = [[item] | item <- eqs_this_location]
eq_permutations (eqs_this_location:eqs_other_locations) = [ item:permutation| item <- eqs_this_location, permutation <- (eq_permutations eqs_other_locations)]
-- ie sequence possible_eq3 == eq_permutations possible_eq3, if Item2 derived equality

arbitrary_weights = Weights 10 10

eqoptimizer3 = do
	putStrLn "eqoptimizer trial #3"
	putStrLn $ "arbitrary weights: " ++ show(arbitrary_weights)
	putStrLn "all scored combinations of eq:"
	mapM_ print [(eq_set, score_set arbitrary_weights eq_set) | eq_set <- (eq_permutations possible_eq3)]
	let best_set = maximumBy (comparing (score_set arbitrary_weights)) (eq_permutations possible_eq3)
	print "best set:"
	print (best_set, score_set arbitrary_weights best_set)

main = do
	eqoptimizer1
	eqoptimizer2
	eqoptimizer3
