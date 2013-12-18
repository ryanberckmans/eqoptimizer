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

main = do
	eqoptimizer1
	eqoptimizer2
