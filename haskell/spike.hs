
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

main = do
	eqoptimizer1
