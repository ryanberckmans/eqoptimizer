module EqOptimizer ( 
	Item, 
	Weights,
	Constraints, 
	scoreItems,
	optimizeOld, -- to prevent unused warning 
	optimize,
	countItemsOfType, 
	parseEqFile,
	main ) where

import GHC.Conc
import Control.Parallel.Strategies
import Data.List
import Data.Ord

data Weights = Weights {
	hpWeight :: !Int,
	manaWeight :: !Int,
	hrWeight :: !Int,
	drWeight :: !Int,
	ssWeight :: !Int
} deriving (Show, Eq)

{-
	Constraints and Weights are both a collection of integers,
	with the collection having a 1-to-1 relationship with hp/mana/hr/dr/ss.
	Seems like there should be some reuse here, but I'm not sure what it is,
	so I just copied Weights and renamed the variables.
-}
data Constraints = Constraints {
	minHp :: !Int,
	minMana :: !Int,
	minHr :: !Int,
	minDr :: !Int,
	minSs :: !Int
} deriving (Show, Eq)

data ItemType = Regular | DH | QO deriving (Show, Eq)

data Item = Item {
	itemName :: !String,
	itemHp :: !Int,
	itemMana :: !Int,
	itemHr :: !Int,
	itemDr :: !Int,
	itemSs :: !Int,
	itemType :: !ItemType -- tbd disallow permutations with more than two DH or QO (this threshold should be configurable)
} deriving (Show, Eq)

scoreItem :: Weights -> Item -> Int
scoreItem weights item = (hpWeight weights) * (itemHp item) + (manaWeight weights) * (itemMana item) + (hrWeight weights) * (itemHr item) + (drWeight weights) * (itemDr item) + (ssWeight weights) * (itemSs item)

scoreItems :: Weights -> [Item] -> Int
scoreItems weights items = sum (map (scoreItem weights) items)

countItemsOfType :: ItemType -> [Item] -> Int
countItemsOfType typeToCount items = length (filter (\item -> itemType item == typeToCount) items)

qoAndDhOk :: [Item] -> Bool
qoAndDhOk items
	| dhCount > 2 || qoCount > 1 = False
	| otherwise = True
	where 
		dhCount = countItemsOfType DH items
		qoCount = countItemsOfType QO items

-- I'm not sure of a better way to do this than with the ugly constraintFunctions and attributeFunctions.
-- This ugliness is probably related to Constraints being a copypaste of Weights. 
-- I think my data problem is turning into a function problem.
constraintsOk :: Constraints -> [Item] -> Bool
constraintsOk constraints items = 
	foldl1 (&&) (map (\(constraint, attribute) -> (constraint constraints) <= sum (map attribute items)) 
		(zip constraintFunctions attributeFunctions))
	where
		constraintFunctions = [minHp, minMana, minHr, minDr, minSs]
		attributeFunctions = [itemHp, itemMana, itemHr, itemDr, itemSs]

-- first pass optimize using 'sequence'
-- 'sequence' explicitly (strictly? non-lazily?) generates the cross product of all eq,
-- which consumes many gbs of memory and crashes everything :).
-- Exponential memory usage in the number of eq locations, O(2^(|items|)
optimizeOld :: Weights -> [[Item]] -> [Item]
optimizeOld weights items = maximumBy (comparing (scoreItems weights)) (filter qoAndDhOk (sequence items))

-- Recurse through each element of the cross product, propagating the best sets
-- back up the call stack. 
-- Inferior sets are discarded immediately, using O(|items|) memory, ie the maximum stack depth.
-- Optimize by aborting a subtree as soon as the partial set fails constraints
optimize :: Weights -> Constraints -> [[Item]] -> [Item]
optimize weights constraints items = optimizeInternal 0 items []
	where
		parallelCutoff = if GHC.Conc.numCapabilities > 1 then 5 else 0 :: Int
		optimizeInternal :: Int -> [[Item]] -> [Item] -> [Item]
		optimizeInternal _ [] candidateSet = if qoAndDhOk candidateSet && constraintsOk constraints candidateSet then candidateSet else []
		optimizeInternal depth (eqLoc:eqLocs) partialCandidateSet =
			if not (qoAndDhOk partialCandidateSet) then [] else do
				let candidateSets = (if depth < parallelCutoff then parMap rpar else map) (\eq -> optimizeInternal (depth+1) eqLocs (eq:partialCandidateSet)) eqLoc
				if length candidateSets > 0 then maximumBy (comparing (scoreItems weights)) candidateSets else []

{-
   format for one item:
   "next
   hp
   mana
   hr
   dr
   ss
   [DH | QO] name"
-}

parseItemType :: String -> ItemType
parseItemType ('D':'H':_) = DH
parseItemType ('Q':'O':_) = QO
parseItemType _ = Regular

parseItem :: [String] -> Item
parseItem itemLines = Item 
	(itemLines !! 6) -- name
	(read (itemLines !! 1) :: Int) -- hp
	(read (itemLines !! 2) :: Int) -- mana
	(read (itemLines !! 3) :: Int) -- hr
	(read (itemLines !! 4) :: Int) -- dr
	(read (itemLines !! 5) :: Int) -- ss
	(parseItemType (itemLines !! 6)) -- ItemType

-- convert the classic EqOptimizer file format into a list of Items,
-- given a passed String list representing each line of text in an eq file
parseEqFile :: [String] -> [Item]
parseEqFile [] = []
parseEqFile file
	| (length file) `mod` 7 == 0 = parseItem (take 7 file) : parseEqFile (drop 7 file)
	| otherwise = error "expected eq file to contain N*7 lines for N items"

main :: IO ()
main = do
	putStrLn $ "number of threads: " ++ show (GHC.Conc.numCapabilities)
	lightEq <- readFile "/home/ryan/Dropbox/medievia/EqOpti/lightEq.txt"
	fingerOneEq <- readFile "/home/ryan/Dropbox/medievia/EqOpti/fingerOneEq.txt"
	bodyEq <- readFile "/home/ryan/Dropbox/medievia/EqOpti/bodyEq.txt"
	headEq <- readFile "/home/ryan/Dropbox/medievia/EqOpti/headEq.txt"
	legsEq <- readFile "/home/ryan/Dropbox/medievia/EqOpti/legsEq.txt"
	feetEq <- readFile "/home/ryan/Dropbox/medievia/EqOpti/feetEq.txt"
	handsEq <- readFile "/home/ryan/Dropbox/medievia/EqOpti/handsEq.txt"
	armsEq <- readFile "/home/ryan/Dropbox/medievia/EqOpti/armsEq.txt"
	shieldEq <- readFile "/home/ryan/Dropbox/medievia/EqOpti/shieldEq.txt"
	aboutEq <- readFile "/home/ryan/Dropbox/medievia/EqOpti/aboutEq.txt"
	waistEq <- readFile "/home/ryan/Dropbox/medievia/EqOpti/waistEq.txt"
	wristOneEq <- readFile "/home/ryan/Dropbox/medievia/EqOpti/wristOneEq.txt"
	wieldEq <- readFile "/home/ryan/Dropbox/medievia/EqOpti/wieldEq.txt"
	heldEq <- readFile "/home/ryan/Dropbox/medievia/EqOpti/heldEq.txt"

	let all_eq = [
		parseEqFile (lines lightEq),
		parseEqFile (lines fingerOneEq),
		parseEqFile (lines bodyEq),
		parseEqFile (lines headEq),
		parseEqFile (lines legsEq),
		parseEqFile (lines feetEq),
		parseEqFile (lines handsEq),
		parseEqFile (lines armsEq),
		parseEqFile (lines shieldEq),
		parseEqFile (lines aboutEq),
		parseEqFile (lines waistEq),
		parseEqFile (lines wristOneEq),
		parseEqFile (lines wieldEq),
		parseEqFile (lines heldEq)
		]
	let default_weights = Weights 1 1 0 8 0
	let default_constraints = Constraints 200 0 10 40 2
	let !best_set = optimize default_weights default_constraints all_eq
	print default_weights
	print default_constraints
	print best_set
	putStrLn $ "best set score: " ++ show (scoreItems default_weights best_set)
	putStrLn $ "best set dh#: " ++ show (countItemsOfType DH best_set)
	putStrLn $ "best set qo#: " ++ show (countItemsOfType QO best_set)
