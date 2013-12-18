module EqOptimizer ( Item, Weights, score_items, max_score_permutation ) where

import Data.List
import Data.Ord

data Weights = Weights {
	hpWeight :: Int,
	manaWeight :: Int,
	hrWeight :: Int,
	drWeight :: Int,
	ssWeight :: Int
}

data Item = Item {
	itemName :: String,
	itemHp :: Int,
	itemMana :: Int,
	itemHr :: Int,
	itemDr :: Int,
	itemSs :: Int,
	isDh :: Bool, -- tbd disallow permutations with more than two DH
	isQo :: Bool -- tbd disallow permutations with more than two QO
}

score_item :: Weights -> Item -> Int
score_item weights item = (hpWeight weights) * (itemHp item) + (manaWeight weights) * (itemMana item) + (hrWeight weights) * (itemHr item) + (drWeight weights) * (itemDr item) + (ssWeight weights) * (itemSs item)

score_items :: Weights -> [Item] -> Int
score_items weights items = sum (map (score_item weights) items)

max_score_permutation :: Weights -> [[Item]] -> [Item]
max_score_permutation weights items = maximumBy (comparing (score_items weights)) (sequence items)
