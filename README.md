# EqOptimizer

Character min-maxing for the game Medievia.com.

EqOptimizer outputs the player best equipment set given a list of possible equipment and character preferences.

# Haskell

I'm implementing EqOptimizer in haskell for fun. My first time working with haskell :-).

## Optimization

*This section inked on 2013/12/19*

The core of EqOptimizer is an optimization function that goes through all combinations of equipment and spits out the best set.

I was pretty happy when the first pass was concise and working:

```haskell
optimize1 :: Weights -> [[Item]] -> [Item]
optimize1 weights items = maximumBy (comparing (scoreItems weights)) 
								(filter okDhAndQoCount (sequence items))
```

Yay! Unfortunately `sequence items` evaluates the cartesian product of the equipment for each wearable location, which is about 100 billion records for my (literally modest) dataset. This uses `O(2^(# of locations))` memory - all the memory. So my computer blew up.

### Space Massacre

My problem is that I don't start evaluating equipment sets until they're all built. It's like picking your favorite meal by eating twenty different plates in one sitting, and then trying to form an opinion.

I need to swap `sequence items` for a recursive approach that will keep track of the current best equipment set and forget the losers asap.

A bit of head scratching and I had something like this:

```haskell
optimize2 :: Weights -> [[Item]] -> [Item]
optimize2 weights items = optimizeInternal weights items []
        where
            optimizeInternal :: Weights -> [[Item]] -> [Item] -> [Item]
            optimizeInternal _ [] candidateSet = candidateSet
            optimizeInternal weights (eqLoc:eqLocs) partialCandidateSet = 
                    maximumBy (comparing (scoreItems weights)) 
                       (map (\eq -> optimizeInternal weights eqLocs (eq:partialCandidateSet)) eqLoc)
```

Feeling triumphant, I hit 'run' and waited for my first real results. And waited. Boy, my software is sophisticated - it takes TIME to run. Approximately forever.

### Time to make it work

`optimize2` takes ~hours to run, crap. This app is supposed to help, and waiting hours between results isn't helpful. In fact, the app is supposed to be run dozens of times in rapid succession using tuning parameters.

So I dug for compiler flags, easy enough:

```shell
ghc -O2 -make -main-is EqOptimizer EqOptimizer.hs
```

Great, `-O2` sped it up 10x or something. However, `forever / 10 ~= 30-60+ minutes`, so I'm not out of the woods yet. More like still lost in a new part of the woods.

I found out that you can annotate Haskell datatypes as 'strict', which means any expression of that type will be evaluated immediately, instead of waiting until its needed:

```haskell
data Item = Item {
	itemName :: !String,
	itemHp :: !Int,
	...
```

The exclaimation marks (called bangs in Haskell) denote strictness, the prior format being `itemName :: String,`. In imperative terms, strictness means the entire data structure is instantiated into memory as soon as you have a reference to it. What? "Uh, isn't that how it always works? I didn't know there was an alternative?" In Haskell, you can possess a "reference" to an "object instance" without the object actually being allocated in memory (or its attributes even being computed). I don't particularly grok the upside, yet, but at a basic level it uses less memory with added computational overhead to manage the laziness.

Strictness and a few other tricks have me down to ~90 seconds, still too slow for users.

### Algorithm Sauce

Still too slow at 90s. You might have noticed that `optimize1` contained a clause not present in `optimize2`, testing sets for `okDhAndQoCount` and trashing ones that fail.

Equipment sets in Medievia are mostly regular gear, but every player gets up to two pieces of dragonhide (DH) equipment, and one quest object (QO).

The algorithm is broken if it permits a winning set with too many DH or QO. Let me fix that really quick... While I'm at it, since `optimize2` builds subtrees of partial equipment sets, why not short-circuit and abandon a subtree as soon as it violates the DH/QO rule?

```haskell
optimize3 :: Weights -> [[Item]] -> [Item]
optimize3 weights items = optimizeInternal weights items []
	where
	optimizeInternal :: Weights -> [[Item]] -> [Item] -> [Item]
	optimizeInternal _ [] candidateSet = 
		if okDhAndQoCount candidateSet then candidateSet else []
	optimizeInternal weights (eqLoc:eqLocs) partialCandidateSet =
		if not (okDhAndQoCount partialCandidateSet) then [] else do
			let candidateSets = map (\eq -> optimizeInternal weights
									eqLocs (eq:partialCandidateSet)) eqLoc
			if length candidateSets > 0 then 
				maximumBy (comparing (scoreItems weights)) candidateSets else []
```

BAM, it turns out that for reasonble equipment, ~80% of the recursion tree can be skipped. Instant win from 90s down to 18s.

### Joyous Parallelism

At the time of this writing, `optimize` runs in about 8 seconds. I'm proud of that run time, from "forever" to 8 seconds.

This section's title isn't sarcasm. Haskell has really cool, insanely easy to use parallelism[1]. I actually started investigating parallelism while the app was still taking 90s. The number of cores used in the `optimize` computation doubled from 1 to 2 (on a 2-core virtual machine), and the runtime halved to 45s.

Take this fibonacci example:

```haskell
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)
```

Haskell allows you to parallelize a function (or any evaluation) with arbitrary granularity. You can chop anything up piecemeal, right in the middle, any time.

Here is the same fibonacci example, with each recursive call marked for parallelism:

```haskell
fib 0 = 0
fib 1 = 1
fib n = ((fib (n-2)) `using` rpar) + ((fib (n-1)) `using` rpar)
```

It looks weird, but `(x using rpar)` just means that the haskell runtime has the option (but not obligation) to evaluate `x` on another core. We say that `x` is a *spark*:

*The argument to `rpar` is called a spark. The runtime collects sparks in a pool and uses this as a source of work when there are spare processors available, using a technique called work stealing. Sparks may be evaluated at some point in the future, or they might not—it all depends on whether there is a spare core available. Sparks are very cheap to create: `rpar` essentially just writes a pointer to the expression into an array.* - Parallel and Concurrent Programming in Haskell

Sparks enable parallelism AND are cheap, yay!  But there's a few more tricks to regulating spark creation (cheap ain't free), so here's the final version of `optimize` I am currently using:

```haskell
-- the 8-second version
optimize4 :: Weights -> [[Item]] -> [Item]
optimize4 weights items = optimizeInternal 0 weights items []
	where
		parallelCutoff = 5 :: Int
		optimizeInternal :: Int -> Weights -> [[Item]] -> [Item] -> [Item]
		optimizeInternal _ _ [] candidateSet = 
			if okDhAndQoCount candidateSet then candidateSet else []
		optimizeInternal depth weights (eqLoc:eqLocs) partialCandidateSet =
			if not (okDhAndQoCount partialCandidateSet) then [] else do
				let candidateSets = 
					(if depth < parallelCutoff then parMap rpar else map) 
					(\eq -> optimizeInternal (depth+1) weights eqLocs (eq:partialCandidateSet))
				 	eqLoc
				if length candidateSets > 0 then maximumBy (comparing (scoreItems weights)) 
				candidateSets else []
```

Next up, my first haskell cli.

[1]: Parallelism is when things happen on two or more cpu cores, whereas concurrency is one core juggling tasks to give the illusion of single-core parallelism.

# Wishlist

## proper cli

```shell
./eqoptimizer --eq-directory ewar --base-hp --base-mana --hp-bonus=85 --no-bless --mana-tallies --no-spirit --no-aura --hp-weight 1 --mana-weight 1 --hr-weight 1 --dr-weight 1 --ss-weight 1 --infmel-weight 1 --infspell-weight 1 --empty-locations=wield,held --output ewar-solution
```

Cli should use finger/wristTwoEq.txt if it exists, otherwise duplicate finger/wristOneEq.txt

## eq catalog

For this and other projects. Listing of medievia max/base eq. Indexed on all stats, locations, alignments. Given eq catalog and class/alignment, output eq data for use in eqoptimizer.
