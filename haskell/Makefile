
GHC_FLAGS=-XBangPatterns -fforce-recomp --make -O2 -threaded -funbox-strict-fields -funfolding-use-threshold=32

all:
	ghc --make spike.hs
	ghc $(GHC_FLAGS) -main-is EqOptimizer EqOptimizer.hs

run: all
	time ./EqOptimizer +RTS -N2 -s

thread-test: all
	ghc $(GHC_FLAGS) -main-is EqOptimizer EqOptimizer.hs
	time ./EqOptimizer +RTS -s
	time ./EqOptimizer +RTS -N2 -s

clean:
	rm -f spike EqOptimizer *.o *.hi
