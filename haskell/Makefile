
CPU_CORES=`cat /proc/cpuinfo | grep "cpu cores" | head -1 | sed -r 's/.*:\s+//'`
GHC_FLAGS=-XBangPatterns --make -O2 -threaded -funbox-strict-fields -funfolding-use-threshold=32

all:
	ghc --make spike.hs
	ghc $(GHC_FLAGS) -main-is EqOptimizer EqOptimizer.hs

run: all
	time ./EqOptimizer +RTS -N$(CPU_CORES) -s

thread-test: all
	time ./EqOptimizer +RTS -s
	time ./EqOptimizer +RTS -N2 -s

clean:
	rm -f spike EqOptimizer *.o *.hi
