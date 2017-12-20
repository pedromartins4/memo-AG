# Speed Runs
run-speed:
#	./Repmin/Benchmark --output Repmin.html
#	./Algol68/Benchmark --output Algol68.html
	./Desk/Benchmark --output Desk.html
#	./HTMLTableFormatter/Benchmark --output HTMLTableFormatter.html
#	./LetIn/Benchmark --output LetIn.html

# Memory Profiling Runs
run-memory:
#	./Repmin/Benchmark +RTS -sstderr -hc -p -K100M
#	hp2ps -e8in -c Benchmark.hp
#	mv Benchmark.ps Repmin.ps
#	./Algol68/Benchmark +RTS -sstderr -hc -p -K100M
#	hp2ps -e8in -c Benchmark.hp
#	mv Benchmark.ps Algol68.ps
	./Desk/Benchmark +RTS -sstderr -hc -p -K100M
	hp2ps -e8in -c Benchmark.hp
	mv Benchmark.ps Desk.ps
#	./HTMLTableFormatter/Benchmark +RTS -sstderr -hc -p -K100M
#	hp2ps -e8in -c Benchmark.hp
#	mv Benchmark.ps HTMLTableFormatter.ps
#	./LetIn/Benchmark +RTS -sstderr -hc -p -K100M
#	hp2ps -e8in -c Benchmark.hp
#	mv Benchmark.ps LetIn.ps

# Speed Compiles
compile-speed:
#	ghc -O2 -i:Repmin/ --make Repmin/Benchmark
#	ghc -O2 -i:Algol68/ --make Algol68/Benchmark
	ghc -O2 -i:Desk/ --make Desk/Benchmark
#	ghc -O2 -i:HTMLTableFormatter/ --make HTMLTableFormatter/Benchmark
#	ghc -O2 -i:LetIn/ --make LetIn/Benchmark

# Memory Profiling Compiles
compile-memory:
#	ghc -O2 -rtsopts -i:Repmin/             --make Repmin/Benchmark             -prof -fforce-recomp
#	ghc -O2 -rtsopts -i:Algol68/            --make Algol68/Benchmark            -prof -fforce-recomp
	ghc -O2 -rtsopts -i:Desk/               --make Desk/Benchmark               -prof -fforce-recomp
#	ghc -O2 -rtsopts -i:HTMLTableFormatter/ --make HTMLTableFormatter/Benchmark -prof -fforce-recomp
#	ghc -O2 -rtsopts -i:LetIn/              --make LetIn/Benchmark              -prof -fforce-recomp

# Clean
clean:
	rm -f Repmin/*.hi
	rm -f Repmin/*.o
	rm -f Repmin/Benchmark
	rm -f Algol68/*.hi
	rm -f Algol68/*.o
	rm -f Algol68/Benchmark
	rm -f Desk/*.hi
	rm -f Desk/*.o
	rm -f Desk/Benchmark
	rm -f HTMLTableFormatter/*.hi
	rm -f HTMLTableFormatter/*.o
	rm -f HTMLTableFormatter/Benchmark
	rm -f Let/*.hi
	rm -f Let/*.o
	rm -f Let/Benchmark