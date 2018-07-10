#  Copyright (C) 2017 Joao Saraiva, Joao P. Fernandes, Pedro Martins,
#  				   Alberto Pardo, Marcos Viera
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program. If not, see <http://www.gnu.org/licenses/>.

# Speed Runs
run-speed:
	./Repmin/Benchmark --output Repmin.html
#	./Algol68/Benchmark --output Algol68.html
#	./Desk/Benchmark --output Desk.html
#	./HTMLTableFormatter/Benchmark --output HTMLTableFormatter.html
#	./LetIn/Benchmark --output LetIn.html

# Memory Profiling Runs
run-memory:
	./Repmin/Benchmark +RTS -sstderr -hc -p -K100M
	hp2ps -e8in -c Benchmark.hp
	mv Benchmark.ps Repmin.ps
#	./Algol68/Benchmark +RTS -sstderr -hc -p -K100M
#	hp2ps -e8in -c Benchmark.hp
#	mv Benchmark.ps Algol68.ps
#	./Desk/Benchmark +RTS -sstderr -hc -p -K100M
#	hp2ps -e8in -c Benchmark.hp
#	mv Benchmark.ps Desk.ps
#	./HTMLTableFormatter/Benchmark +RTS -sstderr -hc -p -K100M
#	hp2ps -e8in -c Benchmark.hp
#	mv Benchmark.ps HTMLTableFormatter.ps
#	./LetIn/Benchmark +RTS -sstderr -hc -p -K100M
#	hp2ps -e8in -c Benchmark.hp
#	mv Benchmark.ps LetIn.ps

# Speed Compiles
compile-speed:
	ghc -O2 -i:Repmin/ --make Repmin/Benchmark
#	ghc -O2 -i:Algol68/ --make Algol68/Benchmark
#	ghc -O2 -i:Desk/ --make Desk/Benchmark
#	ghc -O2 -i:HTMLTableFormatter/ --make HTMLTableFormatter/Benchmark
#	ghc -O2 -i:LetIn/ --make LetIn/Benchmark

# Memory Profiling Compiles
compile-memory:
	ghc -O2 -rtsopts -i:Repmin/             --make Repmin/Benchmark             -prof -fforce-recomp
#	ghc -O2 -rtsopts -i:Algol68/            --make Algol68/Benchmark            -prof -fforce-recomp
#	ghc -O2 -rtsopts -i:Desk/               --make Desk/Benchmark               -prof -fforce-recomp
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