all:
	gcc -c -o alleg.o alleg.c
	ghc -O3 -threaded -prof -lallegro --make -o Life Main.hs alleg.o
	rm *.o *.hi
