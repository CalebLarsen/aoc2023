.PHONY: run i clean

run: main
	./main < input
i:
	ghci main.hs
clean:
	rm build/* main


main: main.hs
	ghc -o main -hidir build -odir build main.hs