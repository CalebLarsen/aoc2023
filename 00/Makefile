.PHONY: run test i clean

run: main
	./main < input
test: main
	./main < test
i:
	ghci main.hs
clean:
	rm build/* main


main: main.hs
	ghc -o main -hidir build -odir build main.hs