BIN		= hoton

all: $(BIN)

hoton: main.hs
	ghc -o $@ --make $<

clean:
	rm *.hi *.o $(BIN)

.PHONY: all clean
