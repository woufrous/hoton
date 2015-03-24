BIN		= hopr

SRC		= main.hs
SRC		+= Hoton/Distributions.hs
OBJ		= $(SRC:.hs=.o)

all: $(BIN)

hopr: $(SRC)
	ghc -o $@ $^

clean:
	rm $(SRC:.hs=.hi) $(OBJ) $(BIN)

.PHONY: all clean
