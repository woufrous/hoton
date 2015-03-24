BIN		= hoton

SRC		= main.hs
SRC		+= Hoton/Distributions.hs
OBJ		= $(SRC:.hs=.o)

all: $(BIN)

hoton: $(SRC)
	ghc -o $@ $^

clean:
	rm $(SRC:.hs=.hi) $(OBJ) $(BIN)

.PHONY: all clean
