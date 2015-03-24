BIN		= hopr

SRC		= main.hs
SRC		+= Hoton/Types.hs Hoton/Distributions.hs Hoton/Vector.hs
OBJ		= $(SRC:.hs=.o)

all: $(BIN)

$(BIN): $(SRC)
	ghc -o $@ $^

hopr_t2: hopr_t2.hs Hoton/Types.hs Hoton/Distributions.hs Hoton/Scene.hs Hoton/Scenes/Forward1D.hs
	ghc -o $@ $^

clean:
	rm $(SRC:.hs=.hi) $(OBJ) $(BIN)

.PHONY: all clean
