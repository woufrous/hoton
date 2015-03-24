BIN			= hopr

HOTON_SRC	= Hoton/Types.hs Hoton/Distributions.hs Hoton/Vector.hs
HOTON_SRC	+= Hoton/Scene.hs Hoton/Scenes/Forward1D.hs
OBJ			= $(HOTON_SRC:.hs=.o)

all: $(BIN)

$(BIN): main.hs $(HOTON_SRC)
	ghc -o $@ $^

hopr_t2: hopr_t2.hs $(HOTON_SRC)
	ghc -o $@ $^

clean:
	rm *.hi *.o $(HOTON_SRC:.hs=.hi) $(OBJ) $(BIN) hopr_t2

.PHONY: all clean
