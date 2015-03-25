BIN			= hopr

_HOTON_SRC	= Hoton/Types.hs Hoton/Distributions.hs Hoton/Vector.hs
_HOTON_SRC	+= Hoton/Scene.hs Hoton/Scenes/Forward1D.hs
HOTON_SRC   = $(patsubst %,src/%,$(_HOTON_SRC))
OBJ			= $(HOTON_SRC:.hs=.o)

GHC			= ghc
GHC_FLAGS	= -isrc

all: $(BIN)

$(BIN): src/main.hs $(HOTON_SRC)
	$(GHC) $(GHC_FLAGS) -o $@ $^

hopr_t2: src/hopr_t2.hs $(HOTON_SRC)
	$(GHC) $(GHC_FLAGS) -o $@ $^

clean:
	rm *.hi *.o $(HOTON_SRC:.hs=.hi) $(OBJ) $(BIN) hopr_t2

.PHONY: all clean
