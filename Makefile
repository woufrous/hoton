BIN			= hopr

_HOTON_SRC	= Hoton/Types.hs Hoton/Distributions.hs Hoton/Vector.hs
_HOTON_SRC	+= Hoton/Scene.hs Hoton/Scenes/Forward1D.hs Hoton/Matrix.hs
_HOTON_SRC	+= Hoton/IO.hs
_HOTON_SRC	+= main.hs
HOTON_SRC   = $(patsubst %,src/%,$(_HOTON_SRC))
OBJ			= $(HOTON_SRC:.hs=.o)

GHC			= ghc
GHC_FLAGS	= -O2 -isrc

all: $(BIN)

$(BIN): $(HOTON_SRC)
	$(GHC) $(GHC_FLAGS) -o $@ $^

clean:
	rm $(HOTON_SRC:.hs=.hi) $(OBJ) $(BIN)

test:
	runhaskell -isrc -itest test/Spec.hs

.PHONY: all clean test
