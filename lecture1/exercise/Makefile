CF = gfortran
# works with F compiler
# FFLAGS = -ieee=full
SRC = $(wildcard *.f90)
EXE = $(SRC:.f90=.x)

.SUFFIXES:
.SUFFIXES: .f90 .x


all: $(EXE)

%.x: %.f90
	$(CF) $< -o $@ # $(FFLAGS)


clean:
	rm -f *~ *.x *.mod


.PHONY: clean

