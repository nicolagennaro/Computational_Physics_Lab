CF = gfortran
# works with F compiler
FFLAGS = -fbounds-check  # -ieee=full
SRC = $(wildcard *.f90)
EXE = $(SRC:.f90=.x)

.SUFFIXES:
.SUFFIXES: .f90 .x


all: $(EXE)

%.x: %.f90
	$(CF) $< -o $@ $(FFLAGS)


hist:
	gnuplot plt_hist.plt

corr:
	gnuplot plt_corr.plt

unif:
	gnuplot plt_corr_mom.plt

clean:
	rm -f *~ *.x *.mod *.png


.PHONY: clean hist corr unif



