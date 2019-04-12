FF = gfortran
# works with F compiler
FFLAGS = -fbounds-check  -Ofast  # -ieee=full



all: gauleg-IIorder.x gauleg-other.x gauleg_nr_test.x es_1b.x CLT.x CLT_exp.x CLT_Lorentz.x




gauleg-IIorder.x: gauleg-IIorder.f90
	$(FF) $< -o $@ $(FFLAGS)


gauleg-other.x: gauleg-other.f90
	$(FF) $< -o $@ $(FFLAGS)


es_1b.x: es_1b.f90
	$(FF) $< -o $@ $(FFLAGS)


CLT.x: CLT.f90
	$(FF) $< -o $@ $(FFLAGS)

CLT_exp.x: CLT_exp.f90
	$(FF) $< -o $@ $(FFLAGS)

CLT_Lorentz.x: CLT_Lorentz.f90
	$(FF) $< -o $@ $(FFLAGS)


gauleg_nr_test.x: nrtype.o nrutil.o nr.o gauleg.o gauleg_nr_test.o 
	$(FF) $^ -o $@ $(FFLAGS)


nrtype.o: nrtype.f90
	$(FF) -c $< -o $@

nrutil.o: nrutil.f90
	$(FF) -c $< -o $@

nr.o: nr.f90
	$(FF) -c $< -o $@

gauleg.o: gauleg.f90
	$(FF) -c $< -o $@


gauleg_nr_test.o: gauleg_nr_test.f90
	$(FF) -c $< -o $@










clean:
	rm -f *~ *.x *.mod *.png *.o


.PHONY: clean



