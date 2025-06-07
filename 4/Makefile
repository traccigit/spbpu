FC=gfortran
FFLAGS=-Wall -g -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto
FOPT=-O3 -ftree-vectorize -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/module_IO.f90 -J obj/ -o obj/module_IO.o
	$(FC) $(FFLAGS) $(FOPT) -c src/module_process.f90 -J obj/ -o obj/module_process.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app ./obj/environment.o ./obj/module_IO.o ./obj/module_process.o ./obj/main.o

clean:
	rm -rf obj/*
	rm -rf bin/*
	rm ./data/data.dat

run:
	cd ./bin; ./app;
	cat bin/output.txt
