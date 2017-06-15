all: poisson_Gauss.x

poisson_Gauss.x: mod_initialize.o mod_parallel.o mod_finalize.o mod_run_Gauss.o poisson.o
	mpif90 -o poisson_Gauss.x poisson.o mod_initialize.o mod_parallel.o mod_finalize.o mod_run_Gauss.o 

poisson.o: poisson.f90
	mpif90 -c poisson.f90

mod_initialize.o: mod_initialize.f90
	mpif90 -c mod_initialize.f90

mod_parallel.o: mod_parallel.f90
	mpif90 -c mod_parallel.f90

mod_run_Gauss.o: mod_run_Gauss.f90
	mpif90 -c mod_run_Gauss.f90

mod_finalize.o: mod_finalize.f90
	mpif90 -c mod_finalize.f90

run:poisson_Gauss.x
	/usr/bin/time mpiexec -n 5 ./poisson_Gauss.x

clean: 
	rm *.mod *.o *.x
