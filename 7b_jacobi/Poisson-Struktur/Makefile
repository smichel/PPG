all: poisson_Jacobi.x

poisson_Jacobi.x: mod_initialize.o mod_peter.o mod_finalize.o mod_run_Jacobi.o poisson.o
	mpif90 -o poisson_Jacobi.x poisson.o mod_initialize.o mod_peter.o mod_finalize.o mod_run_Jacobi.o 

poisson.o: poisson.f90
	mpif90 -c poisson.f90

mod_initialize.o: mod_initialize.f90
	mpif90 -c mod_initialize.f90

mod_peter.o: mod_peter.f90
	mpif90 -c mod_peter.f90

mod_run_Jacobi.o: mod_run_Jacobi.f90
	mpif90 -c mod_run_Jacobi.f90

mod_finalize.o: mod_finalize.f90
	mpif90 -c mod_finalize.f90

run:poisson_Jacobi.x
	/usr/bin/time mpiexec -n 5 ./poisson_Jacobi.x

clean: 
	rm *.mod *.o *.x
