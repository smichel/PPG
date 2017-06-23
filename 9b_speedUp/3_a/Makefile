# Common definitions
CC = f95

all: poisson_Jacobi.x poisson_Gauss.x


poisson_Jacobi.x: mod_initialize.o mod_run_Jacobi.o mod_finalize.o poisson.o
	$(CC) -o poisson_Jacobi.x poisson.o mod_initialize.o mod_run_Jacobi.o mod_finalize.o

poisson_Gauss.x: mod_initialize.o mod_run_Gauss.o mod_finalize.o poisson.o
	$(CC) -o poisson_Gauss.x poisson.o mod_initialize.o mod_run_Gauss.o mod_finalize.o


poisson.o: poisson.f90
	$(CC) -c poisson.f90
	
mod_initialize.o: mod_initialize.f90
	$(CC) -c mod_initialize.f90


mod_run_Jacobi.o: mod_run_Jacobi.f90
	$(CC) -c mod_run_Jacobi.f90

mod_run_Gauss.o: mod_run_Gauss.f90
	$(CC) -c mod_run_Gauss.f90


mod_finalize.o: mod_finalize.f90
	$(CC) -c mod_finalize.f90

	
runjakobi:poisson_Jacobi.x
	/usr/bin/time ./poisson_Jacobi.x
	
rungauss:poisson_Gauss.x
	/usr/bin/time ./poisson_Gauss.x


clean: 
	rm *.mod *.o *.x
