# Common definitions
CC = mpif90

# Compiler flags:
FLAGS = 
# flags for debugging:
#FLAGS =  -Wextra -Wall -g

all: poisson_Jacobi.x


poisson_Jacobi.x: mod_initialize.o mod_run_Jacobi.o mod_finalize.o mod_parallel.o poisson.o
	$(CC) $(FLAGS) -o poisson_Jacobi.x poisson.o mod_initialize.o mod_run_Jacobi.o mod_finalize.o mod_parallel.o


poisson.o: poisson.f90
	$(CC) $(FLAGS) -c poisson.f90
	
mod_initialize.o: mod_initialize.f90
	$(CC) $(FLAGS) -c mod_initialize.f90
	
mod_parallel.o: mod_parallel.f90
	$(CC) $(FLAGS) -c mod_parallel.f90


mod_run_Jacobi.o: mod_run_Jacobi.f90
	$(CC) $(FLAGS) -c mod_run_Jacobi.f90


mod_finalize.o: mod_finalize.f90
	$(CC) $(FLAGS) -c mod_finalize.f90

	
run:poisson_Jacobi.x
	/usr/bin/time mpiexec -n 7 ./poisson_Jacobi.x
	


clean: 
	rm *.mod *.o *.x
