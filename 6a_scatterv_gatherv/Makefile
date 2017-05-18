scatter.x: scatter.f90
	mpif90 -o scatter.x scatter.f90

run:scatter.x
	mpiexec -n 5 ./scatter.x

clean: 
	rm *.mod *.o scatter.x