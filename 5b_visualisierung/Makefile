visualize.x: visualize.f90
	scorep mpif90 -o visualize.x visualize.f90

run:visualize.x
	mpiexec -n 8 ./visualize.x

clean: 
	rm *.mod *.o visualize.x