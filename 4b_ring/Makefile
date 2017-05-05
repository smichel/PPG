
ring.x: ring.f90
	mpif90 -o ring.x ring.f90

run:ring.x
	mpiexec -n 4 ./ring.x

clean: 
	rm *.mod *.o ring.x
