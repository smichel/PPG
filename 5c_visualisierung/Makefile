
pi.x: pi.f90
	scorep mpif90 -o pi.x pi.f90

run:pi.x
	mpiexec -n 10 ./pi.x

clean: 
	rm *.mod *.o pi.x
