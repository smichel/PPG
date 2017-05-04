
pi.x: pi.f90
	mpif90 -o pi.x pi.f90

run:pi.x
	mpiexec -n 22 ./pi.x

clean: 
	rm *.mod *.o pi.x
