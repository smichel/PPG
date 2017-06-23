export OMP_NUM_THREADS=4 

pi.x: pi_openMP.f90
	gfortran -fopenmp -o pi.x pi_openMP.f90              

run:pi.x
	./pi.x

clean: 
	rm *.mod *.o pi.x
