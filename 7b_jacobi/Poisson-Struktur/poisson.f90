PROGRAM Poisson
	USE initialize
	USE run
	USE finalize
	USE peter
	IMPLICIT NONE
	double precision, dimension(:,:), allocatable :: matrix ! or whatever fits

	call createMatrix(matrix)
	call initializeMatrix(matrix)
	
	call calculate(matrix)

	call freeMatrix(matrix)




	
END PROGRAM Poisson
