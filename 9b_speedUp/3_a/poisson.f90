PROGRAM Poisson
	USE initialize
	USE run
	USE finalize
	IMPLICIT NONE
	double precision, dimension(:,:), allocatable :: matrix ! or whatever fits

	call createMatrix(matrix)
	call initializeMatrix(matrix)
	
	call calculate(matrix)

	call outputMatrix(matrix)
	call freeMatrix(matrix)



	
END PROGRAM Poisson
