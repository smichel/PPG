MODULE initialize
	IMPLICIT NONE
	CONTAINS
	
	SUBROUTINE createMatrix(matrix) !more parameters as needed
		IMPLICIT NONE
		double precision, dimension(:,:), intent(inout) :: matrix	
		!create matrix
		
	END SUBROUTINE createMatrix
	
	SUBROUTINE initializeMatrix(matrix) !more parameters as needed
		IMPLICIT NONE
		double precision, dimension(:,:), intent(inout) :: matrix		
		!initialize matrix	
		
		
	END SUBROUTINE initializeMatrix

END MODULE initialize
