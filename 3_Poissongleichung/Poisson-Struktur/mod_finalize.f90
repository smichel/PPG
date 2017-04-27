MODULE finalize
	IMPLICIT NONE
	CONTAINS
	
	SUBROUTINE freeMatrix(matrix) !more parameters as needed
		IMPLICIT NONE
		double precision, dimension(:,:), intent(inout) :: matrix
		!free matrix
		
	END SUBROUTINE freeMatrix
	
	SUBROUTINE outputMatrix(matrix)!more parameters as needed
		IMPLICIT NONE
		double precision, dimension(:,:), intent(inout) :: matrix	
		!output matrix	
		
		
	END SUBROUTINE outputMatrix

END MODULE finalize
