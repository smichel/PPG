MODULE run
	IMPLICIT NONE
	CONTAINS
	
	SUBROUTINE calculate(matrix) !more parameters as needed
		IMPLICIT NONE
		double precision, dimension(:,:), intent(inout) :: matrix	
		! calculate with jacobi Method
		write(*,*) "It will use Jacobi Method"
		
	END SUBROUTINE calculate

END MODULE run
