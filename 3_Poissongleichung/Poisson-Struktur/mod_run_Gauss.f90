MODULE run
	IMPLICIT NONE
	CONTAINS
	
	SUBROUTINE calculate(matrix) !more parameters as needed
		IMPLICIT NONE
		double precision, dimension(:,:), intent(inout) :: matrix	
		! calculate with Gauß-Seidel Method
		write(*,*) "It will use Gauß-Seidel Method"
		
	END SUBROUTINE calculate

END MODULE run
