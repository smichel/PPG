MODULE finalize
	IMPLICIT NONE
	CONTAINS
	
	SUBROUTINE freeMatrix(matrix) !more parameters as needed
		IMPLICIT NONE
		double precision, dimension(:,:), allocatable, intent(inout) :: matrix
		!free matrix
		deallocate(matrix)
		
	END SUBROUTINE freeMatrix
	
	SUBROUTINE outputMatrix(matrix)!more parameters as needed
		IMPLICIT NONE
		double precision, dimension(0:,0:), intent(inout) :: matrix	
		!output matrix	
		double precision, dimension(0:8,0:8) :: outMatrix 
		integer :: interlines = 22 
		integer :: i,j
		
		do i = 0,8
			do j = 0,8
				outMatrix(i,j) = matrix(i * (interlines + 1), j * (interlines + 1))
				write(*,'(F6.4," ",$)',advance='NO') outMatrix(i,j)
			enddo
			print* 
		enddo 
			
	END SUBROUTINE outputMatrix

END MODULE finalize
