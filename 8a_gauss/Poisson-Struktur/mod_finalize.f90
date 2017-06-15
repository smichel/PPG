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
		double precision, dimension(0:8,0:8) :: outMatrix	! Output Matrix 
		integer :: interlines = 11	! jedes (interlines+1). Element wird ausgegeben 
		integer :: i,j	! Zaehlindizes 
		
		do i = 0,8
			do j = 0,8
				! jede 12. Zeile und Spalte wird ausgegeben 
				outMatrix(i,j) = matrix(i * (interlines + 1), j * (interlines + 1))
				write(*,'(F6.4," ",$)') outMatrix(i,j)
			enddo
			print* 
		enddo 
			
	END SUBROUTINE outputMatrix

END MODULE finalize
