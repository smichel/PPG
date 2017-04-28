MODULE run
	IMPLICIT NONE
	CONTAINS
	
	SUBROUTINE calculate(matrix) !more parameters as needed
		IMPLICIT NONE
		double precision, dimension(0:,0:), intent(inout) :: matrix	
		integer :: dimension = 184
		integer :: iterations = 400000
		integer :: t, i, j		
		double precision, dimension(:,:),allocatable :: diff
		allocate(diff(0:dimension,0:dimension))		
		! calculate with Gauß-Seidel Method
		write(*,*) "It will use Gauß-Seidel Method"
		
		t = 0
		diff(1,1)=1
		
		
		do while (t < iterations)
			do i=1, dimension-1
				do j=1, dimension-1
					matrix(i,j)  = matrix(i,j) -(- matrix(i,j+1) - matrix(i-1,j) + 4.*matrix(i,j) &
					&            - matrix(i+1,j) - matrix(i,j-1))/4.	
				enddo 
			enddo
			
		if (maxval(abs(diff(1:dimension-1,1:dimension-1) &
		& -(matrix(1:dimension-1,1:dimension-1)))) < (10.**(-6.))) then
			EXIT
		endif
		
		diff=matrix
		t = t+1
		enddo 
		write(*,'("Benoetigte Iterationen ", i6.0)') t
		print*
	END SUBROUTINE calculate

END MODULE run
