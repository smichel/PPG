MODULE initialize
	IMPLICIT NONE
	CONTAINS
	
	SUBROUTINE createMatrix(matrix) !more parameters as needed
		IMPLICIT NONE
		double precision, dimension(:,:),allocatable ,intent(inout) :: matrix
		integer, parameter :: dimension = 96	! wird benutzt um Matrix der Groesse (dimension+1) x (dimension+1) zu erstellen
		
		allocate(matrix(0:dimension,0:dimension))	!Matrix wird alloziiert 
		matrix(:,:) = 0.	! Matrix wird mit 0 initialisiert
		
	END SUBROUTINE createMatrix
	
	SUBROUTINE initializeMatrix(matrix) !more parameters as needed
		IMPLICIT NONE
		double precision, dimension(0:,0:), intent(inout) :: matrix		
		!initialize matrix	
		integer, parameter :: dimension = 96
		double precision, parameter :: grid_width = 1./dimension	! Gitterweite	
		integer :: i	! Zaehlindex
		
		! Belegen der Matrix mit Randwerten:
		! matrix(0,0) = matrix(1,1) = 1
		! matrix(0,dimension) = matrix(dimension,0) = 0
		! Randwerte zwischen den Ecken werden linear interpoliert
		do i = 0,dimension
				matrix(i,0) = 1 - i * grid_width
				matrix(0,i) = 1 - i * grid_width
				matrix(dimension,i) = 0 + i * grid_width
				matrix(i,dimension) = 0 + i * grid_width 		
		end do
		
	END SUBROUTINE initializeMatrix

END MODULE initialize
