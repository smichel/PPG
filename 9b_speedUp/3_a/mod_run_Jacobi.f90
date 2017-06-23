MODULE run
	IMPLICIT NONE
	CONTAINS
	
	SUBROUTINE calculate(matrix) !more parameters as needed
		IMPLICIT NONE
		double precision, dimension(0:,0:), intent(inout) :: matrix
		integer :: dimension = 96	! matrix hat (dimension+1)x(dimension+1) Elemente 
		integer :: iterations = 100000	! maximale Anzahl an Iterationen
		integer :: t, i, j	! Zaehlindizes 		
		double precision, dimension(:,:),allocatable :: dummy	! Hilfsmatrix, auf die neue Elemente geschrieben werden
		
		! Hilfsmatrix ist genauso gross wie matrix
		allocate(dummy(0:dimension,0:dimension))
			
		! calculate with jacobi Method
		write(*,*) "It will use Jacobi Method"
		
		t = 0
		
		! Schleife wird durchlaufen, solange die maximale Anzahl an Iterationen 
		! nicht ueberschritten ist (wenn nicht vorher die gewuenschte Genauigkeit erreicht ist)
		do while (t < iterations)
			do j=1, dimension-1
				do i=1, dimension-1
					! neues Element wird aus umliegenden alten Elementen berechnet und 
					! zunächst in die Hilfsmatrix geschrieben
					dummy(i,j)  = -(- matrix(i,j+1) - matrix(i-1,j) + 4.*matrix(i,j) &
					&            - matrix(i+1,j) - matrix(i,j-1))/4.	
				enddo 
			enddo
		! die Schleife wird vorzeitig verlassen, wenn die maximale Differenz zwischen alten 
		! und neuen Matrixelementen kleiner als die gewuenschte Genauigkeit ist	
! 		if (maxval(abs(matrix(1:dimension-1,1:dimension-1) &
! 		& -(matrix(1:dimension-1,1:dimension-1) + dummy(1:dimension-1,1:dimension-1)))) < (10.**(-6.))) then
! 			EXIT
! 		endif
		! die neue Matrix ist die Summe aus der alten Matrix und der Hilfsmatrix 
		matrix(1:dimension-1,1:dimension-1) = matrix(1:dimension-1,1:dimension-1) &
							&	+ dummy(1:dimension-1,1:dimension-1)
		t = t+1
		enddo 
		
		! Ausgabe der benoetigten Iterationen 
		write(*,'("Benoetigte Iterationen ", i6.0)') t
		print*
	END SUBROUTINE calculate

END MODULE run
