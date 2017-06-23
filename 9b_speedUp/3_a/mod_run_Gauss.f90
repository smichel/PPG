MODULE run
	IMPLICIT NONE
	CONTAINS
	
	SUBROUTINE calculate(matrix) !more parameters as needed
		IMPLICIT NONE
		double precision, dimension(0:,0:), intent(inout) :: matrix	
		integer :: dimension = 96	! matrix hat (dimension+1)x(dimension+1) Elemente 
		integer :: iterations = 400000	! maximale Anzahl an Iterationen
		integer :: t, i, j	! Zaehlindizes 
		double precision, dimension(:,:),allocatable :: diff	! Matrix zur Berechnung des Abbruchkriteriums
		! diff hat die gleiche Größe wie matrix 
		allocate(diff(0:dimension,0:dimension))	
			
		! calculate with Gauß-Seidel Method
		write(*,*) "It will use Gauß-Seidel Method"
		
		t = 0
		diff(:,:) = 0
		
		! Schleife wird durchlaufen, solange die maximale Anzahl an Iterationen 
		! nicht ueberschritten ist (wenn nicht vorher die gewuenschte Genauigkeit erreicht ist) 
		do while (t < iterations)
			do i=1, dimension-1
				do j=1, dimension-1
					! neues Matrixelement wird wird aus (neuen und alten) umliegenden Elementen 
					! berechnet 
					matrix(i,j)  = matrix(i,j) -(- matrix(i,j+1) - matrix(i-1,j) + 4.*matrix(i,j) &
					&            - matrix(i+1,j) - matrix(i,j-1))/4.	
				enddo 
			enddo
		! die Schleife wird vorzeitig verlassen, wenn die maximale Differenz zwischen alten 
		! und neuen Matrixelementen kleiner als die gewuenschte Genauigkeit ist 	
		if (maxval(abs(diff(1:dimension-1,1:dimension-1) &
		& -(matrix(1:dimension-1,1:dimension-1)))) < (10.**(-6.))) then
			EXIT
		endif
		! neue Matrix wird im nächsten Zeitschritt zur Berechnung des Abbruchkriteriums
		! benoetigt
		diff = matrix
		t = t+1
		enddo 
		
		! Ausgabe der Anzahl an benoetigten Iterationen 
		write(*,'("Benoetigte Iterationen ", i6.0)') t
		print*
	END SUBROUTINE calculate

END MODULE run
