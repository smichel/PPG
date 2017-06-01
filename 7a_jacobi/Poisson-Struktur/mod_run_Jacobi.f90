MODULE run
	IMPLICIT NONE
	CONTAINS
	
	SUBROUTINE calculate(matrix) !more parameters as needed
		use mpi
		IMPLICIT NONE
		
		integer :: myRank,numProc,ierror
		double precision, dimension(0:,0:), intent(inout) :: matrix
		integer :: dimension = 96	! matrix hat (dimension+1)x(dimension+1) Elemente 
		integer :: iterations = 400000	! maximale Anzahl an Iterationen
		integer :: t, i, j	! Zaehlindizes 		
		double precision, dimension(:,:),allocatable :: dummy	! Hilfsmatrix, auf die neue Elemente geschrieben werden
		integer, allocatable, dimension(:) :: displacement, sendcounts ! Vektoren fuer ScatterV und GatherV
		integer, allocatable :: chunk(:,:) ! Teilmatrix
		integer :: lines, rest  ! blabla
		
		! calculate with jacobi Method
		write(*,*) "It will use Jacobi Method"
		
		call MPI_INIT(ierror)   ! Initialisieren des Parallelisierungsprozesses 
		call MPI_COMM_RANK(MPI_COMM_WORLD,myRank,ierror)    ! aktueller Rang des Prozesses wird abgefragt
		call MPI_COMM_SIZE(MPI_COMM_WORLD,numProc,ierror)   ! Anzahl an Prozessen wird abgefragt
	
		allocate(displacement(numProc))
		allocate(sendcounts(numProc))
		if (myRank .eq. 0) then
		if (mod((dimension - 1),numProc)==0) then
			
			lines=(dimension - 1)/numProc
			
			do i=1,numProc
			
				sendcounts(i) = lines*(dimension-1)
				displacement(i)=lines*(dimension-1)*(i-1)
				write(*,*) sendcounts(i)
				
				print*
				write(*,*) displacement(i)
				
				print*
			enddo
			
		else 
			
			lines = int((dimension-1)/(numProc-1))
			rest = mod(dimension-1,int((dimension-1)/(numProc-1)))*(dimension-1)
			
			do i=1,numProc
				sendcounts(i) = lines*(dimension-1)
			
				if (i==numProc) then
					sendcounts(i)=rest
				endif
			
				displacement(i)=lines*(dimension-1)*(i-1)
				write(*,*) sendcounts(i)
				
				print*
				write(*,*) displacement(i)
				
				print*
			enddo
					
		endif
		endif
			
		
		! Hilfsmatrix ist genauso gross wie matrix
		allocate(dummy(0:dimension,0:dimension))
			
		
		
		
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
		!if (maxval(abs(matrix(1:dimension-1,1:dimension-1) &
		!& -(matrix(1:dimension-1,1:dimension-1) + dummy(1:dimension-1,1:dimension-1)))) < (10.**(-6.))) then
		!	EXIT
		!endif
		
		
		! die neue Matrix ist die Summe aus der alten Matrix und der Hilfsmatrix 
		matrix(1:dimension-1,1:dimension-1) = matrix(1:dimension-1,1:dimension-1) &
							&	+ dummy(1:dimension-1,1:dimension-1)
		t = t+1
		enddo 
		
		! Ausgabe der benoetigten Iterationen 
		write(*,'("Benoetigte Iterationen ", i6.0)') t
		print*
		call MPI_FINALIZE(ierror)	! Beenden des Parallelisierungsprozesses

	END SUBROUTINE calculate

END MODULE run
