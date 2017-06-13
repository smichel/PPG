MODULE run
	IMPLICIT NONE
	CONTAINS

	
	SUBROUTINE calculate(matrix) !more parameters as needed
		USE mpi
		USE peter
		USE finalize
		IMPLICIT NONE
		
		integer :: myRank,numProc,ierror
		double precision, dimension(0:,0:), intent(inout) :: matrix
		integer :: numEl = 96	! matrix hat (numEl+1)x(numEl+1) Elemente 
		integer :: iterations = 100000	! maximale Anzahl an Iterationen
		integer :: t, i, j	! Zaehlindizes 		
		double precision, dimension(:,:),allocatable :: dummy	! Hilfsmatrix, auf die neue Elemente geschrieben werden
		integer, dimension(:), allocatable :: displacement, sendcounts ! Vektoren fuer ScatterV und GatherV
		double precision, dimension(:,:), allocatable :: chunk ! Teilmatrix
		double precision :: accuracy = 10.**(-7)
		integer :: lines, rest, status(MPI_STATUS_SIZE) 
		integer :: accuracyhit, exitcriteria	! Info ob Genauigkeit erreicht ist
		! calculate with jacobi Method
		
		
		call MPI_INIT(ierror)   ! Initialisieren des Parallelisierungsprozesses 
		call MPI_COMM_RANK(MPI_COMM_WORLD,myRank,ierror)    ! aktueller Rang des Prozesses wird abgefragt
		call MPI_COMM_SIZE(MPI_COMM_WORLD,numProc,ierror)   ! Anzahl an Prozessen wird abgefragt
	

		call getIndices(numProc, numEl, sendcounts, displacement)
! 			write(*,*) sendcounts(:)
! 			print*
! 			write(*,*) displacement(:)
! 			print*
		if (myRank .eq. 0) then 
			write(*,*) "It will use Jacobi Method"	
		endif
		lines = sendcounts(myRank + 1)/(numEl + 1)
		! write(*,*) lines
	
		allocate(chunk((numEl + 1),lines)) ! In Abhängigkeit von der Prozessnummer wird die Teilmatrix alloziiert
		allocate(dummy((numEl + 1),lines))
		
		call MPI_SCATTERV(matrix, sendcounts, displacement, MPI_DOUBLE_PRECISION, chunk, sendcounts(myRank+1),&
			& MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierror) ! Matrix wird auf Teilmatrizen aufgeteilt	

		t = 0
		
		
		! Schleife wird durchlaufen, solange die maximale Anzahl an Iterationen 
		! nicht ueberschritten ist (wenn nicht vorher die gewuenschte Genauigkeit erreicht ist)
		do while (t < iterations)
			dummy = chunk
			do j=2, lines-1
				do i=2, numEl
					! neues Element wird aus umliegenden alten Elementen berechnet und 
					! zunächst in die Hilfsmatrix geschrieben
					dummy(i,j)  = -(- chunk(i,j+1) - chunk(i-1,j) + 4.*chunk(i,j) &
					&            - chunk(i+1,j) - chunk(i,j-1))/4.	
				enddo 
			enddo
			
			if (maxval(abs(dummy(2:numEl-1,2:lines-1))) < accuracy) then
				accuracyhit=1	
			endif
			
		! die neue Matrix ist die Summe aus der alten Matrix und der Hilfsmatrix 
		chunk(2:numEl,2:lines-1) = chunk(2:numEl,2:lines-1) &
							&	+ dummy(2:numEl,2:lines-1)
							! Sind alle accuracyhit = 1 so ist die gewuenschte Genauigkeit erreicht
		call MPI_REDUCE(accuracyhit, exitcriteria, 1, MPI_INTEGER, MPI_PROD, 0, MPI_COMM_WORLD,ierror) 
		! exitcriteria ist wird an alle Prozesse gesendet 
		call MPI_BCAST(exitcriteria, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
		! Falls Genauigkeit erreicht ist wird die Schleife abgebrochen
		if (exitcriteria .eq. 1) then
			exit
		end if
		
		call communicate(numEl, myRank, numProc, lines, chunk)	! Austauschen der Halolines, blockierende Kommunikation

		t = t+1
		
		enddo 
		! Matrizen werden wieder zusammengefuehrt
		call MPI_GATHERV(chunk, sendcounts(myRank+1), MPI_DOUBLE_PRECISION, &
			& matrix, sendcounts, displacement, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierror)
		
				 
		! Ausgabe der benoetigten Iterationen 
		if (myRank .eq. 0) then 
			write(*,'("Benoetigte Iterationen ", i6.0)') t
			print*
			call outputMatrix(matrix)
		endif
		call freeMatrix(chunk)
		call freeMatrix(dummy)
		call MPI_FINALIZE(ierror)	! Beenden des Parallelisierungsprozesses

	END SUBROUTINE calculate

END MODULE run
