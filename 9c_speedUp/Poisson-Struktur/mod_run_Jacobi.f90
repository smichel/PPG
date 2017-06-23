MODULE run
	IMPLICIT NONE
	CONTAINS

	
	SUBROUTINE calculate(matrix) !more parameters as needed
		USE mpi
		USE parallel
		USE finalize
		IMPLICIT NONE
		
		integer :: myRank,numProc,ierror, request, request2,request3,request4
		double precision, dimension(0:,0:), intent(inout) :: matrix
		integer :: numEl = 96	! matrix hat (numEl+1)x(numEl+1) Elemente 
		integer :: iterations = 100000	! maximale Anzahl an Iterationen
		integer :: t, i, j	! Zaehlindizes 		
		double precision, dimension(:,:),allocatable :: dummy	! Hilfsmatrix, auf die neue Elemente geschrieben werden
		double precision, dimension(:),allocatable :: dummy2
		integer, dimension(:), allocatable :: displacement, sendcounts ! Vektoren fuer ScatterV und GatherV
		double precision, dimension(:,:), allocatable :: chunk ! Teilmatrix
		double precision :: accuracy = 10.**(-7)
		integer :: lines, rest, status(MPI_STATUS_SIZE)
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
		allocate(dummy2(numEl+1))
		
		call MPI_SCATTERV(matrix, sendcounts, displacement, MPI_DOUBLE_PRECISION, chunk, sendcounts(myRank+1),&
			& MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierror) ! Matrix wird auf Teilmatrizen aufgeteilt	

		t = 0
		
		
		! Schleife wird durchlaufen, solange die maximale Anzahl an Iterationen 
		! nicht ueberschritten ist 
		do while (t < iterations)
			dummy = chunk

			if (myRank < (numProc-1)) then 
				call MPI_ISEND(chunk(:,lines-1),numEl+1, MPI_DOUBLE_PRECISION, myRank+1, myRank ,MPI_COMM_WORLD, request, ierror)
				call MPI_WAIT(request, status, ierror)
				call MPI_IRECV(chunk(:,lines), numEl+1, MPI_DOUBLE_PRECISION, myRank+1, myRank+1, MPI_COMM_WORLD, request3, ierror)
			endif	
			
			if (myRank > 0) then
				call MPI_IRECV(chunk(:,1),  numEl+1, MPI_DOUBLE_PRECISION, myRank-1, myRank-1, MPI_COMM_WORLD, request2, ierror)
				call MPI_WAIT(request2, status, ierror)
			endif
			
			do j=2, lines-1
			if ((j .eq. 3) .AND. (myRank > 0)) then
				dummy2=dummy(:,2)
				dummy2(2:numEl) = chunk(2:numEl,2) + dummy(2:numEl,2)
				
				call MPI_ISEND(dummy2,numEl+1, MPI_DOUBLE_PRECISION, myRank-1, myRank ,MPI_COMM_WORLD, request4, ierror)
				call MPI_WAIT(request4, status, ierror)
			endif
				do i=2, numEl
					! neues Element wird aus umliegenden alten Elementen berechnet und 
					! zunächst in die Hilfsmatrix geschrieben
					
! 					chunk(i,j)  = chunk(i,j) -(- chunk(i,j+1) - chunk(i-1,j) + 4.*chunk(i,j) &
! 					&            - chunk(i+1,j) - chunk(i,j-1))/4.
					dummy(i,j)  = -(- chunk(i,j+1) - chunk(i-1,j) + 4.*chunk(i,j) &
					&            - chunk(i+1,j) - chunk(i,j-1))/4.	
				enddo
				
				if ((j .eq. lines-2) .AND. (myRank < (numProc-1))) then
					call MPI_WAIT(request3, status, ierror)
				endif  
			enddo

			chunk(2:numEl,2:lines-1) = chunk(2:numEl,2:lines-1) &
							&	+ dummy(2:numEl,2:lines-1)
		
			t = t+1
		
		enddo 
		call MPI_BARRIER(MPI_COMM_WORLD,ierror)
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
