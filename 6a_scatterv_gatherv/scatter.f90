program scatter
	use mpi
	implicit none
	integer :: myRank,numProc,ierror
	integer :: matrix(24,24)
	integer, allocatable :: chunk(:,:) ! Teilmatrix
	integer :: partSum ! Teilsumme aus den einzelnen Teilmatrizen
	integer :: totSum ! Gesamtsumme aus den Teilsummen
	integer :: matSum ! Gesamtsumme aus den einzelnen Matrixelementen
	integer :: lines ! Anzahl der Zeilen der Teilmatrizen
	integer, allocatable, dimension(:) :: displacement, sendcounts ! Vektoren fuer ScatterV und GatherV
	
	call initializeMatrix(matrix)
	
	call MPI_INIT(ierror)   ! Initialisieren des Parallelisierungsprozesses 
	call MPI_COMM_RANK(MPI_COMM_WORLD,myRank,ierror)    ! aktueller Rang des Prozesses wird abgefragt
	call MPI_COMM_SIZE(MPI_COMM_WORLD,numProc,ierror)   ! Anzahl an Prozessen wird abgefragt
	
	allocate(displacement(numProc))
	allocate(sendcounts(numProc))
	
	if (numProc ==5)then	! Falls 5 Prozesse wird die Matrix wie folgt aufgeteilt:
		
		sendcounts =  (/4,5,5,5,5/)*24
		displacement =(/0,4,9,14,19/)*24
				
	elseif (numProc == 7) then	! Falls 7 Prozesse s.o.
		
		sendcounts =  (/3,3,3,3,4,4,4/)*24
		displacement =(/0,3,6,9,12,16,20/)*24
		
	endif
	
	lines = sendcounts(myRank+1)/24
	
	allocate(chunk(24,lines)) ! In Abhängigkeit von der Prozessnummer wird die Teilmatrix alloziiert
	
	call MPI_SCATTERV(matrix, sendcounts, displacement, MPI_INTEGER, chunk, sendcounts(myRank+1),&
			& MPI_INTEGER, 0, MPI_COMM_WORLD, ierror) ! Matrix wird auf Teilmatrizen aufgeteilt
	
	chunk = chunk*(myRank+1)	! Die Teilmatrizen werden mit der (Prozessor ID +1) multipliziert
	partSum = sum(chunk)	! Dann wird die Summe der Elemente der Teilmatrizen gebildet
	
	call MPI_REDUCE(partSum, totSum, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierror)
	
	call MPI_GATHERV(chunk, sendcounts(myRank+1), MPI_INTEGER, matrix, sendcounts, displacement, &
			& MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
	
	matSum = sum(matrix)	! Summe der Elemente der veränderten Gesamtmatrix
	
	if (myRank .eq. 0) then
		write(*,*) 'Result from partial matrices: ',totSum 
		write(*,*) 'Result from one matrix: ', matSum
	endif
	
	deallocate(chunk)
	call MPI_FINALIZE(ierror)	! Beenden des Parallelisierungsprozesses
end program 

subroutine initializeMatrix(matrix) ! Initialisierung der Matrix
	integer, intent(out) :: matrix(24,24)
	integer :: i,j
	
	do j=1,24
		do i=1,24
			matrix(i,j) = 576 - (i-1) - (j-1) * 24
		enddo 
	enddo
	
end subroutine initializeMatrix