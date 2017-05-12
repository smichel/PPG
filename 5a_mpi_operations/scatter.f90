program scatter
	use mpi
	implicit none
	integer :: myRank,numProc,ierror
	integer :: matrix(24,24)
	integer :: numEl	! Anzahl an Matrixelementen pro Prozess
	integer, allocatable :: chunk(:,:) ! Teilmatrix
	integer :: partSum ! Teilsumme aus den einzelnen Teilmatrizen
	integer :: totSum ! Gesamtsumme aus den Teilsummen
	integer :: matSum ! Gesamtsumme aus den einzelnen Matrixelementen

	call initializeMatrix(matrix)
	
	call MPI_INIT(ierror)   ! Initialisieren des Parallelisierungsprozesses 
	call MPI_COMM_RANK(MPI_COMM_WORLD,myRank,ierror)    ! aktueller Rang des Prozesses wird abgefragt
	call MPI_COMM_SIZE(MPI_COMM_WORLD,numProc,ierror)   ! Anzahl an Prozessen wird abgefragt
	
	allocate(chunk(24,24/numProc))
	
	numEl = 24*24/numProc	! Je nach Anzahl der Prozessoren sind die Teilmatrizen variabel groß
	call MPI_SCATTER(matrix, numEl, MPI_INTEGER, chunk, numEl, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
	
	chunk = chunk*(myRank+1)	! Die Teilmatrizen werden mit der (Prozessor ID +1) multipliziert
	partSum = sum(chunk)	! Dann wird die Summe der Elemente der Teilmatrizen gebildet
	
	call MPI_REDUCE(partSum, totSum, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierror)
	call MPI_GATHER(chunk, numEl, MPI_INTEGER, matrix, numEl, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
	
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