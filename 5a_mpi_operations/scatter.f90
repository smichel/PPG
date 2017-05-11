program scatter
	use mpi
	implicit none
	integer :: myRank,numProc,ierror
	integer :: matrix(24,24)
	integer :: numEl
	integer, allocatable :: chunk(:,:)
	integer :: partSum, totSum, matSum

	call initializeMatrix(matrix)
	
	call MPI_INIT(ierror)   ! Initialisieren des Parallelisierungsprozesses 
	call MPI_COMM_RANK(MPI_COMM_WORLD,myRank,ierror)    ! aktueller Rang des Prozesses wird abgefragt
	call MPI_COMM_SIZE(MPI_COMM_WORLD,numProc,ierror)   ! Anzahl an Prozessen wird abgefragt
	
	allocate(chunk(24,24/numProc))
	
	numEl = 24*24/numProc	
	call MPI_SCATTER(matrix, numEl, MPI_INTEGER, chunk, numEl, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
	
	chunk = chunk*(myRank+1)
	partSum = sum(chunk)
	
	call MPI_REDUCE(partSum, totSum, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierror)
	call MPI_GATHER(chunk, numEl, MPI_INTEGER, matrix, numEl, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
	
	matSum = sum(matrix)
	
	if (myRank .eq. 0) then
		write(*,*) 'Result from partial matrices: ',totSum 
		write(*,*) 'Result from one matrix: ', matSum
	endif

	call MPI_FINALIZE(ierror)	! Beenden des Parallelisierungsprozesses
end program 

subroutine initializeMatrix(matrix)
	integer, intent(out) :: matrix(24,24)
	integer :: i,j
	
	do j=1,24
		do i=1,24
			matrix(i,j) = 576 - (i-1) - (j-1) * 24
		enddo 
	enddo
	
end subroutine initializeMatrix