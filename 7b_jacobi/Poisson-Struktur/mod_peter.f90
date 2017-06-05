MODULE peter
	IMPLICIT NONE
	CONTAINS
		
	SUBROUTINE getIndices(numProc, numEl, sendcounts, displacement)
		IMPLICIT NONE
		integer, intent(in) :: numProc, numEl
		integer, allocatable, dimension(:), intent(out) :: sendcounts, displacement
		integer :: lines, rest 
		integer :: i
		
		allocate(displacement(numProc))
		allocate(sendcounts(numProc))
		
		if (mod((numEl - 1),numProc)==0) then !comments need to be added
			! write (*,*) 'Fall 1'
			lines=(numEl - 1)/numProc
			
			do i=1,numProc
			
				sendcounts(i) = lines*(numEl+1)
				displacement(i)=lines*(numEl+1)*(i-1)

			enddo
			
		else 
			if (mod(numEl-1,numProc-1) == 0) then
				lines = int((numEl-1)/(numProc))
				rest = mod(numEl-1,numProc)
				! write (*,*) 'Fall 2', lines, rest			
				do i=1,numProc
					sendcounts(i) = lines*(numEl+1)
			
					if (i==numProc) then
						sendcounts(i)=lines * (numEl +1)+rest*(numEl +1)
					endif
			
					displacement(i)=lines*(numEl+1)*(i-1)

				enddo
				
			else

				lines = int((numEl-1)/(numProc-1))
				rest = mod(numEl-1,numProc-1)
				! write (*,*) 'Fall 3', lines, rest			
				do i=1,numProc
					sendcounts(i) = lines*(numEl+1)
			
					if (i==numProc) then
						sendcounts(i)=rest*(numEl +1)
					endif
			
					displacement(i)=lines*(numEl+1)*(i-1)

				enddo
			endif		
		endif
		

		sendcounts = sendcounts + 2 * (numEl + 1)
			
	END SUBROUTINE getIndices
	
	SUBROUTINE communicate(numEl, myRank, numProc, lines, chunk)
		USE mpi
		IMPLICIT NONE
		integer, intent(in) :: numProc, myRank, numEl, lines
		double precision, dimension(:,:), intent(inout) :: chunk
		integer :: status(MPI_STATUS_SIZE), ierr, request
		
		if (myRank .lt. (numProc-1)) then
			call MPI_SEND(chunk(:,lines-1),numEl+1, MPI_DOUBLE_PRECISION, myRank+1, 99 ,MPI_COMM_WORLD, ierr)
			call MPI_RECV(chunk(:,lines),  numEl+1, MPI_DOUBLE_PRECISION, myRank+1, 99,MPI_COMM_WORLD, status, ierr)
		end if
		
		if (myRank .gt. 0) then	
			call MPI_RECV(chunk(:,1), numEl+1, MPI_DOUBLE_PRECISION, myRank-1, 99, MPI_COMM_WORLD, status, ierr)
			call MPI_SEND(chunk(:,2), numEl+1, MPI_DOUBLE_PRECISION, myRank-1, 99, MPI_COMM_WORLD, ierr)
		endif
		
		
		
		
	END SUBROUTINE communicate
END MODULE peter 
