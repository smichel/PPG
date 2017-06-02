MODULE parallel
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
			write (*,*) 'Fall 1'
			lines=(numEl - 1)/numProc
			
			do i=1,numProc
			
				sendcounts(i) = lines*(numEl+1)
				displacement(i)=lines*(numEl+1)*(i-1)

			enddo
			
		else 
			if (mod(numEl-1,numProc-1) == 0) then
				lines = int((numEl-1)/(numProc))
				rest = mod(numEl-1,numProc)
				write (*,*) 'Fall 2', lines, rest			
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
				write (*,*) 'Fall 3', lines, rest			
				do i=1,numProc
					sendcounts(i) = lines*(numEl+1)
			
					if (i==numProc) then
						sendcounts(i)=rest*(numEl +1)
					endif
			
					displacement(i)=lines*(numEl+1)*(i-1)

				enddo
			endif		
		endif
		
! 		do i=1,numProc
! 			sendcounts(i) = sendcounts(i) + 2 * (numEl + 1)
! 			write(*,*) sendcounts(i)
! 			print*
! 			write(*,*) displacement(i)
! 			print*
! 		enddo 
		

			
	END SUBROUTINE getIndices
END MODULE parallel 
