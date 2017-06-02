MODULE parallel
	IMPLICIT NONE
	CONTAINS
		
	SUBROUTINE getIndices(numProc, numEl, sendcounts, displacement)
		IMPLICIT NONE
		integer, parameter, intent(in) :: numProc, numEl
		integer, allocatable, dimension(:), intent(out) :: sendcounts, displacement
		
		if (mod((numEl - 1),numProc)==0) then
			
			lines=(numEl - 1)/numProc
			
			do i=1,numProc
			
				sendcounts(i) = lines*(numEl-1)
				displacement(i)=lines*(numEl-1)*(i-1)

			enddo
			
		else 
			
			lines = int((numEl-1)/(numProc-1))
			rest = mod(numEl-1,int((numEl-1)/(numProc-1)))*(numEl-1)
			
			do i=1,numProc
				sendcounts(i) = lines*(numEl-1)
			
				if (i==numProc) then
					sendcounts(i)=rest
				endif
			
				displacement(i)=lines*(numEl-1)*(i-1)

			enddo
					
		endif
		
		do i=1,numProc
			if ((i .eq. 1)) then
				sendcounts(i) = sendcounts(i) + 1 * lines * (numEl - 1)
			elseif (i .eq. numProc)	then
				sendcounts(i) = sendcounts(i) + 1 * lines * (numEl - 1)
				displacement(i) = displacement(i) - (numEl - 1)
			else
				sendcounts(i) = sendcounts(i) + 2 * lines * (numEl - 1)
				displacement(i) = displacement(i) - (numEl - 1)
			endif
		enddo 
		

			
	END SUBROUTINE getIndices
END MODULE parallel 