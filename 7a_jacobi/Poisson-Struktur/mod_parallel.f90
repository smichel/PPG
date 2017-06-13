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
		! Fuer verschiedene Anzahl an Prozesssen werden sendcounts und displacement berechnet
		! Zunaechst werden nur die Zeilen verteilt auf denen auch gerechnet wird
		
		if (mod((numEl - 1),numProc)==0) then ! Falls die Anzahl der Zeilen auf alle Prozesse aufgeteilt werden kann
			! write (*,*) 'Fall 1'
			lines=(numEl - 1)/numProc
			
			do i=1,numProc
			
				sendcounts(i) = lines*(numEl+1)
				displacement(i)=lines*(numEl+1)*(i-1)

			enddo
			
		else 
			if (mod(numEl-1,numProc-1) == 0) then	! Sonderfall, falls der letzte Prozess keine Zeilen zum Rechnen bekommt
				lines = int((numEl-1)/(numProc))	! In diesem Fall bekommt der letzte Prozess mehr Zeilen als alle anderen
				rest = mod(numEl-1,numProc)
				! write (*,*) 'Fall 2', lines, rest			
				do i=1,numProc
					sendcounts(i) = lines*(numEl+1)
			
					if (i==numProc) then
						sendcounts(i)=lines * (numEl +1)+rest*(numEl +1)
					endif
			
					displacement(i)=lines*(numEl+1)*(i-1)

				enddo
				
			else ! Der letzte Prozess bekommt die uebrig gebliebenen Zeilen

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
		

		sendcounts = sendcounts + 2 * (numEl + 1) ! Jede Teilmatrix bekommt noch zwei Linien als Randbedingung
			
	END SUBROUTINE getIndices
	
	
	
	SUBROUTINE communicate(numEl, myRank, numProc, lines, chunk)
		USE mpi
		IMPLICIT NONE
		integer, intent(in) :: numProc, myRank, numEl, lines
		double precision, dimension(:,:), intent(inout) :: chunk
		integer :: status(MPI_STATUS_SIZE), ierr, request
		
		if (myRank .lt. (numProc-1)) then ! Alle Prozesse ausser dem letzten Prozess uebergeben ihre vorletze Zeile an den Prozess davor.
			call MPI_SEND(chunk(:,lines-1),numEl+1, MPI_DOUBLE_PRECISION, myRank+1, 99 ,MPI_COMM_WORLD, ierr)
			call MPI_RECV(chunk(:,lines),  numEl+1, MPI_DOUBLE_PRECISION, myRank+1, 99,MPI_COMM_WORLD, status, ierr)
		end if
		
		if (myRank .gt. 0) then	! Alle Prozesse ausser dem ersten Prozess uebergen ihre zweite Zeile an den Prozess dahinter.
			call MPI_RECV(chunk(:,1), numEl+1, MPI_DOUBLE_PRECISION, myRank-1, 99, MPI_COMM_WORLD, status, ierr)
			call MPI_SEND(chunk(:,2), numEl+1, MPI_DOUBLE_PRECISION, myRank-1, 99, MPI_COMM_WORLD, ierr)
		endif
	END SUBROUTINE communicate
END MODULE parallel 
