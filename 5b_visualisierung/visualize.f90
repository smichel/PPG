program visualize
	use mpi
	implicit none
	integer :: myRank,numProc,ierror, status(MPI_STATUS_SIZE)
	integer :: gift = 42	! Zufällige Variable die zwischen Prozessen ausgetauscht wird
	integer :: proc	! Index der einzelnen Prozesse
	
	call MPI_INIT(ierror)   ! Initialisieren des Parallelisierungsprozesses 
	call MPI_COMM_RANK(MPI_COMM_WORLD,myRank,ierror)    ! aktueller Rang des Prozesses wird abgefragt
	call MPI_COMM_SIZE(MPI_COMM_WORLD,numProc,ierror)   ! Anzahl an Prozessen wird abgefragt
	call MPI_BARRIER(MPI_COMM_WORLD, ierror)	! Barrier bis alle Prozesse initialisiert haben
	
	! Hier wird von Prozess 3 die Variable "gift" an Prozess 2 gesendet.
	if (myRank .eq. 3) then	
		call MPI_SEND(gift, 1, MPI_INTEGER, 2, 42, MPI_COMM_WORLD, ierror)
	endif
	! s.o. hier wird die Variable empfangen
	if (myRank .eq. 2) then
		call MPI_RECV(gift, 1, MPI_INTEGER, 3, 42, MPI_COMM_WORLD, status, ierror)
	endif
	
	call MPI_BARRIER(MPI_COMM_WORLD, ierror)
	
	call MPI_BCAST(gift, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror) ! Der Master versendet mit MPI_BCAST 
									! die Variable "gift" an alle anderen Prozesse
	
 	call MPI_BARRIER(MPI_COMM_WORLD, ierror)
 	
	! Der Masterprozess schickt mit MPI_SEND die Variable "gift" an alle anderen Prozesse
	if (myRank .eq. 0) then 
		do proc = 1, numProc-1
			call MPI_SEND(gift, 1, MPI_INTEGER, proc, 42, MPI_COMM_WORLD, ierror)
		enddo
	endif
	
	! Alle außer dem Masterprozess erhalten die Variable "gift" vom Masterprozess
	if (myRank .ne. 0) then
		call MPI_RECV(gift, 1, MPI_INTEGER, 0, 42, MPI_COMM_WORLD, status, ierror)
	endif

	call MPI_FINALIZE(ierror)	! Beenden des Parallelisierungsprozesses
	
end program