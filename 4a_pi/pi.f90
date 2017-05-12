program pi
	use mpi
	implicit none
	
	integer :: i	! Zaehlindex
	integer :: myid,numproc,ierr,master,rank,sice, status(MPI_STATUS_SIZE)
	integer :: resolution = 10**9 ! Aufloesung
	
	double precision :: width  ! Gitterweite
	double precision :: partsum = 0 ! Teilsumme
	double precision :: integral = 0 ! Pi
	double precision :: pie	! Pi
	
	width = 1.d0/resolution 
	master = 0
	
	call MPI_INIT(ierr)	! Initialisieren der Parallelisierung
		CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)	! Welcher Prozess ist gerade aktiv
		CALL MPI_COMM_SIZE(MPI_COMM_WORLD,sice,ierr)	! Anzahl an Prozessen
				
		do i = 1+(rank*(resolution/sice)), (rank+1)*(resolution/sice)	! Durch 'rank' wird die Schleife auf die Prozesse verteilt
			partsum=partsum + ((4.d0/(1.d0+(i*width)**2)+4.d0/(1.d0+((i-1)*width)**2))/2.d0*width)	! Pi wird durch Integration berechnet
		enddo
		if (rank .ne. master) then	! Alle nicht Masterprozesse senden ihr Teilergebnis (partsum) an Master
			call MPI_SEND(partsum,1,MPI_DOUBLE_PRECISION,master,2017,MPI_COMM_WORLD,ierr)
		else
			integral = partsum	! partsum ist hier der vom Masterprozess berechnete Teil von Pi
			do i = 1, sice-1	! fuer alle weiteren Prozesse wird die Teilsumme abgerufen
				call MPI_RECV(partsum,1,MPI_DOUBLE_PRECISION,i,2017,MPI_COMM_WORLD,status,ierr)
				integral=integral+partsum	! Aufsummieren der Teilergebnisse
			enddo
			write(*,*) integral	! Ausgabe von Pi
		endif
		
	CALL MPI_FINALIZE(ierr)	! Beenden des Parallelisierungsprozesses
	

	
	
end program pi