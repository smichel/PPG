program pi
	use mpi
	implicit none
	
	integer :: i	! Zaehlindex
	integer :: myid,numproc,ierr,master,rank,sice, status(MPI_STATUS_SIZE)
	integer :: resolution = 10**9 ! Auflösung
	
	double precision :: width  ! Gitterweite
	double precision :: partsum = 0 ! Teilsumme
	double precision :: integral = 0 ! Pi
	double precision :: pie	! Pi
	
	width = 1./resolution 
	master = 0
	
	call MPI_INIT(ierr)
		CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
		CALL MPI_COMM_SIZE(MPI_COMM_WORLD,sice,ierr)
				
		do i = 1+(rank*(resolution/sice)), (rank+1)*(resolution/sice)
			partsum=partsum + ((4./(1+(i*width)**2)+4./(1+((i-1)*width)**2))/2.*width)
		enddo
		if (rank .ne. master) then
			call MPI_SEND(partsum,1,MPI_DOUBLE_PRECISION,master,2017,MPI_COMM_WORLD,ierr)
		else
			integral = partsum
			do i = 1, sice-1
				call MPI_RECV(partsum,1,MPI_DOUBLE_PRECISION,i,2017,MPI_COMM_WORLD,status,ierr)
				integral=integral+partsum
			enddo
			write(*,*) integral
		endif
		
	CALL MPI_FINALIZE(ierr)
	

	
	
end program pi