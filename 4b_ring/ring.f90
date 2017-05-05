program ring
        use mpi
        implicit none
         integer :: myRank, ierror, istatus(MPI_STATUS_SIZE), numproc, master
         integer :: currentRank, mySum,i
         
        call MPI_INIT(ierror)
        call MPI_COMM_RANK(MPI_COMM_WORLD,myRank,ierror)
        call MPI_COMM_SIZE(MPI_COMM_WORLD,numproc,ierror)
        
        currentRank = myRank
        mySum = currentRank
        do i = 1,numproc-1
                if (myRank == numproc-1) then
                        call MPI_SEND(currentRank,1,MPI_INTEGER,0, 2017, MPI_COMM_WORLD, ierror)
                else
                        call MPI_SEND(currentRank,1,MPI_INTEGER,myRank+1, 2017, MPI_COMM_WORLD, ierror)
                endif
                if (myRank == 0) then
                        call MPI_RECV(currentRank,1,MPI_INTEGER,numproc-1, 2017, MPI_COMM_WORLD,istatus, ierror)
                else
                        call MPI_RECV(currentRank,1,MPI_INTEGER,myRank-1, 2017, MPI_COMM_WORLD, istatus,ierror)
                endif
                mySum = mySum + currentRank
        enddo
        print*,mySum
        call MPI_FINALIZE(ierror)
end program