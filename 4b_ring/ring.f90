program ring
        use mpi
        implicit none
         integer :: myRank, ierror, istatus(MPI_STATUS_SIZE), numproc, master
         integer :: currentRank, mySum,i
         
        call MPI_INIT(ierror)   ! Initialisieren des Parallelisierungsprozesses 
        call MPI_COMM_RANK(MPI_COMM_WORLD,myRank,ierror)    ! aktueller Rang des Prozesses wird abgefragt
        call MPI_COMM_SIZE(MPI_COMM_WORLD,numproc,ierror)   ! Anzahl an Prozessen wird abgefragt
        
        currentRank = myRank
        mySum = currentRank ! Jeder Prozess bekommt zu Beginn seinen eigenen Rang als Wert zugewiesen
        do i = 1,numproc-1  ! So kann die Schleife verkürzt werden
                if (myRank == numproc-1) then   ! Falls der letzte Prozess erreicht ist , wird currentRank an den Ersten (Master) weitergegeben
                        call MPI_SEND(currentRank,1,MPI_INTEGER,0, 2017, MPI_COMM_WORLD, ierror)
                else    ! Ansonsten wird currentRank an den nächsten weitergegeben
                        call MPI_SEND(currentRank,1,MPI_INTEGER,myRank+1, 2017, MPI_COMM_WORLD, ierror)
                endif
                if (myRank == 0) then   ! Der Masterprozess empfaengt currentRank vom letzten Prozess
                        call MPI_RECV(currentRank,1,MPI_INTEGER,numproc-1, 2017, MPI_COMM_WORLD,istatus, ierror)
                else    ! Alle anderen Prozesse empfangen currentRank vom vorigen Prozess
                        call MPI_RECV(currentRank,1,MPI_INTEGER,myRank-1, 2017, MPI_COMM_WORLD, istatus,ierror)
                endif
                mySum = mySum + currentRank ! Fuer jeden Prozess werden die empfangenen currentRank integer aufsummiert
        enddo
        print*,mySum    ! Ausgabe aller Summen (Menge = numproc)
        call MPI_FINALIZE(ierror)
end program