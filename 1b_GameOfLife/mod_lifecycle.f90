module mod_lifecycle
	implicit none
	contains
	
	subroutine countNeighbors(playGround,neighbors)
		logical, intent(in) :: playGround(0:31,0:21)	! Spielfeld
		integer, intent(out) :: neighbors(30,20)	! Feld mit Anzahl an Nachbarn
		integer :: counter = 0	! Variable zum zaehlen der Nachbarn einer Zelle
		integer :: i,j	! Zaehlindices
		! zaehle Nachbarn fuer jede Zelle auf dem Spielfeld 
		do i = 1,30
			do j = 1,20
				counter = 0
				if (playGround(i+1,j)) counter = counter + 1 
				if (playGround(i,j+1)) counter = counter + 1
				if (playGround(i-1,j)) counter = counter + 1
				if (playGround(i,j-1)) counter = counter + 1
				if (playGround(i+1,j-1)) counter = counter + 1
				if (playGround(i-1,j-1)) counter = counter + 1
				if (playGround(i-1,j+1)) counter = counter + 1
				if (playGround(i+1,j+1)) counter = counter + 1
				neighbors(i,j) = counter ! schreibe Anzahl an Nachbarn raus
			enddo
		enddo
	end subroutine

	subroutine developLife(playGround)
		logical :: playGround(0:31,0:21)	! Spielfeld
		integer :: i,j	! Zaehlindices
		integer :: neighbors(30,20)	! Feld mit Anzahl an Nachbarn
		
		! zahle Nachbarn jeder Zelle
		call countNeighbors(playGround,neighbors)
		
		! setze Status jeder Zelle abhaengig von ihrer Anzahl an Nachbarn
		do i = 1,30
			do j = 1,20
				! eine tote zelle mit genau 3 Nachbarn wird zur lebenden Zelle
				if (.NOT.(playGround(i,j)) .AND. neighbors(i,j) == 3) then
					playGround(i,j) = .TRUE.
				! eine lebende Zelle stirbt bei weniger als 2 oder mehr als 3 Nachbarn
				else if (playGround(i,j) .AND. (neighbors(i,j)<2 .OR. neighbors(i,j) > 3)) then
					playGround(i,j) = .FALSE.
				endif
			enddo
		enddo

	end subroutine
end module
