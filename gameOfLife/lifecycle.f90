module mod_lifecycle
	implicit none
contains
	
	subroutine countNeighbors(playground,neighbors)
		logical, intent(in) :: playground(0:31,0:21)
		integer, intent(out) :: neighbors(30,20)
		integer :: counter = 0
		integer :: i,j
		do i = 1,30
			do j = 1,20
				counter = 0
				if (playground(i-1,j-1)) counter = counter + 1 
				if (playground(i,j-1)) counter = counter + 1
				if (playground(i+1,j-1)) counter = counter + 1
				if (playground(i-1,j)) counter = counter + 1
				if (playground(i+1,j)) counter = counter + 1
				if (playground(i-1,j+1)) counter = counter + 1
				if (playground(i,j+1)) counter = counter + 1
				if (playground(i+1,j+1)) counter = counter + 1
				neighbors(i,j) = counter
			enddo
		enddo
	end subroutine

	subroutine developLife(playground)
		logical :: playground(0:31,0:21)
		integer :: i,j
		integer :: neighbors(30,20)
		call countNeighbors(playground,neighbors)
		do i = 1,30
			do j = 1,20
				if (.NOT.(playground(i,j)) .AND. neighbors(i,j) == 3) then
					playground(i,j) = .TRUE.
				else if (playground(i,j) .AND. (neighbors(i,j)<2 .OR. neighbors(i,j) > 3)) then
					playground(i,j) = .FALSE.
				endif
			enddo
		enddo

	end subroutine
end module
