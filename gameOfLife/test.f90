program test
use mod_initializeField
implicit none
integer :: i
logical,allocatable :: playGround(:,:)
call createField(30,20,playGround)
call createFigures(playGround)
do i=0,21
	write(*,*) playGround(0:31,i)
enddo
end program