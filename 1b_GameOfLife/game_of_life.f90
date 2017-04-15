program game_of_life
use mod_lifecycle
use mod_initializeField
use mo_glider
logical,allocatable :: playground(:,:)
integer :: outputUnit = 6
!playground(4,4) = .TRUE.
!playground(4,5) = .TRUE.
!playground(4,6) = .TRUE.
open(outputUnit, encoding='UTF-8') ! change character set to UTF-8 for stdout,
! printTwoDLogical needs that.

call createField(30,20,playground)
call createFigures(playground)
call printTwoDLogical(6, playground)
do i = 1,100
	call developLife(playground)
	call printTwoDLogical(outputUnit, playground(1:30,1:20))
enddo
end program game_of_life
