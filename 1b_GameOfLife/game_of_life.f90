program game_of_life

	use mod_lifecycle
	use mod_initializeField
	use mo_glider

	logical,allocatable :: playground(:,:)	! Spielfeld
	integer :: outputUnit = 6

	open(outputUnit, encoding='UTF-8') ! change character set to UTF-8 for stdout,
	! wird fuer printTwoDLogical gebraucht

	call createField(30,20,playground)
	call createFigures(playground)
	call printTwoDLogical(6, playground(1:30,1:20))

	do i = 1,10
		call developLife(playground)
		call printTwoDLogical(outputUnit, playground(1:30,1:20))
	enddo
end program game_of_life
