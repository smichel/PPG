program game_of_life

	use mod_lifecycle
	use mod_initializeField
	use mo_glider

	logical,allocatable :: playGround(:,:)	! Spielfeld
	integer :: outputUnit = 6

	open(outputUnit, encoding='UTF-8')	! aendert character set zu UTF-8 fuer stdout,
	! wird fuer printTwoDLogical gebraucht

	! ruft subroutine createField auf, um das Spielfeld zu generieren
	call createField(30,20,playGround)
	! ruft subroutine createFigures auf, um das Spielfeld zu bevoelkern
	call createFigures(playGround)
	! ruft subroutine printTwoDLogical auf, um das Spielfeld anzuzeigen
	call printTwoDLogical(outputUnit, playGround(1:30,1:20))

	! fuert 160 Iterationen der subroutine developLife aus und gibt das Ergebnis aus
	do i = 1,160
		! wende zyklische Randbedingungen vor jedem Lifecycle an 
		call setBoundaries(playGround)
		! ruft subroutine developLife auf, um naechsten Zustand im Lebenszyklus zu erhalten
		call developLife(playGround)
		call printTwoDLogical(outputUnit, playGround(1:30,1:20))
	enddo
end program game_of_life
