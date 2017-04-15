program circle
	implicit none
	integer, parameter :: k_number = 32	!Anzahl der Personen im Kreis
	integer, parameter :: k_step = 3	!Jeder dritte Person fliegt raus
	integer :: i,o,c	!Indexvariablen
	integer, dimension(:,:), allocatable :: i_circle	!1. Spalte alle Personen, 2.Spalte im Kreis/ausserhalb des Kreises
	integer, dimension(:), allocatable :: i_output	!Output
	
	
	allocate(i_circle(k_number,2))
	allocate(i_output(k_number))
	
	! Personennummer und Status der Personen im Kreis wird festgelegt
	do i = 1,k_number
		i_circle(i,2)=1
		i_circle(i,1)=i
	end do
	
	
	o=0	!Zaehlt wie viele Personen ausserhalb des Kreises sind
	c=0	!Iteriert von 0 bis k_step
	! Schleife wird durchlaufen bis alle Personen ausserhalb des Kreises sind
	do while (o<k_number)
		! Schleife durchlaeuft alle Personen
		do i=1,k_number
			! wenn Person im Kreis steht erhoeht sich c
			if (i_circle(i,2)==1) then
				c = c + 1
			end if
			! bei der dritten gezaehlten Person fliegt die Person aus dem Kreis 
			if (c == k_step) then
				i_output(o+1) = i_circle(i,1)	!Person wird rausgeschrieben
				i_circle(i,2) = 0	! Status der Person wird geaendert
				o = o + 1
				c = 0
			end if
		end do
	end do
	! Liste der Personennummern wird ausgegeben
	write(*,'(I2.2)') i_output(:)
	
end program circle