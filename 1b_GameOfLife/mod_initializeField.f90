module mod_initializeField
	implicit none
	contains
	
		subroutine createField(x,y,field)
			integer, intent(in) :: x, y
			logical, allocatable, intent(out) :: field(:,:)
			allocate(field(0:x+1,0:y+1))
		end subroutine createField
	
		subroutine createFigures(field)
			integer :: blinker_x, blinker_y
			integer :: toad_x, toad_y
			integer :: beacon_x,beacon_y
			logical, intent(inout) :: field(0:,0:)
			blinker_x=4
			blinker_y=4
			
			toad_x=10
			toad_y=3

			beacon_x=13
			beacon_y=13

			field(blinker_x:blinker_x+2,blinker_y)=.TRUE.
			
			field(toad_x+1:toad_x+3,toad_y)=.TRUE.
			field(toad_x:toad_x+2,toad_y+1)=.TRUE.
			
			field(beacon_x+1:beacon_x+2,beacon_y+1)=.TRUE.
			field(beacon_x+1,beacon_y+2)=.TRUE.
			field(beacon_x+4,beacon_y+3)=.TRUE.
			field(beacon_x+3:beacon_x+4,beacon_y+4)=.TRUE.
		end subroutine createFigures

end module mod_initializeField


