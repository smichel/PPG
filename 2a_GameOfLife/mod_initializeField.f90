module mod_initializeField
	implicit none
	contains
	
		subroutine createField(x,y,playGround)
			integer, intent(in) :: x, y
			logical, allocatable, intent(out) :: playGround(:,:)
			allocate(playGround(0:x+1,0:y+1))
		end subroutine createField
	
		subroutine createFigures(playGround)
			logical, intent(inout) :: playGround(0:,0:)
			integer :: glider_x, glider_y
			integer :: lwss_x, lwss_y

			glider_x = 1
			glider_y = 1
			
			lwss_x = 1
			lwss_y = 12

			playGround(glider_x,glider_y)=.true.
			playGround(glider_x+2,glider_y)=.true.
			playGround(glider_x+1:glider_x+2,glider_y+1)=.true.
			playGround(glider_x+1,glider_y+2)=.true.
			

			playGround(lwss_x,lwss_y)=.true.
			playGround(lwss_x+3,lwss_y)=.true.
			playGround(lwss_x+4,lwss_y+1)=.true.
			playGround(lwss_x:lwss_x+4,lwss_y+2)=.true.
			playGround(lwss_x+1:lwss_x+3,lwss_y+2)=.false.
			playGround(lwss_x+1:lwss_x+4,lwss_y+3)=.true.





		end subroutine createFigures

end module mod_initializeField


