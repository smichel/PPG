program pi
use omp_lib
implicit none


integer :: i
integer :: resolution = 10**9
double precision :: partsum, width


partsum = 0
width = 1.d0/resolution


!$OMP PARALLEL DO DEFAULT(NONE)  &
!$OMP SHARED(width) PRIVATE(i) &
!$OMP REDUCTION (+: partsum)
!Berechnen von pi / Reduction summiert die Teilsummen
do i = 1, resolution
	partsum=partsum + ((4.d0/(1.d0+(i*width)**2)+4.d0/(1.d0+((i-1)*width)**2))/2.d0*width)
end do                 
!$OMP END PARALLEL DO 


write(*,*) partsum


end program pi