module algorithmTester
	use sorter
	integer, parameter :: kMaxValue = 100000000
	integer, parameter :: kMaxBarWidth = 30
contains
	! Seeds the builtin random number generator with the date, ensuring a different sequence on each execution.
	subroutine randomize()
		integer, dimension(:), pointer :: date
		integer :: seedSize

		call random_seed(size=seedSize)
		if(seedSize < 8) seedSize = 8
		allocate(date(seedSize))
		date = 0
		call date_and_time(values=date)
		call random_seed(put=date)
	end subroutine

	! Fill an array with values in a nice range
	subroutine randomizeArray(array)
		real, intent(inout) :: array(:)
		call random_number(array)
		array = array*kMaxValue
	end subroutine

	subroutine printArray(array)
		real, intent(in) :: array(:)
		integer :: i, j
		do i = lbound(array, 1), ubound(array, 1)
			write(*,'(f9.0," ",$)') array(i)
			do j = 1, int(array(i))*kMaxBarWidth/kMaxValue
				write(*,'("-",$)')
			end do
			print*
		end do
	end subroutine

	subroutine runTests()
		real :: someData(1:20)
		call randomize()

		print*,"bubble sort:"
		call randomizeArray(someData)
		call bubbleSort(someData)
		call printArray(someData)

		print*
		print*,"insertion sort:"
		call randomizeArray(someData)
		call insertionSort(someData)
		call printArray(someData)

		print*
		print*,"slow sort:"
		call randomizeArray(someData)
		call slowSort(someData)
		call printArray(someData)

		print*
		print*,"bucket sort:"
		call randomizeArray(someData)
		call startBucketSort(someData)
		call printArray(someData)
	end subroutine
end module

