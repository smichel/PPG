! (c) 2012 Körner, Hübbe
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

module mo_glider
	use mo_utilities
	implicit none
	private utf32
	!This character kind allows us to access the entire unicode character set. For details, see the wikipedia article for UTF-32.
	integer, parameter :: utf32=selected_char_kind('ISO_10646')
contains

	subroutine makeGlider(glider)
		logical, dimension(:,:), pointer, intent(out) :: glider
		allocate(glider(1:3,1:3))
		glider = .true.
		glider(2, 1) = .false.
		glider(1, 2:3) = .false.
		glider(3, 3) = .false.
	end subroutine
	
	!The outputUnit must be opened with UTF-8 as the encoding!
	subroutine printTwoDLogical(outputUnit, matrix)
		integer, intent(in) :: outputUnit
		logical, dimension(:,:), intent(in) :: matrix
		integer :: width, height
		character(kind=utf32, len=1), dimension(:,:), allocatable :: characterMatrix
		! z'1b' hexadezimale Zahl, dezimaler Wert ist 27 
		! char(int(z'00B7'), utf32) ist das Zeichen mit der hexad. Nummer 00B7 
		! des Zeichensatzes 'ISO_10646'
		! 00B7 = middle dot, 2588 = full block
		character(kind=utf32, len=1) :: blockChar = char(int(z'2588'), utf32), emptyChar = char(int(z'00B7'), utf32)
		character(len=22) :: formatString
		
		!Create the character matrix.
		width = ubound(matrix, 1)
		height = ubound(matrix, 2)
		allocate(characterMatrix(1:width, 1:height))
		where (matrix)
			characterMatrix = blockChar
		else where
			characterMatrix = emptyChar
		end where

		!Write it out.
		formatString = '(' // intToStr(width) // 'a)'
		write(outputUnit, formatString) characterMatrix
		formatString = '(' // intToStr(width) // '("="))'
		write(outputUnit, formatString)
		
		!Waste some time.
		call portable_sleep(1.0)
		deallocate(characterMatrix)
	end subroutine
	
end module

