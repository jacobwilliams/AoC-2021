program problem_2

use aoc_utilities

implicit none

integer,dimension(2) :: xz !! [horizontal position, depth]
integer,dimension(3) :: xza !! [horizontal position, depth, aim]
integer :: i, ival, iunit
character(len=:),allocatable :: line
logical :: status_ok
type(string),dimension(:),allocatable :: vals

xz = 0
xza = 0

open(newunit=iunit,file='inputs/day2.txt')
do i = 1, number_of_lines_in_file(iunit)

    call read_line_from_file(iunit,100,line,status_ok)

    call split(line,' ',100,vals)

    read(vals(2)%str, '(I10)') ival ! string to integer

    select case (vals(1)%str)
    case('forward')
        xz(1) = xz(1) + ival
        xza(1) = xza(1) + ival          ! increases your horizontal position by X units.
        xza(2) = xza(2) + (xza(3)*ival) ! increases your depth by your aim multiplied by X
    case('up')
        xz(2) = xz(2) - ival
        xza(3) = xza(3) - ival
    case('down')
        xz(2) = xz(2) + ival
        xza(3) = xza(3) + ival
    case default; error stop 'error'
    end select

end do

write(*,*) '2A: horizontal * depth: ', product(xz)

write(*,*) '2B: horizontal * depth: ', product(xza(1:2))

end program problem_2