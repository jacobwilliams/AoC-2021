program problem_7

use aoc_utilities

implicit none

integer :: iunit, i, j, k, n
character(len=:),allocatable :: line
logical :: status_ok
type(string),dimension(:),allocatable :: vals
integer,dimension(:),allocatable :: list
integer :: min_fuel

open(newunit=iunit,file='inputs/day7.txt')

call read_line_from_file(iunit,line,status_ok)

call split(line,',',vals)
n = size(vals)

allocate(list(n))
do i = 1, n
    list(i) = vals(i)%to_int()
end do

min_fuel = minval([(sum(abs(list-i)), i = 0, maxval(list))])
write(*,*) '7a: ', min_fuel

min_fuel = minval([( sum( [(sum( [(k, k = 1, abs(list(j)-i))] ), j = 1, size(list) )] ), i = 0, maxval(list) )])
write(*,*) '7b: ', min_fuel

end program problem_7
