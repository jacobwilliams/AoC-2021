program problem_7

use aoc_utilities

implicit none

integer :: iunit, n, i, j, k
character(len=:),allocatable :: line
logical :: status_ok
type(string),dimension(:),allocatable :: vals
integer,dimension(:),allocatable :: list, diff
integer :: min_fuel, fuel

open(newunit=iunit,file='inputs/day7.txt')

call read_line_from_file(iunit,line,status_ok)

call split(line,',',vals)
n = size(vals)

allocate(list(n))
do i = 1, n
    list(i) = vals(i)%to_int()
end do

!****************************

min_fuel = minval([(sum(abs(list-i)), i = 0, maxval(list))])

write(*,*) '7a: ', min_fuel

!****************************
min_fuel = huge(1)
do i = 0, maxval(list)
    diff = abs(list-i)
    fuel = sum( [(sum( [(k, k = 1, diff(j))] ), j = 1, size(list) )] )
    if (fuel < min_fuel) then
        min_fuel = fuel
    end if
end do
write(*,*) '7b: ', min_fuel

end program problem_7
