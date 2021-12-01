program problem_1

use aoc_utilities

implicit none

integer,dimension(:),allocatable :: iarray
integer :: i, n

iarray = read_file_to_integer_array('inputs/day1.txt')

n = 0
do i = 2, size(iarray)
    if (iarray(i)>iarray(i-1)) n = n + 1
end do

write(*,*) '1A: number measurements larger than the previous: ', n

n = 0
do i = 2, size(iarray)-2
    if (sum(iarray(i:i+2)) > sum(iarray(i-1:i+1))) n = n + 1
end do
write(*,*) '1B: number sums larger than the previous: ', n

end program problem_1