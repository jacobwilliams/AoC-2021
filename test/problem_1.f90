program problem_1

use aoc_utilities

implicit none

integer,dimension(:),allocatable :: iarray, iarray2
integer :: i, n

iarray = read_file_to_integer_array('inputs/day1.txt')

n = 0
do i = 2, size(iarray)
    if (iarray(i)>iarray(i-1)) n = n + 1
end do

write(*,*) '1A: number measurements larger than the previous: ', n

allocate(iarray2(0))
do i = 1, size(iarray)-2
    iarray2 = [iarray2, sum(iarray(i:i+2))]
end do

n = 0
do i = 2, size(iarray2)
    if (iarray2(i)>iarray2(i-1)) n = n + 1
end do
write(*,*) '1B: number sums larger than the previous: ', n

end program problem_1