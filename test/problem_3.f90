program problem_3

use aoc_utilities

implicit none

integer :: i, ival, iunit, n, n_lines, j, n_ones, n_zeros
character(len=:),allocatable :: line
logical :: status_ok
integer,dimension(:,:),allocatable :: bits
integer,dimension(:),allocatable :: most_common_bit, least_common_bit

open(newunit=iunit,file='inputs/day3.txt')

n_lines = number_of_lines_in_file(iunit)
write(*,*) 'n_lines = ', n_lines

do i = 1, n_lines

    call read_line_from_file(iunit,100,line,status_ok)
    !write(*,*) line

    if (i==1) then
        n = len(line) ! number of cols
        allocate(bits(n_lines,n))
        allocate(most_common_bit(n))
        allocate(least_common_bit(n))
    end if

    do j = 1, n
        read(line(j:j), '(I1)') bits(i,j) ! string to integer
    end do

end do

most_common_bit = 0
least_common_bit = 0
do i = 1, n
    n_ones = count(bits(:,i)==1)
    n_zeros = n_lines - n_ones
    if (n_ones>n_zeros) then
        most_common_bit(i) = 1
    else
        least_common_bit(i) = 1
    end if
    !write(*,*) i, n_ones, n_zeros

end do

write(*,'(A,*(I1))') 'most_common_bit  (binary) = ', most_common_bit   ! 2566
write(*,'(A,*(I1))') 'least_common_bit (binary) = ', least_common_bit  ! 1529
! ... use https://www.mathsisfun.com/binary-decimal-hexadecimal-converter.html

write(*,*) '3A: ', 2566 * 1529

end program problem_3
