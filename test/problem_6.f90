program problem_6

use aoc_utilities

implicit none

integer :: iunit, n, i, x, y, j, x1, x2, y1, y2
character(len=:),allocatable :: line_from_file
logical :: status_ok
type(string),dimension(:),allocatable :: vals
integer,dimension(:),allocatable :: list
integer :: n_days

open(newunit=iunit,file='inputs/day6-test.txt')

call read_line_from_file(iunit,line_from_file,status_ok)
call split(line_from_file,',',vals)
n = size(vals)

allocate(list(n))
do i = 1, n
    list(i) = vals(i)%to_int()
end do

write(*,*) 'initial size: ', size(list)
!write(*,*) list

n_days = 80
do i = 1, n_days
    list = list - 1
    if (i==n_days) exit
    write(*,*) i, size(list)
    n = count(list==0)
    do j = 1, n
        list = [list,9]
    end do
    where (list==0) list = 7
end do

write(*,*) '6A: final size:', size(list)


end program problem_6
