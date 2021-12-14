program problem_6

use aoc_utilities
use iso_fortran_env, only: ip => int64

implicit none

integer :: iunit, n, i, n_days, icase
character(len=:),allocatable :: line
logical :: status_ok
type(string),dimension(:),allocatable :: vals
integer,dimension(:),allocatable :: list
integer(ip),dimension(0:8) :: counts   ! need 64 bit integer for part B or it will overflow

open(newunit=iunit,file='inputs/day6.txt')

call read_line_from_file(iunit,line,status_ok)
call split(line,',',vals)
n = size(vals)

allocate(list(n))
do i = 1, n
    list(i) = vals(i)%to_int()
end do

do icase = 1, 2

    if (icase==1) then
        n_days = 80
    else
        n_days = 256
    end if

    do i = 0, 8
        counts(i) = count(list==i)
    end do
    !write(*,*) 'counts: ', counts

    do i = 1, n_days
        counts = [ counts(1), counts(2), counts(3), counts(4), &
                   counts(5), counts(6), counts(0) + counts(7), &
                   counts(8), counts(0) ]
    end do
    !write(*,*) 'final counts:', counts

    if (icase==1) then
        write(*,*) '6A: number of fish:', sum(counts)
    else
        write(*,*) '6B: number of fish:', sum(counts)
    end if

end do

end program problem_6
