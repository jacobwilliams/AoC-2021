program problem_3

use aoc_utilities

implicit none

integer :: i, iunit, n, n_lines, j, n_ones, n_zeros, k
character(len=:),allocatable :: line
logical :: status_ok
integer,dimension(:,:),allocatable :: bits
integer,dimension(:),allocatable :: most_common_bit, least_common_bit, n_ones_count, n_zeros_count
logical,dimension(:),allocatable :: keep

open(newunit=iunit,file='inputs/day3.txt')

n_lines = number_of_lines_in_file(iunit)
! write(*,*) 'n_lines = ', n_lines

allocate(keep(n_lines))

do i = 1, n_lines

    call read_line_from_file(iunit,100,line,status_ok)
    !write(*,*) line

    if (i==1) then
        n = len(line) ! number of cols
        allocate(bits(n_lines,n))
        allocate(most_common_bit(n))
        allocate(least_common_bit(n))
        allocate(n_ones_count(n))
        allocate(n_zeros_count(n))
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
end do

write(*,'(A,*(I1))') 'most_common_bit  (binary) = ', most_common_bit   ! 2566
write(*,'(A,*(I1))') 'least_common_bit (binary) = ', least_common_bit  ! 1529
! ... use https://www.mathsisfun.com/binary-decimal-hexadecimal-converter.html

write(*,*) '3A: ', 2566 * 1529

!*******************************

! oxygen

keep = .true.
! write(*,*) '***',count(keep)
do i = 1, n
    n_ones  = count(keep .and. bits(:,i)==1)
    n_zeros = count(keep) - n_ones

    do j = 1, n_lines
        ! oxygen
        if (n_ones>=n_zeros) then ! keep ones with 1
            if (keep(j) .and. bits(j,i)==0) keep(j) = .false. ! remove ones with 0
        else ! keep ones with 0
            if (keep(j) .and. bits(j,i)==1) keep(j) = .false. ! remove ones with 1
        end if
    end do

end do

! write(*,*) 'keep: ', count(keep)
do i = 1, n_lines
    if (keep(i)) write(*,'(A,*(I1))') 'oxygen:  ', bits(i,:)
end do


! c02

keep = .true.
! write(*,*) '***',count(keep)
do i = 1, n
    n_ones  = count(keep .and. bits(:,i)==1)
    n_zeros = count(keep) - n_ones
    do j = 1, n_lines
        ! oxygen
        if (n_zeros<=n_ones) then ! keep ones with 1
            if (keep(j) .and. bits(j,i)==1) keep(j) = .false. ! remove ones with 1
        else ! keep ones with 0
            if (keep(j) .and. bits(j,i)==0) keep(j) = .false. ! remove ones with 0
        end if
    end do

    if (count(keep)==1) exit ! we are done I guess?

end do

! write(*,*) 'keep: ', count(keep)
do i = 1, n_lines
    if (keep(i)) write(*,'(A,*(I1))') 'c02:     ', bits(i,:)
end do

! oxygen: 101101100111  -> 2919
! c02:    011111010101  -> 2005

write(*,*) '3B: ', 2919 * 2005

end program problem_3
