program problem_5b

use aoc_utilities

implicit none

type :: line
    integer :: x1 = 0
    integer :: y1 = 0
    integer :: x2 = 0
    integer :: y2 = 0
end type line

integer :: iunit, n_lines, i, x, y, j, dx, dy
integer,dimension(:,:),allocatable :: map
type(line),dimension(:),allocatable :: lines
character(len=:),allocatable :: line_from_file
logical :: status_ok
type(string),dimension(:),allocatable :: vals
type(string),dimension(:),allocatable :: initial_vals, final_vals
integer :: max_x, max_y
logical :: horizontal, vertical, neither

open(newunit=iunit,file='inputs/day5.txt')

n_lines = number_of_lines_in_file(iunit)

allocate(lines(n_lines))

! first read all the lines:
do i = 1, n_lines
    call read_line_from_file(iunit,100,line_from_file,status_ok)
    call split(line_from_file,' -> ',100,vals)
    if (size(vals)/=2) error stop 'invalid line'
    call split(vals(1)%str,',',100,initial_vals)
    call split(vals(2)%str,',',100,final_vals)
    if (size(initial_vals)/=2) error stop 'invalid initial_vals'
    if (size(final_vals)/=2) error stop 'invalid final_vals'
    read(initial_vals(1)%str,*) lines(i)%x1
    read(initial_vals(2)%str,*) lines(i)%y1
    read(final_vals(1)%str,*)   lines(i)%x2
    read(final_vals(2)%str,*)   lines(i)%y2
end do
! write(*,*) lines

! now, size the map:
max_x = maxval([lines(:)%x1, lines(:)%x2])
max_y = maxval([lines(:)%y1, lines(:)%y2])
!write(*,*) 'max x,y = ', max_x, max_y

allocate(map(0:max_y, 0:max_x))
map = 0

! now, populate the map with the lines:
do i = 1, n_lines

    ! horizontal, vertical, or diagonal (45 deg) ...

    horizontal = lines(i)%y1 == lines(i)%y2
    vertical   = lines(i)%x1 == lines(i)%x2
    neither = (.not. horizontal) .and. (.not. vertical)

    if (horizontal) then
        dx = sign(1,lines(i)%x2-lines(i)%x1)
        dy = 0
    else if (vertical) then
        dx = 0
        dy = sign(1,lines(i)%y2-lines(i)%y1)
    else
        dx = sign(1,lines(i)%x2-lines(i)%x1)
        dy = sign(1,lines(i)%y2-lines(i)%y1)
    end if

    x = lines(i)%x1
    y = lines(i)%y1
    do
        map(y, x) = map(y, x) + 1
        if (x==lines(i)%x2 .and. y==lines(i)%y2) exit ! done
        x = x + dx
        y = y + dy
    end do

end do

! visualize the board:
do i = 0, ubound(map,1)
    do j = 0, ubound(map,2)
        if (map(i,j)==0) then
            write(998,'(A)',advance='NO') '.'
        else
            write(998,'(I1)',advance='NO') map(i,j)
        end if
    end do
    write(998,*) ''
end do

write(*,*) '5B: number where 2 or more overlap: ', count(map>1)

end program problem_5b
