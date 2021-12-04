program problem_4

use aoc_utilities

implicit none

type :: board
  integer,dimension(5,5) :: nums = 0
  logical,dimension(5,5) :: mask = .false.
  logical :: won = .false.
end type board

type(board),dimension(:),allocatable :: boards
integer :: iunit, n_lines, i, n_boards, n_nonblank_lines, iboard, j, k
character(len=:),allocatable :: line
logical :: status_ok
integer,dimension(:),allocatable :: draws
type(string),dimension(:),allocatable :: vals
integer :: winning_draw, winning_board, winning_draw_num, score
integer :: num_not_won

open(newunit=iunit,file='inputs/day4.txt')

! count boards
n_lines = number_of_lines_in_file(iunit)
n_nonblank_lines = 0
do i = 1, n_lines
    call read_line_from_file(iunit,100,line,status_ok)
    if (line/='') n_nonblank_lines = n_nonblank_lines + 1
end do
n_boards = (n_nonblank_lines - 1) / 5
! write(*,*) 'n_lines           = ', n_lines
! write(*,*) 'n_nonblank_lines  = ', n_nonblank_lines
! write(*,*) 'n_boards          = ', n_boards

allocate(boards(n_boards))
iboard = 0
j = 0
rewind(iunit)
do i = 1, n_lines
    call read_line_from_file(iunit,100,line,status_ok)
    if (.not. allocated(line)) cycle
    if (i==1) then
        ! numbers to be drawn:
        call split(line,',',100,vals)
        allocate(draws(size(vals)))
        do j = 1, size(vals)
            read(vals(j)%str, *) draws(j)
        end do
    else if (line == '') then
        iboard = iboard + 1
        j = 0
        cycle
    else
        ! read the jth line of the iboard
        j = j + 1
        read(line, '(I2,1X,I2,1X,I2,1X,I2,1X,I2))') boards(iboard)%nums(j,:)
    end if
end do

! write(*,*) boards(1)%nums

! now, play the game:
main: do i = 1, size(draws)

    ! each board:
    do j = 1, size(boards)
        where (boards(j)%nums == draws(i))
            boards(j)%mask = .true.
        end where
    end do

    ! check for any wins:
    do j = 1, size(boards)
        do k = 1, 5
            if (all(boards(j)%mask(:,k))) then ! cols
                winning_draw_num = i
                winning_draw = draws(i)
                winning_board = j
                exit main
            elseif (all(boards(j)%mask(k,:))) then ! rows
                winning_draw_num = i
                winning_draw = draws(i)
                winning_board = j
                exit main
            end if
        end do
    end do

end do main

write(*,*) 'board', winning_board, 'wins on draw', winning_draw_num, '(', winning_draw,')'

! write(*,*) boards(winning_board)%mask
!The score of the winning board can now be calculated.
!Start by finding the sum of all unmarked numbers on that board;
!in this case, the sum is 188. Then, multiply that sum by the
!number that was just called when the board won, 24, to get
!the final score, 188 * 24 = 4512.

score = 0
do i = 1, 5
    score = score + sum( boards(winning_board)%nums(:,i), mask = .not. boards(winning_board)%mask(:,i) )
end do
score = score * winning_draw

write(*,*) '4A: ', score

!**************** find the last to win:

do i = 1, size(boards)
    boards(i)%mask = .false. ! reset boards
end do

! now, play the game:
main2: do i = 1, size(draws)

    ! each board:
    do j = 1, size(boards)
        where (boards(j)%nums == draws(i))
            boards(j)%mask = .true.
        end where
    end do

    ! check for any wins:
    do j = 1, size(boards)
        do k = 1, 5
            if (all(boards(j)%mask(:,k))) then ! cols
                boards(j)%won = .true.
            elseif (all(boards(j)%mask(k,:))) then ! rows
                boards(j)%won = .true.
            end if
        end do
    end do

    ! check if only one left that hasn't won:
    if ( count(boards(:)%won) == n_boards-1 ) then ! only one left:
        do j = 1, size(boards)
            if (.not. boards(j)%won) then
                winning_board = j
            end if
        end do
    elseif (count(boards(:)%won) == n_boards) then ! last one done
        winning_draw_num = i
        winning_draw = draws(i)
        exit main2
    end if

end do main2

write(*,*) 'board', winning_board, 'last win on draw', winning_draw_num, '(', winning_draw,')'

score = 0
do i = 1, 5
    score = score + sum( boards(winning_board)%nums(:,i), mask = .not. boards(winning_board)%mask(:,i) )
end do
score = score * winning_draw

write(*,*) '4B: ', score

end program problem_4
