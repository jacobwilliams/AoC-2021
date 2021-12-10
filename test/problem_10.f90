program problem_10

    use aoc_utilities
    use iso_fortran_env, only: ip => int64

    implicit none

    integer :: iunit, i, j, k, n, n_lines
    character(len=:),allocatable :: line
    logical :: status_ok
    character(len=1),dimension(:),allocatable :: tmp
    character(len=:),allocatable :: tmp2
    logical :: valid, line_valid
    integer :: points
    integer(ip) :: total_score
    integer,dimension(1) :: iloc
    integer(ip),dimension(:),allocatable :: total_score_vec

    character(len=1),dimension(*),parameter :: opens  = ['(','[','{','<']
    character(len=1),dimension(*),parameter :: closes = [')',']','}','>']
    integer,dimension(*),parameter :: closes_points = [3,57,1197,25137]

    open(newunit=iunit,file='inputs/day10.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)

    points = 0
    allocate(tmp(0))
    allocate(total_score_vec(0))

    do i = 1, n_lines

        total_score = 0
        call read_line_from_file(iunit,line,status_ok)
        valid = .true.
        line_valid = .true.
        if (allocated(tmp)) deallocate(tmp)
        allocate(tmp(0))

        do k = 1, len(line)

            tmp = [tmp, line(k:k)]
            n = size(tmp)
            if (n==1) cycle

            if (any(tmp(n-1)==opens) .and. any(tmp(n)==closes)) then
                ! open/close pair. is it valid?
                valid = .false.
                do j = 1, size(opens)
                    if (tmp(n-1) == opens(j) .and. tmp(n) == closes(j)) then
                        valid = .true.
                        exit
                    end if
                end do
                if (.not. valid) then
                    line_valid = .false.
                    iloc = findloc(closes,tmp(n))
                    points = points + closes_points(iloc(1))
                end if
                if (size(tmp)==2) then
                    deallocate(tmp)
                    allocate(tmp(0))
                else
                    tmp = tmp(1:n-2) ! remove the last two
                end if
            end if

        end do

        if (line_valid .and. size(tmp)>0) then
            !write(*,*) 'incomplete line: ', line
            ! completion string:
            tmp2 = ' '
            do j = size(tmp),1,-1
                iloc = findloc(opens,tmp(j))
                tmp2 = tmp2//closes(iloc(1))
                total_score = total_score * 5 + iloc(1)
            end do
            !write(*,*) tmp2//' total_score = ', total_score
            total_score_vec = [total_score_vec, total_score]

        end if

    end do

    write(*,*) '10a: points = ', points

    call sort_ascending_64(total_score_vec)
    write(*,*) '10b: middle score = ', total_score_vec(size(total_score_vec)/2+1)

end program problem_10