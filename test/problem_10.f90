program problem_10

    use aoc_utilities
    !use iso_fortran_env, only: ip => int64

    implicit none

    integer :: iunit, i, j, k, n, n_lines
    character(len=:),allocatable :: line
    logical :: status_ok
    character(len=1),dimension(:),allocatable :: tmp
    logical :: valid
    integer :: points
    integer,dimension(1) :: iloc

    character(len=1),dimension(*),parameter :: opens  = ['(','[','{','<']
    character(len=1),dimension(*),parameter :: closes = [')',']','}','>']
    integer,dimension(*),parameter :: closes_points = [3,57,1197,25137]

    open(newunit=iunit,file='inputs/day10.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)

    points = 0
    allocate(tmp(0))

    do i = 1, n_lines

        call read_line_from_file(iunit,line,status_ok)
      !  valid = .true.
        ! if (allocated(tmp)) deallocate(tmp)
        ! allocate(tmp(0))

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

        ! if (valid .and. size(tmp)>0) then
        !     write(*,*) 'incomplete line: ', tmp,' ->', line
        ! end if

    end do

    write(*,*) '10a: points = ', points





end program problem_10