program problem_15

    use aoc_utilities

    !! see: https://en.wikipedia.org/wiki/Dijkstra's_algorithm#Pseudocode

    implicit none

    type :: pair
        integer,dimension(2) :: ij = -1
    end type pair

    logical :: status_ok
    integer :: iunit, i, j, k, n_rows, n_cols
    character(len=:),allocatable :: line
    integer,dimension(:,:),allocatable :: map
    logical,dimension(:,:),allocatable :: visited
    integer,dimension(:,:),allocatable :: dist
    type(pair),dimension(:,:),allocatable :: prev
    integer :: min_risk
    integer,dimension(2) :: iloc

    open(newunit=iunit,file='inputs/day15.txt', status='OLD')
    n_rows = number_of_lines_in_file(iunit)
    do i = 1, n_rows
        call read_line_from_file(iunit,line,status_ok)
        if (i==1) then
            n_cols = len(line)
            allocate(map(n_rows,n_cols))
            allocate(visited(n_rows,n_cols))
            allocate(dist(n_rows,n_cols))
            allocate(prev(n_rows,n_cols))
        end if
        read(line,'(*(I1))') map(i,:)
    end do

    ! ! write:
    ! do i = 1, n_rows
    !     write(*,'(*(I1))') map(i,:)
    ! end do

    min_risk = huge(1)
    dist = huge(1)
    visited = .false.
    dist(1,1) = 0

    do

        iloc = minloc(dist, mask=.not. visited)
        i = iloc(1)
        j = iloc(2)
        write(*,*) 'min loc: ', i,j

        visited(i,j) = .true.

        if (i == n_rows .and. j == n_cols) exit ! we are done

        call check([i,j], [i+1,j])
        call check([i,j], [i,j+1])
        call check([i,j], [i-1,j])
        call check([i,j], [i,j-1])

    end do

    write(*,*) '15a: dist = ', dist(n_rows, n_cols)

    contains

    subroutine check(u, v)
        implicit none
        integer,dimension(2),intent(in) :: u ! current
        integer,dimension(2),intent(in) :: v ! neighbor
        integer :: alt

        if (v(1)<1 .or. v(2)<1 .or. v(1)>n_rows .or. v(2)>n_cols) return
        if (visited(v(1),v(2))) return ! already visited this one

        alt = dist(u(1),u(2)) + map(v(1),v(2))

        if (alt < dist(v(1),v(2))) then
            dist(v(1),v(2)) = alt
            prev(v(1),v(2)) = pair(u)
        end if

    end subroutine check

end program problem_15