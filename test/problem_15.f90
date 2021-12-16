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
    integer,dimension(2) :: iloc
    integer :: icase, fact

    integer,parameter :: n_cases = 2
    character(len=1),dimension(2),parameter :: ab = ['a', 'b']

    do icase = 1, n_cases

        if (icase==1) then
            fact = 1
        else
            fact = 5
        end if

        open(newunit=iunit,file='inputs/day15.txt', status='OLD')
        n_rows = number_of_lines_in_file(iunit)
        do i = 1, n_rows
            call read_line_from_file(iunit,line,status_ok)
            if (i==1) then
                n_cols = len(line)
                if (allocated(map)) deallocate(map); allocate(map(n_rows*fact,n_cols*fact))
                if (allocated(visited)) deallocate(visited); allocate(visited(n_rows*fact,n_cols*fact))
                if (allocated(dist)) deallocate(dist); allocate(dist(n_rows*fact,n_cols*fact))
                if (allocated(prev)) deallocate(prev); allocate(prev(n_rows*fact,n_cols*fact))
            end if
            read(line,'(*(I1))') map(i,1:n_cols)
        end do
        if (icase==2) then ! expand the map

            do i = 2, fact
                map(1:n_rows, n_cols*(i-1)+1:n_cols*i) = mod(map(1:n_rows, n_cols*(i-2)+1:n_cols*(i-1)) + 1, 10)
                where (map==0) map = 1 ! not efficient
            end do
            do i = 2, fact
                map(n_rows*(i-1)+1:n_rows*i, 1:n_cols*fact) = mod(map(n_rows*(i-2)+1:n_rows*(i-1), 1:n_cols*fact) + 1, 10)
                where (map==0) map = 1 ! not efficient
            end do

            n_rows = n_rows * fact
            n_cols = n_cols * fact

        end if
        ! ! write:
        ! do i = 1, n_rows
        !     write(*,'(*(I1))') map(i,:)
        ! end do

        dist = huge(1)
        visited = .false.
        dist(1,1) = 0

        do

            iloc = minloc(dist, mask=.not. visited)
            i = iloc(1)
            j = iloc(2)
            visited(i,j) = .true.

            if (i == n_rows .and. j == n_cols) exit ! we are done

            call check([i,j], [i+1,j])
            call check([i,j], [i,j+1])
            call check([i,j], [i-1,j])
            call check([i,j], [i,j-1])

        end do

        write(*,*) '15'//ab(icase)//': dist = ', dist(n_rows, n_cols)

    end do

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