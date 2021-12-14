program problem_13

    use aoc_utilities

    implicit none

    logical :: status_ok, read_grid
    integer :: iunit, istep, i, j, k, n_lines, n_nodes, x, y, maxy, maxx
    character(len=:),allocatable :: line
    integer,dimension(:),allocatable :: xvec, yvec, foldxy
    integer,dimension(:,:),allocatable :: grid
    type(string),dimension(:),allocatable :: vals
    character(len=1),dimension(:),allocatable :: fold
    character(len=1) :: f

    open(newunit=iunit,file='inputs/day13.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)

    read_grid = .true.
    allocate(xvec(0)); allocate(yvec(0))
    allocate(foldxy(0))
    do i = 1, n_lines
        call read_line_from_file(iunit,line,status_ok)
        if (line=='') then
            read_grid = .false.
            cycle
        end if
        if (read_grid) then
            ! read grid points
            call split(line,',',vals)
            x = vals(1)%to_int()
            y = vals(2)%to_int()
            xvec = [xvec, x]
            yvec = [yvec, y]
        else
            ! read folds
            call split(line(12:),'=',vals)
            f = vals(1)%str
            if (allocated(fold)) then ! for some reason we have to do this with gfortran
                fold = [fold, f]
            else
                fold = [f]
            end if
            foldxy = [foldxy, vals(2)%to_int()]
        end if
    end do

    allocate( grid(0:maxval(yvec), 0:maxval(xvec)) )
    grid = 0
    do i = 1, size(xvec)
        grid( yvec(i), xvec(i) ) = 1
    end do
    maxy = ubound(grid,1) ! initialize : will be updated for each fold
    maxx = ubound(grid,2)

    !   2
    ! 00|000  -> x
    ! 11|111
    ! 22|221
    !y

    ! or :

    !    3
    ! 000|00  -> x
    ! 111|11
    ! 222|21
    !y

    do i = 1, size(fold)
        !write(*,*) 'fold along ', fold(i), foldxy(i)
        select case (fold(i))
        case('x')
            do x = 1, min(foldxy(i), maxx-foldxy(i))
                do y = 0, maxy
                    if (grid(y,foldxy(i)+x)==1 .or. grid(y, foldxy(i)-x)==1) grid(y,foldxy(i)-x) = 1
                end do
            end do
            maxx = foldxy(i) - 1
        case('y')
            do y = 1, min(foldxy(i), maxy-foldxy(i))
                do x = 0, maxx
                    if (grid(foldxy(i)+y,x)==1 .or. grid(foldxy(i)-y, x)==1) grid(foldxy(i)-y,x) = 1
                end do
            end do
            maxy = foldxy(i) - 1
        end select
        if (i==1) write(*,*) '13a: number visible = ', count(grid(0:maxy,0:maxx)==1)
    end do

    write(*,*) '13b: '
    do k = 0, maxy
        do j = 0, maxx
            if (grid(k,j)==0) then
                write(*,'(A1)',advance='NO') ' '
            else
                write(*,'(A1)',advance='NO') '#'
            end if
        end do
        write(*,*) ''
    end do

end program problem_13
