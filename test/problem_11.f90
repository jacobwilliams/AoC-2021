program problem_11

    use aoc_utilities
    use iso_fortran_env, only: ip => int64

    implicit none

    integer,parameter :: n = 10
    integer,parameter :: n_steps = 100

    integer :: iunit, istep, i, j, k
    character(len=:),allocatable :: line
    logical :: status_ok
    integer,dimension(n,n) :: grid
    logical,dimension(n,n) :: mask
    integer :: n_flashes
    integer :: all_flash_step

    open(newunit=iunit,file='inputs/day11.txt', status='OLD')

    ! read in the grid:
    do i = 1, n
        call read_line_from_file(iunit,line,status_ok)
        !write(*,*) line
        read(line,'(*(I1))') grid(i,:)
    end do

    n_flashes = 0
    all_flash_step = -1
    istep = 0
    do

        istep = istep + 1
        grid = grid + 1
        mask = .false.

        if (any(grid>9)) then ! process flashes
            do j = 1, n
                do k = 1, n
                    ! if it flashed and hasn't already been accounted for:
                    call check_flash(j,k,mask,n_flashes)
                end do
            end do
            where (grid>9) grid = 0 ! reset flashed ones
        end if

        ! write(*,*) 'step ', istep
        ! do j = 1, n
        !     write(*,'(*(I1))') grid(j,:)
        ! end do

        if (all(grid==0)) then
            all_flash_step = istep
            exit
        end if

    end do

    write(*,*) '11a: n_flashes      = ', n_flashes
    write(*,*) '11b: all_flash_step = ', all_flash_step

    contains

        recursive subroutine check_flash(i,j,mask,n_flashes)
        implicit none
        integer,intent(in) :: i,j
        logical,dimension(n,n),intent(inout) :: mask
        integer,intent(inout) :: n_flashes

        integer :: x,y

        if (i<1 .or. i>n .or. j<1 .or. j>n) return ! off the edge

        ! if it has flashed and hasn't already been accounted for:
        if (grid(i,j)>9 .and. .not. mask(i,j)) then
            mask(i,j) = .true. ! can only flash once per step

            ! only counting flashes for part A
            if (istep<=n_steps) then
                n_flashes = n_flashes + 1
            end if

            ! ooo
            ! o*o
            ! ooo
            ! update surrounding ones:
            do x = -1, 1
                do y = -1, 1
                    associate (ii => i+x, jj => j+y)
                        if ((x==0 .and. y==0) .or. ii<1 .or. ii>n .or. jj<1 .or. jj>n) cycle
                        grid(ii,jj) = grid(ii,jj) + 1
                        call check_flash(ii,jj,mask,n_flashes)
                    end associate
                end do
            end do

        end if

        end subroutine check_flash

end program problem_11