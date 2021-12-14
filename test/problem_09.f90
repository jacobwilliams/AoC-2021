program problem_9

    use aoc_utilities
    use iso_fortran_env, only: ip => int64

    implicit none

    integer :: iunit, i, j, k, n, n_lines
    character(len=:),allocatable :: line
    logical :: status_ok
    integer :: risk
    integer,dimension(:,:),allocatable :: ivec
    logical,dimension(:,:),allocatable :: mask
    integer,dimension(:),allocatable :: low_i
    integer,dimension(:),allocatable :: low_j
    integer(ip),dimension(:),allocatable :: basin_size
    integer,dimension(3) :: largest
    integer,dimension(1) :: iloc

    open(newunit=iunit,file='inputs/day9.txt')
    n_lines = number_of_lines_in_file(iunit)

    ! create the array of integers:
    do i = 1, n_lines
        call read_line_from_file(iunit,line,status_ok)
        if (.not. allocated(ivec)) then
            allocate(ivec(0:n_lines+1,0:len(line)+1))
            allocate(mask(0:n_lines+1,0:len(line)+1))
            ivec = huge(1) ! trick for the edges
        end if
        do j = 1, len(line)
            read(line(j:j),'(I1)') ivec(i,j)
        end do
    end do

    risk = 0
    allocate(low_i(0)); allocate(low_j(0))
    do i = 1 , ubound(ivec,1)-1
        do j = 1, ubound(ivec,2)-1
            if (all(ivec(i,j)<[ ivec(i-1,j),&
                                ivec(i+1,j),&
                                ivec(i,j-1),&
                                ivec(i,j+1) ])) then
                risk = risk + ivec(i,j) + 1
                low_i = [low_i, i]; low_j = [low_j, j]
                !write(*,*) 'low point: ', i, j, ivec(i,j)
            end if
        end do
    end do

    write(*,*) '9a : risk level = ', risk

    !***********************************************************************
    allocate(basin_size(size(low_i)))
    basin_size = 0
    do i = 1, size(low_i)
        mask = .false. ! initialize to false, will be true if part of the basin
        call count_basin_size(low_i(i), low_j(i), mask)
        basin_size(i) = count(mask)
    end do
    !write(*,*) 'basin sizes: ', basin_size

    ! find the 3 largest:
    do j = 1, 3
        iloc = maxloc(basin_size)
        largest(j) = basin_size(iloc(1))
        basin_size(iloc(1)) = 0
    end do
    !write(*,*) 'largest: ', largest

    write(*,*) '9b : product of 3 largest basin sizes = ', product(largest)

    contains

    recursive subroutine count_basin_size(i,j,mask)
    implicit none
    integer,intent(in) :: i,j
    logical,dimension(0:,0:),intent(inout) :: mask

    if (ivec(i,j)<9) then
        mask(i,j) = .true.
        if (.not. mask(i+1,j)) call count_basin_size(i+1,j,mask)
        if (.not. mask(i-1,j)) call count_basin_size(i-1,j,mask)
        if (.not. mask(i,j+1)) call count_basin_size(i,j+1,mask)
        if (.not. mask(i,j-1)) call count_basin_size(i,j-1,mask)
    end if

    end subroutine count_basin_size

    end program problem_9
