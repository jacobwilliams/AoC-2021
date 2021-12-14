program problem_14

    use aoc_utilities

    implicit none

    logical :: status_ok
    integer :: iunit, i, j, k, n_lines, n, istep, n_pairs, icase
    character(len=:),allocatable :: line

    character(len=:),allocatable :: pattern
    type(string),dimension(:),allocatable :: vals
    character(len=2),dimension(:),allocatable :: pairs
    character(len=1),dimension(:),allocatable :: chars_to_insert
    character(len=1),dimension(:),allocatable :: pattern_vec
    integer :: n_most_common, n_least_common
    integer,dimension(26) :: counts
    ! integer,dimension(*),parameter :: cases = [10, 40]
    ! character(len=1),dimension(*),parameter :: ab = ['a', 'b']
    integer,dimension(*),parameter :: cases = [10]
    character(len=1),dimension(*),parameter :: ab = ['a']

    type :: pattern_pair
        character(len=2) :: pair
        character(len=:),allocatable :: inserted_chars
    end type pattern_pair
    type(pattern_pair),dimension(:),allocatable :: pattern_pair_array

    do icase = 1, size(cases)

        open(newunit=iunit,file='inputs/day14.txt', status='OLD')
        n_lines = number_of_lines_in_file(iunit)

        call read_line_from_file(iunit,pattern,status_ok)
        call read_line_from_file(iunit,line,status_ok)
        if (allocated(pairs)) deallocate(pairs); allocate(pairs(n_lines))
        if (allocated(chars_to_insert)) deallocate(chars_to_insert); allocate(chars_to_insert(n_lines))
        do i = 3, n_lines
            call read_line_from_file(iunit,line,status_ok)
            call split(line,' -> ',vals)
            pairs(i-2) = vals(1)%str
            chars_to_insert(i-2) = vals(2)%str
        end do
       ! write(*,*) 'initial: ', pattern

        do istep = 1, cases(icase)
            !write(*,*) 'case, step: ', icase, istep
            call pattern_to_array(pattern,pattern_pair_array)
            n_pairs = size(pattern_pair_array)
            do i = 1, n_lines
                do j = 1, n_pairs
                    if (pattern_pair_array(j)%pair == pairs(i)) then
                        pattern_pair_array(j)%inserted_chars = pattern_pair_array(j)%inserted_chars//chars_to_insert(i)
                    end if
                end do
            end do
            call array_to_pattern(pattern_pair_array,pattern)
            !write(*,*) istep, pattern
        end do

        !  What do you get if you take the quantity of the most
        !  common element and subtract the quantity of the least common element?
        if (allocated(pattern_vec)) deallocate(pattern_vec); allocate(pattern_vec(len(pattern)))
        do i = 1, len(pattern)
            pattern_vec(i) = pattern(i:i)
        end do
        counts = [(count(pattern_vec==achar(iachar('A')+i-1)), i = 1, 26)]
        write(*,*) '14'//ab(icase)//': max-min  = ', maxval( counts, mask = counts /= 0 ) - minval( counts, mask = counts /= 0 )

    end do

    contains

    subroutine pattern_to_array(p,array)
    character(len=*),intent(in) :: p
    type(pattern_pair),dimension(:),allocatable,intent(out) :: array

    integer :: i, n_pairs

    n_pairs = len(p) - 1
    allocate(array(n_pairs))
    do i = 1, n_pairs
        array(i)%pair = p(i:i+1)
        array(i)%inserted_chars = ''
    end do

    end subroutine pattern_to_array

    subroutine array_to_pattern(array,p)
    type(pattern_pair),dimension(:),intent(in) :: array
    character(len=:),allocatable,intent(out) :: p

    integer :: i, n_pairs

    n_pairs = size(array)
    p = ''
    do i = 1, n_pairs
        if (i==1) p = p // array(i)%pair(1:1)
        p = p // array(i)%inserted_chars//array(i)%pair(2:2)
    end do

    end subroutine array_to_pattern

end program problem_14