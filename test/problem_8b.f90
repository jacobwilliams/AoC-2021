program problem_8b

    use aoc_utilities

    implicit none

    integer :: iunit, i, j, k, n, n_lines
    character(len=:),allocatable :: line, readings_str
    logical :: status_ok
    type(string),dimension(:),allocatable :: vals
    type(string),dimension(:),allocatable :: vals_readings
    type(string),dimension(:),allocatable :: vals_outputs
    integer,dimension(4) :: output_count
    integer :: total

    logical,dimension(7, 7) :: matrix

    ! 0 - 9 correct string values (sorted):
    character(len=*),dimension(10),parameter :: numbers = ['abcefg ', &
                                                           'cf     ', &
                                                           'acdeg  ', &
                                                           'acdfg  ', &
                                                           'bcdf   ', &
                                                           'abdfg  ', &
                                                           'abdefg ', &
                                                           'acf    ', &
                                                           'abcdefg', &
                                                           'abcdfg ' ]
    character(len=*),dimension(7),parameter :: abcdefg=['a','b','c','d','e','f','g'] ! full set
    character(len=:),allocatable :: full_str
    character(len=1),dimension(:),allocatable :: full_str_vec
    character(len=1) :: ch
    integer,dimension(7) :: icounts
    character(len=1) :: a,b,c,d,e,f,g
    integer,dimension(1) :: iloc
    character(len=:),allocatable :: converted_string
    integer :: output_num, output_sum

    !----------------------------
    ! 1:   C  F
    ! 7: A C  F
    ! 4:  BCD F
    ! 8: ABCDEFG
    ! 2: A CDE G
    ! 3: A CD FG
    ! 5: AB D FG
    ! 6: AB DEFG
    ! 0: ABC EFG
    ! 9: ABCD FG
    !----------------------------

    ! frequency analysis:

    ! first get the counts of the string: '0123456789':
    full_str = ''
    do i = 1, 10
        full_str = full_str//trim(numbers(i))
    end do
    write(*,*) len(full_str)
    allocate(full_str_vec(len(full_str)))
    do i = 1, len(full_str)
        full_str_vec(i) = full_str(i:i)
    end do
    ! histogram
    write(*,*) ''
    write(*,*) 'histogram:'
    do i = 1, 7
        ch = achar(i+iachar('a')-1)
        write(*,*) ch, ':', count(full_str_vec==ch)
    end do
    write(*,*) ''

    !----------------------------
    ! algorithm to get them (after generating the above histogram):

    ! letter    char count in full list    how to get it
    ! e:           4                       <---- known, just count chars and we have it
    ! b:           6                       <---- known, just count chars and we have it
    ! f:           9                       <---- known, just count chars and we have it
    ! c:           8                       <---- from 1: the non f character in the only one with 2 letters
    ! a:           8                       <---- the other one with 8 character count
    ! d:           7                       <---- from 4: the non (b,c,f) character in the one with 4 letters
    ! g:           7                       <---- the other one with 7 character count

    a=' '
    b=' '
    c=' '
    d=' '
    e=' '
    f=' '
    g=' '
    output_sum = 0

    open(newunit=iunit,file='inputs/day8.txt')

    n_lines = number_of_lines_in_file(iunit)

    total = 0
    do i = 1, n_lines
        call read_line_from_file(iunit,line,status_ok)
        call split(trim(line),'|',vals)
        call split(trim(adjustl(vals(1)%str)),' ',vals_readings)
        readings_str = ''
        do j = 1, size(vals_readings)
            readings_str = readings_str//trim(vals_readings(j)%str)
        end do
        icounts = compute_counts(readings_str) ! counts for a-g

        ! these we get just with the frequency analysis:
        iloc = findloc(icounts,4); e = abcdefg(iloc(1))
        iloc = findloc(icounts,6); b = abcdefg(iloc(1))
        iloc = findloc(icounts,9); f = abcdefg(iloc(1))

        ! c: 8 <---- from 1: the non f character in the only one with 2 letters
        do j = 1, size(vals_readings)
            if (len(vals_readings(j)%str) == 2) then
                if (vals_readings(j)%str(1:1)==f) then
                    c = vals_readings(j)%str(2:2)
                else
                    c = vals_readings(j)%str(1:1)
                end if
            end if
        end do

        ! a: 8 <---- the other one with 8 character count (that isn't c)
        do j = 1, size(icounts)
            if (icounts(j)==8) then
                if (abcdefg(j)/=c) then
                    a = abcdefg(j)
                    exit
                end if
            end if
        end do

        ! d: 7 <---- from 4: the non (b,c,f) character in the one with 4 letters
        dloop : do j = 1, size(vals_readings)
            if (len(vals_readings(j)%str) == 4) then ! has 4 letters
                do k = 1, 4
                    if (vals_readings(j)%str(k:k)==b .or. &
                        vals_readings(j)%str(k:k)==c .or. &
                        vals_readings(j)%str(k:k)==f ) then
                        cycle
                    else
                        d = vals_readings(j)%str(k:k)
                        exit dloop
                    end if
                end do
            end if
        end do dloop

        ! g: 7 <---- the other one with 7 character count (that's isn't d)
        do j = 1, size(icounts)
            if (icounts(j)==7) then
                if (abcdefg(j)/=d) then
                    g = abcdefg(j)
                    exit
                end if
            end if
        end do

        ! now, from the outputs, compute the number:
        call split(trim(adjustl(vals(2)%str)),' ',vals_outputs)

        output_num = 0
        k = 0
        do j = 4,1,-1
            k = k + 1
            converted_string = convert(vals_outputs(j)%str)
            !write(*,*) vals_outputs(j)%str, ' -> ', converted_string, ' -> ', to_number(converted_string)
            output_num = output_num + to_number(converted_string) * 10**(k-1)
        end do

        output_sum = output_sum + output_num

    end do

    write(*,*) '8b: ', output_sum

    contains

    function compute_counts(str) result(icounts)
    implicit none
    character(len=*),intent(in) :: str ! concatenated string of all readings
    integer,dimension(7) :: icounts    ! counts of a-g

    character(len=1),dimension(:),allocatable :: vec
    integer :: i
    character(len=1) :: ch

    allocate(vec(len_trim(str)))
    do i = 1, len(str)
        vec(i) = str(i:i) ! a temp array just to use count
    end do
    do i = 1, 7
        ch = achar(i+iachar('a')-1)
        icounts(i) = count(vec==ch)
    end do

    end function compute_counts

    function to_number(str) result(inum)
        implicit none
        character(len=*),intent(in) :: str
        integer :: inum
        integer :: i,j,k
        logical :: match

        main: do i = 1, size(numbers)
            if (len_trim(str)/=len_trim(numbers(i))) cycle  ! not a match
            ! output can be in any order so we sort it
            ! numbers is already sorted
            inum = i-1
            match = .true.
            do j = 1, len(str)
                if (index(numbers(i), str(j:j)) <= 0 ) then
                    match = .false.
                    cycle main
                end if
            end do
            if (match) return
        end do main
        error stop 'error: string not found: '//str

    end function to_number

    function convert(str) result(str2)
        implicit none
        character(len=*),intent(in) :: str
        character(len=len(str)) :: str2
        integer :: i
        integer,dimension(1) :: iloc
        character(len=1),dimension(7) :: key
        key = [a,b,c,d,e,f,g]
        do i = 1, len(str)
            iloc = findloc(key, str(i:i))
            str2(i:i) = abcdefg(iloc(1))
        end do

    end function convert

end program problem_8b
