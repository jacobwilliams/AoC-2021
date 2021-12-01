module aoc_utilities

    use iso_fortran_env

    implicit none

    private

    type,public :: string
        character(len=:),allocatable :: str
    end type string

    public :: read_file_to_integer_array, read_file_to_integer64_array
    public :: number_of_lines_in_file
    public :: sort_ascending
    public :: split
    public :: read_line_from_file
    public :: unique

contains

!****************************************************************
    function read_file_to_integer_array(filename) result(iarray)

    character(len=*),intent(in) :: filename
    integer,dimension(:),allocatable :: iarray

    integer :: i, iunit, n_lines, istat

    open(newunit=iunit, file=filename, iostat=istat)
    if (istat /= 0) error stop ' error reading file'

    n_lines = number_of_lines_in_file(iunit)
    allocate(iarray(n_lines))
    do i = 1, n_lines
        read(iunit, '(I10)') iarray(i)
    end do

    close(iunit)

    end function read_file_to_integer_array
!****************************************************************

!****************************************************************
    function read_file_to_integer64_array(filename) result(iarray)

    character(len=*),intent(in) :: filename
    integer(int64),dimension(:),allocatable :: iarray

    integer :: i, iunit, n_lines, istat

    open(newunit=iunit, file=filename, iostat=istat)
    if (istat /= 0) error stop ' error reading file'

    n_lines = number_of_lines_in_file(iunit)
    allocate(iarray(n_lines))
    do i = 1, n_lines
        read(iunit, *) iarray(i)
    end do

    close(iunit)

    end function read_file_to_integer64_array
!****************************************************************

!****************************************************************
    function number_of_lines_in_file(iunit) result(n_lines)

    implicit none

    integer,intent(in)  :: iunit   !! the file unit number
                                    !! (assumed to be open)
    integer :: n_lines   !! the number of lines in the file

    character(len=1) :: tmp
    integer :: istat

    rewind(iunit)
    n_lines = 0
    do
        read(iunit,fmt='(A1)',iostat=istat) tmp
        if (is_iostat_end(istat)) exit
        n_lines = n_lines + 1
    end do
    rewind(iunit)

    end function number_of_lines_in_file
!****************************************************************

!*******************************************************************************
!>
!  Sorts an integer array `ivec` in increasing order.
!  Uses a basic recursive quicksort
!  (with insertion sort for partitions with \(\le\) 20 elements).

    subroutine sort_ascending(ivec)

    implicit none

    integer,dimension(:),intent(inout) :: ivec

    integer,parameter :: max_size_for_insertion_sort = 20 !! max size for using insertion sort.

    call quicksort(1,size(ivec))

    contains

        recursive subroutine quicksort(ilow,ihigh)

        !! Sort the array

        implicit none

        integer,intent(in) :: ilow
        integer,intent(in) :: ihigh

        integer :: ipivot !! pivot element
        integer :: i      !! counter
        integer :: j      !! counter

        if ( ihigh-ilow<=max_size_for_insertion_sort .and. ihigh>ilow ) then

            ! do insertion sort:
            do i = ilow + 1,ihigh
                do j = i,ilow + 1,-1
                    if ( ivec(j) < ivec(j-1) ) then
                        call swap(ivec(j),ivec(j-1))
                    else
                        exit
                    end if
                end do
            end do

        elseif ( ihigh-ilow>max_size_for_insertion_sort ) then

            ! do the normal quicksort:
            call partition(ilow,ihigh,ipivot)
            call quicksort(ilow,ipivot - 1)
            call quicksort(ipivot + 1,ihigh)

        end if

        end subroutine quicksort

        subroutine partition(ilow,ihigh,ipivot)

        !! Partition the array, based on the
        !! lexical ivecing comparison.

        implicit none

        integer,intent(in)  :: ilow
        integer,intent(in)  :: ihigh
        integer,intent(out) :: ipivot

        integer :: i,ip

        call swap(ivec(ilow),ivec((ilow+ihigh)/2))
        ip = ilow
        do i = ilow + 1, ihigh
            if ( ivec(i) < ivec(ilow) ) then
                ip = ip + 1
                call swap(ivec(ip),ivec(i))
            end if
        end do
        call swap(ivec(ilow),ivec(ip))
        ipivot = ip

        end subroutine partition

    end subroutine sort_ascending
!*******************************************************************************

!*******************************************************************************
!>
!  Swap two integer values.

    pure elemental subroutine swap(i1,i2)

    implicit none

    integer,intent(inout) :: i1
    integer,intent(inout) :: i2

    integer :: tmp

    tmp = i1
    i1  = i2
    i2  = tmp

    end subroutine swap
!*******************************************************************************

!*****************************************************************************************
!>
!  Split a character string using a token.
!  This routine is inspired by the Python split function.
!
!### Example
!````Fortran
!   character(len=:),allocatable :: s
!   type(string),dimension(:),allocatable :: vals
!   s = '1,2,3,4,5'
!   call split(s,',',vals)
!````

    pure subroutine split(str,token,chunk_size,vals)

    implicit none

    character(len=*),intent(in)  :: str
    character(len=*),intent(in)  :: token
    integer,intent(in)           :: chunk_size  !! for expanding vectors
    type(string),dimension(:),allocatable,intent(out) :: vals

    integer :: i          !! counter
    integer :: len_str    !! significant length of `str`
    integer :: len_token  !! length of the token
    integer :: n_tokens   !! number of tokens
    integer :: i1         !! index
    integer :: i2         !! index
    integer :: j          !! counters
    integer,dimension(:),allocatable :: itokens !! start indices of the
                                                !! token locations in `str`

    len_token = len(token)  ! length of the token
    n_tokens  = 0           ! initialize the token counter
    j         = 0           ! index to start looking for the next token

    ! first, count the number of times the token
    ! appears in the string, and get the token indices.
    !
    ! Examples:
    !  ',         '    --> 1
    !  '1234,67,90'    --> 5,8
    !  '123,      '    --> 4

    ! length of the string
    if (token == ' ') then
        ! in this case, we can't ignore trailing space
        len_str = len(str)
    else
        ! safe to ignore trailing space when looking for tokens
        len_str = len_trim(str)
    end if

    j = 1
    n_tokens = 0
    do
        if (j>len_str) exit      ! end of string, finished
        i = index(str(j:),token) ! index of next token in remaining string
        if (i<=0) exit           ! no more tokens found
        call expand_vector(itokens,n_tokens,chunk_size,i+j-1)  ! save the token location
        j = j + i + (len_token - 1)
    end do
    call expand_vector(itokens,n_tokens,chunk_size,finished=.true.)  ! resize the vector

    allocate(vals(n_tokens+1))

    if (n_tokens>0) then

        len_str = len(str)

        i1 = 1
        i2 = itokens(1)-1
        if (i2>=i1) then
            vals(1)%str = str(i1:i2)
        else
            vals(1)%str = ''  !the first character is a token
        end if

        !      1 2 3
        !    'a,b,c,d'

        do i=2,n_tokens
            i1 = itokens(i-1)+len_token
            i2 = itokens(i)-1
            if (i2>=i1) then
                vals(i)%str = str(i1:i2)
            else
                vals(i)%str = ''  !empty element (e.g., 'abc,,def')
            end if
        end do

        i1 = itokens(n_tokens) + len_token
        i2 = len_str
        if (itokens(n_tokens)+len_token<=len_str) then
            vals(n_tokens+1)%str = str(i1:i2)
        else
            vals(n_tokens+1)%str = ''  !the last character was a token
        end if

    else
        !no tokens present, so just return the original string:
        vals(1)%str = str
    end if

    end subroutine split
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add elements to the integer vector in chunks.

    pure subroutine expand_vector(vec,n,chunk_size,val,finished)

    implicit none

    integer,dimension(:),allocatable,intent(inout) :: vec
    integer,intent(inout)       :: n           !! counter for last element added to `vec`.
                                               !! must be initialized to `size(vec)`
                                               !! (or 0 if not allocated) before first call
    integer,intent(in)          :: chunk_size  !! allocate `vec` in blocks of this size (>0)
    integer,intent(in),optional :: val         !! the value to add to `vec`
    logical,intent(in),optional :: finished    !! set to true to return `vec`
                                               !! as its correct size (`n`)

    integer,dimension(:),allocatable :: tmp  !! temporary array

    if (present(val)) then
        if (allocated(vec)) then
            if (n==size(vec)) then
                ! have to add another chunk:
                allocate(tmp(size(vec)+chunk_size))
                tmp(1:size(vec)) = vec
                call move_alloc(tmp,vec)
            end if
            n = n + 1
        else
            ! the first element:
            allocate(vec(chunk_size))
            n = 1
        end if
        vec(n) = val
    end if

    if (present(finished)) then
        if (finished) then
            ! set vec to actual size (n):
            if (allocated(tmp)) deallocate(tmp)
            allocate(tmp(n))
            tmp = vec(1:n)
            call move_alloc(tmp,vec)
        end if
    end if

    end subroutine expand_vector
!*******************************************************************************

!*****************************************************************************************
!>
!  Reads the next line from a file.

    subroutine read_line_from_file(iunit,chunk_size,line,status_ok)

    implicit none

    integer,intent(in) :: iunit
    integer,intent(in) :: chunk_size
    character(len=:),allocatable,intent(out) :: line
    logical,intent(out) :: status_ok !! true if no problems

    integer :: nread  !! character count specifier for read statement
    integer :: istat  !! file read io status flag
    character(len=chunk_size) :: buffer !! the file read buffer

    nread  = 0
    buffer = ''
    line   = ''
    status_ok = .true.

    do
        ! read in the next block of text from the line:
        read(iunit,fmt='(A)',advance='NO',size=nread,iostat=istat) buffer
        if (IS_IOSTAT_END(istat) .or. IS_IOSTAT_EOR(istat)) then
            ! add the last block of text before the end of record
            if (nread>0) line = line//buffer(1:nread)
            exit
        else if (istat==0) then ! all the characters were read
            line = line//buffer  ! add this block of text to the string
        else  ! some kind of error
            error stop 'Read error'
            status_ok = .false.
            exit
        end if
    end do

    end subroutine read_line_from_file
!*****************************************************************************************

!*****************************************************************************************
function unique(vec) result(vec_unique)
! Return only the unique values from vec.

implicit none

integer,dimension(:),intent(in) :: vec
integer,dimension(:),allocatable :: vec_unique

integer :: i,num
logical,dimension(size(vec)) :: mask

mask = .false.

do i=1,size(vec)

    !count the number of occurrences of this element:
    num = count( vec(i)==vec )

    if (num==1) then
        !there is only one, flag it:
        mask(i) = .true.
    else
        !flag this value only if it hasn't already been flagged:
        if (.not. any(vec(i)==vec .and. mask) ) mask(i) = .true.
    end if

end do

!return only flagged elements:
allocate( vec_unique(count(mask)) )
vec_unique = pack( vec, mask )

!if you also need it sorted, then do so.
! For example, with slatec routine:
!call ISORT (vec_unique, [0], size(vec_unique), 1)

end function unique
!*****************************************************************************************

end module aoc_utilities