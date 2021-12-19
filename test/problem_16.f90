module problem_16

    use aoc_utilities
    use iso_fortran_env, only: ip =>int64

    implicit none

    type :: packet_type
        integer(ip) :: version = -1
        integer(ip) :: type_id = -1
        integer(ip) :: length_type_id = -1
        integer(ip) :: value = -1
        type(packet_type),dimension(:),allocatable :: subpackets
        contains
        procedure :: parse => parse_packet
        procedure :: evaluate => evaluate_packet
    end type packet_type

    integer(ip) :: version_sum

    contains

    recursive elemental function evaluate_packet(me) result(val)
        !! part b
        implicit none
        class(packet_type),intent(in) :: me
        integer(ip) :: val

        select case (me%type_id)
        case(4); val = me%value
        case(0); val = sum(me%subpackets%evaluate())
        case(1); val = product(me%subpackets%evaluate())
        case(2); val = minval(me%subpackets%evaluate())
        case(3); val = maxval(me%subpackets%evaluate())
        case(5); val = merge(1,0,(me%subpackets(1)%evaluate() > me%subpackets(2)%evaluate()))
        case(6); val = merge(1,0,(me%subpackets(1)%evaluate() < me%subpackets(2)%evaluate()))
        case(7); val = merge(1,0,(me%subpackets(1)%evaluate() == me%subpackets(2)%evaluate()))
        case default
            error stop 'oops: unknown packet id'
        end select

    end function evaluate_packet

    recursive subroutine parse_packet(me, i, bin)
        ! parse a packet starting at index i
        implicit none
        class(packet_type),intent(out) :: me
        integer(ip),intent(inout) :: i !! current index in bin
        integer(ip),dimension(:),intent(in) :: bin

        integer(ip),dimension(:),allocatable :: tmp
        integer(ip) :: num_subpackets, j, iend, ipacket, packet_len, istart
        type(packet_type) :: tmp_packet
        logical :: last_packet
        type(packet_type),dimension(:),allocatable :: tmp_subpackets

        istart = i

        me%version = binary_to_decimal(pop(bin, i, 3_ip))
        me%type_id = binary_to_decimal(pop(bin, i, 3_ip))

        version_sum = version_sum + me%version
        !write(*,*) 'version_sum = ', version_sum

        select case (me%type_id)

        case(4) ! literal value
            ! write(*,*) '=======literal value======= i = ', i
            ! write(*,*) '  packet version: ', me%version
            ! write(*,*) '  packet id:      ', me%type_id

            if (allocated(tmp)) deallocate(tmp)
            allocate(tmp(0))
            ! parse the value
            do
                last_packet = all(pop(bin, i, 1_ip)==0) ! first bit
                tmp = [tmp, pop(bin, i, 4_ip)]          ! next 4 bits add to number
                if (last_packet) exit
            end do

            me%value = binary_to_decimal(tmp)

        case default ! operator packet
            ! write(*,*) '=======operator packet======= i = ', i
            ! write(*,*) '  packet version: ', me%version
            ! write(*,*) '  packet id:      ', me%type_id

            tmp = pop(bin, i, 1_ip)
            me%length_type_id = binary_to_decimal(tmp)

            select case (me%length_type_id)
            case(0)

                ! next 15 bits are a number that represents
                ! the total length in bits of the sub-packets
                ! contained by this packet.

                packet_len = binary_to_decimal(pop(bin,i,15_ip))
                iend = i + packet_len  ! one after the end of the subpacket string
                ipacket = 1
                allocate(me%subpackets(1))
                do
                    call me%subpackets(ipacket)%parse(i, bin)
                    if (i==iend) exit
                    ipacket = ipacket + 1
                    tmp_packet = packet_type()

                    !me%subpackets = [me%subpackets, tmp_packet] ! ... this is crashing for the full problem! (compiler bug??)
                    if (allocated(tmp_subpackets)) deallocate(tmp_subpackets)  ! this one works with ifort 2021.1, but crashes with gfortran
                    allocate(tmp_subpackets(size(me%subpackets) + 1))
                    tmp_subpackets(1:size(me%subpackets)) = me%subpackets
                    tmp_subpackets(ipacket) = tmp_packet
                    deallocate(me%subpackets)
                    call move_alloc(tmp_subpackets, me%subpackets)
                end do

            case(1)

                ! the next 11 bits are the number of sub-packets
                ! immediately contained

                num_subpackets = binary_to_decimal(pop(bin,i,11_ip))

                allocate(me%subpackets(num_subpackets))
                do j = 1, num_subpackets
                    call me%subpackets(j)%parse(i, bin)
                end do

            case default
                error stop 'invalid length_type_id'
            end select

        end select

    end subroutine parse_packet

    function pop(bin, i, n) result(vec)
        ! pop n elements from the array, starting at index i.
        implicit none
        integer(ip),dimension(:),intent(in) :: bin
        integer(ip),intent(inout) :: i
        integer(ip),intent(in) :: n
        integer(ip),dimension(:),allocatable :: vec

        if (n>0 .and. i<=size(bin)) then
            vec = bin(i:i+n-1)
            i = i + n
        end if

    end function pop

    function binary_to_decimal(b) result(dec) ! from problem 3
        implicit none
        integer(ip),dimension(:),intent(in) :: b
        integer(ip) :: dec
        integer(ip) :: j,n
        n = size(b)
        dec = sum( [( 2**(j-1) * b(n-j+1), j = 1,n )] )
    end function binary_to_decimal

    function hex2bits(h) result(b)
        !! hex string to array of bits ... probably there is a better way to do this...
        character(len=*),intent(in) :: h
        integer(ip),dimension(:),allocatable :: b
        integer(ip) :: i

        allocate(b(0))

        do i = 1, len_trim(h)
            select case (h(i:i))
            case('0'); b = [b, int([0,0,0,0], ip) ]
            case('1'); b = [b, int([0,0,0,1], ip) ]
            case('2'); b = [b, int([0,0,1,0], ip) ]
            case('3'); b = [b, int([0,0,1,1], ip) ]
            case('4'); b = [b, int([0,1,0,0], ip) ]
            case('5'); b = [b, int([0,1,0,1], ip) ]
            case('6'); b = [b, int([0,1,1,0], ip) ]
            case('7'); b = [b, int([0,1,1,1], ip) ]
            case('8'); b = [b, int([1,0,0,0], ip) ]
            case('9'); b = [b, int([1,0,0,1], ip) ]
            case('A'); b = [b, int([1,0,1,0], ip) ]
            case('B'); b = [b, int([1,0,1,1], ip) ]
            case('C'); b = [b, int([1,1,0,0], ip) ]
            case('D'); b = [b, int([1,1,0,1], ip) ]
            case('E'); b = [b, int([1,1,1,0], ip) ]
            case('F'); b = [b, int([1,1,1,1], ip) ]
            case default; error stop 'unknown hex character: '//h(i:i)
            end select
        end do

    end function hex2bits

end module problem_16

program test
    use problem_16

    implicit none

    logical :: status_ok
    integer :: iunit
    integer(ip) :: i, n
    character(len=:),allocatable :: line
    integer(ip),dimension(:),allocatable :: bin
    logical :: new_packet
    type(packet_type) :: packet
    integer(ip) :: answer, istart

    open(newunit=iunit,file='inputs/day16.txt', status='OLD')
    call read_line_from_file(iunit,line,status_ok)

    bin = hex2bits(line)

   !write(*,'(*(I1))') bin

    new_packet = .true.
    i = 1 ! current index in bin
    n = size(bin) ! number of bits
    version_sum = 0 ! part a

    ! process:
    do
        istart = i
        call packet%parse(i, bin)
        if (i>n) exit
        if (all(bin(i:)==0)) exit     ! ignore if the rest is zeros:
    end do

    write(*,*) '16a: ', version_sum

    answer = packet%evaluate()
    write(*,*) '16b: ', answer

end program test