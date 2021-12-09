program problem_8

use aoc_utilities

implicit none

integer :: iunit, i, j, k, n, n_lines
character(len=:),allocatable :: line
logical :: status_ok
type(string),dimension(:),allocatable :: vals
type(string),dimension(:),allocatable :: vals_readings
type(string),dimension(:),allocatable :: vals_outputs
integer,dimension(4) :: output_count
integer :: total

logical,dimension(7, 7) :: matrix

open(newunit=iunit,file='inputs/day8.txt')

n_lines = number_of_lines_in_file(iunit)

total = 0
do i = 1, n_lines
    call read_line_from_file(iunit,line,status_ok)
    call split(trim(line),'|',vals)

    ! gdcafe eacb adc gbfda afdceb edgbcf badfc ecgbafd ac fdbce | ebfcd cefdab bdfgeca egbdacf
    call split(trim(adjustl(vals(2)%str)),' ',vals_outputs)
    !write(*,*) 'n outputs = ', size(vals_outputs)
    do j = 1, size(vals_outputs)
        output_count(j) = len_trim(vals_outputs(j)%str)
    end do
    !write(*,*) output_count

    ! count digits 1, 4, 7, or 8
    total = total + count(output_count==2) + & ! 1
                    count(output_count==4) + & ! 4
                    count(output_count==3) + & ! 7
                    count(output_count==7)     ! 8

end do

write(*,*) '8a : Total = ', total


end program problem_8
