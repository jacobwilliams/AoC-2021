program problem_12

    use aoc_utilities
    use iso_fortran_env, only: ip => int64
    use dag_module

    implicit none

    integer,parameter :: max_str_len = 10

    integer :: iunit, istep, i, j, k, n_lines, n_nodes
    character(len=:),allocatable :: line
    logical :: status_ok
    type(string),dimension(:),allocatable :: vals
    type(dag) :: d
    integer,dimension(1) :: iloc,jloc
    character(len=max_str_len),dimension(:,:),allocatable :: pairs
    character(len=max_str_len),dimension(:),allocatable :: nodes
    character(len=max_str_len),dimension(:),allocatable :: path
    character(len=max_str_len),dimension(:),allocatable :: next_nodes
    integer :: n_paths
    integer :: part
    character(len=1),dimension(2),parameter :: ab = ['a','b']

    open(newunit=iunit,file='inputs/day12.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)
    allocate(pairs(n_lines,2))

    ! read in the file:
    do i = 1, n_lines
        call read_line_from_file(iunit,line,status_ok)
        !write(*,*) line
        call split(line,'-',vals)
        pairs(i,1) = vals(1)%str
        pairs(i,2) = vals(2)%str
        call add_node_to_list(nodes,vals(1)%str)
        call add_node_to_list(nodes,vals(2)%str)
    end do
    !write(*,*) 'nodes = ', nodes
    n_nodes = size(nodes)

    !*****************************
    ! ! create the dag file:
    ! call d%set_vertices(n_nodes)
    ! do i = 1, n_lines
    !     iloc = findloc(nodes, pairs(i,1))
    !     jloc = findloc(nodes, pairs(i,2))
    !     call d%set_edges(iloc(1),[jloc(1)])
    ! end do
    ! do i = 1, n_nodes
    !     if (nodes(i)=='start' .or. nodes(i)=='end') then
    !         call d%set_vertex_info(i,label=trim(nodes(i)),attributes='shape=square,fillcolor="SlateGray1",style=filled')
    !     else
    !         call d%set_vertex_info(i,label=trim(nodes(i)),attributes='shape=circle,fillcolor="cornsilk",style=filled')
    !     end if
    ! end do
    ! call d%save_digraph('problem12.dot','RL',300)
    ! call d%destroy()
    ! call execute_command_line('dot -Tpng -o problem12.png problem12.dot')
    !*****************************

    do part = 1,2
        n_paths = 0
        if (allocated(path)) deallocate(path)
        allocate(path(1))
        path(1) = 'start'
        next_nodes = get_next_nodes(path(1)) ! all the nodes we can go to from this one
        do i = 1, size(next_nodes)
            call traverse(path,next_nodes(i))
        end do
        write(*,*) '12'//ab(part)//': n_paths = ', n_paths
    end do

    contains

    function get_next_nodes(node) result(next_nodes)
        implicit none
        character(len=max_str_len),intent(in) :: node
        character(len=max_str_len),dimension(:),allocatable :: next_nodes

        integer :: i

        do i = 1, n_lines
            if (pairs(i,1)==node) then
                call add_node_to_list(next_nodes,pairs(i,2))
            elseif (pairs(i,2)==node) then
                call add_node_to_list(next_nodes,pairs(i,1))
            end if
        end do

    end function get_next_nodes

    recursive subroutine traverse(list,next_node)
        implicit none
        character(len=max_str_len),dimension(:),allocatable,intent(in) :: list
        character(len=max_str_len),intent(in) :: next_node

        character(len=max_str_len),dimension(:),allocatable :: tmp
        character(len=max_str_len),dimension(:),allocatable :: next_nodes
        integer :: i,j

        if (next_node=='start') return ! invalid

        if (next_node=='end') then ! valid path
            if (allocated(list)) then
                tmp = list
            end if
            call add_node_to_list(tmp,next_node) ! add it to the path
            n_paths = n_paths + 1
            return
        end if

        ! otherwise, see if it's a valid path for this next step
        if ( lowercase(next_node) ) then
            if (part==1) then
                ! can visit no small cave more than once
                if (any(list == next_node)) return ! if we have already visied this node:
            elseif (part==2) then
                ! can visit a single small cave twice
                if (count(list == next_node)==2) return ! already visited this one twice
                do j = 1, n_nodes
                    if (lowercase(nodes(j))) then
                        if (count(list == nodes(j))==2) then ! one has already been visited twice, can't visit any more small ones twice
                            if (next_node == nodes(j) .or. count(list == next_node)==1) return
                        end if
                    end if
                end do
            end if
        end if

        if (allocated(list)) tmp = list
        call add_node_to_list(tmp,next_node) ! add it to the path
        next_nodes = get_next_nodes(next_node) ! all the nodes we can go to from this one
        do i = 1, size(next_nodes)
            call traverse(tmp,next_nodes(i))
        end do

    end subroutine traverse

    pure elemental logical function lowercase(c)
    implicit none
    character(len=*),intent(in) :: c
    lowercase = c(1:1) >= 'a' .and. c(1:1) <= 'z'
    end function lowercase

    subroutine add_node_to_list(nodes,name)
        implicit none
        character(len=max_str_len),dimension(:),allocatable :: nodes
        character(len=*),intent(in) :: name
        character(len=max_str_len),dimension(:),allocatable :: tmp
        integer :: n
        if (allocated(nodes)) then
            n = size(nodes)
            allocate(tmp(n+1))
            tmp(1:n) = nodes
            deallocate(nodes)
            tmp(n+1) = name
            call move_alloc(tmp, nodes)
        else
            nodes = [name]
        end if
    end subroutine add_node_to_list

    subroutine expand(list,i)
        implicit none
        integer,dimension(:),allocatable,intent(inout) :: list
        integer,intent(in) :: i
        if (allocated(list)) then
            list = [list, i]
        else
            list = [i]
        end if
    end subroutine expand

end program problem_12