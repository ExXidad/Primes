module reading_file
    implicit none
    contains
    subroutine return_array(u, data_from_file, max_value)
        integer :: u, ios = 0, i = 0, array_length
        integer :: max_value
        integer, dimension(:), allocatable :: data_from_file

        read(u,*) array_length
        read(u,*) max_value

        allocate(data_from_file(array_length))

        do while (ios == 0)
            i=i+1
            read(u,*,iostat=ios) data_from_file(i)
        end do

    end subroutine
end module

program main
  use reading_file
  use binsearch
  implicit none
  integer :: un = 1
  integer :: input, max_value, amount_of_elems = 1
  integer :: i, j, m
  logical :: search_result = .false.
  integer, dimension(:), allocatable :: data_from_file, array


  read(*,*) input
  if (input <= 0) stop "Wrong input"


  open(unit = un,file = "primes.txt")

  call return_array(un, data_from_file, max_value)

  close(un)

  search_result = search(data_from_file, input)

  if (search_result .eqv. .true.) then
    write(*,*) "Prime. Input was in library."

  else if (input <= max_value) then
    write(*,*) "Not in library"
    write(*,*) "Not prime. Lesser or equal than previous maximum value."

  else
    write(*,*) "Not in library. Calculating..."


    allocate(array(input))


    do i=1,input
        array(i) = i
    end do


    do m=2,input
        if (array(m)/=0) then
            do j=2*m,input,m
                array(j) = 0
            end do
            amount_of_elems = amount_of_elems+1
        end if
    end do


    if (input > max_value) then
        open(unit=un,file="primes.txt")
        write(un,*) amount_of_elems
        write(un,*) input
        do i=1,input
            if (array(i)/=0) then
                write(un,*) array(i)
            end if
        end do
        close(un)
    end if


    if (array(input)/=0) then
        write(*,*) "Prime"
    else
        write(*,*) "Not Prime"
    end if
  end if

end
