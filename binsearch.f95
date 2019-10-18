module binsearch
    implicit none
    contains
    function search(array,elem) result (elem_is_there)
        implicit none
        integer :: divider_position, low, up
        integer :: length, elem, divider
        logical :: elem_is_there, sorted_upQ = .false.
        integer, dimension(:) :: array

        length = size(array,1)
        low = 1; up = length; divider_position = (up + low)/2
        elem_is_there = .false.
        if (array(low)<array(up)) then
            sorted_upQ = .true.
        end if

        do while ((low<=up) .and. (elem_is_there .eqv. .false.))
            divider = array(divider_position)

            if (elem == divider) then
                elem_is_there = .true.


            else if ((elem < divider) .eqv. (sorted_upQ .eqv. .true.)) then
                up = divider_position - 1
                divider_position = low + (up - low)/2
                length = up - low + 1


            else if ((elem > divider) .eqv. (sorted_upQ .eqv. .true.)) then
                low = divider_position + 1
                divider_position = low + (up - low)/2
                length = up - low + 1

            end if
        end do

        divider = array(divider_position)
        if (elem == divider) then
            elem_is_there = .true.
        end if
    end function
end module
