!Name: Jake Goode
!Student Number: 1202742
!Date: 01-19-2024
!Assignment 1: Iterative Quicksort

!This program uses iterative quick sort to sort integers provided from file.
program iqsort
    
    use intIO !Uses modules for reading and writing files.
    use stackADT !Uses modules for stack implementation.
    implicit none

    integer :: n
    integer, allocatable :: array(:)
    integer, allocatable :: stack(:)
    real :: startT, endT, execTime

    call readUnsorted(array) !Reads integers in file into array.
    
    n = size(array) !Used to get number of elements in array.
    allocate(stack(n))

    stack = 0 !Clears allocated array.
    
    call cpu_time(startT)

    call iterativeQsort(array, stack, 1, n) !Sorts the array using iterative quicksort.

    call cpu_time(endT)
    
    execTime = (endT - startT)

    write(*, '(A, G0.9)') NEW_LINE('A') // 'Elapsed time for iterative Quicksort in seconds: ', execTime

    call writeSorted(array) !Writes sorted array to sortedNUM.txt file.

    deallocate(array)
    deallocate(stack)

    contains

        !Subroutine uses iterative quick sort to sort array of integers from file. Translated from provided Pascal code.
        !Refactored code to update pivot location and swap numbers around while partitioning within loop.
        subroutine iterativeQsort(array, stack, low, high)

            integer :: low, high, pos, pivot, i, x, h, l, temp
            integer, allocatable, intent(inout) :: array(:)
            integer, allocatable, intent(inout) :: stack(:)
            logical :: empty

            pos = 0 !Position of the current location on stack.

            call isEmpty(pos, empty)

            if(.not. empty) then

                call clear(stack, pos)
            end if

            call push(stack, pos, low) !Push the first index number onto the stack (1).
            call push(stack, pos, high) !Push the final index number onto the stack (n; depends on number of integers in file).

            !Loop until stack is empty.
            do while(pos >= 1)

                call pop(stack, pos, h) !Pop the high value from stack.
                call pop(stack, pos, l) !Pop the low value from stack.

                x = array(h) !Set value to compare pivot to.
                pivot = (l - 1)
                i = l

                !Loop to partition and sort the array and set it's pivot.
                do while(i <= (h - 1))

                    if(array(i) <= x) then
                        
                        pivot = pivot + 1

                        !Swap array values around.
                        temp = array(pivot)
                        array(pivot) = array(i)
                        array(i) = temp
                    end if
                    
                    i = i + 1 !Increment until all values in array have been checked.
                end do

                !Swap with pivot.
                temp = array(pivot + 1)
                array(pivot + 1) = array(h)
                array(h) = temp

                pivot = pivot + 1

                !Add index values to stack if unsorted elements found on left side of pivot.
                if((pivot - 1) > l) then

                    call push(stack, pos, l)
                    call push(stack, pos, (pivot - 1))
                end if

                !Add index values to stack if unsorted elements found on right side of pivot.
                if((pivot + 1) < h) then

                    call push(stack, pos, (pivot + 1))
                    call push(stack, pos, h)
                end if
            end do
        
        end subroutine iterativeQsort

end program iqsort
