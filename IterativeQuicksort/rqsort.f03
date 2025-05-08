!Name: Jake Goode
!Student Number: 1202742
!Date: 01-19-2024
!Assignment 1: Iterative Quicksort

!This program uses recursion quick sort to sort integers provided from file.
program rqsort
    
    use intIO !Uses modules for reading and writing files.
    implicit none

    integer :: n
    integer, allocatable :: array(:)
    real :: startT, endT, execTime

    call readUnsorted(array) !Reads integers in file into array.
    
    n = size(array) !Used to get number of elements in array.
    
    call cpu_time(startT)

    call recursiveQsort(array, 1, n) !Sort the array using recursive quicksort.

    call cpu_time(endT)
    
    execTime = (endT - startT)
    
    write(*, '(A, G0.9)') NEW_LINE('A') // 'Elapsed time for recursive Quicksort in seconds: ', execTime

    call writeSorted(array) !Writes sorted array to sortedNUM.txt file.

    deallocate(array)

    contains

    !Subroutine uses recursion to sort array of integers from file using quick sort.
    recursive subroutine recursiveQsort(array, low, high)

        implicit none

        integer :: low, high, x, temp, i, j
        integer, allocatable, intent(inout) :: array(:)

        x = array((low + high) / 2) !Used as pivot to sort array.
        i = low
        j = high

        !Loop to sort array until left and right side pointers overlap.
        do
            !Loop to check that all left side values are less than the pivot.
            do while(array(i) < x)

                i = i + 1
            end do

            !Loop to chack that all right side values are greater than the pivot.
            do while(x < array(j))

                j = j - 1
            end do

            if(i >= j) exit !Overlap occurred.

            !Swap the values around.
            temp = array(i)
            array(i) = array(j)
            array(j) = temp

            i = i + 1 !Goes up the array.
            j = j - 1 !Goes down the array.
        end do

        if(low < (i - 1)) call recursiveQsort(array, low, i - 1) !Sort left side of partition.

        if((j + 1) < high) call recursiveQsort(array, j + 1, high) !Sort right side of partition.

    end subroutine recursiveQsort

end program rqsort
