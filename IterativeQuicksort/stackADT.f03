!Name: Jake Goode
!Student Number: 1202742
!Date: 01-19-2024
!Assignment 1: Iterative Quicksort

!Module for stack implementation.
module stackADT
    
    implicit none

    contains

        !Subroutine to check if stack is empty.
        subroutine isEmpty(pos, empty)
        
            integer, intent(in) :: pos
            logical, intent(out) :: empty

            empty = (pos == 0) !True if empty, false otherwise.

        end subroutine isEmpty

        !Subroutine to push index value of array to stack.
        subroutine push(stack, pos, index)
        
            integer, intent(inout) :: pos
            integer, intent(in) :: index
            integer, allocatable, intent(inout) :: stack(:)

            pos = pos + 1 !Move position of stack up by one.
            stack(pos) = index

        end subroutine push

        !Subroutine to retrive index value of array from stack.
        subroutine pop(stack, pos, index)

            integer, intent(inout) :: pos
            integer, intent(out) :: index
            integer, allocatable, intent(inout) :: stack(:)

            index = stack(pos)
            pos = pos - 1 !Move position of stack down by one.

        end subroutine pop

        !Subrountine to clear stack.
        subroutine clear(stack, pos)
            
            integer, intent(inout) :: pos
            integer, allocatable, intent(inout) :: stack(:)

            stack = 0 !Set's whole array of stack to 0 (clear).
            pos = 0

        end subroutine clear
    
end module stackADT
