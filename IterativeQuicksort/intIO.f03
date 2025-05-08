!Name: Jake Goode
!Student Number: 1202742
!Date: 01-19-2024
!Assignment 1: Iterative Quicksort

!Module for reading and writing files.
module intIO
    
    implicit none

    contains
    
        !Subroutine to read integers from provided file into an allocated array.
        subroutine readUnsorted(array)
        
            integer, allocatable, intent(out) :: array(:)
            integer :: nLines, io, fsize
            character(len=150) :: fname
            character(len=15) :: number
            logical :: fexist

            write(*, '(A)', advance = 'no') 'Enter the file name for sorting: '
            read(*, '(A)') fname

            inquire(file = fname, exist = fexist)
            inquire(file = fname, size = fsize)

            if(.not. fexist) then
                
                write(*,*) NEW_LINE('A') // 'File does not exist in system'
                stop !Stops program.

            else if(fsize < 1) then
                
                write(*,*) NEW_LINE('A') // 'File is empty'
                stop
            else 

                open(unit = 1, file = fname, iostat = io, status = 'old', action = 'read')
                
                if(io /= 0) then
                    
                    write(*,*) NEW_LINE('A') // 'Cannot open file'
                    stop
                end if

                nLines = 0 !Lines present in file.

                !Loop to count number of lines in opened file.
                do                    
                    read(1, *, iostat = io)
                    
                    if(io /= 0) exit
                    
                    !Check for blank line.
                    if(len(number) > 0) then
                        
                        nLines = nLines + 1
                    end if
                end do

                rewind 1 !Move back to the first line of the file.
                allocate(array(nLines))

                read(1, *) array

                close(1)
            end if

        end subroutine readUnsorted

        !Subroutine to write sorted array to file. If file already exists, user is promted to overwrite.
        subroutine writeSorted(array)

            integer, allocatable, intent(in) :: array(:)
            integer :: n, i
            logical :: fexist
            character(len=5) :: fover

            n = SIZE(array) !Size of array.

            inquire(file = 'sortedNUM.txt', exist = fexist)

            if(.not. fexist) then
                
                !If file doesn't already exist, create new file and save array to file.
                open(unit = 2, file = 'sortedNUM.txt', status = 'new', action = 'write')

                !Write array to file.
                do i = 1, n

                    write(2, '(I0)') array(i)
                end do

                close(2)

                write(*,*) NEW_LINE('A') // 'File has been created with sorted array.'
            else 
                
                !Else file already exists, so ask user if they wish to overwrite.
                write(*,*) NEW_LINE('A') // 'File already exists, overwrite existing file? y for yes, n for no (y/n)?'
                
                !Loop to get correct user input of y/Y or n/N.
                do                    
                    read(*, '(A)') fover

                    if(trim(fover) == 'Y' .or. trim(fover) == 'y') then
                        
                        open(unit = 2, file = 'sortedNUM.txt', status = 'replace', action = 'write')

                        !Write array to file.
                        do i = 1, n

                            write(2, '(I0)') array(i)
                        end do
        
                        close(2)

                        write(*,*) NEW_LINE('A') // 'File has been overwritten with new sorted array.'

                        exit !Exit loop.

                    else if(trim(fover) == 'N' .or. trim(fover) == 'n') then

                        write(*,*) NEW_LINE('A') // 'Exiting program without overwriting file.'

                        exit
                    else
                        
                        write(*, '(A)', advance = 'no') NEW_LINE('A') // 'Invalid input. Type y or n to overwrite file: '
                    end if
                end do
            end if
        
        end subroutine writeSorted
    
end module intIO
