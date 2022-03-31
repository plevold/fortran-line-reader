program example
    implicit none

    block
        ! Write a file that we can read back afterwards
        integer :: unit
        open(newunit=unit, file='file.txt')
        write(unit, '(a)') 'Hello world'
        write(unit, '(a)') 'This line is longer than the previous line'
        write(unit, '(a)') 'This line is shorter than the previous'
        write(unit, '(a)') 'Now we are done'
        close(unit)
    end block

    block
        ! Use this library to read the file
        use line_reader_mod, only: &
            line_reader_t, &
            error_t

        type(line_reader_t) :: reader
        type(error_t), allocatable :: error
        character(len=:), allocatable :: line

        fallible: block
            reader = line_reader_t()
            call reader%open('file.txt', error)
            if (allocated(error)) exit fallible
            do while (reader%has_next())
                call reader%read_line(line, error)
                if (allocated(error)) exit fallible
                write(*, '(a)') 'Got line: "' // line // '"'
            end do
        end block fallible
        if (allocated(error)) then
            write(*,'(a)') 'Unexpected error!'
            write(*,'(a)') error%display()
            error stop
        end if
    end block
end program