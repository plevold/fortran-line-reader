module line_reader_test
    use line_reader_mod, only: line_reader_t, error_t

    implicit none
    private
    public :: test_line_reader

    character, parameter :: LF = char(10)
    character, parameter :: CR = char(13)
    character(len=2), parameter :: CRLF = CR // LF

contains


    subroutine test_line_reader()
        call set_up
        call read_should_fail_if_not_initialized
        call nexts_from_lf_file
        call nexts_from_crlf_file
        call read_lf_with_default_buffer_size
        call read_crlf_with_default_buffer_size
        call read_after_eof_should_fail
        call tear_down
        write(*,*) 'Ok!'
    end subroutine

    subroutine set_up()
        integer :: unit
        call delete_file('default_file.txt')
        call delete_file('lf_file.txt')
        call delete_file('crlf_file.txt')

        open(newunit=unit, file='default_file.txt', status='replace')
        write(unit, '(a)') 'A short line'
        write(unit, '(a)') 'Another short line'
        close(unit)

        open(newunit=unit, file='lf_file.txt', status='replace')
        write(unit, '(a)', advance='no') 'A short line' // LF
        write(unit, '(a)', advance='no') 'Another short line' // LF
        write(unit, '(a)', advance='no') &
            'A line that is very long so that will hopefully exceed a buffer capacity of 64 bytes' // LF
        close(unit)

        open(newunit=unit, file='crlf_file.txt', status='replace')
        write(unit, '(a)', advance='no') 'A short line' // CRLF
        write(unit, '(a)', advance='no') 'Another short line' // CRLF
        write(unit, '(a)', advance='no') &
            'A line that is very long so that will hopefully exceed a buffer capacity of 64 bytes' // CRLF
        close(unit)
    end subroutine


    subroutine tear_down()
        call delete_file('default_file.txt')
        call delete_file('lf_file.txt')
        call delete_file('crlf_file.txt')
    end subroutine


    subroutine delete_file(filename)
        character(len=*), intent(in) :: filename

        integer :: unit
        integer :: status

        ! Ignore errors e.g. if file does not exist
        open(newunit=unit, iostat=status, file=filename, status='old')
        if (status == 0) then
            close(unit, status='delete')
        end if
    end subroutine


    subroutine read_should_fail_if_not_initialized()
        type(line_reader_t) :: reader
        character(len=:), allocatable :: line
        type(error_t), allocatable :: error

        call reader%next(line, error)
        if (.not. allocated(error)) error stop 'Expected next to fail'
    end subroutine


    subroutine nexts_from_lf_file()
        type(line_reader_t) :: reader
        type(error_t), allocatable :: error

        reader = line_reader_t(cap=64)
        call reader%open('lf_file.txt', error)
        if (allocated(error)) error stop 'Unexpected error: ' // error%display()
        call read_and_check_lines(reader)
    end subroutine


    subroutine nexts_from_crlf_file()
        type(line_reader_t) :: reader
        type(error_t), allocatable :: error

        reader = line_reader_t(cap=64)
        call reader%open('crlf_file.txt', error)
        if (allocated(error)) error stop 'Unexpected error: ' // error%display()
        call read_and_check_lines(reader)
    end subroutine


    subroutine read_lf_with_default_buffer_size()
        type(line_reader_t) :: reader
        type(error_t), allocatable :: error

        call reader%open('lf_file.txt', error)
        if (allocated(error)) error stop 'Unexpected error: ' // error%display()
        call read_and_check_lines(reader)
    end subroutine


    subroutine read_crlf_with_default_buffer_size()
        type(line_reader_t) :: reader
        type(error_t), allocatable :: error

        call reader%open('crlf_file.txt', error)
        if (allocated(error)) error stop 'Unexpected error: ' // error%display()
        call read_and_check_lines(reader)
    end subroutine


    subroutine read_after_eof_should_fail()
        type(line_reader_t) :: reader
        type(error_t), allocatable :: error
        character(len=:), allocatable :: line

        fallible: block
            call reader%open('default_file.txt', error)
            if (allocated(error)) exit fallible
            call reader%next(line, error)
            if (allocated(error)) exit fallible
            call reader%next(line, error)
            if (allocated(error)) exit fallible
            call reader%next(line, error)
            if (allocated(error)) exit fallible
        end block fallible
        if (allocated(error)) error stop 'Unexpected error: ' // error%display()

        call reader%next(line, error)
        if (.not. allocated(error)) error stop 'Expected error after EOF'
    end subroutine


    subroutine read_and_check_lines(reader)
        type(line_reader_t), intent(inout) :: reader
        character(len=:), allocatable :: line
        type(error_t), allocatable :: error

        fallible: block
            call reader%next(line, error)
            if (allocated(error)) exit fallible
            if (line /= 'A short line') error stop 'Unexpected line (1): "' // line // '"'
            call reader%next(line, error)
            if (allocated(error)) exit fallible
            if (line /= 'Another short line') error stop 'Unexpected line (2): "' // line // '"'
            call reader%next(line, error)
            if (allocated(error)) exit fallible
            if (line /= 'A line that is very long so that will hopefully exceed a buffer capacity of 64 bytes') &
                    error stop 'Unexpected line (3): "' // line // '"'
            return
        end block fallible
        error stop 'Unexpected error: ' // error%display()
    end subroutine

end module
