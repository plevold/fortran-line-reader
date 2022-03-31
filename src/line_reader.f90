module line_reader_mod
    use, intrinsic :: iso_fortran_env, only: iostat_end
    implicit none

    private
    public line_reader_t

    public error_t


    type :: line_reader_t
        private
        ! Buffer capacity
        integer :: cap = 8192
        ! Search for Windows newlines (CRLF)?
        logical :: win_newline = .true.
        ! Search for Linux newlines (LF)?
        logical :: linux_newline = .true.
        ! Buffer to read data into
        character(len=:), allocatable :: buffer
        ! Start position of current data in buffer
        integer :: start = 0
        ! End position of current data in buffer
        integer :: end = 0
        ! Was EOF reach during last read?
        logical :: eof_reached = .false.
        ! File unit
        integer :: unit
        ! File opened?
        logical :: opened = .false.
    contains
        procedure :: open
        procedure :: read_line
        procedure :: has_next
        procedure :: close
        final :: finalize
    end type

    interface line_reader_t
        module procedure init
    end interface


    type :: error_t
        private
        character(len=:), allocatable :: message
    contains
        procedure :: display
    end type


    character, parameter :: NULL = char(0)
    character, parameter :: LF = char(10)
    character, parameter :: CR = char(13)
    character(len=2), parameter :: CRLF = CR // LF
    integer, parameter :: MIN_CAP = 3
    integer, parameter :: DEFAULT_CAP = 8192

contains


    !> Initialize a line_reader_t
    type(line_reader_t) pure function init(cap, win_newline, linux_newline) result(this)
        !> Buffer capacity. Default: 8192 characters
        integer, optional, intent(in) :: cap
        !> Terminate line at Windows style newlines (CRLF)? Default: True
        logical, optional, intent(in) :: win_newline
        !> Terminate line at Linux style newlines (LF)? Default: True
        logical, optional, intent(in) :: linux_newline

        integer :: the_cap

        the_cap = DEFAULT_CAP
        if (present(cap)) the_cap = max(cap, MIN_CAP)
        if (present(win_newline)) this%win_newline = win_newline
        if (present(linux_newline)) this%linux_newline = linux_newline

        allocate(character(len=the_cap) :: this%buffer)
    end function


    !> Open a file for reading
    subroutine open(this, filename, error)
        class(line_reader_t), intent(inout) :: this
        !> Filename
        character(len=*), intent(in) :: filename
        !> Error flag
        type(error_t), allocatable, intent(out) :: error

        integer :: iostat
        character(len=256) :: error_message

        if (this%opened) then
            error = error_t('Another file has already been opened')
            return
        end if

        open(newunit=this%unit, file=filename, status='old', access='stream',   &
            iostat=iostat, iomsg=error_message)
        if (iostat /= 0) then
            error = error_t(trim(error_message))
            return
        end if
        if (.not. allocated(this%buffer)) then
            allocate(character(len=DEFAULT_CAP) :: this%buffer)
        end if
        this%opened = .true.
        !TODO: Do first read here? Otherwise has_next will be incorrect for an empty file
    end subroutine


    !> Read a line from the opened file
    subroutine read_line(this, line, error)
        class(line_reader_t), intent(inout) :: this
        !> Line read from file
        character(len=:), allocatable, intent(out) :: line
        !> Allocated if read caused an error
        type(error_t), allocatable, intent(out) :: error

        integer :: stat
        character(len=256) :: error_message
        integer :: prev_pos, pos

        if (.not. this%opened) then
            error = error_t('No file opened')
            return
        end if

        line = ''

        if (this%eof_reached .and. this%end == 0) then
            error = error_t('EOF has already been reached')
            return
        end if

        if (this%end /= 0) then
            call append(line, this%buffer, this%start, this%end, &
                    this%win_newline, this%linux_newline)
        end if

        do while (this%start == 0 .and. this%end == 0)
            this%start = 1
            inquire(unit=this%unit, pos=prev_pos)
            read(this%unit, iostat=stat, iomsg=error_message) this%buffer
            select case(stat)
                case (0, iostat_end)
                    if (stat == iostat_end) then
                        this%eof_reached = .true.
                        inquire(unit=this%unit, pos=pos)
                        this%end = pos - prev_pos
                    else
                        this%end = len(this%buffer)
                    end if
                    call append(line, this%buffer, this%start, this%end, &
                            this%win_newline, this%linux_newline)
                    if (this%eof_reached) return
                case default
                    error = error_t(trim(error_message))
                    return
            end select
        end do
    end subroutine


    !> Returns true if file has more lines
    logical pure function has_next(this)
        class(line_reader_t), intent(in) :: this

        if (.not. this%opened) then
            has_next = .false.
        else if (this%eof_reached) then
            has_next = this%end /= 0
        else
            has_next = .true.
        end if
    end function


    !> Close the already opened file
    subroutine close(this)
        class(line_reader_t), intent(inout) :: this

        if (this%opened) then
            close(this%unit)
            this%opened = .false.
        end if
    end subroutine


    subroutine finalize(this)
        type(line_reader_t), intent(inout) :: this

        call this%close()
    end subroutine


    subroutine append(line, buffer, start, end, win_newline, linux_newline)
        character(len=:), allocatable, intent(inout) :: line
        character(len=*), intent(inout) :: buffer
        integer, intent(inout) :: start
        integer, intent(inout) :: end
        logical, intent(in) :: win_newline
        logical, intent(in) :: linux_newline

        integer :: idx
        integer :: win_idx
        integer :: linux_idx
        integer :: n

        win_idx = 0
        linux_idx = 0
        if (win_newline) then
            win_idx = index(buffer(start:end), CRLF)
        end if
        if (linux_newline) then
            linux_idx = index(buffer(start:end), LF)
        end if
        if (win_idx > 0 .and. linux_idx > 0) then
            if (win_idx < linux_idx) then
                idx = win_idx
                n = 2
            else
                idx = linux_idx
                n = 1
            end if
        else if (win_idx > 0) then
            idx = win_idx
            n = 2
        else
            idx = linux_idx
            n = 1
        end if

        if (idx == 0) then
            line = line // buffer(start:end)
            start = 0
            end = 0
        else
            line = line // buffer(start:start + idx - 2)
            start = start + idx + n - 1
        end if
    end subroutine


    pure function display(this) result(chars)
        class(error_t), intent(in) :: this
        character(len=:), allocatable :: chars

        chars = 'Error: ' // this%message
    end function

end module