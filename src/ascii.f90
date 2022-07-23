!
! ASCII controlled Intcode
!
  module ascii_mod
    use intcode_mod
    implicit none

    integer, parameter :: LF_CHAR=10

  contains

    subroutine run_ascii(file)
      character(len=*), intent(in) :: file

      type(computer_t) :: PMD
      integer :: istat, i, val
      character(len=100) :: input_line

      call PMD % Load_from_file(file)
      call PMD % Reset(100,1)
      istat = 0
      MLOOP: do
        call PMD % Run(istat)

        select case(istat)
        case(SOUTBUF_READY, SOUTBUF_FULL)
          if (PMD % IsEmpty_outbuf()) error stop 'run_ascii - empty outbuf'
          call PMD % Read_outbuf(val)
          if (val==LF_CHAR) then
            write(*,*)
          else if (val>31 .and. val<127) then
            write(*,'(a)', advance='no') achar(val)
          else
            write(*,*)
            write(*,'("Computer says ",i0)') val
          end if

        case(SINBUF_EMPTY)
          write(*,'(a)',advance='no') ' >> '
          read(*,'(a)') input_line
          do i=1, len_trim(input_line)
            call PMD % set_inbuf(iachar(input_line(i:i)))
          end do
          call PMD % set_inbuf(LF_CHAR)

        case(SHALT)
          print '("Computer halts.")'
          exit MLOOP
        case default
          error stop 'run_ascii - unexpected stat'
        end select
      end do MLOOP

    end subroutine run_ascii

  end module ascii_mod
