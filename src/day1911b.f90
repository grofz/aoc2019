  module day1911b_mod
    use intcode_mod, only : computer_t, SINBUF_EMPTY, SHALT
    use day1911_mod, only : board_t, HEADING_UP, WHITE
    implicit none

    type, public :: robot_t
      type(computer_t) :: ZX128
      type(board_t)    :: board
    contains
      procedure :: Init => robot_init
      procedure :: Walk => robot_walk
    end type

  contains

    subroutine robot_Init(this, file)
      class(robot_t), intent(out) :: this
      character(len=*), intent(in) :: file

      call this%ZX128 % Load_from_file(file)
      call this%Zx128 % Reset(1,2)
      call this%board % Init()
      call this%board % Setrobot([0,0], HEADING_UP)
    end subroutine


    subroutine robot_walk(this)
      class(robot_t), intent(inout) :: this

      integer, allocatable :: commands(:)
      integer :: color, status

 print '("Robot ready,")'
          call this%board % Paintcolor(color=WHITE) ! part2
      MLOOP: do
        ! send to the computer the actual color
        color = this%board % Getcolor()
 print '("At ",i4,",",i4," hdg ",i0,"    Color ",i0)', &
 this%board%rob_xy, this%board%rob_hdg, color
        call this%ZX128 % Set_inbuf(color)
        call this%ZX128 % Run(status)
 print '("Status code ",i0)', status

        select case(status)
        case (SINBUF_EMPTY, SHALT)
          commands = int(this%ZX128 % Get_outbuf(), kind=4)
          if (size(commands) /= 2) &
             error stop 'robot_walk - not correct command received'
          if (commands(1)<0 .or. commands(1)>1) &
             error stop 'robot_walk - not correct command (1)'
          if (commands(2)<0 .or. commands(1)>2) &
             error stop 'robot_walk - not correct command (1)'

 print '("Commands ",i0,1x,i0,"   and moving...")', commands
          call this%board % Paintcolor(color=commands(1))
          call this%board % Maketurn(commands(2))
          call this%board % Forward()

        case default
          error stop 'robot_walk - unexpected  status code'
        end select
        if (status == SHALT) exit
      end do MLOOP

      commands = this%ZX128 % Get_outbuf()
      if (size(commands) /= 0) &
        print *, 'WARNING: unprocessed output left'

print *, this%board%getlimits()
print *, this%board%map%Count()
      call this%board%print()




    end subroutine robot_walk

  end module day1911b_mod
