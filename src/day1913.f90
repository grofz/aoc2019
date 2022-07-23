  module day1913_mod
    use kinds_m, only : I8B
    use intcode_mod, only : computer_t, SOUTBUF_FULL, SINBUF_EMPTY, SHALT, &
        SOUTBUF_READY
    implicit none

    ! display size
    integer, parameter :: SCREEN_X = 43, SCREEN_Y = 22

    ! Tiles:  0=empty, 1=wall, 2=block, 3=paddle, 4=ball
    character(len=1), parameter :: &
       TILES(0:4) = [ ' ', 'W', '#', '_', 'O']
    integer, parameter :: ID_BALL=4, ID_WALL=1, ID_PADDLE=3, ID_BLOCK=2, ID_EMPTY=0

    ! CWD as "computer with display"
    type, public, extends(computer_t) :: cwd_t
      character(len=1), allocatable :: dsp(:,:)
      integer :: score = 0
      integer :: bpos(2)=-1, bpos0(2)=-1
      integer :: ppos(2)=-1
      logical :: autopilot_on = .false.
      real :: tsleep = 0.0
    contains
      procedure :: Init_cwd
      procedure :: Display
      procedure :: Play, Process_output, Predict_ball
    end type

  contains

    subroutine Init_cwd(this, file, free_token)
      class(cwd_t), intent(out)     :: this
      character(len=*), intent(in)  :: file
      logical, intent(in), optional :: free_token

      ! prepare intcode interpreter
      call this % Load_from_file(file)
      call this % Reset(2,3)
      if (present(free_token)) then
        ! to play the game, instruction must be modified
        if (free_token) call this % overwrite(0,2)
      end if

      ! initialize the display memory
      allocate(this % dsp(0:SCREEN_X, 0:SCREEN_Y))
      this % dsp = TILES(ID_EMPTY)
    end subroutine



    subroutine Display(this)
      class(cwd_t), intent(inout) :: this
      integer :: i, j, sx, sy
      character(len=1) :: sval
      real :: t0, t1
      character(len=1), parameter :: DUMMY='?' ! signaling value
!
! Paddle is not in correct postion in "dsp" state when Display() is called.
! Use saved position instead. Return "dsp" to its current state at the end.
!
      sval = DUMMY
      if (.not. any(this%ppos<0)) then
        ! paddle position is not uknown
        sx = this%ppos(1)
        sy = this%ppos(2)
        sval = this%dsp(sx,sy)
        this%dsp(sx,sy) = '~'
      end if

      ! reset terminal
      if (this%tsleep /= 0.0) write(*,'(a)',advance='no') char(27)//'c'

      do j=0, SCREEN_Y
        do i=0, SCREEN_X
          write(*,'(a1)',advance='no') this % dsp(i,j)
        end do
        write(*,*)
      end do
      write(*,'("Score ",i0, 3(a," [",i0,", ",i0,"] "))') this % score, &
           '  Ball ',this%bpos, '  Velo ',this%bpos-this%bpos0, '  Paddle ', this % ppos
      write(*,*) ! empty line

      ! sleep?
      call cpu_time(t0)
      do
        call cpu_time(t1)
        if (t1-t0 > this%tsleep) exit
      end do

      if (sval /= DUMMY) this%dsp(sx,sy) = sval
    end subroutine Display



    subroutine Play(this, is_autopilot, tsleep, score)
      class(cwd_t), intent(inout) :: this
      logical, intent(in) :: is_autopilot
      real, intent(in) :: tsleep
      integer, intent(out) :: score

      integer :: status, inpkey

      ! buffer some input values to before auto-pilot takes over
      if (is_autopilot) then
        call this % Set_inbuf(0)
        call this % Set_inbuf(0)
        this % autopilot_on = .true.
      else
        this % autopilot_on = .false.
      end if
      this % tsleep = tsleep

      status = 0
      do
        if (status==SHALT) exit
        call this % Run(status)

        select case(status)
        case (SOUTBUF_FULL, SHALT, SOUTBUF_READY)
          call this % Process_output
          cycle
        case (SINBUF_EMPTY)
          ! We should not be here, as the input buffer should be fed by auto-pilot
          call this % Display()

          ! But just ask user for the input...
          write(*,'(a)',advance='no') 'Input from user requested ?'
          read(*,*) inpkey
          call this % Set_inbuf(inpkey)
        case default
          error stop 'Play - unexpected status code'
        end select
      end do

      call this % Display()
      score = this%score

    end subroutine Play



    subroutine Process_output(this)
      class(cwd_t), intent(inout) :: this
!
! Expecting 3 values in the buffer. Update display state. If "ball" is drawn by Intcode,
! then runs "paddle auto controller" and feeds the input buffer.
!
      integer, allocatable :: dout(:)
      integer(I8B), allocatable :: dout128(:)
      integer :: inpauto, inp, inpfut, xfut(2), xauto(2)

      if (this % Isempty_outbuf()) then
        error stop 'process_output - buffer is empty'
      end if

      dout128 = this % Get_outbuf()
      dout = int(dout128)
      if (size(dout)/=3) error stop 'process_output - 3 values expected'
      ! dout = [x-position, y-position, char-code]
      ! positions are in the range 0 to N-1

      if (dout(1)==-1 .and. dout(2)==0) then
        ! output score
        this % score = dout(3)

      else if (dout(1)<0 .or. dout(2)<0 .or. dout(1)>SCREEN_X .or. &
          dout(2)>SCREEN_Y) then
        error stop 'process_output - out of screen coordinates'

      else
        ! usefull for debugging - what Intcode does
        print '("At position [",i0,",",i0,"] pixel ",a," overwritten by ",a)',&
            dout(1),dout(2), &
            '"'//this%dsp(dout(1),dout(2))//'"', '"'//TILES(dout(3))//'"'

        ! Update display memory
        if (dout(3)<lbound(TILES,1) .or. dout(3)>ubound(TILES,1)) &
            error stop 'process_output - value out of char set'
        this%dsp(dout(1), dout(2)) = TILES(dout(3))

        ! Record position of ball and paddle
        if (dout(3)==ID_BALL) then
          ! Intcode wants to display the ball
          this%bpos0 = this%bpos
          this%bpos(1)=dout(1)
          this%bpos(2)=dout(2)

          ! Now we should decide next ball position and move paddle accordingly
          call this % Display()
          if (this%ppos(1)<0) goto 900 ! do nothing more until paddle position is known
          call this % Predict_ball(inpauto, xauto)
          call Run_clone(this, inpfut, xfut)

          if (inpfut /= inpauto .or. any(xauto/=xfut)) then
            print *, 'Inconsistent: (algorithm/virtual)',inpauto, inpfut
            print '("Algorithm ",i0,1x,i0,"  Virtual ",i0,i0)',xauto,xfut
          end if

          ! Which input to use?
          if (this % autopilot_on) then
            inp = inpauto
          else
            print '("Auto pilot suggests ",i0)', inpauto
            write(*,'(a)',advance='no') 'Input from user requested ?'
            read(*,*) inp
          end if

          ! Move paddle and update its position
          call this % Set_inbuf(inp)
          select case(inp)
          case(1)
            this%ppos(1) = this%ppos(1) + 1
          case(-1)
            this%ppos(1) = this%ppos(1) - 1
          end select


        else if (dout(3)==3) then
          ! Paddle displayed / Process only first time (as the value is not actual later)
          if (this%ppos(1)<0) then
            this%ppos(1)=dout(1)
            this%ppos(2)=dout(2)
          end if
        end if
      end if
      900 continue
    end subroutine Process_output



    subroutine Predict_ball(this, move, xnext)
      class(cwd_t), intent(inout)    :: this
      integer, intent(out)           :: move     ! where paddle should move -1, +1, 0
      integer, intent(out), optional :: xnext(2) ! ball position in next step
!
! Using current "dsp" state, ball and paddle position, predict ball velocity after
! bouncing.
!
      integer :: v(2), v0(2), x(2), xnext0(2), vz(2)
      character(len=1) :: ngb_ver, ngb_hor, ngb_dia
      character(len=1) :: dsp0(-1:1,-1:1)  ! ball neighbouring pixels
      logical :: is_ver, is_hor, is_dia    ! is there an obstacle in that direction?
      logical :: swap_v(2)
      integer :: i, j, z, ibp, jbp

      ! If ball position is at the bottom, the game is lost. Do nothing
      if (this%bpos(2)==SCREEN_Y) then
        move = 0
        return
      end if

      if (this%bpos0(1) < 0) then
        v= [1, 1] ! ball velocity at the start of the game is known
      else
        v = this%bpos - this%bpos0
      end if
      v0 = v
      x = this%bpos
      dsp0 = this%dsp(x(1)-1:x(1)+1,x(2)-1:x(2)+1)

      ! Because paddle position is not actual in "dsp", modify neighbourhood by using
      ! the saved position instead.
      ibp = this%ppos(1)-x(1)
      jbp = this%ppos(2)-x(2)
      where (dsp0==TILES(ID_PADDLE)) dsp0=TILES(ID_EMPTY)
      if (jbp>=-1 .and. jbp<=1 .and. ibp>=-1 .and. ibp<=1) &
          dsp0(ibp,jbp) = TILES(ID_PADDLE)

      if (any(v==0)) then
        print *, 'prediction fails - zero velocity'
        error stop 'STOP STOP'
      else if (any(abs(v)>1)) then
        print *, 'velocity too big - prediction fails'
        error stop 'STOP STOP'
      end if

      ! See, if there is a free path in the ball moving direction.
      ! If there is a wall/paddle/block in the way, change the velocity and remove
      ! the block. Repeat for the new direction until free path direction is found.
      ! It seems that three passes at the maximum should be enough. The fourth pass
      ! just verifies that this assumption is correct.
      !
      ZLOOP: do z=1,4
      vz = v

        ! Test the corner where the ball is heading
        i = v(1)
        j = v(2)
        ngb_hor = dsp0(i,0)
        ngb_ver = dsp0(0,j)
        ngb_dia = dsp0(i,j)
        is_hor = blocking(ngb_hor)
        is_ver = blocking(ngb_ver)
        is_dia = blocking(ngb_dia)
        call check_corner(ngb_hor, ngb_ver, ngb_dia, is_hor, is_ver, is_dia, swap_v)
        where(swap_v) v = -v
        if (is_hor) call break(dsp0(i,0))
        if (is_ver) call break(dsp0(0,j))
        if (is_dia) call break(dsp0(i,j))

        select case(z)
        case(1)
          continue
        case(2)
          if (any(vz/=v)) print *,'Double change of direction'
        case(3)
          if (any(vz/=v)) print *,'Triple change of direction'
        case(4)
          if (any(vz/=v)) error stop 'Predict_ball - ball is locked'
        end select
        if (all(v==vz)) exit
      end do ZLOOP

      ! Next position
      xnext0 = x + v
      if (present(xnext)) xnext = xnext0

      ! Paddle move?
      if (this%ppos(1) > xnext0(1)) then
        move = -1
      elseif (this%ppos(1) < xnext0(1)) then
        move = 1
      else
        move = 0
      end if

      ! To debug, print  the surrounding area
      900 format("|",a,"|",a,"|",a,"|")
      associate(a => x(1), b=> x(2), dsp => this%dsp)
      print 900, dsp(a-1,b-1), dsp(a,b-1), dsp(a+1,b-1)
      print 900, dsp(a-1,b), dsp(a,b), dsp(a+1,b)
      print 900, dsp(a-1,b+1), dsp(a,b+1), dsp(a+1,b+1)
      print '("Velocity before ",i0,1x,i0," after ",i0,1x,i0)',v0,v
      end associate
      print *
      print 900, dsp0(-1,-1), dsp0(0,-1), dsp0(+1,-1)
      print 900, dsp0(-1, 0), dsp0(0, 0), dsp0(+1, 0)
      print 900, dsp0(-1,+1), dsp0(0,+1), dsp0(+1,+1)

    contains
      logical function blocking(ch)
        character(len=1), intent(in) :: ch
        if(ch == TILES(ID_WALL) .or. ch == TILES(ID_BLOCK) .or. &
           ch == TILES(ID_PADDLE)) then
          blocking = .true.
        else if (ch == TILES(ID_EMPTY)) then
          blocking = .false.
        else
          print *, 'unexpected char ', ch
        end if
      end function

      subroutine break(ch)
        character(len=1), intent(inout) :: ch
        if (ch == TILES(ID_BLOCK)) ch = TILES(ID_EMPTY)
      end subroutine

    end subroutine



    subroutine check_corner(ngb_hor, ngb_ver, ngb_dia, is_hor, is_ver, is_dia, swap_v)
      character(len=1), intent(in) :: ngb_hor, ngb_ver, ngb_dia
      logical, intent(in)  :: is_hor, is_ver, is_dia
      logical, intent(out) :: swap_v(2)
!
! There are three ngb - pixels that are tested. They can be "blocking" or "empty", so
! there are 2^3 = 8 combinations (I...VIII)
!
      swap_v = .false.

      ! CASE (I+II)
      ! # #        # .                                     # # .
      ! # o   or   . o       ---> bounce both directions   # 2 .
      !                                                    . . 13
      if (is_dia .and. (is_hor .eqv. is_ver)) then
        swap_v = .true.

      ! CASE (III)
      ! . .                                                3 . .
      ! . o                  ---> no velocity change       . 2 .
      !                                                    . . 1
      else if (.not. is_dia .and. .not. is_hor .and. .not. is_ver) then
        continue

      ! CASE(IV)
      ! . #
      ! # o                  ---> same as CASE I/II
      !
      else if (is_hor .and. is_ver .and. .not. is_dia) then
        swap_v = .true.

      ! CASE(V) + CASE(VI)
      ! # #     . #                                       # # ,
      ! . o  or . o          ---> bounce vertically       . 2 .
      !                                                   3 . 1
      else if (is_ver) then
        swap_v(2) = .true.

      ! CASE(VII) + CASE(VIII)
      ! # .     . .                                       # . 3
      ! # o  or # o          --> bounce horizontally      # 2 .
      !                                                   . . 1
      else if (is_hor) then
        swap_v(1) = .true.

      else
        error stop 'logical error'
      end if
    end subroutine check_corner



! ===================================================================================
! Not needed anymore, but was useful for debugging and figuring out the ball bouncing
! mechanics
! ===================================================================================



    subroutine Run_clone(this, move, x0)
      type(cwd_t), intent(in) :: this
      integer, intent(out) :: x0(2), move
!
! Run clone of current state to predict ball position in its next move.
!
      type(cwd_t) :: clone
      integer :: state
      integer, allocatable :: outbuf(:)

      x0 = -99
      call this % Clone(clone)
      call clone % Set_inbuf(0)  ! provide input...
      do
        ! run and wait for the output that plots the ball
        call clone % Run(state)
        outbuf = clone % Get_outbuf()
        if (outbuf(3)==ID_BALL) then
          x0(1) = outbuf(1)
          x0(2) = outbuf(2)
          exit
        end if

        ! alternative exit
        if (state==SINBUF_EMPTY .or. state==SHALT) exit
      end do

      ! Use "x0" prediction to move the paddle in the real game
      if (this%ppos(1) > x0(1)) then
        move = -1
      elseif (this%ppos(1) < x0(1)) then
        move = 1
      else
        move = 0
      end if
   print *, 'Clone simulation leaving (prediction is)', x0
    end subroutine Run_clone

  end module day1913_mod
