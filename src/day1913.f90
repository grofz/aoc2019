  module day1913_mod
    use kinds_m, only : I8B
    use intcode_mod, only : computer_t, SOUTBUF_FULL, SINBUF_EMPTY, SHALT
    implicit none

    ! monitor size
    integer, parameter :: SCREEN_X = 43, SCREEN_Y = 22

    ! 0=empty, 1=wall, 2=block, 3=paddle, 4=ball
    character(len=1), parameter :: &
       TILES_CH(0:4) = [ ' ', 'W', '#', '_', 'O']
    integer, parameter :: ID_BALL=4, ID_WALL=1, ID_PADDLE=3, ID_BLOCK=2, ID_EMPTY=0

    ! CWD as "computer with display"
    type, public, extends(computer_t) :: cwd_t 
      character(len=1), allocatable :: dsp(:,:)
      integer :: score = 0
      integer :: bpos(2)=-1, bpos0(2)=-1, bpos_pred0(2)=-1
      integer :: ppos(2)=-1
    contains
      procedure :: Init_cwd
      procedure :: Display
      procedure :: Play, Process_output, Predict_ball
    end type 
  contains

    subroutine Run_clone(this, x0, move)
      type(cwd_t), intent(in) :: this
      integer, intent(out) :: x0(2), move
!
! Run clone of current state to predict ball position in the
! next move (This is last resort - I could not figure out bouncing rules)
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



    subroutine Init_cwd(this, file, free_token)
      class(cwd_t), intent(out)     :: this
      character(len=*), intent(in)  :: file
      logical, intent(in), optional :: free_token

      ! prepare intcode interpreter
      call this % Load_from_file(file)
      call this % Reset(1,3)
      if (present(free_token)) then
        ! to play the game, instruction must be modified
        if (free_token) call this % overwrite(0,2)
      end if

      ! initialize the monitor
      allocate(this % dsp(0:SCREEN_X, 0:SCREEN_Y))
      this % dsp = TILES_CH(0)
    end subroutine



    subroutine Display(this)
      class(cwd_t), intent(inout) :: this
      integer :: i, j, sx, sy
      character(len=1) :: sval
!
! Intcode redraws ball position before I can read it.
! Use saved position instead. Return monitor to its current state
! before exit
!
      sval = '?'
      if (.not. any(this%bpos<0)) then
        sx = this%bpos(1)
        sy = this%bpos(2)
        sval = this%dsp(sx,sy)
        this%dsp(sx,sy) = '@'
      end if

      do j=0, SCREEN_Y
        do i=0, SCREEN_X
          write(*,'(a1)',advance='no') this % dsp(i,j)
        end do
        write(*,*)
      end do
      write(*,'("Score ",i0,4(a,1x,i0,", ",i0))')&
        this % score, '  Ball ',this%bpos, &
        ' exp ', this%bpos_pred0, '  Velo ',this%bpos-this%bpos0, &
        '  Paddle ', this % ppos
      write(*,*) ! empty line

      if (sval /= '?') this%dsp(sx,sy) = sval
    end subroutine 



    subroutine Play(this, score)
      class(cwd_t), intent(inout) :: this
      integer, intent(out) :: score

      integer :: status, inpkey, status_copy, x0(2), inpauto 
      integer :: inpfut, xfut(2)

      status = 0
      do
        if (status==SHALT) exit
        call this % Run(status)

        select case(status)
        case (SOUTBUF_FULL, SHALT)  
          call this % Process_output
          cycle
        case (SINBUF_EMPTY)
          ! Two methods to predict ball postion in future step
          ! (the second one wotks only for half of puzzle so far)
          call Run_clone(this, xfut, inpfut)
          call this % Predict_ball(x0, inpauto)

          ! Diagnostic messages to debug errors
          if (inpfut /= inpauto) then
            print *, 'Incosistent: (algorithm/virtual)',inpauto, inpfut
          end if
          call this % Display()
          if (this%bpos_pred0(1)/=-1) then ! previous prediction exist
            if (this%bpos_pred0(1)/=this%bpos(1)) then
              print '("Algoithm error: predicted: ",i0,1x,i0,"  actual ",i0,1x,i0)', &
              this%bpos_pred0, this%bpos
            else if (any(this%bpos_pred0/=this%bpos)) then
              print '("Algoithm y-axis error: ",i0,1x,i0,"  actual ",i0,1x,i0)', &
              this%bpos_pred0, this%bpos
            end if
          end if

         !if (inpauto <= 1 .and. inpauto >= -1 .and. &
         !     this%ppos(1)==this%bpos(1)) then
          if ( inpauto <= 1 .and. inpauto >= -1 ) then

            ! WHAT MWTHOD FOR AUTOCONTROL?
            !call this % Set_inbuf(inpauto)
            call this % Set_inbuf(inpfut)
            this%bpos_pred0 = x0 ! save for future use
          else
            ! manual for first two inputrs
            write(*,'(a)',advance='no') 'Input ?'
            read(*,*) inpkey
            call this % Set_inbuf(inpkey)
          end if
        case default
          error stop 'Play - unexpected status code'
        end select

      end do
      call this % Display()
      score = this%score

    end subroutine



    subroutine Process_output(this)
      class(cwd_t), intent(inout) :: this

      integer, allocatable :: dout(:)
      integer(I8B), allocatable :: dout128(:)

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
print *, dout(1), dout(2), '[ '//TILES_CH(dout(3))//']', &
        ' overwrites ['//this%dsp(dout(1),dout(2))//']'
        ! change display
        if (dout(3)<lbound(TILES_CH,1) .or. dout(3)>ubound(TILES_CH,1)) &
            error stop 'process_output - value out of char set'
        this%dsp(dout(1), dout(2)) = TILES_CH(dout(3))

        ! record position of ball and paddle
        if (dout(3)==4) then
          this%bpos0 = this%bpos
          this%bpos(1)=dout(1)
          this%bpos(2)=dout(2)
        else if (dout(3)==3) then
          this%ppos(1)=dout(1)
          this%ppos(2)=dout(2)
        end if
      end if
    end subroutine



    subroutine Predict_ball(this, xnext, move)
      class(cwd_t), intent(inout) :: this
      integer, intent(out) :: xnext(2), move
      integer :: v(2), v0(2), x(2)
      character(len=1) :: ngb_ver, ngb_hor, ngb_dia
      character(len=1), allocatable, save :: dsp0(:,:)
      logical :: is_ver, is_hor, is_dia ! obstacle in the direction
      integer :: i, j
      logical, save :: do_not_know

      xnext = 99
      move = 99
      v = this%bpos - this%bpos0
      v0 = v
      x = this%bpos
      if (.not. allocated(dsp0)) then
        dsp0 = this%dsp
        print *, 'prediction fails - no last image'
        return
      end if
      if (any(v==0)) then
        print *, 'prediction fails - zero velocity'
        return
      else if (any(abs(v)>1)) then
        print *, 'velocity too big - prediction fails'
        return
      end if

      i = x(1) + v(1)
      j = x(2) + v(2)
      ngb_hor = dsp0(i, x(2))
      ngb_ver = dsp0(x(1), j)
      ngb_dia = dsp0(i, j)
      is_hor = blocking(ngb_hor)
      is_ver = blocking(ngb_ver)
      is_dia = blocking(ngb_dia)

! Extra cae for top WALL
do_not_know = .false.
if (x(2)==1 .and. v(2)<0 ) then
!if (x(2)==1 .and. v(2)<0 .and. v(1)<0 .and. first_case) then
   v = -v
   print *, 'EXTRA CASE AT ROP WALL',  x(1)
   do_not_know = .true.
   !first_case = .false.
   goto 100
end if


      ! # #      # .
      ! # o   or . o
      !
      if (is_dia .and. (is_hor .eqv. is_ver)) then
        if (is_dia .and. .not. is_hor .and. .not. is_ver .and. &
                ngb_dia==TILES_CH(ID_BLOCK)) then
          !continue
          !print *, 'special case for hiting alone block'
          v = -v 
        else
          v = -v 
        end if

      ! . .
      ! . o
      !
      else if (.not. is_dia .and. .not. is_hor .and. .not. is_ver) then
        continue

      ! . #
      ! # o
      !
      else if (is_hor .and. is_ver .and. .not. is_dia) then
        v = -v ! or nothing ?
        continue

      ! # #     . #
      ! . o  or . o
      !    
      else if (is_ver) then
        v(2) = -v(2)

      ! # .     . .
      ! # o  or # o
      !    
      else if (is_hor) then
        v(1) = -v(1)

      else
        error stop 'logical error'
      end if
      100 continue


      ! Next position
      xnext = x + v
      dsp0 = this%dsp

      ! paddle move?
      if (this%ppos(1) > xnext(1)) then
        move = -1
      elseif (this%ppos(1) < xnext(1)) then
        move = 1
      else
        move = 0
      end if
 if (do_not_know) move = 0
 print *, 'next positioin ', xnext, 'sugest ',move, do_not_know



      ! To debug
      associate(x => x(1), y=> x(2))
      write(*,'(3(a1,1x,a1,1x/))') dsp0(x-1,y-1),dsp0(x,y-1),dsp0(x+1,y-1), &
      dsp0(x-1,y),dsp0(x,y),dsp0(x+1,y),dsp0(x-1,y+1),dsp0(x,y+1),dsp0(x+1,y+1) 
      print '("Velocity before ",i0,1x,i0," after ",i0,1x,i0)',v0,v
      end associate


    contains
      logical function blocking(ch)
        character(len=1), intent(in) :: ch
        if(ch == TILES_CH(ID_WALL) .or. ch == TILES_CH(ID_BLOCK) .or. &
           ch == TILES_CH(ID_PADDLE)) then
          blocking = .true.
        else if (ch == TILES_CH(ID_EMPTY)) then
          blocking = .false.
        else
          print *, 'unexpected char ', ch
        end if
      end function

    end subroutine

  end module day1913_mod
