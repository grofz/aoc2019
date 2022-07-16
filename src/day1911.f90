!
! Board class
!
  module day1911_mod
    use tree_m, only : rbtr_t, tree_mold, DAT_KIND
    use abstract_container, only : ERR_CONT_OK, ERR_CONT_ISNOT, ERR_CONT_IS
    implicit none
!   private

    integer, parameter :: BLACK = 0, WHITE = 1
    integer, parameter :: DEFAULT_COLOR = BLACK

    integer, parameter :: COMM_PAINT_BLACK=0, COMM_PAINT_WHITE=1
    integer, parameter :: COMM_LEFTTURN=0, COMM_RIGHTTURN=1
    integer, parameter, public :: HEADING_UP=0, HEADING_LEFT=3, &
            HEADING_RIGHT=1, HEADING_DOWN=2 

    type hash_t
      integer :: xy(2), val ! position, color
    end type

    ! For keeeping the state of the hull panels and robot position
    type, public :: board_t
      type(rbtr_t) :: map
      integer :: rob_xy(2)=[0,0], rob_hdg=HEADING_UP
    contains
       procedure :: Init => board_init ! initialization is COMPULSORY
       procedure :: Getcolor   ! color=@(xy*) 
       procedure :: Paintcolor ! @(xy*,color)
       procedure :: Setrobot   ! @(xy,heading)
       procedure :: Forward, Maketurn ! @(), @(iturn)
       !procedure :: Getrobot   ! @(xy,heading)
       procedure :: Getlimits  ! (x0,x1,y0,y1)=@()
       ! * - if "xy" ommited, then robots position is used
       procedure :: Print => board_print
       final :: board_final
    end type board_t

  contains
    subroutine board_final(this)
       type(board_t), intent(inout) :: this
       call this%map % Removeall()
     end subroutine board_final



     subroutine board_init(this)
       class(board_t), intent(out) :: this
       this % map = rbtr_t(hash_compare)
     end subroutine board_init



     integer function Getcolor(this, xy)
       class(board_t), intent(in) :: this
       integer, intent(in), optional :: xy(2)
       if (present(xy)) then
         getcolor = hash_get(this%map, xy)
       else
         getcolor = hash_get(this%map, this%rob_xy)
       end if
     end function



     subroutine Paintcolor(this, xy, color)
       class(board_t), intent(inout) :: this
       integer, intent(in), optional :: xy(2)
       integer, intent(in)           :: color
       if (present(xy)) then
         call hash_update(this%map, xy, color)
       else
         call hash_update(this%map, this%rob_xy, color)
       end if
     end subroutine



     subroutine Setrobot(this, xy, heading)
       class(board_t), intent(inout) :: this
       integer, intent(in) :: xy(2), heading
       this % rob_xy = xy
       this % rob_hdg = heading
     end subroutine



     subroutine Forward(this)
       class(board_t), intent(inout) :: this
       select case(this % rob_hdg)
       case(HEADING_UP)
         this % rob_xy(2) = this % rob_xy(2) + 1
       case(HEADING_DOWN)
         this % rob_xy(2) = this % rob_xy(2) - 1
       case(HEADING_LEFT)
         this % rob_xy(1) = this % rob_xy(1) - 1
       case(HEADING_RIGHT)
         this % rob_xy(1) = this % rob_xy(1) + 1
       case default
         error stop 'board: Forward - heading is invalid'
       end select
     end subroutine



     subroutine Maketurn(this, turn)
       class(board_t), intent(inout) :: this
       integer, intent(in)           :: turn
       associate(h => this % rob_hdg)
       select case(turn)
       case(COMM_LEFTTURN)
         h = mod(h+1, 4)
       case(COMM_RIGHTTURN)
         h = 3 - mod(4-h, 4)
       case default
         error stop 'board: Maketurn - invalid direction'
       end select
       end associate
     end subroutine


     
     function Getlimits(this) result(lims)
       class(board_t), intent(in) :: this
       integer :: lims(4)

       integer(DAT_KIND), allocatable :: handle(:)
       type(hash_t) :: adat
       integer :: ierr

       lims = 0
       if (this%map % Isempty()) return
       lims(1) =  huge(lims)
       lims(2) = -huge(lims)
       lims(3:4) = lims(1:2)
       call this%map % Resetcurrent(handle)
       do
          adat = transfer(this%map % NextRead(handle, ierr), adat)
          if (ierr /= 0) exit

          ! We can use value in adat
          if (adat%xy(1) < lims(1)) lims(1) = adat%xy(1)
          if (adat%xy(2) < lims(3)) lims(3) = adat%xy(2)
          if (adat%xy(1) > lims(2)) lims(2) = adat%xy(1)
          if (adat%xy(2) > lims(4)) lims(4) = adat%xy(2)
        enddo
     end function

     

     subroutine board_print(this, charset, limits)
       class(board_t), intent(in) :: this
       character(len=*), intent(in) :: charset
       integer, intent(in), optional :: limits(4)

       character(len=1), parameter :: UNKNOWN_CH='?'
       integer :: limits0(4), xoffset, yoffset, nx, ny, i, ierr, x1, y1
       integer :: ch0, ch1
       character(len=:), allocatable :: lines(:)
       integer(DAT_KIND), allocatable :: handle(:)
       type(hash_t) :: adat

       ! Automatic or manual limit
       if (present(limits)) then
         limits0 = limits
       else
         limits0 = this%Getlimits()
       end if
       nx = limits0(2)-limits0(1)+1
       ny = limits0(4)-limits0(3)+1
       xoffset = 1 - limits0(1)
       yoffset = 1 - limits0(3)
       allocate(character(len=nx) :: lines(ny))
       lines = ''

       if (this%map % Isempty()) then
         print *, 'Warning - empty board'
         return
       end if
       call this%map % Resetcurrent(handle)
       ch0 = 0
       !ch1 = ch0 + len(charset)-1
       do
         adat = transfer(this%map % NextRead(handle, ierr), adat)
         if (ierr /= 0) exit

         associate(x=>adat%xy(1), y=>adat%xy(2))
         x1 = nx-x-xoffset+1
         y1 = ny-y-yoffset+1
         if (x1>=1 .and. x1<=nx .and. y1>=1 .and. y1<=ny) then
            associate(m => adat%val-ch0+1)
            if (m>=1 .and. m<=len(charset)) then
              lines(y1)(x1:x1) = charset(m:m)
            else
              lines(y1)(x1:x1) = UNKNOWN_CH
            end if
            end associate
         end if
         end associate
       end do

   do i=1,ny
     print '(a)', lines(i)
   end do

     end subroutine board_print



! ==================================
! MAP TO STORE THE STATE OF THE HULL
! ==================================

     pure integer function hash_compare(a, b) result(res)
      integer(DAT_KIND), intent(in) :: a(:), b(:)
  !
  ! user function for the TREE hash
  !
      type(hash_t) :: ahash, bhash
      ahash = transfer(a, ahash)
      bhash = transfer(b, bhash)

      if (ahash%xy(1) < bhash%xy(1)) then
        res = 1
      else if (ahash%xy(1) > bhash%xy(1)) then
        res = -1
      else
        if (ahash%xy(2) < bhash%xy(2)) then
          res = 1
        else if (ahash%xy(2) > bhash%xy(2)) then
          res = -1
        else
          res = 0
        end if
      end if
    end function hash_compare



    integer function hash_get(hash, adr) result(val)
    type(rbtr_t), intent(in) :: hash
    integer, intent(in) :: adr(2)
  !
  ! Ask, if "key" has been stored. Return "val" or "0" (no entry)
  !
      integer(DAT_KIND), allocatable :: handle(:)
      integer :: ierr
      type(hash_t) :: dat

      dat % xy = adr
      call hash % Find(transfer(dat,tree_mold), handle, ierr)
      select case(ierr)
      case(ERR_CONT_OK)
        dat = transfer(hash % Read(handle), dat)
        if (any(dat % xy /= adr)) error stop 'hash_get - wrong key returned'
        val = dat % val
      case(ERR_CONT_ISNOT)
        val = DEFAULT_COLOR
      case default
        error stop 'hash_get - uknown exit code'
      end select
    end function hash_get



    subroutine hash_update(hash, adr, val)
      type(rbtr_t), intent(inout) :: hash
      integer, intent(in) :: adr(2), val
  !
  ! Update pair "key - val" in hash, or store new one.
  !
      integer(DAT_KIND), allocatable :: handle(:)
      type(hash_t) :: olddat, newdat
      integer :: ierr

      olddat % xy = adr
      newdat % xy = adr
      newdat % val = val

      call hash % Find(transfer(olddat,tree_mold), handle, ierr)

      select case(ierr)
      case(ERR_CONT_OK)
        call hash % UpdateCurrent(handle, transfer(newdat,tree_mold))
      case(ERR_CONT_ISNOT)
        call hash % Add(transfer(newdat,tree_mold))
      case default
        error stop 'hash_update - uknown exit code'
      end select
    end subroutine hash_update



    subroutine hash_print(hash)
      type(rbtr_t), intent(in) :: hash
      integer(DAT_KIND), allocatable :: handle(:)

      integer :: ierr
      type(hash_t) :: adat

      ! Iterate all nodes
      if (.not. hash % Isempty()) then
        print *
        call hash % Resetcurrent(handle)
        do
          adat = transfer(hash % NextRead(handle, ierr), adat)
          if (ierr /= 0) exit

          ! We can use value in adat
          print *, 'Node = ', adat % xy, adat % val
          print *, ' ___ = ', hash % Printcurrentnode(handle)
        enddo
      else
        print *
        print *, 'Empty tree: '
      endif
      print *
    end subroutine hash_print


  end module day1911_mod
