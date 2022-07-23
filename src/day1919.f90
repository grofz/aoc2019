  module day1919_mod
    use intcode_mod, only : computer_t, SINBUF_EMPTY, SOUTBUF_READY, SHALT
    implicit none

    type, public :: scanner_t
      type(computer_t) :: ZX
    contains
      procedure :: Init
      procedure :: Scan
    end type

  contains

    subroutine Init(this, file)
      class(scanner_t), intent(out) :: this
      character(len=*), intent(in) :: file
      call this%ZX % Load_from_file(file)
    end subroutine



    function Scan(this, x, y) result(val)
      class(scanner_t), intent(inout) :: this
      integer, intent(in)             :: x, y
      integer                         :: val

      integer :: istat

      call this%ZX % Reset(2,1)
      call this%ZX % Set_inbuf(x)
      call this%ZX % Set_inbuf(y)
      call this%ZX % Run(istat)
      if (istat == SOUTBUF_READY) then
        call this%ZX % Read_outbuf(val)
      else
        error stop 'scan - unexpected istat'
      end if
      call this%ZX % Run(istat)
      if (istat /= SHALT) error stop 'scan - computer still running'
    end function



    subroutine test_beam(SC, cnt)
      type(scanner_t), intent(inout) :: SC
      integer, intent(out)           :: cnt

      integer, parameter :: NX = 50, NY = 50
      integer :: efield(0:NX-1, 0:NY-1), istat, i, j
      integer, allocatable :: elims(:,:)

      efield = 0
      do j=lbound(efield,2),ubound(efield,2)
      do i=lbound(efield,1),ubound(efield,1)
        efield(i,j) = SC % Scan(i,j)
      end do
      end do
      cnt = count(efield==1)

      print '(50(a1))', convert(efield)
      print '("Active pixels in 50x50 area ",i0)', cnt

    end subroutine test_beam



    subroutine track_beam(SC, ship_size, ans)
      type(scanner_t), intent(inout) :: SC
      integer, intent(in)            :: ship_size
      integer, intent(out)      :: ans

      integer :: y, x, LB_ship(2), RT_ship(2) 
      integer, allocatable :: elim(:,:)

      ! Left and right limit mark of the beam at position "Y"
      allocate(elim(2, 2000))
      elim = -1

      ! Jump forward where regular pattern of beam begins...
      y = 5
      do x = 1, 30
        if (SC % Scan(x,y)==1) exit
      end do

      ! ... and track the beam line by line (y-coordinate)
      elim(:,y) = x
      do
        call scan_line(SC, y, elim(1,y), elim(2,y))

        ! Left-top and right-bottom corners of the ship
        LB_ship = [elim(1,y), y]
        RT_ship = [LB_ship(1)+ship_size-1, LB_ship(2)-ship_size+1]

        ! Exit if ship fits
        if (RT_ship(2)>0) then
          if (RT_ship(1) <= elim(2,RT_ship(2)) ) exit
        end if

        ! Scan next line otherwise
        y = y + 1
        elim(:,y) = elim(:,y-1)
      enddo

      print '("Ship coordinates: X from ",i0," to ",i0,&
        &" and Y from ",i0," to ",i0)', &
        & lb_ship(1), rt_ship(1), lb_ship(2), rt_ship(2)

      ans = lb_ship(1)*10000 + rt_ship(2)
      print '("Answer (19/2) is ",i0)', ans
    end subroutine track_beam



    subroutine scan_line(SC, y, x0, x1)
      type(scanner_t), intent(inout) :: SC
      integer, intent(in)    :: y
      integer, intent(inout) :: x0, x1
      integer, parameter :: MAX_SCAN = 5

      integer :: i

      ! Move markers so they are at in the beam
      do i=1,MAX_SCAN
        if (SC % Scan(x0,y)==1) exit
        x0 = x0+1
      end do
      if (i==MAX_SCAN+1) error stop 'scan line - bean lost x0'
      do i=1,MAX_SCAN
        if (SC % Scan(x1,y)==1) exit
        x1 = x1+1
      end do
      if (i==MAX_SCAN+1) error stop 'scan line - bean lost x1'

      ! move left marker until end of beam
      do i=1,MAX_SCAN
        x0 = x0 - 1
        if (SC % Scan(x0,y)==0) exit
      end do
      if (i==MAX_SCAN+1) error stop 'scan line - bean lost left'
      x0 = x0+1

      ! move right marker until end of beam
      do i=1,MAX_SCAN
        x1 = x1 + 1
        if (SC % Scan(x1,y)==0) exit
      end do
      if (i==MAX_SCAN+1) error stop 'scan line - bean lost right'
      x1 = x1-1
      !print '("Calibration at line ",i0," [",i0,",",i0,"]  ",i0)', &
      ! y, x0, x1, x1-x0+1
    end subroutine



    elemental character(len=1) function convert(i)
      integer, intent(in) :: i
      select case(i)
      case(0)
        convert = '.'
      case(1)
        convert = '#'
      case default
        convert = '?'
      end select
    end function

  end module day1919_mod
