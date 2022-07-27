  module day1922_mod
    implicit none
    integer, parameter :: IXB = selected_int_kind(36)

    type, public :: cards_t
      integer(IXB) :: start = 0_IXB
      integer(IXB) :: step  = 1_IXB
      integer(IXB) :: n = 0_IXB
    contains
      generic :: operator(.com.) => combine
      procedure, private :: combine
      procedure :: pick_card
      procedure :: Print => cards_print
      procedure :: deal_ns, cut, deal_wi, deal_file
      procedure :: findloc => cards_findloc
    end type cards_t
    interface cards_t
      module procedure cards_new
    end interface

    integer(IXB), parameter :: SMALL_DECK=10007
    integer(IXB), parameter :: BIG_DECK=119315717514047_IXB
    integer(IXB), parameter :: NREP    =101741582076661_IXB

  contains

    function cards_new(n) result(this)
      type(cards_t)            :: this
      integer(IXB), intent(in) :: n
      this%n = int(n, kind=IXB)
    end function



    function pick_card(this, k) result(val)
      class(cards_t), intent(in) :: this
      integer(IXB), intent(in)   :: k
      integer(IXB)               :: val
!
! Pick k-th card from the card deck
!
      integer(IXB) :: k0
      k0 = k
      if (k<0) k0 = this%n-abs(k) 
      val = mod(k0 * this%step + this%start, this%n)
    end function



    subroutine cards_print(this)
      class(cards_t), intent(in) :: this
      integer(IXB) :: i
      if (this%n<100) then
        write(*,'(20(i0,1x))') (this%pick_card(i), i=0, this%n-1)
      else
        write(*,'("Deck starts at ",i0," with step ",i0," of ",i0," cards.")')&
          this%start, this%step, this%n
      end if
    end subroutine



    subroutine deal_ns(this)
      class(cards_t), intent(inout) :: this
      integer(IXB) :: val
      if (this%step<0) error stop 'deal_ns precheck failed'
      val = this % pick_card(-1_IXB)
      this % start = val
      this % step = (this%n - this%step)
      if (this%step<0) error stop 'deal_ns postheck failed'
    end subroutine



    subroutine cut(this, j)
      class(cards_t), intent(inout) :: this
      integer, intent(in)      :: j
      this % start = this % Pick_card(int(j,kind=IXB))
    end subroutine 



    subroutine deal_wi(this, k)
      class(cards_t), intent(inout) :: this
      integer, intent(in)           :: k

      integer(IXB) :: next_card, w, w1, i2, i3

      if (this%step<0) error stop 'deal_wi precheck failed'

      ! first card remains the same
      ! what is the next card?
      i3 = 0
      i2 = 1
      do
        w = (this%n-i3)/k
        i3 = mod(i3+(w+1)*k, this%n)
        i2 = i2 + w+1
        if (i3==1) exit
      end do
      i2=i2-1

      next_card = this%Pick_card(i2)
      if (next_card > this%start) then
        this % step = next_card - this % start
      else 
        this % step = next_card+this%n - this%start
      end if
      if (this%step<0) error stop 'deal_wi postcheck failed'
    end subroutine



    function cards_findloc(this,val) result(ind)
      class(cards_t), intent(in) :: this
      integer, intent(in)        :: val
      integer                    :: ind

      integer(IXB) :: i
      ind = -1
      do i=0, this%n-1
        if (this % Pick_card(i)==val) then
          ind = i
          exit
        end if
      end do
      if (ind==-1) error stop 'cards_findloc - not found'
    end function



    subroutine deal_file(deck, file)
      class(cards_t), intent(inout) :: deck
      character(len=*), intent(in)  :: file

      integer :: fid, ios, n
      character(len=1000) :: line, w1, w2, w3

      open(newunit=fid, file=file, status='old')

      do
        read(fid,'(a)',iostat=ios) line
        if (ios /= 0) exit

        if (line(1:3)=='cut') then
          read(line,*) w1, n
          call deck % Cut(n)

        else if (line(1:6)=='deal w') then
          read(line,*) w1, w2, w3, n
          call deck % Deal_wi(n)

        else if (line(1:6)=='deal i') then
          call deck % Deal_ns()

        else
          error stop 'uknown deal technique'
        end if
      end do
      close(fid)

    end subroutine



    function findloc2(arr,val) result(ind)
      integer, intent(in) :: arr(:), val
      integer :: ind, i

      ind = 0
      do i = 1, size(arr)
        if (arr(i)==val) then
          ind = i
          exit
        end if
      end do

    end function



    function combine(a,b) result(res)
      class(cards_t), intent(in) :: a, b
      type(cards_t)              :: res
      res % n = a % n
      if (res % n /= b % n) error stop 'deck size must be the same'
      res % start = mod(b%start * a%step + a%start, a%n)
      res % step = mod(a%step * b%step, a%n)
    end function

  end module day1922_mod
