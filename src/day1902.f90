!
! Day 2: 1202 Program Alarm
!
  module day1902_mod
    implicit none

    type computer_t
      integer, allocatable :: mem(:)
      integer, allocatable :: rom(:)
      integer :: ptr
      integer :: n
    contains
      procedure :: init, setinput
      procedure :: reset
      procedure :: run
      procedure :: step
    end type computer_t

    integer, parameter :: TARGET_RESULT=19690720

    integer, parameter :: IADD=1, IMUL=2, IEND=99, STATE_ERROR=-2, STATE_END=-1, STATE_RUNNING=0

  contains

    subroutine init(this, mem0)
      class(computer_t), intent(out) :: this
      integer, intent(in) :: mem0(:)
      integer :: n
      n = size(mem0)
      allocate(this%mem(0:n-1))
      allocate(this%rom(0:n-1))
      this%mem(0:n-1) = mem0
      this%rom(0:n-1) = mem0
      this%ptr = 0
      this%n = n
    end subroutine



    subroutine reset(this)
      class(computer_t), intent(inout) :: this
      this%mem = this%rom
      this%ptr = 0
    end subroutine



    subroutine setinput(this,inp)
      class(computer_t), intent(inout) :: this
      integer, intent(in) :: inp(2)
      this%mem(1) = inp(1)
      this%mem(2) = inp(2)
    end subroutine setinput



    subroutine run(this)
      class(computer_t), intent(inout) :: this
      integer :: ierr

      do
        call this%step(ierr)

       !print '("Position: ",i0," return ",i0)', this%ptr, ierr
       !print '(8(i9,1x))', this%mem
        if (ierr /= STATE_RUNNING) exit
      end do
    end subroutine



    subroutine step(this,ierr)
      class(computer_t), intent(inout) :: this
      integer, intent(out) :: ierr

      integer :: ad(3), pt(3), op, res

      op = this%mem(this%ptr)
      select case(op)
      case(IEND)
        ierr = STATE_END
        return
      case default
        ierr = STATE_ERROR
        print *, 'error instruction at positiion ',this%ptr
        return
      case(IADD, IMUL)
        ierr = STATE_RUNNING
      end select

      if (this%ptr+3 > this%n) error stop 'going behing memory'
      pt = this%ptr + [1, 2, 3]
      ad = this%mem(pt)
      if (any(ad<0 .or. ad>this%n)) error stop 'going behind memory'
      if (op==IADD) then
        res = this%mem(ad(1)) + this%mem(ad(2))
      else if (op==IMUL) then
        res = this%mem(ad(1)) * this%mem(ad(2))
      end if
      this%mem(ad(3)) = res
      this%ptr = this%ptr + 4
    end subroutine 


  end module day1902_mod
