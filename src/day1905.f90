!
! Day 5: Sunny with a Chance of Asteroids
! (Version 2.0 of our computer from day 2)
!
  module day1905_mod
    implicit none

    type computer_t
      private
      integer, allocatable :: mem(:) ! working memory state
      integer, allocatable :: rom(:) ! read-only memory
      integer              :: ptr    ! instruction pointer
      integer              :: n      ! memory size
      integer :: inbuf, outbuf       ! input / output
    contains
      procedure :: Load => computer_load
      procedure :: Reset => computer_reset
      procedure :: Run => computer_run
      procedure :: Step => computer_step
      procedure :: Set_inbuf, Get_outbuf
      procedure :: setinput, getoutput
    end type computer_t

    integer, parameter :: OPADD=1, OPMUL=2, OPIN=3, OPOUT=4, OPEND=99
    integer, parameter :: OPJMP_TRUE=5, OPJMP_FALSE=6, OPLT=7, OPEQ=8
    integer, parameter :: STATE_ENDED=-1, STATE_RUNNING=0
    integer, parameter :: DBGL=2 ! debug-level (0, 1)

  contains

!
! Input / output operations with the computer
!
    subroutine set_inbuf(this, val)
      class(computer_t), intent(inout) :: this
      integer, intent(in)              :: val
      this % inbuf = val
    end subroutine

    integer function get_outbuf(this) result(val)
      class(computer_t), intent(in) :: this
      val = this % outbuf
    end function


    subroutine computer_load(this, mem0)
      class(computer_t), intent(out) :: this
      integer, intent(in) :: mem0(:)
!
! Load program "mem0" to memory. Initialize computer.
!
      this%n = size(mem0)
      if (allocated(this%rom)) deallocate(this%rom)
      if (allocated(this%mem)) deallocate(this%mem)
      allocate(this%rom(0:this%n-1))
      allocate(this%mem(0:this%n-1))
      this%rom(0:this%n-1) = mem0 ! TODO - remove () left from =
      call this % Reset()
    end subroutine computer_load



    subroutine computer_reset(this)
      class(computer_t), intent(inout) :: this
!
! Reset memory to state in its ROM. Initialize computer.
!
      if (.not. allocated(this%rom) .or. .not. allocated(this%mem)) &
          error stop 'computer_reset - memory not initialized'
      if (size(this%mem) /= this%n .or. size(this%rom) /= this%n) & 
          error stop 'computer_reset - memory allocation inconsistent'
      this%mem = this%rom
      this%ptr = 0
    end subroutine computer_reset





    subroutine computer_run(this)
      class(computer_t), intent(inout) :: this
!
! Run the program until stop.
!
      integer :: ierr

      do
        call this % Step(ierr)
!print '("Position: ",i0," return ",i0)', this%ptr, ierr
!print '(8(i9,1x))', this%mem
        if (ierr /= STATE_RUNNING) exit
      end do
    end subroutine computer_run



    subroutine computer_step(this, ierr)
      class(computer_t), intent(inout) :: this
      integer, intent(out) :: ierr
!
! Do one instruction.
!
      integer :: i, imod(3), jmp, val(3)
      integer :: ad(3), pt(3), op, res

      if (num2bit(DBGL,1)) print '("Pointer at ",i4," op-value ",i0)',this%ptr, this%mem(this%ptr)

      ! Extract parameter mode from the instruction
      op = this%mem(this%ptr)
      do i=3,1,-1
        imod(i) = op/10**(i+1)
        op = mod(op, 10**(i+1))
      end do

      ! Identify instruction
      select case(op)
      case default
        print  '("Pointer at ",i0," uknown instruction ",i0)', this%ptr, this%mem(this%ptr)
        error stop 'computer_step - invalid instruction'
      case(OPEND)
        ierr = STATE_ENDED
        if (num2bit(DBGL,2)) print '("Stop the program")'
        goto 999
      case(OPIN, OPOUT)
        jmp = 2 ! one parameter instructions
      case(OPJMP_TRUE, OPJMP_FALSE)
        jmp = 3 ! two parameter instructions
      case(OPADD, OPMUL, OPLT, OPEQ)
        jmp = 4 ! three parameters instructions
      end select
      ierr = STATE_RUNNING

      ! Get arguments acording to the mode (0-address, 1-immediate)
      if (this%ptr+jmp-1 > this%n-1) error stop 'computer_step - reaching behind allocated memory'
      do i = 1, 3
        if (i > jmp-1) exit
        ad(i)  = this%mem(this%ptr+i)
        select case(imod(i))
        case(0) ! address value
          if (ad(i)<0 .or. ad(i)>this%n-1) error stop 'computer_step - address value out of memory'
          val(i) = this%mem(ad(i))
        case(1) ! immediate mode
          val(i) = ad(i)
        case default
          error stop 'computer_step - invalid mode'
        end select
      end do

      !if (jmp==4 .and. imod(3) /= 0) print *,'Warning OP 3rd parameter is wrong mode'
      if (jmp==4 .and. imod(3) /= 0) error stop 'Warning OP 3rd parameter is wrong mode'

      select case(op)
      case(OPADD)
        res = val(1) + val(2)
        this%mem(ad(3)) = res
        if (num2bit(DBGL,2)) &
            print '("Add ",i0," (",i0") and ",i0," (",i0,") and store to ",i0," (",i0,")")', &
            ad(1), val(1), ad(2), val(2), ad(3), res

      case(OPMUL)
        res = val(1) * val(2)
        this%mem(ad(3)) = res
        if (num2bit(DBGL,2)) &
            print '("Mul ",i0," (",i0") and ",i0," (",i0,") and store to ",i0," (",i0,")")', &
            ad(1), val(1), ad(2), val(2), ad(3), res

      case(OPLT)
        res = 0
        if (val(1) < val(2)) res = 1
        this%mem(ad(3)) = res
        if (num2bit(DBGL,2)) &
            print '("Is ",i0," (",i0") less than ",i0," (",i0,") and store to ",i0," (",i0,")")', &
            ad(1), val(1), ad(2), val(2), ad(3), res

      case(OPEQ)
        res = 0
        if (val(1) == val(2)) res = 1
        this%mem(ad(3)) = res
        if (num2bit(DBGL,2)) &
            print '("Is ",i0," (",i0") equal to ",i0," (",i0,") and store to ",i0," (",i0,")")', &
            ad(1), val(1), ad(2), val(2), ad(3), res

      case(OPIN)
        this%mem(ad(1)) = this%inbuf
   if (imod(1)/=0) print *,'Warning IN parameter is wrong mode'
 print '("Input: Value ",i0," saved to address ",i0)', this%inbuf, ad(1)

      case(OPOUT)
        this%outbuf = val(1)
 if (imod(1)==0) then
 print '("Output: Value ",i0," got from address ",i0)', this%outbuf, ad(1)
 else
 print '("Output: Value ",i0," directly ",l1)', this%outbuf, ad(1)==this%outbuf
 endif

      case(OPJMP_TRUE)
        if (num2bit(DBGL,2)) &
          print '("Jump if ",i0," (",i0,") is true to position ",i0," (",i0,") ?")', &
          ad(1),val(1),ad(2),val(2)
        
        if (val(1) /= 0) then ! jump to (PAR 2)
          if (val(2)<0 .or. val(2)>this%n-1) error stop 'jump instruction leading outside memory'
          this % ptr = val(2)
          jmp = 0
          if (num2bit(DBGL,2)) print '("...jumped")'
        end if

      case(OPJMP_FALSE)
        if (num2bit(DBGL,2)) &
          print '("Jump if ",i0," (",i0,") is false to position ",i0," (",i0,") ?")', &
          ad(1),val(1),ad(2),val(2)
        if (val(1) == 0) then ! jump to (PAR 2)
          if (val(2)<0 .or. val(2)>this%n-1) error stop 'jump instruction leading outside memory'
          this % ptr = val(2)
          jmp = 0
          if (num2bit(DBGL,2)) print '("...jumped")'
        end if

      case default
        error stop 'computer_step - invalid op'
      end select
      this % ptr = this % ptr + jmp

      999 continue
    end subroutine computer_step



    logical function num2bit(val, bit) result(ison)
      integer, intent(in) :: val, bit
      integer :: val0, i, ison0
      integer, parameter :: MAXBIT=2
      val0 = val
      ison0 = 42
      do i=MAXBIT,bit,-1
        ison0 = val0 / 2**(i-1)
        val0 = mod(val0, 2**(i-1))
      end do
      if (ison0==0) then
        ison = .false.
      elseif (ison0==1) then
        ison = .true.
      else
        error stop 'num2bit - val is too high for current MAXBIT'
      end if
    end function num2bit
        



! =================================================
! Just for the compatibility with version 1 (Day 2)
! =================================================

    subroutine setinput(this,inp)
      class(computer_t), intent(inout) :: this
      integer, intent(in) :: inp(2)
      this%mem(1) = inp(1)
      this%mem(2) = inp(2)
    end subroutine setinput



    integer function getoutput(this)
      class(computer_t), intent(in) :: this
      getoutput = this%mem(0)
    end function

  end module day1905_mod
