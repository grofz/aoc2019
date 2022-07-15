!
! Advent of code 2019 - Intcode computer interpreter
!
  module intcode_mod
    use kinds_m, only : IXB => I8B, I4B
    use queue128_mod, only : queue_t
    use memory_mod, only : memory_t
    implicit none
    private

    type, public ::  computer_t
      private
      !integer, allocatable :: mem(:) 
      type(memory_t) :: mem               ! memory is separate class
      integer(IXB), allocatable :: rom(:) ! read-only mem (initial program copy)
      integer(IXB)              :: ptr=0  ! instruction pointer
      integer(IXB)              :: rbs=0  ! relative base
      integer              :: n      ! memory size (basic memory)
      type(queue_t) :: inbuf, outbuf ! input / output buffers
    contains
      ! load program from integer array or file (initialization)
      generic :: Load => computer_load64, computer_load128 ! @(arr)
      procedure :: Load_from_file          ! @(file)
      ! reset (clear memory, reset io-buffers size)
      procedure :: Reset => computer_reset ! @(inbuf_size, outbuf_size)
      ! run - run instructions until interuption
      procedure :: Run => computer_run     ! @(status)
      ! step - process one instruction (if not waiting for i/o operations)
      procedure :: Step => computer_step   ! @()  
      ! I/O BUFFERS
      ! set_inbuf - send single value to input buffer
      generic :: Set_inbuf => set_inbuf64, set_inbuf128      ! @(val-in)
      ! get_outbuf - export buffer to array and clean it
      generic :: Get_outbuf => get_outbuf128                 ! vals(:)=@()
      ! read_outbuf - remove just one value from buffer
      generic :: Read_outbuf => read_outbuf64, read_outbuf128 ! @(val-out)
      ! isempty_outbuf, isfull_inbuf - logical funs testing buffer state
      procedure :: Isempty_outbuf, Isfull_inbuf               ! @()
      procedure :: legacy_setinput, legacy_getoutput          
      ! PRIVATE GENERICS
      procedure, private :: computer_load64, computer_load128
      procedure, private :: set_inbuf64, set_inbuf128, read_outbuf64
      procedure, private :: get_outbuf64, get_outbuf128, read_outbuf128
    end type computer_t

    integer, parameter :: OPADD=1, OPMUL=2, OPIN=3, OPOUT=4, OPEND=99
    integer, parameter :: OPJMP_TRUE=5, OPJMP_FALSE=6, OPLT=7, OPEQ=8, OPADJ=9
    integer, parameter, public :: SHALT=-1, SRUNNING=0, SINBUF_EMPTY=-2, SOUTBUF_FULL=-3

    ! Debugging mode (DBGL is a sum of items)
    ! 0 or 1 : echo instruction pointer
    ! 0 or 2 : echo individual instructions
    ! 0 or 4 : echo I/O operations
    ! 0 or 8 : empty/full buffer complains
    integer, parameter :: DBGL=0 !15  

    ! Adjusting the computer
    integer, parameter :: IOBUF_SIZE = 3 ! this is the default buffer size

  contains

!
! Input / output operations with the computer
!
    subroutine set_inbuf64(this, val)
      class(computer_t), intent(inout) :: this
      integer, intent(in)              :: val
      call this%inbuf % Insert(int(val,kind=IXB))
    end subroutine

    subroutine set_inbuf128(this, val)
      class(computer_t), intent(inout) :: this
      integer(IXB), intent(in)              :: val
      call this%inbuf % Insert(val)
    end subroutine



    function get_outbuf64(this) result(vals)
      class(computer_t), intent(inout) :: this
      integer, allocatable :: vals(:)
      integer, allocatable :: vals128(:)
      vals128 = this%outbuf % Export()
      vals = vals128 ! auto conversion from 128 to 64?
      this%outbuf = queue_t(maxsize=IOBUF_SIZE)
    end function

    function get_outbuf128(this) result(vals)
      class(computer_t), intent(inout) :: this
      integer(IXB), allocatable :: vals(:)
      vals = this%outbuf % Export()
      this%outbuf = queue_t(maxsize=IOBUF_SIZE)
    end function



    subroutine read_outbuf64(this, val)
      class(computer_t), intent(inout) :: this
      integer, intent(out) :: val
      integer(IXB) :: val128
      call this%outbuf % Remove(val128)
      if (val128 > huge(val)) error stop 'read_outbuf - too big to convert'
      val = int(val128, kind=I4B)
    end subroutine

    subroutine read_outbuf128(this, val)
      class(computer_t), intent(inout) :: this
      integer(IXB), intent(out) :: val
      call this%outbuf % Remove(val)
    end subroutine



    logical function isempty_outbuf(this) result(isempty)
      class(computer_t), intent(in) :: this
      isempty = this%outbuf % Isempty()
    end function

    logical function isfull_inbuf(this) result(isfull)
      class(computer_t), intent(in) :: this
      isfull = this%inbuf % Isfull()
    end function



!
! Load program "mem0" to memory. Initialize computer.
!
    subroutine computer_load64(this, mem0)
      class(computer_t), intent(out) :: this
      integer, intent(in) :: mem0(:)
      this%n = size(mem0)
      if (allocated(this%rom)) deallocate(this%rom)
      allocate(this%rom(0:this%n-1))
      this%rom(0:this%n-1) = mem0 ! TODO - remove () left from =
      call this % Reset()
print '(a)', 'Computer_load64 called'
    end subroutine computer_load64

    subroutine computer_load128(this, mem0)
      class(computer_t), intent(out) :: this
      integer(IXB), intent(in) :: mem0(:)
      this%n = size(mem0)
      if (allocated(this%rom)) deallocate(this%rom)
      allocate(this%rom(0:this%n-1))
      this%rom(0:this%n-1) = mem0 ! TODO - remove () left from =
      call this % Reset()
print '(a)', 'computer_load128 called'
    end subroutine computer_load128



    subroutine computer_reset(this, inbuf_size, outbuf_size)
      class(computer_t), intent(inout) :: this
      integer, intent(in), optional :: inbuf_size, outbuf_size
!
! Reset memory to state in its ROM. Initialize computer. Empty IO buffers.
!
      integer :: inbuf0, outbuf0

      inbuf0 = IOBUF_SIZE
      outbuf0 = IOBUF_SIZE
      if (present(inbuf_size)) inbuf0 = inbuf_size
      if (present(outbuf_size)) outbuf0 = outbuf_size

      if (.not. allocated(this%rom) ) &
          error stop 'computer_reset - memory not initialized'
      if (size(this%rom) /= this%n) & 
          error stop 'computer_reset - memory allocation inconsistent'
      !this%mem = this%rom
      call this%mem % Init(this%rom)
      this%ptr = 0_IXB
      this%rbs = 0_IXB
      this%inbuf = queue_t(maxsize=inbuf0)
      this%outbuf = queue_t(maxsize=outbuf0)
    end subroutine computer_reset



    subroutine computer_run(this, istatus)
      class(computer_t), intent(inout) :: this
      integer, intent(out), optional :: istatus
!
! Run the program until interupt.
!
      integer :: istatus0

      do
        call this % Step(istatus0)
        if (istatus0 /= SRUNNING) exit
      end do
      if (present(istatus)) istatus = istatus0
!print '(a,i2)', 'Computer stops. Exit code ',istatus0

    end subroutine computer_run



    subroutine computer_step(this, ierr)
      class(computer_t), intent(inout) :: this
      integer, intent(out) :: ierr
!
! Process single instruction.
!
      integer :: i, imod(3), jmp, op
      integer(IXB) :: val(3), ad(3), res, tmp_io, op128

      if (num2bit(DBGL,1)) print '("Pointer at ",i4," op-value ",i0)', &
          this%ptr, this%mem%Read(this%ptr)

      ! Extract parameter mode from the instruction
      op128 = this%mem%Read(this%ptr)
      if (op128 > huge(op)) error stop 'computer_step - op value is 128bit number'
      op = int(op128, kind=I4B)
      do i=3,1,-1
        imod(i) = op/10**(i+1)
        op = mod(op, 10**(i+1))
      end do

      ! Identify instruction. Can we proceed?
      select case(op)
      case default 
        print  '("Pointer at ",i0," uknown instruction ",i0)', this%ptr, this%mem%Read(this%ptr)
        error stop 'computer_step - invalid instruction'

      case(OPEND)
        ierr = SHALT
        if (num2bit(DBGL,2)) print '("Halt instruction")'
        goto 999

      case(OPIN)
        if (this%inbuf % Isempty()) then
          if (num2bit(DBGL,4)) print '(a)', 'Input buffer is empty.'
          ierr = SINBUF_EMPTY
          goto 999
        else
          jmp = 2 ! one parameter instruction
        end if

      case(OPOUT)
        if (this%outbuf % Isfull()) then
          if (num2bit(DBGL,4)) print '(a)', 'Output buffer is full'
          ierr = SOUTBUF_FULL
          goto 999
        else
          jmp = 2 ! one parameter instruction
        end if

      case(OPADJ)
        jmp = 2 ! one parameter instruction

      case(OPJMP_TRUE, OPJMP_FALSE)
        jmp = 3 ! two parameter instructions

      case(OPADD, OPMUL, OPLT, OPEQ)
        jmp = 4 ! three parameters instructions

      end select
      ierr = SRUNNING

      ! Get arguments acording to the mode (0-address, 1-immediate, 2-relative)
      if (this%ptr+jmp-1 > this%n-1) error stop 'computer_step - reaching behind allocated memory'
      do i = 1, 3
        if (i > jmp-1) exit
        ad(i)  = this%mem%Read(this%ptr+i)
        select case(imod(i))
        case(0) ! address value
          !if (ad(i)<0 .or. ad(i)>this%n-1) error stop 'computer_step - address value out of memory'
          val(i) = this%mem%Read(ad(i))
        case(1) ! immediate mode
          val(i) = ad(i)
        case(2) ! relative mode
          !val(i) = this%mem%Read(ad(i)+this%rbs)
          ad(i) = ad(i) + this%rbs
          val(i) = this%mem%Read(ad(i))
        case default
          error stop 'computer_step - invalid mode'
        end select
      end do

      if (jmp==4 .and. imod(3) == 1) error stop 'Warning OP 3rd parameter is wrong mode'

      ! It should be possible to process the instruction
      select case(op)
      case(OPADD)
        res = val(1) + val(2)
        call this%mem%Write(ad(3), res)
        if (num2bit(DBGL,2)) &
            print '("Add ",i0," (",i0") and ",i0," (",i0,") and store to ",i0," (",i0,")")', &
            ad(1), val(1), ad(2), val(2), ad(3), res

      case(OPMUL)
        res = val(1) * val(2)
        call this%mem%Write(ad(3), res)
        if (num2bit(DBGL,2)) &
            print '("Mul ",i0," (",i0") and ",i0," (",i0,") and store to ",i0," (",i0,")")', &
            ad(1), val(1), ad(2), val(2), ad(3), res

      case(OPLT)
        res = 0
        if (val(1) < val(2)) res = 1
        call this%mem%Write(ad(3), res)
        if (num2bit(DBGL,2)) &
            print '("Is ",i0," (",i0") less than ",i0," (",i0,") and store to ",i0," (",i0,")")', &
            ad(1), val(1), ad(2), val(2), ad(3), res

      case(OPEQ)
        res = 0
        if (val(1) == val(2)) res = 1
        call this%mem%Write(ad(3), res)
        if (num2bit(DBGL,2)) &
            print '("Is ",i0," (",i0") equal to ",i0," (",i0,") and store to ",i0," (",i0,")")', &
            ad(1), val(1), ad(2), val(2), ad(3), res

      case(OPIN)
        call this%inbuf % Remove(tmp_io)
        call this%mem%Write(ad(1), tmp_io)
        if (imod(1)==1) error stop 'Warning IN parameter is wrong mode'
        if (num2bit(DBGL,3)) &
            print '("IO reading to address ",i0," (",i0,"). Input buffer items left ",i0)', &
            ad(1), tmp_io, this%inbuf % Size()

      case(OPOUT)
        tmp_io = val(1)
        call this%outbuf % Insert(tmp_io)
        if (num2bit(DBGL,3)) &
            print '("IO writing ",i0," (",i0,"). Output buffer items now ",i0)', &
            ad(1), tmp_io, this%outbuf % Size()

      case(OPJMP_TRUE)
        if (num2bit(DBGL,2)) &
          print '("Jump if ",i0," (",i0,") is true to position ",i0," (",i0,") ?")', &
          ad(1),val(1),ad(2),val(2)
        
        if (val(1) /= 0) then ! jump to (second parameter) if true (first parameter)
          if (val(2)<0 .or. val(2)>this%n-1) error stop 'jump instruction leading outside memory'
          this % ptr = val(2)
          jmp = 0
          if (num2bit(DBGL,2)) print '("...jumped")'
        end if

      case(OPJMP_FALSE)
        if (num2bit(DBGL,2)) &
          print '("Jump if ",i0," (",i0,") is false to position ",i0," (",i0,") ?")', &
          ad(1),val(1),ad(2),val(2)
        if (val(1) == 0) then ! jump to (second parameter) if false (first parameter)
          if (val(2)<0 .or. val(2)>this%n-1) error stop 'jump instruction leading outside memory'
          this % ptr = val(2)
          jmp = 0
          if (num2bit(DBGL,2)) print '("...jumped")'
        end if

      case(OPADJ)
        this%rbs = this%rbs + val(1)
        if (num2bit(DBGL,2)) &
          print '("Adjust base by ",i0," (",i0,")  Base now ",i0)', &
          ad(1),val(1),this%rbs

      case default
        error stop 'computer_step - invalid op'
      end select
      this % ptr = this % ptr + jmp

      999 continue ! jump here if not possible to process the instruction
    end subroutine computer_step



    logical function num2bit(val, bit) result(ison)
      integer, intent(in) :: val, bit
!
! Helper function to define what diagnostic messages to print
!
      integer :: val0, i, ison0
      integer, parameter :: MAXBIT=8
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
! Reading code from file and loading it into memory
! =================================================

    subroutine load_from_file(this, file)
      use parse_mod, only : read_strings, string_t, split
      class(computer_t), intent(out) :: this
      character(len=*), intent(in) :: file

      type(string_t), allocatable :: lines(:), tokens(:)
      integer(IXB), allocatable :: iarr(:)

      lines = read_strings(file)
      if (size(lines)/=1) &
        error stop 'computer - load_from_file - input file not a single line'
      call split(lines(1)%str,',',tokens)
      iarr = tokens % To_int128()
      call this % Load(iarr)
      print '("Intcode ",i0," instructions loaded")', this%n
      print '("[... ",5(i6,1x)," ]")', iarr(max(1,this%n-4):)
    end subroutine load_from_file



! =================================================
! Just for the compatibility with version 1 (Day 2)
! =================================================

    subroutine legacy_setinput(this,inp)
      class(computer_t), intent(inout) :: this
      integer, intent(in) :: inp(2)
      !this%mem(1) = inp(1)
      !this%mem(2) = inp(2)
      call this%mem%write(1, inp(1))
      call this%mem%write(2, inp(2))
    end subroutine legacy_setinput

    integer function legacy_getoutput(this)
      class(computer_t), intent(in) :: this
      legacy_getoutput = this%mem%Read(0)
    end function 

  end module intcode_mod
