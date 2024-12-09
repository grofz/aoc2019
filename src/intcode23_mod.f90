! A better intcode machine (new implementation)
!
! DONE:
! - memory as a map (via "list module")
!
! TODO:
! - input / output queues
! - actual opcodes

module intcode23_mod
  use iso_fortran_env, only : int32, int64
  use queue_mod
  use rbnode_mod, only : rbbasetree_t, rbnode_t, rbnode_freetree, rbnode_find, &
    rbnode_read, rbnode_insert, rbnode_update, rbnode_successor
  use common_mod, only : DATA_KIND, mold
  use parse_mod, only : read_strings, string_t, split

  implicit none
  private

  integer, parameter :: ADDRESS_KIND=int64, VALUE_KIND=int64 ! must be same
  public ADDRESS_KIND, VALUE_KIND

  type memory_t
    type(rbbasetree_t) :: tree
  contains
    procedure :: read => memory_read, write => memory_write
    procedure :: readfile => memory_readfile
    procedure :: copy => memory_copy, restore => memory_restore
    procedure :: size => memory_size, print => memory_print
    final :: memory_finalize
  end type memory_t
!public memory_t ! temporary for testing

  type memoryitem_t
    integer(ADDRESS_KIND) :: addr
    integer(VALUE_KIND) :: value
  end type

  type, public :: machine_t
    private
    type(memory_t) :: mem
    type(queue_t)  :: inp, out
    integer(ADDRESS_KIND) :: ip = 0, base = 0
  contains
    procedure :: load => machine_load
    procedure :: step => machine_step
    procedure :: pushinput => machine_pushinput
    procedure :: popoutput => machine_popoutput
    procedure :: readaddr => machine_readaddr
    procedure :: writeaddr => machine_writeaddr
  end type

  integer, public, parameter :: STAT_RUN=0, STAT_HALT=1, STAT_OUTPUT_READY=2, STAT_WAIT_INPUT=3

contains 

  ! ======
  ! Memory
  ! ======

  function memory_read(this, addr) result(value)
    class(memory_t), intent(in) :: this
    integer(ADDRESS_KIND), intent(in) :: addr
    integer(VALUE_KIND) :: value

    type(memoryitem_t) :: item
    type(rbnode_t), pointer :: found

    item%addr = addr
    found => rbnode_find(this%tree%root, transfer(item,mold), compare)
    if (.not. associated(found)) &
        error stop 'memory_read - address not defined'
    item = transfer(rbnode_read(found),item)
    value = item%value
    if (item%addr /= addr) error stop 'memory_read - internal error'
  end function memory_read


  subroutine memory_write(this, addr, value)
    class(memory_t), intent(inout) :: this
    integer(ADDRESS_KIND), intent(in) :: addr
    integer(VALUE_KIND), intent(in) :: value

    type(memoryitem_t) :: item
    type(rbnode_t), pointer :: found

    item%addr = addr
    item%value = value
    found => rbnode_find(this%tree%root, transfer(item,mold), compare)
    if (associated(found)) then ! update
      call rbnode_update(found, transfer(item,mold), compare)
    else                        ! insert new node
      call rbnode_insert(this%tree, rbnode_t(transfer(item,mold)), compare)
    end if
  end subroutine memory_write


  subroutine memory_readfile(this, file)
    class(memory_t), intent(inout) :: this
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:), tokens(:)
    type(memoryitem_t), allocatable :: memitems(:)
    integer(DATA_KIND), allocatable :: values(:)
    integer, allocatable :: starts(:)
    integer :: i, nsize

    if (associated(this%tree%root)) &
        error stop 'memory_readfile - memory is not empty'

    lines = read_strings(file)
    if (size(lines)/=1) &
        error stop 'memory_readfile: input file not a single line'
    call split(lines(1)%str,',',tokens)
    if (size(tokens)<1) &
        error stop 'memory_readfile: program is empty'

    memitems = string_to_int(tokens)
    nsize = size(transfer(memitems(1), mold))
    allocate(values(size(memitems)*nsize))
    allocate(starts(size(memitems)))
    do i=1, size(memitems)
      memitems(i)%addr = i-1  ! memory is zero-based
      starts(i) = (i-1)*nsize+1
      values(starts(i):starts(i)+nsize-1) = transfer(memitems(i),mold)
    end do
    this%tree%root => rbnode_t(values, starts)

  contains
    elemental function string_to_int(this0) result(item)
      class(string_t), intent(in) :: this0
      type(memoryitem_t) :: item
      integer :: iostat

      if (.not. allocated(this0%str)) &
          error stop 'memory_readfile - error, string unallocated'
      read(this0%str,*,iostat=iostat) item%value
      if (iostat /= 0) &
          error stop 'memory_readfile - integer not recognized'
      item%addr = -1
    end function string_to_int

  end subroutine memory_readfile


  subroutine memory_copy(this, copy)
    class(memory_t), intent(in) :: this
    type(memory_t), intent(out) :: copy

    copy%tree%root => rbnode_t(this%tree%root)
  end subroutine memory_copy


  subroutine memory_restore(this, copy)
    class(memory_t), intent(inout) :: this
    type(memory_t), intent(in) :: copy

    call rbnode_freetree(this%tree%root)
    this%tree%root => rbnode_t(copy%tree%root)
  end subroutine memory_restore


  integer function memory_size(this) result(n)
    class(memory_t), intent(in) :: this
    n = this%tree%size()
  end function memory_size


  subroutine memory_print(this)
    class(memory_t), intent(in) :: this

    type(rbnode_t), pointer :: visitor
    type(memoryitem_t) :: item
    integer, parameter :: values_per_line = 10
    integer :: i

    visitor => this%tree%leftmost()
    write(*,'("Printing out memory (size = ",i0,") ...")') this%size()
    i = 0
    do
      if (.not. associated(visitor)) exit
      i = i + 1

      item = transfer(rbnode_read(visitor), item)
      write(*,'("[",i0,",",i0,"] ")',advance='no') item%addr, item%value
      if (mod(i,values_per_line)==0) write(*,*)

      visitor => rbnode_successor(visitor)
    end do
    write(*,*)
  end subroutine memory_print


  integer function compare(a,b) result(cmp)
    integer(DATA_KIND), dimension(:), intent(in) :: a, b

    type(memoryitem_t) :: am, bm

    am = transfer(a,am)
    bm = transfer(b,bm)

    if (am%addr < bm%addr) then       ! a < b  => -1
      cmp = -1
    else if (am%addr == bm%addr) then ! a == b =>  0
      cmp = 0
    else                              ! a > b  =>  1
      cmp = 1
    end if
  end function compare


  elemental impure subroutine memory_finalize(this)
    type(memory_t), intent(inout) :: this
    call rbnode_freetree(this%tree%root)
  end subroutine memory_finalize


  ! =======
  ! Machine
  ! =======

  function machine_step(this) result(istat)
    class(machine_t), intent(inout) :: this
    integer :: istat

    integer :: op, modes(3)
    logical :: jump

    call decode_opcode(this, op, modes)

    ! do nothing if halt or waiting for input
    if (op==99) then
      istat = STAT_HALT
      return
    else if (op==3 .and. this%inp%isempty()) then
      istat = STAT_WAIT_INPUT
      return
    end if

    ! process instruction
    istat = STAT_RUN
    jump = .false.
    select case (op)
    case(1) ! add (VVA)
      call this%mem%write(get_addr(this,3,modes(3)), &
        get_value(this,1,modes(1)) + get_value(this,2,modes(2)))

    case(2) ! multiply (VVA)
      call this%mem%write(get_addr(this,3,modes(3)), &
        get_value(this,1,modes(1)) * get_value(this,2,modes(2)))

    case(3) ! input (A)
      if (this%inp%isempty()) error stop 'machine_step - empty input'
      call this%mem%write(get_addr(this,1,modes(1)), transfer(this%inp%dequeue(),0_VALUE_KIND))

    case(4) ! output (V)
      call this%out%enqueue(transfer(get_value(this,1,modes(1)),mold))
      istat = STAT_OUTPUT_READY

    case(5) ! 5=jump-if-true (2VV): if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
      associate (val=>get_value(this,1,modes(1)))
        if (val /= 0) then
          this%ip = get_value(this,2,modes(2))
          jump = .true.
        end if
      end associate

    case(6) ! 6=jump-if-false (2VV): if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
      associate (val=>get_value(this,1,modes(1)))
        if (val == 0) then
          this%ip = get_value(this,2,modes(2))
          jump = .true.
        end if
      end associate

    case(7) ! 7=less than (3VVA): if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
      associate(res=> (get_value(this,1,modes(1)) < get_value(this,2,modes(2))) )
        if (res) then
          call this%mem%write(get_addr(this,3,modes(3)), 1_VALUE_KIND)
        else
          call this%mem%write(get_addr(this,3,modes(3)), 0_VALUE_KIND)
        end if
      end associate

    case(8) ! 8=equals (3VVA): if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
      associate(res=> (get_value(this,1,modes(1)) == get_value(this,2,modes(2))) )
        if (res) then
          call this%mem%write(get_addr(this,3,modes(3)), 1_VALUE_KIND)
        else
          call this%mem%write(get_addr(this,3,modes(3)), 0_VALUE_KIND)
        end if
      end associate

    case(9) ! 9=adjust-base (1V)
      this%base = this%base + get_value(this, 1, modes(1))

    case default
      error stop 'machine_step - invalid opcode'
    end select

    if (.not. jump) this%ip = this%ip + no_of_pars(op) + 1

  end function machine_step


  subroutine decode_opcode(this, op, modes)
    class(machine_t), intent(in) :: this
    integer,  intent(out) :: op, modes(3)

    integer :: instr, k

    modes = -1
    instr = int(this%mem%read(this%ip), int32)
    op = mod(instr,100)
    do k=1, no_of_pars(op)
      modes(k) = mod(instr/10**(k+1),10)
    end do
  end subroutine decode_opcode


  integer function no_of_pars(op) result(npars)
    integer, intent(in) :: op
    select case(op)
    case(1,2,7,8) ! add, multiply, less-than, equals
      npars = 3
    case(5,6)     ! jump-if-true, jump-if-false
      npars = 2
    case(3,4,9)   ! input, output, adjust-base
      npars = 1
    case(99)      ! halt
      npars = 0
    case default
      error stop 'no_of_pars: invalid op code'
    end select
  end function no_of_pars


  function get_value(this, ipos, mode) result(val)
    type(machine_t), intent(in) :: this
    integer, intent(in) :: ipos, mode
    integer(VALUE_KIND) :: val

    associate(addr=>this%mem%read(this%ip+ipos))
    select case(mode)
    case(0) ! position mode
      val = this%mem%read(addr)
    case(1) ! immediate mode
      val = addr
    case(2) ! relative mode
      val = this%mem%read(addr+this%base)
    case default
      error stop 'get_value - invalid mode'
    end select
    end associate
  end function get_value


  function get_addr(this, ipos, mode) result(addr)
    type(machine_t), intent(in) :: this
    integer, intent(in) :: ipos, mode
    integer(ADDRESS_KIND) :: addr

    associate(addr0=>this%mem%read(this%ip+ipos))
    select case(mode)
    case(0) ! position mode
      addr = addr0
    case(1) ! immediate mode
      error stop 'get_addr - immediate mode invalid in address context'
    case(2) ! relative mode
      addr = addr0+this%base
    case default
      error stop 'get_addr - invalid mode'
    end select
    end associate
  end function get_addr


  subroutine machine_load(this, file)
    class(machine_t), intent(inout) :: this
    character(len=*), intent(in) :: file

    if (associated(this%mem%tree%root)) then
      ! clean memory and reset instruction pointers of the old machine
      call rbnode_freetree(this%mem%tree%root)
      this%ip = 0
      this%base = 0
    end if
    call this%mem%readfile(file)
  end subroutine machine_load


  subroutine machine_pushinput(this, val)
    class(machine_t), intent(inout) :: this
    integer(VALUE_KIND), intent(in) :: val
    call this%inp%enqueue(transfer(val,mold))
  end subroutine machine_pushinput


  function machine_popoutput(this) result(val)
    class(machine_t), intent(inout) :: this
    integer(VALUE_KIND) :: val
    if (.not. this%out%isempty()) then
      val = transfer(this%out%dequeue(), 1_VALUE_KIND)
    else
      error stop 'machine_popoutput - no output'
    end if
  end function machine_popoutput


  subroutine machine_writeaddr(this, addr, val)
    class(machine_t), intent(inout) :: this
    integer(ADDRESS_KIND), intent(in) :: addr
    integer(VALUE_KIND), intent(in) :: val

    call this%mem%write(addr, val)
  end subroutine machine_writeaddr


  function machine_readaddr(this, addr) result(val)
    class(machine_t), intent(in) :: this
    integer(ADDRESS_KIND), intent(in) :: addr
    integer(VALUE_KIND) :: val

    val = this%mem%read(addr)
  end function machine_readaddr

end module intcode23_mod