! A better intcode machine (new implementation)
!
! DONE:
! - memory as a map (via "list module")
!
! TODO:
! - input / output queues
! - actual opcodes

module intcode23_mod
  use iso_fortran_env, only : int64
  use rbnode_mod, only : rbbasetree_t, rbnode_t, rbnode_freetree, rbnode_find, &
    rbnode_read, rbnode_insert, rbnode_update, rbnode_successor
  use common_mod, only : DATA_KIND, mold
  use parse_mod, only : read_strings, string_t, split

  implicit none
  private

  integer, parameter :: ADDRESS_KIND=int64, VALUE_KIND=int64
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
public memory_t ! temporary for testing

  type memoryitem_t
    integer(ADDRESS_KIND) :: addr
    integer(VALUE_KIND) :: value
  end type

  type, public :: machine_t
    type(memory_t) :: mem
    integer(ADDRESS_KIND) :: ip
  end type

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
      memitems(i)%addr = i-1
      starts(i) = 2*i-1
      values(starts(i):starts(i)+nsize-1) = transfer(memitems(i),mold)
    end do
    if (associated(this%tree%root)) &
        error stop 'memory_readfile - memory is not empty'
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

end module intcode23_mod