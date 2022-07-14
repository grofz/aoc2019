  module memory_mod
    implicit none
    private

    type, public :: memory_t
      integer, allocatable :: mem(:)
    contains
      procedure :: init => memory_init
      procedure :: read => memory_read
      procedure :: write => memory_write
      procedure :: dump => memory_dump
    end type

  contains
     subroutine memory_init(this, arr)
       class(memory_t), intent(out) :: this
       integer, intent(in)          :: arr(:)

       allocate(this%mem(0:size(arr)-1))
       this%mem = arr
     end subroutine memory_init



     pure integer function memory_read(this, adr) result(val)
       class(memory_t), intent(in) :: this
       integer, intent(in)         :: adr
       if (.not. allocated(this%mem)) error stop 'memory_read - mem not initialized'
       if (adr<0 .or. adr>size(this%mem)-1) error stop 'memory_read - addr out of range'
       val = this%mem(adr)
     end function memory_read



     pure subroutine memory_write(this,adr,val)
       class(memory_t), intent(inout) :: this
       integer, intent(in)            :: adr, val
       if (.not. allocated(this%mem)) error stop 'memory_write - mem not initialized'
       if (adr<0 .or. adr>size(this%mem)-1) error stop 'memory_write - addr out of range'
       this%mem(adr) = val
     end subroutine memory_write



     subroutine memory_dump(this)
       class(memory_t), intent(in) :: this
       if (.not. allocated(this%mem)) error stop 'memory_dump - mem not initialized'
       print '(8(i9,1x))', this%mem
     end subroutine memory_dump
    
  end module memory_mod
