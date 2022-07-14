  module memory_mod
    use kinds_m, only : IXB => I8B
    use tree_m, only : rbtr_t, tree_mold, DAT_KIND
    use abstract_container, only : ERR_CONT_OK, ERR_CONT_ISNOT, ERR_CONT_IS
    implicit none
    private

    type hash_t
      integer(IXB) :: adr
      integer(IXB) :: val ! to update value directly
    end type


    type, public :: memory_t
      integer(IXB), allocatable :: mem(:)
      integer(IXB) :: r0, r1 ! address range for direct-access
      type(rbtr_t) :: extmem
    contains
      generic :: init => memory_init64, memory_init128
      generic :: read => memory_read64, memory_read128
      generic :: write => memory_write64, memory_write128
      procedure :: dump => memory_dump
      procedure, private :: memory_init64, memory_init128, memory_read64 
      procedure, private :: memory_write64, memory_write128, memory_read128
      final :: memory_final
    end type

  contains
     subroutine memory_final(this)
       type(memory_t), intent(inout) :: this
       call this%extmem % Removeall()
     end subroutine memory_final



     subroutine memory_init64(this, arr)
       class(memory_t), intent(out) :: this
       integer, intent(in)          :: arr(:)

       allocate(this%mem(0:size(arr)-1))
       this%mem = int(arr, kind=IXB)
       this % r0 = 0
       this % r1 = size(arr)-1
       this % extmem = rbtr_t(hash_compare)
     end subroutine memory_init64

     subroutine memory_init128(this, arr)
       class(memory_t), intent(out) :: this
       integer(IXB), intent(in)          :: arr(:)

       allocate(this%mem(0:size(arr)-1))
       this%mem = arr
       this % r0 = 0
       this % r1 = size(arr)-1
       this % extmem = rbtr_t(hash_compare)
     end subroutine memory_init128



     pure integer function memory_read64(this, adr) result(val)
       class(memory_t), intent(in) :: this
       integer, intent(in)         :: adr
       if (.not. allocated(this%mem)) error stop 'memory_read64 - mem not initialized'
       if (adr<this%r0 .or. adr>this%r1) error stop 'memory_read64 - addr out of range'
       val = this%mem(adr) ! auto-conversion from 128 to 64?
     end function memory_read64

     !pure integer(IXB) function memory_read128(this, adr) result(val)
     integer(IXB) function memory_read128(this, adr) result(val)
       class(memory_t), intent(in) :: this
       integer(IXB), intent(in)    :: adr
       if (.not. allocated(this%mem)) error stop 'memory_read - mem not initialized'
       if (adr<this%r0 .or. adr>this%r1) then
         val = hash_get(this%extmem, adr)
!print *, 'MEM_READ adr/val', adr, val
       else
         val = this%mem(adr)
       end if
     end function memory_read128



     pure subroutine memory_write64(this,adr,val)
       class(memory_t), intent(inout) :: this
       integer, intent(in)            :: adr, val
       if (.not. allocated(this%mem)) error stop 'memory_write - mem not initialized'
       if (adr<this%r0 .or. adr>this%r1) error stop 'memory_write - addr out of range'
       this%mem(adr) = int(val, kind=IXB)
     end subroutine memory_write64

     subroutine memory_write128(this,adr,val)
     !pure subroutine memory_write128(this,adr,val)
       class(memory_t), intent(inout) :: this
       integer(IXB), intent(in)       :: adr, val
       if (.not. allocated(this%mem)) error stop 'memory_write - mem not initialized'
       if (adr<this%r0 .or. adr>this%r1) then
         call hash_update(this%extmem, adr, val)
!print *, 'MEM_WRITE adr/val', adr, val
       else        
         this%mem(adr) = val
       end if
     end subroutine memory_write128



     subroutine memory_dump(this)
       class(memory_t), intent(in) :: this
       if (.not. allocated(this%mem)) error stop 'memory_dump - mem not initialized'
       print '(8(i9,1x))', this%mem
       print '("Extended memory contains ",i0," blocks")', this%extmem%Count()
     end subroutine memory_dump



  pure integer function hash_compare(a, b) result(res)
    integer(DAT_KIND), intent(in) :: a(:), b(:)
!
! user function for the TREE hash
!
    type(hash_t) :: ahash, bhash
    ahash = transfer(a, ahash)
    bhash = transfer(b, bhash)

    if (ahash%adr == bhash%adr) then
      res = 0
    else if (ahash%adr < bhash%adr) then
      res = 1
    else
      res = -1
    end if
  end function hash_compare



  integer(IXB) function hash_get(hash, adr) result(val)
    type(rbtr_t), intent(in) :: hash
    integer(IXB), intent(in) :: adr
!
! Ask, if "key" has been stored. Return "val" or "0" (no entry)
!
    integer(DAT_KIND), allocatable :: handle(:)
    integer :: ierr
    type(hash_t) :: dat

    dat % adr = adr
    call hash % Find(transfer(dat,tree_mold), handle, ierr)
    select case(ierr)
    case(ERR_CONT_OK)
      dat = transfer(hash % Read(handle), dat)
      if (dat % adr /= adr) error stop 'hash_get - wrong key returned'
      val = dat % val
    case(ERR_CONT_ISNOT)
      val = 0_IXB
    case default
      error stop 'hash_get - uknown exit code'
    end select
  end function hash_get



  subroutine hash_update(hash, adr, val)
    type(rbtr_t), intent(inout) :: hash
    integer(IXB), intent(in) :: adr, val
!
! Update pair "key - val" in hash, or store new one.
!
    integer(DAT_KIND), allocatable :: handle(:)
    type(hash_t) :: olddat, newdat
    integer :: ierr

    olddat % adr = adr
    newdat % adr = adr
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


    
  end module memory_mod
