!
! This module defines the interface for using a "container"
! The actual implementation can be a linked-list, binary-tree, etc.
!
  module abstract_container
    implicit none
    private
    public container_t, compare_fun

    ! Kind of integer arrays used to store data or pass iterator "handle"
    integer, parameter, public :: DAT_KIND = selected_int_kind(9)

    ! Error handling flags
    integer, parameter, public :: &
        ERR_CONT_OK = 0, ERR_CONT_IS = -1, ERR_CONT_ISNOT = -2, &
        ERR_CONT_END = 1

    type, abstract :: container_t
    contains
      procedure(init_fun), deferred :: initialize
      procedure(add_fun), deferred :: add
      procedure(add_fun), deferred :: remove
      procedure(removeall_fun), deferred :: removeall
      procedure(resetcurrent_fun), deferred :: resetcurrent
      procedure(nextread_fun), deferred :: nextread
      procedure(isin_fun), deferred :: isin
      procedure(isempty_fun), deferred :: isempty
      procedure(count_fun), deferred :: count
      procedure(copy_fun), deferred :: copy
      procedure(print_fun), deferred :: print
      generic :: assignment(=) => copy
      procedure, non_overridable :: datasize, constantdatasize
    end type container_t



    ! Function to compare two tree nodes
    abstract interface
      integer function compare_fun(a, b)
        import DAT_KIND
        integer(DAT_KIND), intent(in) :: a(:), b(:)
      end function
    end interface



    abstract interface
      subroutine init_fun(this)
        import
        class(container_t), intent(inout) :: this
!
! This method MUST be used before other methods !
! Given any (even in use) container object on input, this function must
! nullify its pointers and initialize other components. The object on
! the output must be an empty container.
!
! Warning: intent must be INOUT
!
      end subroutine

      subroutine add_fun(this, dat, ierr) ! "add" and "remove"
        import
        class(container_t), intent(inout) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
        integer, intent(out), optional :: ierr
        ! Erorr handling:  
        ! for "add": ERR_CONT_OK or ERR_CONT_IN if added element is
        !            already present in the container
        ! for "remove": ERR_CONT_OK or ERR_CONT_ISNOT if deleted element
        !               is not in the container
      end subroutine

      subroutine removeall_fun(this)
        import
        class(container_t), intent(inout) :: this
!
! Remove all elements from the container
!
      end subroutine

      pure subroutine resetcurrent_fun(this, handle)
        import
        class(container_t), intent(in) :: this
        integer(DAT_KIND), allocatable, intent(out) :: handle(:)
!
! Return "handle" used by iterator operation
!
      end subroutine 

      function nextread_fun(this, handle, ierr) result(dat)
        import
        integer(DAT_KIND), allocatable :: dat(:)
        class(container_t), intent(in) :: this
        integer(DAT_KIND), intent(inout) :: handle(:)
        integer, intent(out), optional :: ierr
!
! Move "handle" to the next element and return its data
! Error handling: ERR_CONT_OK or ERR_CONT_END if "handle" points at the
!                 last element in the container
      end function

      logical function isin_fun(this, dat)
        import
        class(container_t), intent(in) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
      end function

      pure logical function isempty_fun(this)
        import
        class(container_t), intent(in) :: this
      end function

      integer function count_fun(this)
        import
        class(container_t), intent(in) :: this
!
! Return number of elements in the container
!
      end function

      subroutine copy_fun(aout, bin)
        import
        class(container_t), intent(out) :: aout
        class(container_t), intent(in) :: bin
!
! Make a "deep"copy of the container
!
      end subroutine

      subroutine print_fun(this)
        import
        class(container_t), intent(in) :: this
!
! Print all elements in the container (for debugging only)
!
      end subroutine

    end interface

  contains

!
! Inquire the container on the (maximum) size of "data" array for single 
! element and whether the size is same for all elements.
!
    function datasize(this) result(n)
      class(container_t), intent(in) :: this
      integer :: n

      integer(DAT_KIND), allocatable :: dat(:), handle(:)
      integer :: ierr

      n = 0
      call this % resetcurrent(handle)
      do
        dat = this % nextread(handle, ierr)
        if (ierr /= ERR_CONT_OK) exit
        n = max(size(dat), n)
      enddo
    end function datasize

    logical function constantdatasize(this) result(same)
      class(container_t), intent(in) :: this
      
      integer(DAT_KIND), allocatable :: dat(:), handle(:)
      integer :: ierr, n

      call this % resetcurrent(handle)
      n = -2 
      same = .true.
      do
        dat = this % nextread(handle, ierr)
        if (ierr /= ERR_CONT_OK) exit
        if (n == -2) n = size(dat)
        if (size(dat) /= n) then
          same = .false.
          exit
        endif
      enddo
    end function constantdatasize

  end module abstract_container
