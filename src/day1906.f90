  module day1906_mod
    use parse_mod, only : string_t, split
    implicit none

    integer, parameter :: LEN_NAME=3, UNDEFINED=-1
    character(len=LEN_NAME), parameter :: NULL_NAME = '???'

    type object_t
      character(len=LEN_NAME) :: name
      type(object_t), pointer :: parent => null()
      integer :: norbits = UNDEFINED
    end type object_t

    type object_ptr
      type(object_t), pointer :: ptr
    contains
      procedure :: getkey => object_getkey
    end type object_ptr

  contains

    subroutine make_map(lines, listobj)
      type(string_t), intent(in) :: lines(:)
      type(object_ptr), intent(out), allocatable :: listobj(:)
!
! Construct the orbit map from the input
!
      integer :: i, j, loc(2), nobj
      type(string_t), allocatable :: words(:)
      type(object_ptr) :: dat
      type(object_ptr), allocatable :: wrk(:)
      allocate(listobj(100))
      nobj = 0

      do i=1,size(lines)
        call split(lines(i)%str,')',words)
        if (size(words)/=2) error stop 'make_map - invalid format'

        do j=1,2
          loc(j) = object_find(listobj(1:nobj), words(j)%str)
          if (loc(j)==0) then
            allocate(dat%ptr)
            dat%ptr % name = words(j)%str
            !if (words(j)%str=='COM') dat%ptr % norbits = 0
            call object_add(listobj, nobj, dat, .true.)
            loc(j) = nobj ! object inserted at the end of list
          end if
        end do

        ! Make a link "A)B"   -->   A <-- B
        listobj(loc(2))%ptr % parent => listobj(loc(1))%ptr
      end do

      ! crop the unuesed elements from the list
      allocate(wrk(1:nobj))
      wrk = listobj(1:nobj)
      call move_alloc(wrk, listobj)
    end subroutine make_map


    subroutine count_orbits(listobj, totorbits)
      type(object_ptr), intent(in) :: listobj(:)
      integer, intent(out) :: totorbits

      type(object_ptr), allocatable :: worklist(:)
      integer :: i, norbits, nlist

      totorbits = 0
      do i=1,size(listobj)
        if (allocated(worklist)) deallocate(worklist)
        allocate(worklist(10))
        nlist = 0
        call count_one_orbit(listobj(i)%ptr, norbits, worklist, nlist)
        totorbits = totorbits+norbits
  !print '(a3," couted ",i0,".  Worklist size ",i0)', listobj(i)%Getkey(), norbits, nlist
      end do
    end subroutine count_orbits



    recursive subroutine count_one_orbit(me, myres, worklist, nlist)
      type(object_t), intent(in), pointer :: me
      integer, intent(out) :: myres
      type(object_ptr), allocatable, intent(inout) :: worklist(:)
      integer, intent(inout) :: nlist
      integer :: res

      if (me%norbits /= UNDEFINED) then
        myres = me%norbits
        return
      else if (.not. associated(me%parent)) then
        myres = 0
        return
      else
        call count_one_orbit(me%parent, res, worklist, nlist)
        myres = res+1
        me%norbits = myres
      end if
    end subroutine count_one_orbit



    subroutine list_to_com(me, list)
      type(object_t), pointer, intent(in) :: me
      type(object_ptr), intent(out), allocatable :: list(:)

      integer :: nlist
      type(object_t), pointer :: cur
      type(object_ptr), allocatable :: wrk(:)
      type(object_ptr) :: dat

      cur => me
      allocate(list(10))
      nlist = 0
      do
        if (.not. associated(cur)) exit
        dat%ptr => cur
        call object_add(list, nlist, dat)
        cur => cur%parent
      end do
      allocate(wrk(1:nlist))
      wrk = list(1:nlist)
      call move_alloc(wrk, list)        
    end subroutine list_to_com


    function object_getkey(this) result(key)
      class(object_ptr), intent(in) :: this
      character(len=LEN_NAME) :: key
      if (associated(this%ptr)) then
        key = this%ptr % name
      else
        key = NULL_NAME
      end if
    end function

    integer function object_find(arr, key) result(loc)
      class(object_ptr), intent(in) :: arr(:)
      character(len=*), intent(in) :: key
      integer :: i
      loc = 0
      do i=1,size(arr)
        if (key /= arr(i)%Getkey()) cycle
        loc = i
        exit
      end do
    end function object_find

    subroutine object_add(arr, n, newitem, isfast)
      type(object_ptr), intent(inout), allocatable :: arr(:)
      integer, intent(inout) :: n
      type(object_ptr), intent(in) :: newitem
      logical, intent(in), optional :: isfast ! do not search for duplicity if true

      type(object_ptr), allocatable :: wrk(:)
      integer :: loc, nmax
      logical :: isfast0

      nmax = size(arr)
      isfast0 = .false.
      if (present(isfast)) isfast0 = isfast
      if (.not. isfast0) then
        loc = object_find(arr(1:n), newitem%Getkey())
        if (loc /= 0) return ! object is in list, do not make duplicates
      end if

      if (n == nmax) then
        allocate(wrk(nmax*2))
        wrk(1:n) = arr(1:n)
        call move_alloc(wrk, arr)
      end if
      n = n + 1
      arr(n) = newitem
    end subroutine object_add
  end module day1906_mod
