  module selectable_mod
    implicit none

    type, abstract :: selectable_at
      integer :: ilab
      integer :: next
    contains
      generic :: operator(==) => selectable_eq
      generic :: operator(<)  => selectable_lt
      procedure(swap_ai), deferred :: Swap
      procedure(mask_ai), deferred :: Mask
      ! bindings for operators
      procedure(operator_ai), deferred :: selectable_eq
      procedure(operator_ai), deferred :: selectable_lt
    end type selectable_at

    abstract interface
      subroutine swap_ai(a, b)
        import selectable_at
        class(selectable_at), intent(inout) :: a, b
        ! Swap values "a" and "b".
      end subroutine

      logical function mask_ai(a, mskid)
        import selectable_at
        class(selectable_at), intent(in) :: a
        integer, intent(in)              :: mskid
        ! Return true if "a" falls in category defined by a condition "mskid".
      end function

      logical function operator_ai(a, b)
        import selectable_at
        class(selectable_at), intent(in) :: a, b
      end function operator_ai
    end interface

    integer, parameter :: END_OF_LIST = 0

  contains

    subroutine label_unique(arr, mskid, heads)
      class(selectable_at), intent(inout) :: arr(:)
      integer, intent(in) :: mskid
      integer, allocatable, intent(out) :: heads(:)
!
! Mark duplicit entries by the same "ilab" component. Set "next" component
! to move between duplicities. Also construct index to positions of the first
! occurence.
!
      integer :: i, j, n, icurr, iprev, iroot
      integer :: nlabs ! distinct entries counter
      integer, allocatable :: wrk(:)

      ! Initialize
      n = size(arr)
      arr % ilab = -1
      arr % next = -1
      allocate(heads(n))
      heads = -1
      nlabs = 0

      do i = 1, n
        ! Skip entries that do not fit to mask
        if (.not. arr(i) % Mask(mskid)) cycle

        ! Skip already identified item
        if (arr(i) % ilab /= -1) cycle

        ! New distinct entry found. Found all following duplicates in the array
        nlabs = nlabs + 1
        arr(i) % ilab = nlabs
        heads(nlabs) = i
        iprev = i
        iroot = i
        do j = i+1, n
          if (.not. arr(j) % Mask(mskid)) cycle
          if (arr(j) % ilab /= -1) cycle
          if (.not. arr(j)==arr(iroot)) cycle

          ! duplicit entry: mark it and link it to a previously found one
          arr(j) % ilab = nlabs
          arr(iprev) % next = j
          iprev = j
        end do
        ! "0" marks the end of list of duplicit entries 
        arr(iprev) % next = END_OF_LIST
      end do

      ! shrink "heads" to the actual number of found values
      allocate(wrk(nlabs))
      wrk = heads(1:nlabs)
      call move_alloc(wrk, heads)

    end subroutine label_unique



    subroutine sort_unique(arr, heads)
      class(selectable_at), intent(inout) :: arr(:)
      integer, intent(in) :: heads(:)
!
! Bubble sort item linked by "next" entries. Starting points are in "heads"
!
      integer :: i, j, group, istart, tmp
      logical :: was_swap

      do group = 1, size(heads)
        istart = heads(group)
        call sanity_check(istart)

        ! bubble sort
        do
          was_swap = .false.
          i = istart
          do
            j = arr(i) % next
            if (j==END_OF_LIST) exit
            call sanity_check(j)
            if (.not. arr(i)<arr(j)) then
              call arr(i) % Swap(arr(j))
              tmp = arr(i) % next
              arr(i) % next = arr(j) % next
              arr(j) % next = tmp
              was_swap = .true.
            end if
            i = j
          end do
          if (.not. was_swap) exit
        end do

      end do

    contains
      subroutine sanity_check(ii)
        integer, intent(in) :: ii
        if (ii<1 .or. ii>size(arr)) &
            error stop 'sort_unique - some index is out of range'
      end subroutine

    end subroutine sort_unique

  end module selectable_mod
