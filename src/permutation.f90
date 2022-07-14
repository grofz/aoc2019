!
! Heap's algorithm to generate all possible permutations of objects
! https://en.wikipedia.org/wiki/Heap%27s_algorithm
!
  module permutation_mod
    implicit none
    private
    public permutate

    type, public :: permutation_generator_t
      integer :: cnt = 0 ! counter of generated permutations
      integer :: tot =-1 ! total number of permutations ("N!")
      integer :: i = 1
      integer, allocatable :: c(:)
    contains
      procedure :: init => permutation_init
      procedure :: next => permutation_next
      procedure :: remaining => permutation_remaining
    end type

  contains

    subroutine permutation_init(this, arr)
      class(permutation_generator_t), intent(out) :: this
      integer, intent(in) :: arr(0:)
!
! Initialize the generator. Does not count as permutation sequence.
!
      this % cnt = 0
      this % tot = factorial(size(arr))
      this % i = 1
      allocate(this % c(0:size(arr)-1))
      this % c = 0

    end subroutine permutation_init



    subroutine permutation_next(this, arr, status_ok)
      class(permutation_generator_t), intent(inout) :: this
      integer, intent(inout) :: arr(0:)
      logical, intent(out) :: status_ok
!
! Reshuffle items in "arr" to a new permutation sequence
! (does nothing in the first call).
! False "status_ok" flags there are no more sequences
! (the last sequence was generated in the last call)
!
      integer :: n

      n = size(arr)
      if (.not. allocated(this%c)) &
          error stop 'permutation_next - generator not initialized'
      if (size(this%c) /= n) error stop 'permutation_next - array size mismatch'

      ! First call does nothing (first permutation sequence)
      if (this % cnt == 0) then
        this % cnt = this % cnt + 1
        status_ok = .true.
        return
      end if

      do
        if (this%i >= n) then
          ! the last permutation was already generated
          status_ok = .false.
          exit
        end if

        if (this % c(this%i) < this%i) then
          if (mod(this%i,2)==0) then ! "i" is even
            call swap(arr(0), arr(this%i))
          else
            call swap(arr(this%c(this%i)), arr(this%i))
          end if
          this%c(this%i) = this%c(this%i) + 1
          this%i = 1
          ! report output
          status_ok = .true.
          this % cnt = this % cnt + 1
          exit
        else
          this%c(this%i) = 0
          this%i = this%i + 1
        end if
      end do

      ! check if a correct number of permutations was called
      if (.not. status_ok) then
        if (this%cnt /= factorial(n)) &
            error stop 'permutation_next - wrong number of permutations'
      end if
    end subroutine permutation_next



    pure integer function permutation_remaining(this) result(rem)
      class(permutation_generator_t), intent(in) :: this
!
! Number of sequences left to be generated.
!
      rem = this % tot - this % cnt
    end function


    subroutine permutate(arr)
      integer, intent(inout) :: arr(0:)
!
! Just print all permutatoin sequences.
!
      integer :: c(0:size(arr)-1), n, i
      integer :: cnt ! counter to validate

      cnt = 0
      n = size(arr)
      c = 0

      call output(arr)

      i = 1
      do
        if (i >= n) exit
        if (c(i) < i) then
          if (mod(i,2)==0) then ! "i" is even
            call swap(arr(0), arr(i))
          else
            call swap(arr(c(i)), arr(i))
          end if
          call output(arr)
          c(i) = c(i) + 1
          i = 1
        else
          c(i) = 0
          i = i + 1
        end if

      end do

      ! check if a correct number of permutations was called
      if (cnt /= factorial(n)) error stop 'permutate - wrong number of permutations'

    contains
      subroutine output(arr)
        integer, intent(in) :: arr(:)
        print '(10(i3,1x))', arr
        cnt = cnt + 1
      end subroutine
    end subroutine permutate



    subroutine swap(a,b)
      integer, intent(inout) :: a, b
      integer :: tmp
      tmp = a
      a = b
      b = tmp
    end subroutine



    integer function factorial(n)
      integer, intent(in) :: n
      integer :: i
      factorial = 1
      do i=1, n
        factorial = factorial * i
      end do
    end function factorial

  end module permutation_mod
