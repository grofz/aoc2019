  module prime_numbers_mod
    use kinds_m, only : I8B, I4B, I1B
    implicit none

  contains

    function get_prime_numbers(n) result (res)
      integer, intent(in) :: n
      integer(I4B), allocatable :: res(:)
!
! Sieve of Erstosthenes - slow but simple algorithm
!
      logical(I1B), allocatable :: a(:)
      integer :: i, imax, j

      if (n <= 1) error stop 'get_prime_numbers - n must be 2 or larger'
      allocate(a(2:n), res(2:n))
      a = .true.
      do i=2,n
        res(i) = i
      end do

      imax = floor(sqrt(float(n)))
      do i=2, imax
        if (.not. a(i)) cycle
        j = i*i
        do
          if (j>n) exit
          a(j) = .false.
          j = j + i
        end do
      end do
      res = pack(res, mask=a)
    end function



    function get_lcm(vals) result(lcm)
      integer, intent(in) :: vals(:)
      integer(i8B) :: lcm
!
! Least commong multiplier of numbers in the array
!
      integer, allocatable :: primes(:), vals0(:), cnt(:)
      integer :: max_val, n, ipr, ival
      logical :: was_divisible
      integer(I8B) :: tmp

      max_val = maxval(vals)
      primes = get_prime_numbers(max_val)
      allocate(cnt(size(primes)))
      cnt = 0
      vals0 = vals

      ipr = 1
      do 
        if (ipr > size(primes)) exit
        was_divisible = .false.
        do ival=1, size(vals)
          if (mod(vals0(ival), primes(ipr))==0) then
            vals0(ival) = vals0(ival) / primes(ipr)
            was_divisible = .true.
          end if
        end do

        if (was_divisible) then
          cnt(ipr) = cnt(ipr)+1
        else
          ipr = ipr + 1
        end if
      end do


      lcm = 1_I8B
      do ipr = 1, size(primes)
        if (cnt(ipr)==0) cycle
        tmp = int(primes(ipr), kind=I8B)**cnt(ipr)
        lcm = lcm * tmp
      end do

    end function

  end module prime_numbers_mod

