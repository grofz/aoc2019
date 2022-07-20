!
! Day 16 - Flawed frequency transmission
!
  module day1916_mod
    use kinds_m, only : I1B
    implicit none

    interface fft_phases
      module procedure fft_phases64, fft_phases8
    end interface

  contains

    pure function sig2num(str) result(arr)
      character(len=*), intent(in) :: str
      integer, allocatable :: arr(:)
!
! Transformation of character "1234" to array of integers [1,2,3,4]
!
      integer :: i, n, ios

      n = len_trim(str)
      allocate(arr(n))
      do i=1,n
        read(str(i:i),*,iostat=ios) arr(i)
        if (ios /= 0) error stop 'sig2num - conversion error'
      end do
    end function


!
! Return "j"-th phase of FFT with input "inp".
!
    pure function fft_phases64(inp,j) result(out)
      integer, intent(in) :: inp(:)
      integer, intent(in) :: j
      integer, allocatable :: out(:), ker(:,:)
      integer :: i, n

      n = size(inp,1)
      allocate(out(n), ker(n, n))
      ker = kernel(n)
      out = inp
      do i=1, j
        out = fft_trun(abs(fft_slow(out, ker)))
      end do
    end function

    pure function fft_phases8(inp,j) result(out)
      integer(I1B), intent(in) :: inp(:)
      integer, intent(in) :: j
      integer(I1B), allocatable :: out(:)
      integer :: i, n

      n = size(inp,1)
      allocate(out(n))
      out = inp
      do i=1, j
        out = fft_dirty(out)
      end do
    end function



    pure function fft_dirty(vec) result(res)
      integer(I1B), intent(in) :: vec(:)
      integer(I1B), allocatable :: res(:)
!
! Make FFT of the second half of input. Size of vector must
! be multiply of 2. Lower half of transformation matrix is 
!
!                           (example for n=8)
!                     .. .. .. ..   .. .. .. ..  (k=1,4) 
!                      0  0  0  0    1  1  1  1  (k=5)
!                      0  0  0  0    0  1  1  1  (k=6)
!                      0  0  0  0    0  0  1  1  (k=6)
!                      0  0  0  0    0  0  0  1  (k=6)
!
! Therefore the second half of the result can be calculated in O(N) time
! complexity. The first half of result is incorrect.
!
      integer :: i, n, val

      n = size(vec,1)
      if (mod(n,2)/=0) error stop 'fft_dirty - vector size must be even'
      allocate(res(n))
      res(1:n/2) = 0
      val = 0
      do i=n,(n/2)+1,-1
        val = val + int(vec(i))
        ! We must truncate because "vec" and "res" are 1 byte
        res(i) = mod(abs(val),10)
      end do
    end function



    pure function fft_trun(vec) result(res)
      integer, intent(in) :: vec(:)
      integer, allocatable :: res(:)
!
! Leave the 10^0 digit only: 17 -> 7, -32 -> 2, ...
!
      allocate(res(size(vec,1)))
      res = mod(abs(vec),10)
    end function 


    pure function fft_slow(vec, ker) result(res)
      integer, intent(in) :: vec(:), ker(:,:)
      integer, allocatable :: res(:)
!
! One phase of "Flawed-frequency-transmision" transformation. Result vector
! obtained by the transformation matrix "kernel" multiplied by the input vector.
! It has O(N^2)
!

      allocate(res(size(vec,1)))
      res = matmul(ker,vec)
    end function


    pure function kernel(n) result(aa)
      integer, intent(in) :: n
      integer, allocatable :: aa(:,:)
!
! Transformation square matrix of size "n" 
!
      integer :: i
      allocate(aa(n,n))
      do i=1,n
        aa(i,:) = pattern(n, i)
      end do
    end function kernel



    pure function pattern(n, k) result(arr)
      integer, intent(in) :: n, k
      integer, allocatable :: arr(:)
!
! One line of transformation matrix. "n" - number of elements, "k" - element order
!
      integer, allocatable :: pat(:), wrk(:)
      integer :: i, nrep

      ! basic pattern is 0, 1, 0, -1 with repeated digits "k" times
      pat = [repeat(0,k), repeat(1,k), repeat(0,k), repeat(-1,k)]

      ! number of times pattern must repeat to fill the array
      ! first element of the array is not used (offset +1)
      nrep = (n+1)/size(pat) + 1
      allocate(wrk(nrep*size(pat)))
      do i=1,nrep
        wrk((i-1)*size(pat)+1:i*size(pat)) = pat
      enddo
      allocate(arr(n))
      arr = wrk(2:n+1)
    end function



    pure function repeat(p, n) result(arr)
      integer, intent(in) :: p, n
      integer, allocatable :: arr(:)
!
! Array with number "p" repeated "n" times
!
      allocate(arr(n))
      arr = p
    end function

  end module
