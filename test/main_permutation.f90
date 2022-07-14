  program permutation_driver
    use permutation_mod
    implicit none
    integer, allocatable :: arr(:), brr(:)
    type(permutation_generator_t) :: gen
    logical :: status_ok

    print '("Permutation driver")'
    allocate(arr(0))
    arr = [1, 2, 3]
    brr = arr
    call permutate(arr)
    print *

 print *, 'remaining = ',gen % Remaining()
    call gen % Init(brr)
 print *, 'remaining = ',gen % Remaining()
    do
      call gen % Next(brr,status_ok)
      if (status_ok) then
        print *, 'remaining = ',gen % Remaining()
        print '(10(i3,1x))', brr
      else
        print *, 'remaining = ',gen % Remaining()
        exit
      end if
    end do



  end program permutation_driver
