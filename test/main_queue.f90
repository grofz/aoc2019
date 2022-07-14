  program queue_driver
    use queue_mod
    implicit none

    type(queue_t) :: buffer
    integer, allocatable :: arr(:)
    integer :: i, j, k

    arr = [1, 2, 3, 4, 5]
    !buffer = queue_t(arr,10)
    buffer = queue_t(maxsize=10)

    do j=1,5
    print *, 'next_round adding'
    do i=10,16
      if (.not. buffer%Isfull()) then
        call buffer%Insert(i)
      else
        print *, 'queue is full '
        exit
      end if
    end do

    print '(10(i3,1x))', buffer%Export() 

    print *, '..now removing'
    do k=1,3
      if (.not. buffer%Isempty()) then
        call buffer%Remove(i)
        write(*,'(i3,1x)',advance='no') i
      else
        write(*,*)
        print *, 'queie is empty'
        exit
      end if
    end do
    if (k==4) write(*,*)
    end do


  end program queue_driver

