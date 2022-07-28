  module day1924_mod
    implicit none

    character(len=1), parameter :: BUG='#', SPACE='.'
    integer, parameter :: DIRVEC(2,4)=reshape([1,0,-1,0,0,1,0,-1],[2,4])

  contains

    subroutine print_bugs(ain)
      character(len=1), intent(in) :: ain(0:,0:)
      integer :: ni, i, nj
      ni = size(ain,1)-2
      nj = size(ain,2)-2
      do i=1,ni
        write(*,'(*(a1))') ain(i,1:nj)
      end do
    end subroutine


    function one_step2(ain) result(aout)
      character(len=1), intent(in), allocatable :: ain(:,:,:)
      character(len=1),allocatable :: aout(:,:,:)

      integer :: imin, imax, jmin, jmax, kmin, kmax, imid, jmid, i, j, k, ngb
      allocate(aout, source=ain)
      aout = SPACE

      kmin = lbound(ain,3)
      kmax = ubound(ain,3)
      jmin = lbound(ain,2)
      jmax = ubound(ain,2)
      imin = lbound(ain,1)
      imax = ubound(ain,1)
      imid = (imin+imax)/2
      jmid = (jmin+jmax)/2

      do i=imin,imax
      do j=jmin,jmax
      do k=kmin,kmax
        if (i==imid .and. j==jmid) cycle
        aout(i,j,k)=ain(i,j,k)
        ngb = count_bugs(ain,i,j,k)
        select case(ain(i,j,k))
        case(BUG)
          if (ngb/=1) aout(i,j,k)=SPACE
        case(SPACE)
          if (ngb>=1 .and. ngb<=2) aout(i,j,k)=BUG
        case default
          error stop 'unexpected character'
        end select
      end do
      end do
      end do

    end function



    function one_step(ain) result(aout)
      character(len=1), intent(in) :: ain(0:,0:)
      !character(len=1) :: aout(lbound(ain,1):ubound(ain,1),lbound(ain,2):ubound(ain,2))
      character(len=1),allocatable :: aout(:,:)

      integer :: i,j,ni,nj, ngb, k

      allocate(aout(lbound(ain,1):ubound(ain,1),lbound(ain,2):ubound(ain,2)))
      aout = SPACE
      ni = size(ain,1)-2
      nj = size(ain,2)-2
      do i=1,ni
      do j=1,nj
        ngb = 0
        do k=1,4
          if (ain(i+DIRVEC(1,k), j+DIRVEC(2,k))==BUG) ngb = ngb+1
        end do

        ! a bug dies - unless there is exactly one bug in adjacent tiles
        ! empty space becomes infested - exactly one or two bugs adjacent
        aout(i,j)=ain(i,j)
        select case(ain(i,j))
        case(BUG)
          if (ngb/=1) aout(i,j)=SPACE
        case(SPACE)
          if (ngb>=1 .and. ngb<=2) aout(i,j)=BUG
        case default
          error stop 'unexpected character'
        end select
      end do
      end do
    end function


    function biod(ain) result(val)
      character(len=1), intent(in) :: ain(0:,0:)
      integer :: val
      integer :: i, j, ni, nj, cnt
      cnt = 0
      val = 0
      ni = size(ain,1)-2
      nj = size(ain,2)-2
      do i=1,ni
      do j=1,nj
        if (ain(i,j)=='#') val = val + 2**cnt
        cnt = cnt+1
      end do
      end do
    end function



    subroutine add_list(list,n,val,new)
      integer, intent(inout), allocatable :: list(:)
      integer, intent(inout)              :: n
      integer, intent(in)                 :: val
      logical, intent(out)                :: new

      integer, allocatable :: wrk(:)
      integer :: i

      ! Allocate new list or extend a full list
      if (.not. allocated(list)) then
        allocate(list(10))
        n = 0
      end if
      if (size(list)==n) then
        allocate(wrk(2*n))
        wrk(1:n) = list
        call move_alloc(wrk, list)
      end if

      ! Find first element in the list that is larger or equal val
      do i=1,n
        if (list(i) < val) cycle
        exit
      end do

      if (i==n+1) then
        ! all elements are smaller than val / add val at the end of list
        n = n + 1
        new = .true.
        list(n) = val
      else if (list(i)==val) then
        ! there is same element in the list
        new = .false.
      else
        ! move larger elements one position down / add val
        list(i+1:n+1) = list(i:n)
        list(i) = val
        n = n + 1
        new = .true.
      end if

    end subroutine add_list



    function count_bugs(grid,i,j,k) result(val)
      character(len=1), intent(in), allocatable :: grid(:,:,:)
      integer, intent(in)                       :: i, j, k
      integer                                   :: val

      ! i, j are 5x5: k is level
      integer :: kmin, kmax, jmin, jmax, imin, imax
      integer :: dir, i1, j1, imid, jmid, k1

      kmin = lbound(grid,3)
      kmax = ubound(grid,3)
      jmin = lbound(grid,2)
      jmax = ubound(grid,2)
      imin = lbound(grid,1)
      imax = ubound(grid,1)
      imid = (imin+imax)/2
      jmid = (jmin+jmax)/2

      val = 0
      do dir=1,4
        i1 = i + DIRVEC(1,dir)
        j1 = j + DIRVEC(2,dir)
        if (i1==imid .and. j1==jmid) then
          ! tile in the inner level (+1)
          k1 = k+1
          if (k1<=kmax) then
            if (j<j1) then
              ! left border
              val = val + count(grid(:,jmin,k1)==BUG)
            else if (j1<j) then
              ! right border
              val = val + count(grid(:,jmax,k1)==BUG)
            else if (i<i1) then
              ! top border
              val = val + count(grid(imin,:,k1)==BUG)
            else if (i1<i) then
              ! bottom border
              val = val + count(grid(imax,:,k1)==BUG)
            else
              error stop 'impossible'
            end if
          end if

        else if (i1>=imin .and. j1>=jmin .and. i1<=imax .and. j1<=jmax) then
          ! tile at the same level
          if (grid(i1,j1,k)==BUG) val = val + 1

        else
          ! tile at the outer level (-1)
          k1 = k-1      
          if (k1>=kmin) then
            if (i1 < imin) then
              ! left tile in outer level
              if (grid(imid-1,jmid,k1)==BUG) val = val + 1
            else if (i1 > imax) then
              ! right tile in outer level
              if (grid(imid+1,jmid,k1)==BUG) val = val + 1
            else if (j1 < jmin) then
              ! top tile in outer level
              if (grid(imid,jmid-1,k1)==BUG) val = val + 1
            else if (j1 > jmax) then
              ! bottom tile in outer level
              if (grid(imid,jmid+1,k1)==BUG) val = val + 1
            else
              error stop 'impossible 2'
            end if
          end if
        end if
      end do
    end function count_bugs

  end module day1924_mod
