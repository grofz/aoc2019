  module day1910_mod
    use selectable_mod, only : selectable_at, label_unique, sort_unique
    use parse_mod, only : read_strings, string_t
    implicit none
    private

    public calculate_vectors, shoot_asteroids, read_input_from_file

    ! "ray_t" stores the "direction" from asteroid to another asteroid
    ! it distinquishes for different quadrants ngb falls in
    ! 
    type, extends(selectable_at) :: ray_t
      logical :: sy = .false., sx = .false. ! .T. negative, .F. positive
      integer :: y = 0, x = 0, fac = 1
      logical :: is_active = .true.
      integer :: ngbptr = -1 ! index to another asteroid
    contains
      procedure :: Print => ray_print
      procedure :: Swap => ray_swap
      procedure :: Mask => ray_mask
      procedure :: selectable_eq => ray_eq, selectable_lt => ray_lt
    end type ray_t
    interface ray_t
      module procedure ray_new
    end interface

    ! module wise representation of the input
    type(string_t), allocatable :: asteroid_map(:)

  contains

    subroutine calculate_vectors(xypos, id_station, visible_max)
      integer, intent(in) :: xypos(:,:)
      integer, intent(out) :: id_station, visible_max ! asteroid with largest visibility
      integer :: n, i, j, mskid
      integer :: visible
      type(ray_t) :: rays(size(xypos,dim=2))
      integer, allocatable :: heads(:)

      n = size(xypos,2)
      id_station = -1
      visible_max = -1
      do i=1,n
        do j=1,n
          rays(j) = ray_t(xypos(1,j)-xypos(1,i), xypos(2,j)-xypos(2,i))
        enddo
        visible = -1 ! do not count visibility of itself
        do mskid = 1, 4
          call label_unique(rays, mskid, heads)
          visible = visible + size(heads)
        end do
        if (visible > visible_max) then
          visible_max = visible
          id_station = i
        end if
      enddo

      ! Visualize the situation
      !call print_map() ! original situation
      print '("Visible ",i0,". Maximum visibility ",i0," [",i0,1x,i0,"]")', &
          visible, visible_max, xypos(:,id_station)
      print *

      ! Mark position of the station
      associate(x=>xypos(1,id_station)+1, y=>xypos(2,id_station)+1)
        asteroid_map(y)%str(x:x)='X'
      end associate
      call print_map() ! station marked

    end subroutine calculate_vectors



    subroutine shoot_asteroids(xypos, id_s, ans2)
      integer, intent(in) :: xypos(:,:)
      integer, intent(in) :: id_s ! station position
      type(ray_t) :: rays(size(xypos,dim=2))
      integer :: j, i, xshot, yshot, ishot, shoot_cnt, shoot_left
      integer, allocatable :: heads(:)
      integer :: kbeg, kend, kstep
      integer :: ans2, xshot200, yshot200

      ! Calculate vectors, pick them according to quadrants and sort them
      do j=1,size(xypos,dim=2)
        rays(j) = ray_t(xypos(1,j)-xypos(1,id_s), xypos(2,j)-xypos(2,id_s))
        rays(j) % ngbptr = j
        if (j==id_s) rays(j) % is_active = .false.
      enddo
      call sort_angles(rays)

      shoot_cnt = 0
      shoot_left = size(xypos,2)-1
      CIRCLE: do
        do i=1,4
          ! label those belonging to the quadrant / push closest first
          call label_unique(rays, i, heads)
          call sort_unique(rays, heads)

          ! 1. and 3. quadrant backwards, 2. and 4. forwards
          if (mod(i,2)==0) then ! 2./4. forward
            kbeg=1; kend=size(heads); kstep=1
          else                  
            kend=1; kbeg=size(heads); kstep=-1
          end if
          do j = kbeg, kend, kstep
            ishot = rays(heads(j)) % ngbptr
            xshot = xypos(1,ishot)
            yshot = xypos(2,ishot)

            ! asteroid down...
            rays(heads(j)) % is_active = .false.
            shoot_cnt = shoot_cnt + 1
            shoot_left = shoot_left-1
            print '("Shooting #",i0," at position ",i0,1x,i0)', &
              shoot_cnt, xshot, yshot

            ! update map
            asteroid_map(yshot+1)%str(xshot+1:xshot+1)=' '

            ! Remember 200th asteroid shot
            if (shoot_cnt==200) then
              xshot200 = xshot
              yshot200 = yshot
              ans2 = xshot200*100+yshot200
            end if

          end do
          print '("Completed ",i0,". quadrant")',i
        enddo
        call print_map() ! current situation at the end of rotation
        print '("Completed rotation. Remains to shoot ",i0)', shoot_left
        if (shoot_left==0) exit CIRCLE
      enddo CIRCLE
      print '("Answer 10/2: #200 at ",i0,",",i0,"  = ",i0,l2)', &
        xshot200, yshot200, ans2, ans2==802 .or. ans2==1205

    end subroutine shoot_asteroids



    subroutine sort_angles(rays)
      type(ray_t), intent(inout) :: rays(:)
 !
 ! sort according to the intercept(angle) "y/x"
 !
      integer :: i, j, n
      type(ray_t) :: tmp
      n = size(rays)
      do i=2,n
        tmp = rays(i)
        j=i-1
        do
          if (j<1) exit
          if (.not. is_gt(rays(j),tmp)) exit
          rays(j+1) = rays(j)
          j=j-1
        enddo
        rays(j+1)=tmp
      enddo
    contains
      logical function is_gt(a,b)
        type(ray_t), intent(in) :: a, b
        ! a/b > c/d --> equivalent to a*d > c*b (if a,b,c,d >= 0)
        is_gt = a%y * b%x > b%y * a%x
      end function
    end subroutine sort_angles
          


    subroutine read_input_from_file(file, xypos)
      character(len=*), intent(in) :: file
      integer, allocatable :: xypos(:,:)
      integer :: nx, ny, nasteroids, i, j
      type(string_t), allocatable :: lines(:)
      integer, allocatable :: wrk(:,:)

      lines = read_strings(file)
      nx = len(lines(1)%str)
      ny = size(lines)
      print '("Area expected ",i0," x ",i0)', nx, ny

      ! storing x, y position of all satelites
      ! allocating the maximum size, we will shrink it at the end
      allocate(xypos(2,nx*ny))
      nasteroids = 0
      do i=1,size(lines)
      do j=1,len(lines(i)%str)
        select case(lines(i)%str(j:j))
        case('#')
          nasteroids = nasteroids + 1
          xypos(1,nasteroids) = j-1
          xypos(2,nasteroids) = i-1
        case('.')
          continue
        case default
          error stop 'read_input_from_file 10 - invalid char'
        end select
      end do
      end do

      ! shrink it
      allocate(wrk(2,nasteroids))
      wrk = xypos(:,1:nasteroids)
      call move_alloc(wrk, xypos)
      print '("Asteroids ",i0)', nasteroids

      ! make global copy of the map
      asteroid_map = lines
    end subroutine read_input_from_file



    subroutine print_map()
      integer :: i, j
      if (allocated(asteroid_map)) then
        do i=1, size(asteroid_map)
          print '(a)', asteroid_map(i)%str
        end do
      else
        print *, 'no map'
      end if
    end subroutine print_map



    type(ray_t) function ray_new(dx, dy) result(this)
      integer, intent(in) :: dx, dy
      integer :: dx0, dy0

      dx0 = dx
      dy0 = dy

      !  o---> X                        quadrant: sign_y  sign_x
      !  |                 4 | 1               1:  -T      +F
      !  |                ---+---              2:  +F      +F
      !  v Y               3 | 2               3:  +F      -T
      !                                        4:  -T      -T
      ! Make absolute values and remember quadrant vectors came from by setting
      ! sign_y and sign_x
      !
      if (dy < 0) then
        this % sy = .true.
        dy0 = -dy0
      else
        this % sy = .false.
      end if

      if (dx < 0) then
        this % sx = .true.
        dx0 = -dx0
      else
        this % sx = .false.
      end if

      ! Ill-case  dy/dx  must be in quadrant | is in quadrant at the moment
      ! up        -1/ 0   1                    T/F 1
      ! right      0/ 1   2                    F/F 2
      ! down       1/ 0   3                    F/F 2  (correct-->F/T)
      ! left       0/-1   4                    F/T 3  (correct-->T/T) 
      if (dx==0 .and. .not. this % sy) this % sx = .true. ! down
      if (dy==0 .and. this % sx) this % sy = .true.       ! left

      this % y = dy0
      this % x = dx0
      this % fac = 1
      ! direction is a number (fac=1)*(y/x)
      ! we must reduce so identical directions can be identified
      call ray_reduce(this)
    end function 



    subroutine ray_reduce(this)
      type(ray_t), intent(inout) :: this
      integer :: i
      type(ray_t) :: default
      associate(a=>this%y, b=>this%x, m=>this%fac)

      ! verify assumptions
      if (m /= 1) error stop 'ray_reduce - factor should be one'
      if (a < 0 .or. b < 0) error stop 'ray_reduce - values must be nonnegative'

      if (a==0 .and. b==0) then
        this = default    ! the same point

      else if (a==0) then ! 0/4 -> 4 * 0/1
        m = b
        b = 1

      else if (b==0) then ! 4/0 -> 4 * 1/0
        m = a
        a = 1

      else ! 5/2 -> leave as is,  6/2 -> reduce to 2 * 3/1

        ! Loop i down from min(a,b) to 2
        i = min(a, b)
        do
          if (i < 2) exit
          if (mod(a, i) == 0 .and. mod(b, i) == 0) then
            ! both are divisble by "i" 
            a = a / i
            b = b / i
            m = m * i
            i = min(a, b) ! reset the index
          else
            ! no zero fraction divison possible, try next value
            i = i - 1
          end if
        end do
      end if

      end associate
    end subroutine



    subroutine ray_swap(a, b)
      class(ray_t), intent(inout) :: a
      class(selectable_at), intent(inout) :: b
      type(ray_t) :: tmp

      select type(a)
      type is (ray_t)
        select type(b)
        type is (ray_t)
          tmp = a
          a = b
          b = tmp
        class default
          error stop 'ray_swap - uknown class of b'
        end select
      class default
        error stop 'ray_swap - uknown class of a'
      end select
    end subroutine

    
    
    logical function ray_mask(a, mskid)
      class(ray_t), intent(in) :: a
      integer, intent(in) :: mskid
      integer :: i
      if ((a%sx .eqv. .false.) .and. (a%sy .eqv. .true.)) then
        i = 1
      else if ((a%sx .eqv. .false.) .and. (a%sy .eqv. .false.)) then
        i = 2
      else if ((a%sx .eqv. .true.) .and. (a%sy .eqv. .false.)) then
        i = 3
      else if ((a%sx .eqv. .true.) .and. (a%sy .eqv. .true.)) then
        i = 4
      else
        error stop 'inpossible branch'
      end if
      ray_mask = (i == mskid) .and. (a%is_active)
    end function

    
    
    logical function ray_eq(a, b)
      class(ray_t), intent(in) :: a
      class(selectable_at), intent(in) :: b
      select type(b)
      class is (ray_t)
        ray_eq = a%x == b%x .and. a%y == b%y
        ray_eq = ray_eq .and. (a%sx .eqv. b%sx) .and. (a%sy .eqv. b%sy)
      class default
        error stop 'ray_eq - uknown class of b'
      end select
    end function

    
    
    logical function ray_lt(a, b)
      class(ray_t), intent(in) :: a
      class(selectable_at), intent(in) :: b
      select type(b)
      class is (ray_t)
        ! compare its fac
        ray_lt = a % fac < b % fac
      class default
        error stop 'ray_lt - uknown class of b'
      end select
    end function

    
    
    subroutine ray_print(a)
      class(ray_t), intent(in) :: a
      print '("[",l1,"/",l1,"]+ ",i0,"*(",i0,"/",i0,")   Active? ",l1,"  Id =",i0)', &
        a%sy,a%sx,a%fac,a%y,a%x,a%is_active,a%ngbptr
    end subroutine 

  end module day1910_mod
