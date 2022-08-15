  module day1915_mod
    use intcode_mod, only : computer_t, SINBUF_EMPTY
    use day1911_mod, only : board_t
    implicit none
    private
    public shortest_path


    ! Board values for maze pixels
    integer, parameter :: NOTHING=0, WALL=1, CORRIDOR=2, LEAK=3, HOME=4
    character(len=1), parameter :: CHARSET(4) = ['z', '.', 'X', 'H']

    ! Direction vectors (north=1, south=2, west=3, east=4
    integer, parameter :: DIRVEC(2,4) = reshape([0, 1, 0, -1, +1, 0,-1, 0], [2,4])

    type, public :: mazenode_t
      private
      integer :: id=0       ! index to its position in the array (must be unique)
      integer :: xy(2)=0    ! coordinates
      integer :: lab=0      ! 1=wall, 2=corridor, 3=leak, 4=home
      integer :: ngbs(4)=0  ! index of its four neighbours (0=no ngb)
      integer :: mark(4)=0  ! mark, how many times the path was travelled (0,1,2)
    end type mazenode_t


    type, extends(board_t) :: mazeboard_t
    contains
      procedure :: Printmaze => mazeboard_print
    end type mazeboard_t


    type, public :: maze_t
      private
      type(computer_t) :: ZX
      type(mazeboard_t)    :: map
      type(mazenode_t), allocatable :: top(:) ! vertices (connectivity data)
      integer :: ntop = -1                    ! number of vertices
      integer :: home(2)=0, leak(2)=0, robot(2)=0 ! positions of origin, leak, robot
      integer :: curid, homeid, leakid        ! index to node in "top" array
    contains
      procedure :: Init => maze_new
      procedure :: Print => maze_print
      procedure :: Move => maze_move, MoveMark => maze_move_and_mark
      procedure :: Explore_room, Explore_maze
      procedure, private :: Freedirs => maze_freedirs
    end type maze_t

  contains

    subroutine maze_new(this, file)
      class(maze_t), intent(out) :: this
      character(len=*), intent(in) :: file 
!
! Initialize new maze. Prepare computer (or testing map)
!
      ! set-up computer
      call this%ZX % Load_from_file(file)

      ! set pointers
      this%home = 0
      this%leak = 0 ! same address as home indicates it has not yet been found
      this%robot= 0

      ! initialize tree inside hashing map
      call this%map % Init() 

      ! initialize connectivity data for maze know so far
      this%ntop = 0
      allocate(this%top(100))

      ! add first (home) node
      this%ntop=this%ntop+1
      associate(home_node => this%top(this%ntop))
        home_node % id = this%ntop
        home_node % xy = this % home
        home_node % lab = HOME 
        home_node % ngbs = 0  
        call this%map % Paintcolor(home_node%xy, home_node%id)
        this % curid = home_node % id
        this % homeid = home_node % id
        this % leakid = home_node % id
      end associate

    end subroutine maze_new



    subroutine Explore_maze(this, nodes, homeid, leakid)
      class(maze_t), intent(inout) :: this
      type(mazenode_t), allocatable, intent(out) :: nodes(:)
      integer, intent(out) :: homeid, leakid
!
! Tremaux algorithm to explore the maze.
!
      integer :: mark0, mark1, revmark, i
      integer :: dir0(4), dir1(4), revdir, dir
      integer, allocatable :: freedir(:)

      revdir = 0
      MLOOP: do
        ! Explore neighboring pixels if needed
        if (any(this%top(this%curid)%ngbs<1)) call this % Explore_room()

        associate(node => this%top(this%curid))
          ! Back-tracking direction ("revdir") 
          if (revdir > 0) then
            revmark = node%mark(revdir)
          else
            revmark = 0
          end if

          ! List of walkable directions ("freedir")
          ! Select directions with zero / one mark to arrays "dir0"/"dir1"
          ! We are not allowed to walk any path more than twice
          freedir = this % Freedirs() 
          mark0 = 0
          mark1 = 0
          do i=1,size(freedir)
            ! back-tracking direction is not considered
            if (freedir(i)==revdir) cycle 
            if (node%mark(freedir(i))==0) then
              mark0 = mark0 + 1
              dir0(mark0) = freedir(i)
            else if (node%mark(freedir(i))==1) then
              mark1 = mark1 + 1
              dir1(mark1) = freedir(i)
            else if (node%mark(freedir(i))==2) then
              continue
            else
              error stop 'wrong number of marks'
            end if
          end do
        
          if (mark1==0 .and. mark0 > 0) then
            ! If all paths are un-marked: pick any path forward, ...
            dir = dir0(1)
          else if (revmark == 1) then
            ! ... or backtrack. 
            dir = revdir
          else if (mark0 > 0) then
            ! If backtracking no longer possible (path walked twice)
            ! than pick a path with less marks. Zero times marked...
            dir = dir0(1)
          else if (mark1 > 0) then
            !...and then paths with one mark.
            dir = dir1(1)
          else
            ! All paths are marked twice: Maze is fully explored, we are done.
            exit  MLOOP
          end if
        end associate

        call this % MoveMark(dir)
        revdir = dir
      end do MLOOP

      print '("=== Explore complete ===")'
      print '("Robot position ",i3,1x,i3,"  node index ",i4)', this%robot, this%curid
      print '("Leak position  ",i3,1x,i3,"  node index ",i4)', this%leak, this %leakid
      print '("Maze contains ",i4," nodes")', this%ntop

      allocate(nodes(this%ntop))
      nodes = this%top(1:this%ntop)
      leakid = this % leakid
      homeid = this % homeid

    end subroutine Explore_maze



    subroutine Explore_room(this)
      class(maze_t), intent(inout) :: this
!
! In one room, explore surrounding pixels (if not already known)
!
      integer :: dir, revdir, stat1, stat2

      do dir=1, 4
        revdir = reverse_direction(dir)
        if (this%top(this%curid) % ngbs(dir) > 0) cycle ! direction is known
        call this % Move(dir, stat1)
        if (stat1 > 0) then
          ! direction was walkable, return back
          call this % Move(revdir, stat2)
          if (stat2 == 0) error stop 'maze_explore - returning back failed'
        end if
      end do
    end subroutine Explore_room



    subroutine maze_move_and_mark(this, dir)
      class(maze_t), intent(inout) :: this
      integer, intent(inout) :: dir ! in: direction, out: reverse direction
!
! Move to room and mark the path taken.
!
      integer :: istat, idsrc, id1, dir1

      ! Verify destination room is explored
      associate (dest => this%top(this%curid)%ngbs(dir))
        if (dest<1 .or. dest>this%ntop) error stop 'move_and_mark- unexplored room'
        if (this%top(dest)%lab <= WALL) error stop 'move_and_mark - canot move'
      end associate

      ! Move
      idsrc = this%curid
      call this % Move(dir, istat)
      if (istat <= 0) error stop 'move_and_mark - move unsucessfull'

      ! Mark
      call mark_path(this%top,idsrc,dir,id1,dir1)
      dir = dir1
    end subroutine maze_move_and_mark



    subroutine maze_move(this, dir, istat)
      class(maze_t), intent(inout) :: this
      integer, intent(in)   :: dir
      integer, intent(out)  :: istat ! 0=can not move, 1=moved, 2=moved and found leak

      integer :: computer_status, curpos(2), curid, labnew, revdir, revid

      ! Direction must be between 1 and 4. Get direction to backtrack and
      ! verify source pixel is known.
      revdir = reverse_direction(dir)
      curpos = this%robot + DIRVEC(:,dir)
      revid = this%map % Getcolor(this%robot)
      curid = this%map % Getcolor(curpos)
      if (revid < 1 .or. revid > this%ntop) error stop 'maze_move - source pixel not found'

      ! Inquire our Intcode computer to see if robot can move
      call this%ZX % set_inbuf(dir)
      call this%ZX % Run(computer_status)
      if (computer_status /= SINBUF_EMPTY .or. this%ZX % Isempty_outbuf()) then
        error stop 'maze_move - call to intcode failed'
      end if
      call this%ZX % Read_outbuf(istat)

      select case (istat)
      case(0)
        labnew = WALL
      case(1)
        if (all(curpos==0)) then
          labnew = HOME
        else
          labnew = CORRIDOR
        end if
      case(2)
        labnew = LEAK
      case default
        error stop 'maze_move - output from intcode invalid'
      end select

      ! Test if destination pixel is known.
      if (curid > 0 .and. curid <= this%ntop) then
        ! Pixel is known. Check consistency.
        if (this%top(curid)%lab /= labnew .or. any(this%top(curid)%xy /= curpos)) &
            error stop 'maze_move - our data are inconsistent'
        ! Make connection between pixels (if it does not yet exits)
        if (this%top(revid) % ngbs(dir) < 1) then
          this%top(revid) % ngbs(dir) = curid
        else if (this%top(revid) % ngbs(dir) == curid) then
          continue ! check ok
        else
          error stop 'maze_move - pixel exists, but src connection points somewhere else'
        end if

        if (this%top(curid) % ngbs(revdir) < 1) then
          this%top(curid) % ngbs(revdir) = revid
        else if (this%top(curid) % ngbs(revdir) == revid) then
          continue ! check ok
        else
          error stop 'maze_move - pixel exists, but dst connection points somewhere else'
        end if

      else if (curid==0) then
        ! Unknown pixel. Add it to array and connect
        if (size(this%top)==this%ntop) call reallocate(this%top)
        this%ntop = this%ntop + 1
        curid = this%ntop
        associate(new => this%top(curid))
          new % id = this % ntop
          new % xy = curpos
          new % lab = labnew
          new % ngbs(revdir) = revid
          if (this%top(revid) % ngbs(dir) < 1) then
            this%top(revid) % ngbs(dir) = curid
          else
            error stop 'maze_mode - new pixel, but src connection already pointing'
          end if
          call this%map % Paintcolor(curpos, curid)
        end associate

      else
        error stop 'maze_move - wrong color returned by "board"'
      end if

      ! Move robot (if possible)
      if (labnew /= WALL) then
        this%robot = curpos
        this % curid = curid
      end if

      ! Mark leak (if found)
      if (labnew == LEAK) then
        if (all(this%home==this%leak)) then
          this % leak = curpos
          this % leakid = curid
        else if (all(this%leak==curpos)) then
          continue
        else
          error stop 'maze_move: second leak found?'
        end if
      end if

    end subroutine maze_move



! ========================
! Visalization of the maze
! ========================



    subroutine maze_print(this)
      class(maze_t), intent(in) :: this
      integer :: idrob
      idrob = this%map % GetColor(this%robot)
      call this%map % Printmaze(this%top(1:this%ntop),idrob)
    end subroutine maze_print


    subroutine mazeboard_print(this, top, idrob, limits)
      class(mazeboard_t), intent(in) :: this
      type(mazenode_t), intent(in) :: top(:)
      integer, intent(in), optional :: limits(4)
      integer, intent(in) :: idrob
      integer, allocatable :: map(:,:)
      character(len=1), allocatable :: chmap(:,:)

      integer :: i, j
      character(len=10) :: num

      map = this % Export(limits)
      allocate(chmap(size(map,1),size(map,2)))

      do i=1, size(map,1)
      do j=1, size(map,2)
        associate (m => map(i,j), ch => chmap(i,j))
          if (m < 1 .or. m > size(top)) then
            ch = '?'
          elseif (m==idrob) then
            ch = '@'
          else
            ch = CHARSET(top(m)%lab)
          end if
        end associate
      end do
      end do
      write(num,'(i0)') size(map,1)
      print '('//trim(adjustl(num))//'a1,1x)', chmap

    end subroutine mazeboard_print



! =================    
! Helper functions
! =================    



    subroutine reallocate(arr)
      type(mazenode_t), intent(inout), allocatable :: arr(:)
      type(mazenode_t), allocatable :: wrk(:)
      integer :: nmax
      nmax = size(arr,1)
      allocate(wrk(2*nmax))
      wrk(1:nmax) = arr
      call move_alloc(wrk, arr)
    end subroutine reallocate



    function maze_freedirs(this) result (freedirs)
      class(maze_t), intent(in) :: this
      integer, allocatable :: freedirs(:)
!
! Get all free directions (WALL, LEAK, HOME)
!
      integer :: i, wrk(4), nfree
      associate (ngbs => this%top(this%curid)%ngbs)
        nfree = 0
        do i=1,4
          if (ngbs(i) < 1 .or. ngbs(i) > this%ntop) cycle ! ignore unexplored directions
          if (this%top(ngbs(i)) % lab == WALL) cycle      ! ignore walls
          nfree = nfree + 1
          wrk(nfree) = i
        end do
      end associate
      allocate(freedirs(nfree))
      freedirs = wrk(1:nfree)

    end function maze_freedirs



    subroutine mark_path(tops,id0,dir0,id1,dir1)
      type(mazenode_t), intent(inout) :: tops(:)
      integer, intent(in) :: id0, dir0 ! source node and direction forward
      integer, intent(out):: id1, dir1 ! destination node and direction back

      if (id0 < 1 .or. id0 > size(tops)) error stop 'mark_path: id0 invalid'
      if (dir0<1 .or. dir0>4) error stop 'mark_path: direction invalid'

      id1 = tops(id0) % ngbs(dir0)
      if (id1 < 1 .or. id1 > size(tops)) error stop 'mark_path: id1 invalid'
      dir1 = reverse_direction(dir0)
      associate(m1 => tops(id0) % mark(dir0), m2 => tops(id1) % mark(dir1))
        if (m1<0 .or. m1>2 .or. m2<0 .or. m2>2 .or. m1 /= m2) then
          print '("Marks: ",i0,1x,i0," are invalid ")', m1, m2
          error stop
        endif
        m1 = m1+1
        m2 = m2+1
      end associate
    end subroutine mark_path



    pure integer function reverse_direction(idir) result(irev)
      integer, intent(in) :: idir
      select case(idir)
      case(1)
        irev = 2
      case(2)
        irev = 1
      case(3)
        irev = 4
      case(4)
        irev = 3
      case default
        error stop 'reverse_direction - invalid direction' 
      end select
    end function reverse_direction



! ==================
! Djikstra algorithm
! ==================



    subroutine shortest_path(nodes, src, dest, dd)
      class(mazenode_t), intent(inout) :: nodes(:)
      integer, intent(in) :: src, dest
      integer, allocatable, intent(out) :: dd(:)

      logical, allocatable :: isdone(:)
      integer :: n, i, j, idngb

      ! Remove links to walls from "nodes". Set walls as "visited" so they
      ! do not appear in the queue.
      n = size(nodes)

      allocate(isdone(n))
      allocate(dd(n))
      dd = huge(dd)
      isdone = .false.

      do i=1, n
        if (nodes(i) % lab <= WALL) then
          isdone(i) = .true.
          dd(i) = -1
        end if
        do j=1,4
          idngb = nodes(i) % ngbs(j)
          if (idngb > 0 .and. idngb <= n) then
            if (nodes(idngb)%lab<=WALL) nodes(i) % ngbs(j) = 0
          end if
        end do
      end do

      dd(src) = 0
      MAIN: do

        i = minloc2(dd, .not. isdone)
        if (i == 0) exit MAIN

        do j=1,4
          idngb = nodes(i) % ngbs(j) 
          if (idngb <= 0) cycle    ! skip "null" neighbors
          if (isdone(idngb)) cycle ! skip "visited" neighbors
          if (dd(idngb) > dd(i)+1) then
            dd(idngb) = dd(i)+1
          end if
        end do
        isdone(i) = .true.
      end do MAIN


      print '("Distance to source/destination: ",i0,1x,i0)', dd(src), dd(dest)
      print '("Maximum distance from source ",i0)', maxval(dd)
 print '("AoC day 15 answers valid? ",l2,l2)', dd(dest)==218, maxval(dd)==544

    contains
      pure integer function minloc2(arr, mask) result(mloc)
        integer, intent(in) :: arr(:)
        logical, intent(in) :: mask(:)
        integer :: i, mval

        mval = huge(mval)
        mloc = 0
        do i = 1, size(arr)
          if (.not. mask(i)) cycle
          if (arr(i) >= mval) cycle
          mval = arr(i)
          mloc = i
        end do
      end function minloc2
        
    end subroutine shortest_path

  end module day1915_mod
