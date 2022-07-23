!
! Day 18 - collecting keys in the labyrinth
!
  module day1918_mod
    use graph_mod
    use tree_m, only : rbtr_t, DAT_KIND, tree_mold
    use abstract_container, only : ERR_CONT_OK, ERR_CONT_IS, ERR_CONT_ISNOT
    use kinds_m, only : I8B
    implicit none

    integer, parameter :: DIR_VEC(2,4)=reshape([1,0, -1,0, 0,1, 0,-1],[2,4])
    integer, parameter :: MAX_KEYS =30
    integer, parameter :: MAX_BITS = 6 ! bits to store robot positions 

    type state_t
      integer :: pos(4) = 0        ! position of robot(s)
      integer :: nk=0, kmin = 0    ! no of used keys / ASCII of "a"
      logical :: locked(MAX_KEYS)  ! door locked / key not collected?
      integer :: cost = 0          ! moves so far to get to this state
    contains
      procedure :: Print => state_print
    end type state_t
    interface state_t
      module procedure state_new
    end interface

  contains

    function state_new(g) result(this)
      class(graph_t), intent(in) :: g
      type(state_t)              :: this
!
! Initital state. Find initial robot positions and lock all doors.
!
      integer :: i, kmin, kmax, ir
      type(vertex_t) :: ver

      kmin = 1000
      kmax = -1000
      do i=1,g%Nvertices()
        ver = g%GetVertex(i)
        if (ishome(ver)) then
          read(ver%label(2:2),*) ir
          this % pos(ir) = i
        end if
        if (.not. iskey(ver)) cycle
        if (iachar(ver%label(1:1)) < kmin) kmin = iachar(ver%label(1:1))
        if (iachar(ver%label(1:1)) > kmax) kmax = iachar(ver%label(1:1))
      end do
      this % nk = kmax-kmin+1
      this % kmin = kmin
      this%locked(1:this%nk) = .true.
    end function



    subroutine state_print(this)
      class(state_t), intent(in) :: this
      print 100, this%cost, count(this%locked(1:this%nk)), this%pos, &
          this%locked(1:this%nk)
      100 format(1x,i4,"  locked ",i2,"   @",4(i2,1x),*(l1))
    end subroutine



    subroutine lock_unlock(g, s)
      type(graph_t), intent(inout) :: g
      type(state_t), intent(in)    :: s
!
! Using given state: "close" keys/doors that are locked/un-collected and
! "open" keys/doors that are unlocked/collected.
!
      integer :: i, j
      type(vertex_t) :: ver

      do i=1,g%Nvertices()
        ver = g%GetVertex(i)
        if (isdoor(ver)) then
          j = iachar(ver%label(1:1))-s%kmin+(iachar('a')-iachar('A'))+1
          !j = iachar(ver%label(1:1))-iachar('A')+1
        elseif (iskey(ver)) then
          j = iachar(ver%label(1:1))-s%kmin+1
          !j = iachar(ver%label(1:1))-iachar('a')+1
        else
          cycle
        end if

        if (s % locked(j)) then
          call g % SetIsopen(ver%label, .false.)
        else
          call g % SetIsopen(ver%label, .true.)
        end if
      end do
    end subroutine



    function list_next_allowed(g, s, maxr) result(next)
      type(graph_t), intent(inout) :: g
      !type(graph_t), intent(in) :: g
      type(state_t), intent(in) :: s
      integer, intent(in) :: maxr
      type(state_t), allocatable :: next(:)
  !
  ! Return vector of all valid moves (next state).
  ! (not pure to run faster - opening and closing nodes instead of
  !  making local graph copy in every run)
  !

  !   type(graph_t) :: g0  
      type(state_t) :: wrk(100)
      type(vertex_t) :: robot, ver
      type(edge_t) :: edge
      integer :: nstates, i, j, k, i2, mind, idr
      integer, allocatable :: dd(:)
      logical :: connected

  !   g0 = g
      call lock_unlock(g, s)

      nstates = 0
      RLOOP: do idr = 1, maxr
        robot = g % GetVertex(s%pos(idr))
        dd = g % Djikstra(robot%label)
      
        do i =1, g % Nvertices()
          ver = g % GetVertex(i)
          ! only keys are considered
          if (.not. iskey(ver)) cycle

          ! ignore already taken keys
          !j = iachar(ver%label(1:1))-iachar('a')+1
          j = iachar(ver%label(1:1))-s%kmin+1
          if (.not. s%locked(j)) cycle

          ! look if key is accessible from robots position, ignore if not
          connected = .false.
          mind = huge(dd)
          do k=1,size(ver%cons)
            edge = g%GetEdge(ver%cons(k))
            i2 = edge % dest
            if (dd(i2)/=huge(dd) .and. dd(i2)+edge%weight<mind) then
              connected = .true.
              mind = dd(i2)+edge%weight
            end if
          end do
          if (.not. connected) cycle

          ! "i" is the next key that could be picked-up
          nstates = nstates + 1
          wrk(nstates) = s
          wrk(nstates) % locked(j) = .false.
          wrk(nstates) % pos(idr) = i
          wrk(nstates) % cost = wrk(nstates) % cost + mind
        end do
      end do RLOOP
      allocate(next(nstates))
      next = wrk(1:nstates)
    end function list_next_allowed



    subroutine search2(s0, g, maxr, sout)
      type(state_t), intent(in)    :: s0
      type(graph_t), intent(inout) :: g
      integer, intent(in)          :: maxr
      type(state_t), intent(out)   :: sout

      type(rbtr_t) :: tree
      type(state_t), allocatable :: snext(:)
      type(state_t) :: scur, sold
      integer(DAT_KIND), allocatable :: handle(:)
      integer :: ierr, i, counter
        
      ! Unexplored states will be stored in the tree.
      ! State with more locked foors will be prioritized.
      ! If the same state was reached by a different order of moves,
      ! only the state with smallest number of moves will be stored,
      ! other states will be discarded.
      tree = rbtr_t(hash_compare)
      call tree%Add(transfer(s0,tree_mold))
      counter = 1 ! count number of states evaluated

      MLOOP: do
        call tree%Firstnode(handle,ierr)
        if (ierr /= ERR_CONT_OK) exit
        scur = transfer(tree%Read(handle), scur)
        call scur%Print()

        ! Test if it is the final state?
        ! If yes, only final states should be left in tree. 
        ! Find state with lowest cost and return.
        if (all(.not. scur%locked(1:scur%nk))) then

          call tree%resetcurrent(handle)
          sout = scur
          do
            scur=transfer(tree%NextRead(handle,ierr),scur)
            if (ierr/=0) exit
            if (any(scur%locked(1:scur%nk))) error stop 'infinished states in tree'
            call scur%Print()
            if (scur % cost < sout % cost) sout = scur
          end do
          exit MLOOP
        end if

        ! If not final, remove state from tree and find all valid next steps
        call tree % Remove(tree % Read(handle))
        snext = list_next_allowed(g, scur, maxr)
        if (size(snext)==0) error stop 'next move not found'
        counter = counter + size(snext)

        do i=1, size(snext)
          call tree % Find(transfer(snext(i), tree_mold),handle,ierr)
          select case(ierr)
          case(ERR_CONT_ISNOT)
            ! State not in tree - add it
            call tree % Add(transfer(snext(i), tree_mold))

          case(ERR_CONT_OK)
            ! State already in tree - update only if lower cost found
            sold = transfer(tree % Read(handle), sold)
            if (sold % cost > snext(i) % cost) then
              call tree % UpdateCurrent(handle, transfer(snext(i),tree_mold))
            else
            end if

          case default
            error stop 'search2 - unknonwn ierr'
          end select
        end do

      end do MLOOP
      print '("Search complete. States evaluated ",i0)', counter

   end subroutine search2




! ===============================
! PROCESS INPUT AND CREATE GRAPHS
! ===============================

    function graph_from_raw(arr) result (g)
      character(len=*), intent(in) :: arr(:,:)
      type(graph_t) :: g

      integer :: nx, ny, i, j, i1, j1, k
      integer :: robots, rpos(2,4)
      character(len=:), allocatable :: lab_src, lab_dst

      ! Initialize graph
      g = graph_t()
      ny = size(arr,1)
      nx = size(arr,2)

      ! First pass - add nodes (.,a-A,b-B,@)
      robots = 0
      do i=1,nx
      do j=1,ny
        ! ignore walls
        if (arr(j,i)=='#') cycle 
        ! add keys, doors or corridors
        if (arr(j,i)=='@') then
          robots=robots+1
          rpos(1,robots) = j
          rpos(2,robots) = i
        endif
        call g%Addnode(label(arr(j,i),j,i,robots))
      end do
      end do

      ! Second pass - add edges. Assuming at edges are walls only
      do i=2,nx-1
      do j=2,ny-1
        if (arr(j,i)=='#') cycle 
        if (arr(j,i)=='@') robots=count_robot(j,i)
        lab_src = label(arr(j,i), j, i, robots)
        do k=1,4
          i1 = i + DIR_VEC(1,k)
          j1 = j + DIR_VEC(2,k)
          if (arr(j1,i1)=='#') cycle 
          if (arr(j1,i1)=='@') robots=count_robot(j1,i1)
          lab_dst = label(arr(j1,i1),j1,i1,robots)
          call g%Addedge(lab_src, lab_dst, [1])
        end do
      end do
      end do

    contains
      function label(ch, i, j, r) 
        character(len=*), intent(in) :: ch
        integer, intent(in)          :: i, j, r
        character(len=:), allocatable :: label
        character(len=1) :: rch
        ! get label for corridors, keys, doors and starting robots
        if (ch=='.') then
          label=num2lab(i, j)
        else if (ch=='@') then
          write(rch,'(i1)') r
          label = '@'//rch
        else
          label=ch
        end if
      end function label

      integer function count_robot(j,i)
        integer, intent(in) :: j,i
        integer :: k
        count_robot = -1
        do k=1,4
          if (j==rpos(1,k) .and. i==rpos(2,k)) then
            count_robot = k
            exit
          end if
        end do
        if (count_robot==-1) error stop 'count_robot - robot not recognized' 
      end function
    end function graph_from_raw



    function graph2_from_graph(gbig) result(gsim)
      class(graph_t), intent(in) :: gbig
      type(graph_t)              :: gsim
!
! We reduce the graph by compressing corridors into edges of different lengths
!
      type(graph_t) :: gwrk
      type(vertex_t) :: vert, vert1, vert2
      type(edge_t) :: edge
      integer, allocatable :: dd(:)

      integer :: i, i1, i2, j, mind
      logical :: connected

      gwrk = gbig
      gsim = graph_t()

      ! Copy all keys/doors/home nodes from "big" graph
      do i=1, gwrk%NVertices()
        vert = gwrk%GetVertex(i)
        if (ishome(vert) .or. iskey(vert) .or. isdoor(vert)) &
            call gsim % Addnode(vert%label)
      end do

      ! Loop over all nodes: "current node"
      do i=1, gsim%Nvertices()
        vert = gsim%Getvertex(i)

        ! close all nodes (except "current" and "corridors")
        do i1=1, gwrk%Nvertices()
          vert1 = gwrk%GetVertex(i1)
          if (iachar(vert1%label(1:1))<=iachar('9') .or. vert1%label==vert%label) then
            call gwrk%SetIsopen(vert1%label,.true.)
          else
            call gwrk%SetIsopen(vert1%label,.false.)
          end if
        end do

        ! Find accessible nodes from "current"
        dd = gwrk % Djikstra(vert%label)
        do i1=1, gwrk%Nvertices()
          vert1 = gwrk%GetVertex(i1)
          if (iachar(vert1%label(1:1))<=iachar('9') .or. vert1%label==vert%label) cycle
          connected = .false.
          mind = huge(mind)
          do j=1,size(vert1%cons)
            edge = gwrk%GetEdge(vert1%cons(j))
            i2 = edge % dest
            if (dd(i2)<mind) then
              connected = .true.
              mind = dd(i2)
            end if
          end do

          ! If accessible, add the path in reduced graph
          if (connected) call gsim%Addedge(vert%label,vert1%label,[mind+1])
        end do
      end do

    end function graph2_from_graph


! ================
! HELPER FUNCTIONS
! ================

    pure function num2lab(x, y)
      integer, intent(in) :: x, y
      character(len=:), allocatable :: num2lab
      character(len=3) :: xlab, ylab
      write(xlab,'(i3.3)') x
      write(ylab,'(i3.3)') y
      allocate(character(len=len_trim(xlab)+len_trim(ylab)) :: num2lab)
      num2lab = trim(xlab)//trim(ylab)
    end function

    pure logical function iskey(vert)
      type(vertex_t), intent(in) :: vert
      iskey = iachar(vert%label(1:1)) >= iachar('a') .and. &
              iachar(vert%label(1:1)) <= iachar('z')
    end function

    pure logical function isdoor(vert)
      type(vertex_t), intent(in) :: vert
      isdoor = iachar(vert%label(1:1)) >= iachar('A') .and. &
              iachar(vert%label(1:1)) <= iachar('Z')
    end function

    pure logical function ishome(vert)
      type(vertex_t), intent(in) :: vert
      ishome = iachar(vert%label(1:1)) == iachar('@')
    end function



! =======================
! HASHING STATE FUNCTIONS
! =======================

    pure function dec2bin(num) result(bin)
      integer, intent(in) :: num
      logical :: bin(MAX_BITS)
!
! Convert number to binary
!
      integer :: i, num0, bin0(MAX_BITS)

      if (num/(2**MAX_BITS) > 0) error stop 'dec2bin - too big to convert'
      num0 = num
      do i=MAX_BITS-1, 0, -1
        bin0(i+1) = num0 / (2**i)
        num0 = num0 - bin0(i+1)*(2**i)
      end do
      if (num0 /= 0) error stop 'dec2bin - error conversion'
      bin = .false.
      where (bin0==1) bin = .true.
    end function dec2bin



    pure function eval_state(s) result(val)
      type(state_t), intent(in) :: s
      integer(I8B)              :: val
!
! State = 26bits for locked/unlocked and 4*6bit for robot positions
! (assuming there is maximum of 63 nodes)
! This 50bit state can be uniquely represented by a 64bit integer.
!
      integer :: i, j, ibit
      logical :: bin(MAX_BITS)

      val = 0
      ibit = 1
      do i=1,s%nk
        if ( s%locked(i)) val = val + 2_I8B**(ibit-1)
        ibit = ibit+1
      end do
      do j=1, 4
        bin = dec2bin(s%pos(j))
        do i=1, MAX_BITS
          if (bin(i)) val = val + 2_I8B**(ibit-1)
          ibit = ibit+1
        end do
      end do

    end function eval_state



    pure integer function hash_compare(a, b) result(res)
      integer(DAT_KIND), intent(in) :: a(:), b(:)
!
! user function for the TREE hash
!
      type(state_t) :: ahash, bhash
      integer(I8B) :: aval, bval
      ahash = transfer(a, ahash)
      bhash = transfer(b, bhash)
      aval = eval_state(ahash)
      bval = eval_state(bhash)

      ! Prioritize nodes with less doors unlocked
      if (aval == bval) then
        res = 0
      else if (count(ahash%locked(1:ahash%nk)) < &
               count(bhash%locked(1:bhash%nk))) then
        res = -1
      else if (count(ahash%locked(1:ahash%nk)) > &
               count(bhash%locked(1:bhash%nk))) then
        res = 1
      else if (aval < bval) then
        res = 1
      else
        res = -1
      end if
    end function hash_compare

  end module day1918_mod
