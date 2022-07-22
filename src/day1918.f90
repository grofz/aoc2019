  module day1918_mod
    use graph_mod
    use tree_m, only : rbtr_t, DAT_KIND, tree_mold
    use abstract_container, only : ERR_CONT_OK, ERR_CONT_IS, ERR_CONT_ISNOT
    implicit none

    integer, parameter :: DIR_VEC(2,4)=reshape([1,0, -1,0, 0,1, 0,-1],[2,4])
    integer, parameter :: MAX_KEYS=30

    type state_t
      integer :: pos=0
      integer :: nk=0, kmin=0
      logical :: locked(MAX_KEYS)
      integer :: cost=0
    end type state_t
    interface state_t
      module procedure state_new
    end interface

  contains

    function state_new(g) result(this)
      class(graph_t), intent(in) :: g
      type(state_t)              :: this
      integer :: i, kmin, kmax
      type(vertex_t) :: ver

      kmin = 1000
      kmax = -1000
      do i=1,g%Nvertices()
        ver = g%GetVertex(i)
        if (ishome(ver)) then
          this % pos = i
        end if
        if (.not. iskey(ver)) cycle
        if (iachar(ver%label(1:1)) < kmin) kmin = iachar(ver%label(1:1))
        if (iachar(ver%label(1:1)) > kmax) kmax = iachar(ver%label(1:1))
      end do
      this % nk = kmax-kmin+1
      this % kmin = kmin
      this%locked(1:this%nk) = .true.
    end function



    subroutine lock_unlock(g, s)
      type(graph_t), intent(inout) :: g
      type(state_t), intent(in)    :: s
      integer :: i, j
      type(vertex_t) :: ver

      do i=1,g%Nvertices()
        ver = g%GetVertex(i)
        if (isdoor(ver)) then
          j = iachar(ver%label(1:1))-iachar('A')+1
        elseif (iskey(ver)) then
          j = iachar(ver%label(1:1))-iachar('a')+1
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



    function list_next_allowed(g, s) result(next)
      type(graph_t), intent(inout) :: g
      !type(graph_t), intent(in) :: g
      type(state_t), intent(in) :: s
      type(state_t), allocatable :: next(:)

  !   type(graph_t) :: g0  
      type(state_t) :: wrk(100)
      type(vertex_t) :: current_ver, ver
      type(edge_t) :: edge
      integer :: nstates, i, j, k, i2, mind
      integer, allocatable :: dd(:)
      logical :: connected

  !   g0 = g
      call lock_unlock(g, s)
      current_ver = g%GetVertex(s%pos)
      dd = g % Djikstra(current_ver%label)
      nstates = 0
      
      do i=1,g%Nvertices()
        ver = g%GetVertex(i)
        ! only keys are considered
        if (.not. iskey(ver)) cycle

        ! ignore already taken keys
        j = iachar(ver%label(1:1))-iachar('a')+1
        if (.not. s%locked(j)) cycle

        ! look if key is accessible from open are, ignore if not
        !if (dd(i)==huge(dd)) cycle
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
        wrk(nstates) % pos = i
        wrk(nstates) % cost = wrk(nstates) % cost + mind
      end do
      allocate(next(nstates))
      next = wrk(1:nstates)
    end function list_next_allowed



    recursive subroutine search(s, g, lev, sbest)
      type(state_t), intent(in) :: s
      type(graph_t), intent(inout) :: g
      integer, intent(in) :: lev(:)
      type(state_t), intent(out) :: sbest

      type(state_t), allocatable :: snext(:), sout(:)
      integer :: i, mincost, mini

 !    do i=1,size(lev)
 !      write(*,'(a)', advance='no') '>'
 !    end do
 !    write(*,'("cost ",i0," pos ",i0,*(l2))') s%cost,s%pos,s%locked(1:s%nk)
 !    do i=1,size(lev)
 !      write(*,'(a)', advance='no') '>'
 !    end do
      write(*,'(*(i0,1x))',advance='no') lev
      write(*,'(a)') achar(27)//'c'

      snext = list_next_allowed(g, s)
      if (size(snext)==0) then
        sbest = s
        goto 900
      end if

      allocate(sout(size(snext)))
      mincost = huge(mincost)
      mini = 0
      do i=1,size(snext)
        call search(snext(i), g, [lev,size(snext)-i], sout(i))
        if (sout(i)%cost < mincost) then
          mincost = sout(i)%cost
          mini = i
        end if
      end do
      sbest = sout(mini)

      900 continue
 !    do i=1,size(lev)
 !      write(*,'(a)', advance='no') '<'
 !    end do
 !    write(*,'("     ",i0," pos ",i0,*(l2))') sbest%cost,sbest%pos,sbest%locked(1:sbest%nk)
    end subroutine



    subroutine search2(s0, g, sout)
      type(state_t), intent(in) :: s0
      type(graph_t), intent(inout) :: g
      type(state_t), intent(out) :: sout

      type(rbtr_t) :: tree
      type(state_t), allocatable :: snext(:)
      type(state_t) :: scur, sold
      integer(DAT_KIND), allocatable :: handle(:)
      integer :: ierr, i
      logical :: isfinal
        
      tree = rbtr_t(hash_compare)
      call tree%Add(transfer(s0,tree_mold))

      MLOOP: do
        call tree%Firstnode(handle,ierr)
        if (ierr /= ERR_CONT_OK) exit
        scur = transfer(tree%Read(handle), scur)

        ! Test if it is the end node?
      write(*,'(a)') achar(27)//'c'
 print *, 'Taken from queue', count(scur%locked(1:scur%nk)), tree%count()
 print *, scur%cost, scur%pos, scur%locked(1:scur%nk) 

        if (all(.not. scur%locked(1:scur%nk))) then
          print *, 'final state'
          if (tree % Count() /= 1) then
            print *, 'some nodes remaining', tree%Count()


            ! print remaining
            call tree%resetcurrent(handle)
            do
              scur=transfer(tree%NextRead(handle,ierr),scur)
              if (ierr/=0) exit
 print *, scur%cost, scur%pos, scur%locked(1:scur%nk)
            end do

            stop
          endif
          sout = scur
          exit
        end if

        call tree % Remove(tree % Read(handle))
        snext = list_next_allowed(g, scur)
!print *, 'new steps found ',size(snext)
        do i=1, size(snext)
          if (all(.not. snext(i)%locked(1:snext(i)%nk))) then
            isfinal = .true.
          else
            isfinal = .false.
          endif

          call tree % Find(transfer(snext(i), tree_mold),handle,ierr)
          select case(ierr)
          case(ERR_CONT_ISNOT)
            call tree % Add(transfer(snext(i), tree_mold))
!print *, 'Added new'
!print *, snext(i)%cost, snext(i)%pos, snext(i)%locked(1:snext(i)%nk)
if (isfinal) then
 print *, 'solution found'
 print *, snext(i)%cost, snext(i)%pos, snext(i)%locked(1:snext(i)%nk)
end if

          case(ERR_CONT_OK)
            ! state already exists
            sold = transfer(tree % Read(handle), sold)
!print *, 'New / Old exists'
!print *, snext(i)%cost, snext(i)%pos, snext(i)%locked(1:snext(i)%nk)
!print *, sold%cost, sold%pos, sold%locked(1:sold%nk)
            if (sold % cost > snext(i) % cost) then
              call tree % UpdateCurrent(handle, transfer(snext(i),tree_mold))
if (isfinal) then
 print *, 'solution improved'
 print *, snext(i)%cost, snext(i)%pos, snext(i)%locked(1:snext(i)%nk)
end if
            else
!print *, 'Left unchanged'
            end if
          case default
                  print *, ierr
            error stop 'uknonwn ierr'
          end select
        end do

!print *, 'nect setep', tree%count()
!print *
      end do MLOOP
   end subroutine search2








    function graph_from_raw(arr) result (g)
      character(len=*), intent(in) :: arr(:,:)
      type(graph_t) :: g

      integer :: nx, ny, i, j, i1, j1, k
      character(len=:), allocatable :: lab_src, lab_dst

      ! Initialize graph
      g = graph_t()
      ny = size(arr,1)
      nx = size(arr,2)

      ! First pass - add nodes
      do i=1,nx
      do j=1,ny
        ! ignore walls
        if (arr(j,i)=='#') cycle 
        ! add keys, doors or corridors
        call g%Addnode(label(arr(j,i),j,i))
      end do
      end do

      ! Second pass - add edges. Assuming edges are walls
      do i=2,nx-1
      do j=2,ny-1
        if (arr(j,i)=='#') cycle 
        lab_src = label(arr(j,i),j,i)
        do k=1,4
          i1 = i + DIR_VEC(1,k)
          j1 = j + DIR_VEC(2,k)
          if (arr(j1,i1)=='#') cycle 
          lab_dst = label(arr(j1,i1),j1,i1)
          call g%Addedge(lab_src, lab_dst, [1])
        end do
      end do
      end do

    contains
      function label(ch,i,j) 
        character(len=*), intent(in) :: ch
        integer, intent(in)          :: i, j
        character(len=:), allocatable :: label
        ! get label for corridors, keys, doors
        if (ch=='.') then
          label=num2lab(i, j)
        else
          label=ch
        end if
      end function label
    end function graph_from_raw



    function graph2_from_graph(gbig) result(gsim)
      class(graph_t), intent(in) :: gbig
      type(graph_t)              :: gsim

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
        !if (iachar(vert%label(1:1))>=iachar('@') .and. &
        !    iachar(vert%label(1:1))<=iachar('z')) call gsim % Addnode(vert%label)
        if (ishome(vert) .or. iskey(vert) .or. isdoor(vert)) &
            call gsim % Addnode(vert%label)
      end do

      ! Loop over all nodes
      do i=1, gsim%Nvertices()
        vert = gsim%Getvertex(i)

        ! close all nodes (except "vert" and "corridors")
        do i1=1, gwrk%Nvertices()
          vert1 = gwrk%GetVertex(i1)
          if (iachar(vert1%label(1:1))<=iachar('9') .or. vert1%label==vert%label) then
            call gwrk%SetIsopen(vert1%label,.true.)
          else
            call gwrk%SetIsopen(vert1%label,.false.)
          end if
        end do

        ! Find accessible nodes from current
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

          ! If accessible, add the path
          if (connected) call gsim%Addedge(vert%label,vert1%label,[mind+1])
        end do
      end do

    end function graph2_from_graph


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



! ===============
! HASHING STATES
! ===============

  pure integer function eval_state(s) result(val)
    type(state_t), intent(in) :: s
    integer :: i
    val = 1
    do i=1,s%nk
      if ( s%locked(i)) val = val + 2**(i)
    end do
    val = val * 100
    val = val + s%pos
  end function


  pure integer function hash_compare(a, b) result(res)
    integer(DAT_KIND), intent(in) :: a(:), b(:)
!
! user function for the TREE hash
!
    type(state_t) :: ahash, bhash
    integer :: aval, bval
    ahash = transfer(a, ahash)
    bhash = transfer(b, bhash)
    aval = eval_state(ahash)
    bval = eval_state(bhash)

    if (aval == bval) then
      res = 0
    else if (aval < bval) then
      res = 1
    else
      res = -1
    end if
  end function hash_compare



  end module day1918_mod
