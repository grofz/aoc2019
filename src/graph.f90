  module graph_mod
    implicit none

    integer, parameter :: LABEL_LEN=6

    type, public :: vertex_t
      character(len=LABEL_LEN) :: label=''
      integer, allocatable     :: cons(:)       ! indices to "elist"
      logical                  :: isopen = .true.
    end type
    interface vertex_t
      module procedure vertex_new
    end interface


    type, public :: edge_t
      integer :: src, dest
      integer :: weight = 1
      logical :: isopen = .true.
    end type
    interface edge_t
      module procedure edge_new
    end interface


    type, public :: graph_t
      private
      integer                     :: nv=-1, ne=-1
      type(vertex_t), allocatable :: vlist(:)
      type(edge_t), allocatable   :: elist(:)
    contains
      procedure :: Addnode => graph_addnode ! @(lab)
      procedure :: Addedge => graph_addedge ! @(lab_src, lab_dest, [w(1:1)|w(1:2)])
      procedure :: SetIsopen => graph_setisopen ! @(lab, isopen)
      procedure :: ListVertices => graph_listvertices
      procedure :: ListEdges => graph_listedges
      procedure :: Djikstra ! @(labsrc, [labdst])
      procedure :: GetVertex => graph_getvertex
      procedure :: GetEdge => graph_getedge
      procedure :: Nvertices => graph_nvertices
      procedure :: Nedges => graph_nedges
    end type
    interface graph_t
      module procedure graph_newempty
    end interface


  contains

!
! Constructors of graph_t, vertex_t, edge_t
!
    pure function graph_newempty() result(this)
      type(graph_t) :: this
      integer, parameter :: INIT_SIZE = 10
      this % nv = 0
      this % ne = 0
      allocate(this%vlist(INIT_SIZE), this%elist(INIT_SIZE))
    end function

    pure function vertex_new(label) result(this)
      type(vertex_t) :: this
      character(len=*), intent(in) :: label
      this % label = label
      allocate(this%cons(0))
    end function

    pure function edge_new(src, dest, weight) result(this)
      type(edge_t) :: this
      integer, intent(in)           :: src, dest
      integer, intent(in), optional :: weight
      this % src = src
      this % dest = dest
      if (present(weight)) this % weight = weight
    end function



    pure subroutine graph_addnode(this, label)
      class(graph_t), intent(inout) :: this
      character(len=*), intent(in)  :: label
!
! Add new vertex to the graph or do nothing if the same vertex is already present
!
      integer :: ind

      ind = findvertex0(this%vlist(1:this%nv), label)
      ! Do nothing if vertex exists
      if (ind /= 0) return 
      ! Add it to vlist (extend vlist if full)
      if (this%nv == size(this%vlist)) call extendvlist(this%vlist, this%nv)
      this%nv = this%nv + 1
      this%vlist(this%nv) = vertex_t(label)
    end subroutine graph_addnode



    subroutine graph_addedge(this, label_src, label_dest, weight)
      class(graph_t), intent(inout) :: this
      character(len=*), intent(in) :: label_src, label_dest
      integer, intent(in), optional :: weight(:)
!
! Add one edge (default) or two edges between vertices. 
! One-way edge is added if "weight" is an array of one item.
!
      integer :: weight0(2), nw0, ind_src, ind_dest

      ! Default or user-given weight of edge(s)
      nw0 = 1
      weight0 = [1, 1]
      if (present(weight)) then
        select case(size(weight))
        case(1)
          nw0 = 1
          weight0(1:1) = weight
        case(2)
          nw0 = 2
          weight0 = weight
        case default
          error stop 'graph_addedge - weight must be array of size 1 or 2'
        end select
      end if

      ! vertices must already exist
      ind_src  = findvertex0(this%vlist(1:this%nv), label_src)
      ind_dest = findvertex0(this%vlist(1:this%nv), label_dest)
      if (ind_src==0 .or. ind_dest==0) error stop 'graph_addedge - vertices must exist'

      ! add new edge, or do nothing if it already exists
      call localadd(ind_src, ind_dest, weight0(1))

      ! add another edge in reverse direction (if requested)
      if (nw0 == 2) call localadd(ind_dest, ind_src, weight0(2))

    contains
      subroutine localadd(src, dest, w)
        integer, intent(in) :: src, dest, w
        integer :: ind_j

        ind_j = findedge(this%elist(1:this%ne), src, dest)
        if (ind_j == 0) then
          if (this%ne==size(this%elist)) call extendelist(this%elist, this%ne)
          this%ne = this%ne + 1
          this%elist(this%ne) = edge_t(src, dest, w)

          ! add edge to "cons" in source vertex
          associate(cons => this%vlist(src)%cons)
            if (size(pack(cons, cons==this%ne)) /= 0) &
                error stop 'addedge - pointer to edge existed before edge was created'
            this%vlist(src)%cons = [cons, this%ne]
          end associate
        else
          ! just check the edge is present in "cons"
          associate(cons => this%vlist(src)%cons)
            if (size(pack(cons, cons==ind_j)) /= 1) &
                error stop 'addedge - no pointer to existing edge in cons'
          end associate
        end if

      end subroutine
    end subroutine graph_addedge



    subroutine graph_setisopen(this, label, isopen)
      class(graph_t), intent(inout) :: this
      character(len=*), intent(in)  :: label
      logical, intent(in)           :: isopen
!
! Open or close respective vertex
!
      integer :: ind
      ind = findvertex0(this%vlist(1:this%nv), label)
      if (ind == 0) error stop 'graph_setisopen - vertex not in list'
      this%vlist(ind) % isopen = isopen
    end subroutine



    subroutine graph_listvertices(this)
      class(graph_t), intent(in) :: this

      integer :: i, k

      if (this%nv==0) then
        print '("Graph contains no vertices")'
        return
      end if
      do i=1,this%nv
        associate (v => this%vlist(i))
        write(*, '(a6,"(",l1,") | ")', advance='no') adjustr(v%label), v%isopen
        do k=1,size(v%cons)
          associate (e => this%elist(v%cons(k)))
          write(*, '(i0,"(",l1,")-",a6,"(",l1,")")', advance='no') &
            e%weight, e%isopen, adjustr(this%vlist(e%dest)%label), this%vlist(e%dest)%isopen
          end associate
          if (k/=size(v%cons)) then
            write(*,'(a)',advance='no') ', '
          end if
        end do
        end associate
        write(*,*)
      end do
      print '("Vertices ",i0,"  Edges ",i0)', this%nv, this%ne
    end subroutine



    subroutine graph_listedges(this)
      class(graph_t), intent(in) :: this
    end subroutine



    pure function graph_getvertex(this, ind) result(v)
      class(graph_t), intent(in) :: this
      integer, intent(in)        :: ind
      type(vertex_t)             :: v
      if (ind<1 .or. ind>this%nv) error stop 'getvertex - index is invalid'
      v = this%vlist(ind)
    end function

    pure function graph_getedge(this, ind) result(e)
      class(graph_t), intent(in) :: this
      integer, intent(in)        :: ind
      type(edge_t)               :: e
      if (ind<1 .or. ind>this%ne) error stop 'getedge - index is invalid'
      e = this%elist(ind)
    end function

    pure function graph_nvertices(this) result(nv)
      class(graph_t), intent(in) :: this
      integer                    :: nv
      nv = this % nv
    end function

    pure function graph_nedges(this) result(ne)
      class(graph_t), intent(in) :: this
      integer                    :: ne
      ne = this % ne
    end function




    ! NOT USED PROBABLY REMOVE
    pure function vertex_ispath(this, k, g) result(ispath)
      class(vertex_t), intent(in) :: this
      integer, intent(in)         :: k
      class(graph_t), intent(in)  :: g
      logical                     :: ispath

      integer :: ind_j, src, dest

      if (k<1 .or. k>size(this%cons)) error stop 'vertex_ispath - k is invalid'
      ind_j = this%cons(k)
      if (ind_j<1 .or. ind_j>g%ne) error stop 'vertex_ispath - cons points at invalid edge'
      src = g%elist(ind_j) % src
      dest = g%elist(ind_j) % dest
      if (src<1 .or. src>g%nv .or. dest<1 .or. dest>g%nv) &
          error stop 'vertex_ispath - edge points at invalid vertices'
      ispath = g%vlist(src)%isopen .and. g%elist(ind_j)%isopen .and. g%vlist(dest)%isopen
    end function



    pure function findvertex0(vlist, label) result(ind)
      type(vertex_t), intent(in)   :: vlist(:)
      character(len=*), intent(in) :: label
      integer                      :: ind
!
! Return index of the vertex in the list or "0" if vertex not present
!
      integer :: i
      character(len=LABEL_LEN) :: label0

      ind = 0
      label0 = label
      do i=1,size(vlist)
        if (vlist(i)%label /= label0) cycle
        ind = i
        exit
      end do
    end function findvertex0


    pure function findvertex1(v, g, label) result(ind)
      class(vertex_t), intent(in)  :: v
      class(graph_t), intent(in)   :: g
      character(len=*), intent(in) :: label
      integer                      :: ind
!
! Return index in the list of connections or "0" if connection to vertex not present
!
      integer :: k, ind_src, ind_dest
      character(len=LABEL_LEN) :: label0

      ind = 0
      label0 = label
      ind_dest = findvertex0(g%vlist(1:g%nv), label0)
      ind_src = findvertex0(g%vlist(1:g%nv), v%label)
      do k=1, size(v%cons)
        if (v%cons(k) < 1 .or. v%cons(k) > g%ne) &
            error stop 'findvertex1 - dangling index in cons'
        if (g%elist(v%cons(k))%src/=ind_src .or. g%elist(v%cons(k))%dest/=ind_dest) cycle
        ind = k
        exit
      end do
    end function findvertex1



    pure function findedge(elist, ind_src, ind_dest) result(ind)
      type(edge_t), intent(in) :: elist(:)
      integer, intent(in)      :: ind_src, ind_dest
      integer                  :: ind

      integer :: j

      ind = 0
      do j=1,size(elist)
        if (elist(j)%src /= ind_src .or. elist(j)%dest /= ind_dest) cycle
        ind = j
        exit
      end do
    end function



    pure subroutine extendvlist(vlist, nv)
      type(vertex_t), intent(inout), allocatable :: vlist(:)
      integer, intent(in) :: nv

      type(vertex_t), allocatable :: extended(:)

      allocate(extended(2*size(vlist)))
      extended(1:nv) = vlist(1:nv)
      call move_alloc(extended, vlist)
    end subroutine

    pure subroutine extendelist(elist, ne)
      type(edge_t), intent(inout), allocatable :: elist(:)
      integer, intent(in) :: ne

      type(edge_t), allocatable :: extended(:)

      allocate(extended(2*size(elist)))
      extended(1:ne) = elist(1:ne)
      call move_alloc(extended, elist)
    end subroutine



    function djikstra(this, labsrc, labdst) result(dd)
      class(graph_t), intent(in) :: this
      character(len=*), intent(in) :: labsrc
      character(len=*), intent(in), optional :: labdst
      integer, allocatable :: dd(:)

      logical, allocatable :: isdone(:)
      integer :: indsrc, inddst, ind, k, indngb, j

      ! Convert labels to indices in "vlist"
      indsrc = findvertex0(this%vlist(1:this%nv), labsrc)
      if (indsrc==0) error stop 'djikstra - source node not found'
      if (present(labdst)) then
        inddst = findvertex0(this%vlist(1:this%nv), labdst)
        if (inddst==0) error stop 'djikstra - destination node not found'
      else
        inddst = 0
      end if

      ! Prepare working arrays
      allocate(dd(this%nv), isdone(this%nv))

      dd = huge(dd)
      where (this%vlist(1:this%nv)%isopen)
        isdone = .false.
      else where
        isdone = .true.
      end where

      ! Distance for the source node is zero.
      ! If source node is not open, everything is not accesible and we are done.
      if (this%vlist(indsrc) % isopen) then
        dd(indsrc) = 0
      else
        return
      end if

      MLOOP: do
        ind = minloc2(dd, .not. isdone)
        if (ind == 0) exit MLOOP

        associate(v => this%vlist(ind))
        do k=1,size(v%cons)
          j = v%cons(k)
          indngb = this%elist(j) % dest
          ! skip neighbors with closed path
          if (.not. this%elist(j) % isopen) cycle
          ! skip visited neighbors (closed neighbors were marked visited)
          if (isdone(indngb)) cycle
          if (dd(indngb) > dd(ind)+this%elist(j)%weight) &
              dd(indngb) = dd(ind)+this%elist(j)%weight
        end do
        end associate
        isdone(ind) = .true.

        ! if destination is given, we do not need to complete "dd"
        if (ind==inddst) exit
      end do MLOOP

    contains
      ! just because compiler does not know "minloc"
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
    end function djikstra

  end module graph_mod
