!
! Day 20 - recursive maze
!
  module day1920_mod
    use graph_mod, only : graph_t, edge_t, vertex_t
    implicit none

    integer, parameter :: DIR_VEC(2,4) = reshape([0,1,1,0,0,-1,-1,0],[2,4])

  contains

    function shortest_distance(g) result(dist)
      type(graph_t), intent(in) :: g
      integer                   :: dist

      integer :: id0, id1
      integer, allocatable :: dd(:)

      id0 = g%Findvertex('AA')
      id1 = g%Findvertex('ZZ')
      if (id0==0 .or. id1==0) then
        id0 = g%Findvertex('AA00')
        id1 = g%Findvertex('ZZ00')
        if (id0==0 .or. id1==0) error stop 'labels AA(00) or ZZ(00) not found'
        dd = g%Djikstra('AA00','ZZ00')
      else
        dd = g%Djikstra('AA','ZZ')
      end if
      !
      ! Distance between corridor tiles in graph is "2". 
      ! Distance from corridor to portal is "1".
      !   (.) --2--> (.) --1--> (AB) --1--> (.) --2--> (.)
      ! When the result is divided by 2, it effectively
      ! makes the distance "corridor-portal-corridor" same
      ! length as the distance "corridor-corridor"
      !
      dist = dd(id1)-dd(id0)
      if (dist==huge(dist)) then
        print '("No path has been found")'
        stop
      end if
      dist = dist - 2  ! remove steps AA->start and end->ZZ
      if (mod(dist,2)/=0) error stop 'dist must be even'
      dist = dist / 2  
    end function shortest_distance



    function graph_from_raw(arr,mode) result(g)
      character(len=*), intent(in) :: arr(:,:)
      type(graph_t)                :: g
      integer, intent(in)          :: mode !1=portals, 2=recursive doors
!
! Create graph from maze stored as an array of one-length characters
!
      character(len=:), allocatable :: lab, labngb
      integer :: nx, ny, i, j, dir, i1, j1

      ! Initialize graph
      g = graph_t()
      nx = size(arr,1) ! columns
      ny = size(arr,2) ! lines

      ! First pass - add nodes
      do j=2,ny-1
      do i=2,nx-1
        lab = label_from_arr(arr,i,j,mode)
        if (lab /= '') call g%Addnode(lab)
      end do
      end do

      ! Second pass - add edges
      do j=2,ny-1
      do i=2,nx-1
        lab = label_from_arr(arr,i,j,mode)
        if (lab=='') cycle
        do dir=1,4
          i1 = i + DIR_VEC(1,dir)
          j1 = j + DIR_VEC(2,dir)
          if (i1<2 .or. j1<2 .or. i1>nx-1 .or. j1>ny-1) cycle
          labngb = label_from_arr(arr,i1,j1,mode)
          if (labngb == '') then
            cycle
          else if (len_trim(labngb)<6 .or. len_trim(lab)<6) then
            ! corridor and portal tiles (one half of normal length)
            call g%Addedge(lab, labngb, [1])
          else
            ! edge between two corridor riles 
            call g%Addedge(lab, labngb, [2])
          end if
        end do
      end do
      end do

    end function graph_from_raw



    function reduced_graph(g0) result(gred)
      type(graph_t), intent(in) :: g0
      type(graph_t)             :: gred
!
! Make reduced graph by representing the chains of coorridor tile nodes as
! the edges between portal nodes.
!
      integer :: i, i1
      integer, allocatable :: dd(:)
      type(vertex_t) :: ver, ver1

      ! Copy all portal nodes from the full maze
      gred = graph_t()
      do i=1,g0 % NVertices()
        ver = g0 % GetVertex(i)
        if (len_trim(ver%label)<=4) call gred % Addnode(ver%label)
      end do

      ! Add edge if there is a path between two portal nodes
      MLOOP: do i=1,gred % NVertices()
        ver = gred % GetVertex(i)
        dd = g0 % Djikstra(ver%label)
        do i1=1,g0 % NVertices()
          ver1 = g0 % GetVertex(i1)
          if (len_trim(ver1%label)>4) cycle
          if (dd(i1)==huge(dd)) cycle
          if (dd(i1)==0) cycle
          call gred % Addedge(ver%label, ver1%label, [dd(i1)])
        end do
      end do MLOOP

    end function reduced_graph



    function repeat_graph(gmold, nlev) result(g)
      type(graph_t), intent(in) :: gmold
      integer, intent(in)       :: nlev
      type(graph_t)             :: g
!
! Make a "recursive" maze down to level "nlev" by copying the "gmold"
! maze. Label of portals has to be renamed at each maze level:
!
! level 0 --> outside portal XX00  |  inside portal XX01
! level 1 --> outside portal XX01  |  inside portal XX02
! ...
! level k --> outside portal XX-k  |  inside portal XX-k+1   

      integer :: ilev, i, i1, k
      type(vertex_t) :: ver, ver1
      type(edge_t)   :: ed

      g = graph_t()

      LEVEL: do ilev=0, nlev
        ! Copy vertices
        do i=1, gmold % NVertices()
          ver = gmold % Getvertex(i)
          call g % Addnode(label_plus(ver%label,ilev))
        end do

        ! Copy edges
        do k=1, gmold % Nedges()
          ed = gmold % Getedge(k)
          ver  = gmold % Getvertex(ed % src)
          ver1 = gmold % Getvertex(ed % dest)
          call g % Addedge(label_plus(ver%label,ilev),&
                           label_plus(ver1%label,ilev), [ed % weight])
        end do
      enddo LEVEL

    end function repeat_graph



    function label_plus(lab0,iadd) result(lab1)
      character(len=*), intent(in)  :: lab0
      integer, intent(in)           :: iadd
      character(len=len_trim(lab0)) :: lab1

      integer :: i, j

      i = len_trim(lab0)-1
      read(lab0(i:i+1),*) j
      lab1 = lab0
      write(lab1(i:i+1),'(i2.2)') j+iadd
    end function



    function label_from_arr(arr,i,j,mode) result(label)
      character(len=*), intent(in)  :: arr(:,:)
      integer, intent(in)           :: i, j
      character(len=:), allocatable :: label, label0
      integer, intent(in)           :: mode

      character(len=3) :: xlab, ylab
      integer :: i1, j1, dir

      if (i<2 .or. i>size(arr,1)-1 .or. j<2 .or. j>size(arr,2)-1) &
        error stop 'label_from_arr - items at border should be ignored'

      if (arr(i,j)=='#' .or. arr(i,j)==' ') then
        ! space and walls are ignored
        allocate(character(len=0) :: label)

      else if (arr(i,j)=='.') then
        ! unique name for corridor point
        write(xlab,'(i3.3)') i
        write(ylab,'(i3.3)') j
        allocate(character(len=len_trim(xlab)+len_trim(ylab)) :: label)
        label = trim(xlab)//trim(ylab)

      else if (iachar(arr(i,j)(1:1))>=iachar('A') .and. &
               iachar(arr(i,j)(1:1))<=iachar('Z')) then
        ! portal name
        ! find which neighbor is corridor
        do dir=1,4
          if (arr(i+DIR_VEC(1,dir), j+DIR_VEC(2,dir))=='.') exit
        end do
        if (dir==5) then
          ! letter is not next to the corridor
          allocate(character(len=0) :: label)
        else
          ! second letter is in the oposite direction from corridor tile
          dir = mod(dir+1,4)+1
          i1 = i+DIR_VEC(1,dir)
          j1 = j+DIR_VEC(2,dir)
          allocate(character(len=2) :: label)
          ! labels are named in a top-bottom or left-right order
          label= arr(min(i,i1),min(j,j1))//arr(max(i,i1),max(j,j1))

          ! For recursive maze - portals at outer/inner wall are different
          if (mode==2) then
            if (i==2 .or. i==size(arr,1)-1 .or. j==2 .or. j==size(arr,2)-1) then
              label = label//'00'
            else
              label = label//'01'
            end if
          end if
        end if

      else
        print *, '"'//arr(i,j)//'"'
        error stop 'label_from_arr - unexpected character'
      end if
      
    end function label_from_arr


  end module day1920_mod
