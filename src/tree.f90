! Classes for a binary search tree (BST) container.
!
! grofz@vscht.cz
! March 2022
!
! Version: Work in progress... Submoduled
!
  module tree_m
    use kinds_m, only : I4B, I1B
    use abstract_container
    implicit none
    private
    public compare_fun, basetree_t, basenode_t, rbtr_t, rbnode_t
    public tree_mold

    ! Kind of integer array containing the data
    public DAT_KIND

    ! Variable used as "mold" in transfer function
    integer(DAT_KIND), allocatable :: tree_mold(:)



    ! Base tree class
    type :: basenode_t
      private
      class(basenode_t), pointer :: parent => null()
      class(basenode_t), pointer :: left => null()
      class(basenode_t), pointer :: right => null()
      integer(DAT_KIND), allocatable :: dat(:)
    contains
      procedure :: Parentf => basenode_Parent
      procedure :: Leftchild => basenode_Leftchild
      procedure :: Rightchild => basenode_Rightchild
      procedure :: Grandparent => basenode_Grandparent
      procedure :: Uncle => basenode_Uncle
      !final :: basenode_Destructor ! not needed
    end type basenode_t



    type, extends(container_t) :: basetree_t
      private
      class(basenode_t), pointer :: root => null()
      integer :: nodes = 0
      procedure(compare_fun), pointer, nopass, public :: cfun => null()
    contains
      procedure :: Initialize => basetree_Initialize2
      procedure :: Add => basetree_Add
      procedure :: Remove => basetree_Delete
      procedure :: Removeall => basetree_Removeall
      procedure :: Update => basetree_Update
      procedure :: Updatecurrent => basetree_Updatecurrent
      procedure :: Isin => basetree_Isin
      procedure :: Isempty => basetree_Isempty
      procedure :: Find => basetree_Find
      procedure :: Read => basetree_Read
      procedure :: NextRead => basetree_NextRead
      procedure :: Resetcurrent => basetree_Resetcurrent
      procedure :: Firstnode => basetree_Firstnode
      procedure :: Nextnode => basetree_Nextnode
      procedure :: Printcurrentnode => basetree_Printcurrentnode
      final :: basetree_Destructor
      procedure :: Count => basetree_Nodes
      procedure :: Isvalid_BST => basetree_Isvalid_BST
      procedure :: Height_range => basetree_Height_range
      procedure :: Print => basetree_Display
      procedure :: Copy => basetree_Copy
    end type basetree_t

    interface basetree_t
      module function basetree_Initialize(cfun) result(new)
        type(basetree_t) :: new
        procedure(compare_fun) :: cfun
      end function basetree_Initialize
    end interface basetree_t



    interface
      module subroutine basetree_Initialize2(this)
        class(basetree_t), intent(inout) :: this
      end subroutine

      module function basetree_Printcurrentnode(this, handle) result(str)
        class(basetree_t), intent(in) :: this
        integer(DAT_KIND), intent(in) :: handle(:)
        character(len=:), allocatable :: str
      end function basetree_Printcurrentnode

      module subroutine basetree_Add(this, dat, ierr)
        class(basetree_t), intent(inout) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
        integer, intent(out), optional :: ierr
      end subroutine basetree_Add

      module subroutine Add2(this, dat, newnode, ierr)
        class(basetree_t), intent(inout) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
        class(basenode_t), pointer :: newnode
        integer, intent(out), optional :: ierr
      end subroutine Add2

      module subroutine basetree_Delete(this, dat, ierr)
        class(basetree_t), intent(inout) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
        integer, optional, intent(out) :: ierr
      end subroutine basetree_Delete

      module subroutine basetree_Removeall(this)
        class(basetree_t), intent(inout) :: this
      end subroutine basetree_Removeall

      module subroutine basetree_Update(this, olddat, newdat, ierr)
        class(basetree_t), intent(inout) :: this
        integer(DAT_KIND), intent(in) :: olddat(:), newdat(:)
        integer, intent(out), optional :: ierr
      end subroutine basetree_Update

      module subroutine basetree_Updatecurrent(this, handle, newdat, ierr)
        class(basetree_t), intent(inout) :: this
        integer(DAT_KIND), intent(inout) :: handle(:)
        integer(DAT_KIND), intent(in) :: newdat(:)
        integer, intent(out), optional :: ierr
      end subroutine basetree_Updatecurrent

      module function basetree_Isin(this, dat) result(exists)
        logical :: exists
        class(basetree_t), intent(in) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
      end function basetree_Isin

      module pure function basetree_Isempty(this) result(isempty)
        logical :: isempty
        class(basetree_t), intent(in) :: this
      end function basetree_IsEmpty

      module function Search_node(this, dat) result(foundnode)
        class(basenode_t), pointer :: foundnode
        class(basetree_t), intent(in) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
      end function Search_node

      module subroutine basetree_Find(this, dat, handle, ierr)
        class(basetree_t), intent(in) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
        integer(DAT_KIND), intent(out), allocatable :: handle(:)
        integer, intent(out), optional :: ierr
      end subroutine basetree_Find

      module function basetree_Read(this, handle, ierr) result(dat)
        integer(DAT_KIND), allocatable :: dat(:)
        class(basetree_t), intent(in) :: this
        integer(DAT_KIND), intent(in) :: handle(:)
        integer, optional, intent(out) :: ierr
      end function basetree_Read

      module function basetree_NextRead(this, handle, ierr) result(dat)
        integer(DAT_KIND), allocatable :: dat(:)
        class(basetree_t), intent(in) :: this
        integer(DAT_KIND), intent(inout) :: handle(:)
        integer, optional, intent(out) :: ierr
      end function basetree_NextRead

      pure module function basetree_Nodes(this)
        integer :: basetree_Nodes
        class(basetree_t), intent(in) :: this
      end function basetree_Nodes

      module subroutine basetree_Firstnode(this, handle, ierr)
        class(basetree_t), intent(in) :: this
        integer(DAT_KIND), allocatable, intent(out) :: handle(:)
        integer, intent(out), optional :: ierr
      end subroutine basetree_Firstnode

      module pure subroutine basetree_Resetcurrent(this, handle)
        class(basetree_t), intent(in) :: this
        integer(DAT_KIND), allocatable, intent(out) :: handle(:)
      end subroutine basetree_Resetcurrent

      module subroutine basetree_Nextnode(this, handle, ierr)
        class(basetree_t), intent(in) :: this
        integer(DAT_KIND), intent(inout) :: handle(:)
        integer, intent(out), optional :: ierr
      end subroutine basetree_Nextnode

      module function basetree_Isvalid_BST(this) result(isvalid)
        logical :: isvalid
        class(basetree_t), intent(in) :: this
      end function basetree_Isvalid_BST

      module function basetree_Height_range(this) result(ht)
        integer :: ht(2)
        class(basetree_t), intent(in) :: this
      end function basetree_Height_range

      module subroutine basetree_Destructor(this)
        type(basetree_t), intent(inout) :: this
      end subroutine basetree_Destructor

      module subroutine basetree_Display(this)
        class(basetree_t), intent(in) :: this
      end subroutine basetree_Display

      module subroutine basetree_Copy(aout, bin)
        class(basetree_t), intent(out) :: aout
        class(container_t), intent(in) :: bin
      end subroutine

    end interface



    interface
      module function basenode_Parent(n) result(parentf)
        class(basenode_t), pointer :: parentf
        class(basenode_t), intent(in) :: n
      end function basenode_Parent

      module function basenode_Grandparent(n) result(grandparent)
        class(basenode_t), intent(in) :: n
        class(basenode_t), pointer :: grandparent
      end function basenode_Grandparent

      module function basenode_Leftchild(n) result(leftchild)
        class(basenode_t), pointer :: leftchild
        class(basenode_t), intent(in) :: n
      end function basenode_Leftchild

      module function basenode_Rightchild(n) result(rightchild)
        class(basenode_t), pointer :: rightchild
        class(basenode_t), intent(in) :: n
      end function basenode_Rightchild

      module function Sibling(n) result(sb)
        class(basenode_t), pointer :: sb
        class(basenode_t), intent(in), pointer :: n
      end function Sibling

      module function basenode_Uncle(n) result(uncle)
        class(basenode_t), pointer :: uncle
        class(basenode_t), intent(in) :: n
      end function basenode_Uncle

      module subroutine Rotate_Left(a, piv)
        class(basetree_t), intent(inout) :: a
        class(basenode_t), pointer :: piv
      end subroutine Rotate_Left

      module subroutine Rotate_Right(a, piv)
        class(basetree_t), intent(inout) :: a
        class(basenode_t), pointer :: piv
      end subroutine Rotate_Right

      module function Is_left_child(n)
        logical :: Is_left_child
        class(basenode_t), intent(in), pointer :: n
      end function Is_left_child

      module function Is_right_child(n)
        logical :: Is_right_child
        class(basenode_t), intent(in), pointer :: n
      end function Is_right_child

      module function Leftmost(n) result(lm)
        class(basenode_t), pointer :: lm
        class(basenode_t), pointer, intent(in) :: n
      end function Leftmost
    end interface



    ! extended class for red-black trees
    integer(I1B), parameter :: RED_NODE = 1, BLACK_NODE = 0

    type, extends(basenode_t) :: rbnode_t
      private
      integer(I1B) :: color = RED_NODE
    end type rbnode_t



    type, extends(basetree_t) :: rbtr_t
      private
    contains
      procedure :: Add => rbtr_Add
      procedure :: Remove => rbtr_Delete
      procedure :: Update => rbtr_Update
      procedure :: Updatecurrent => rbtr_Updatecurrent
      procedure :: Isvalid_rbtree => rbtr_Isvalid_rbtree
      procedure :: Printcurrentnode => rbtr_Printcurrentnode
    end type rbtr_t

    interface rbtr_t
      module function rbtr_Initialize(cfun) result(new)
        type(rbtr_t) :: new
        procedure(compare_fun) :: cfun
      end function rbtr_Initialize
    end interface rbtr_t



    interface
      module function rbtr_Printcurrentnode(this, handle) result(str)
        class(rbtr_t), intent(in) :: this
        character(len=:), allocatable :: str
        integer(DAT_KIND), intent(in) :: handle(:)
      end function rbtr_Printcurrentnode

      module subroutine rbtr_Add(this, dat, ierr)
        class(rbtr_t), intent(inout) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
        integer, intent(out), optional :: ierr
      end subroutine rbtr_Add

      module subroutine rbtr_Delete(this, dat, ierr)
        class(rbtr_t), intent(inout) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
        integer, optional, intent(out) :: ierr
      end subroutine rbtr_Delete

      module subroutine rbtr_Update(this, olddat, newdat, ierr)
        class(rbtr_t), intent(inout) :: this
        integer(DAT_KIND), intent(in) :: olddat(:), newdat(:)
        integer, intent(out), optional :: ierr
      end subroutine rbtr_Update

      module subroutine rbtr_Updatecurrent(this, handle, newdat, ierr)
        class(rbtr_t), intent(inout) :: this
        integer(DAT_KIND), intent(inout) :: handle(:)
        integer(DAT_KIND), intent(in) :: newdat(:)
        integer, intent(out), optional :: ierr
      end subroutine rbtr_Updatecurrent

      module function rbtr_Isvalid_rbtree(this) result(isvalid)
        logical :: isvalid
        class(rbtr_t), intent(in) :: this
      end function rbtr_Isvalid_rbtree

    end interface

  end module tree_m
