!
! Tree implementation (SUBMODULE)
!
  submodule(tree_m) basetree
    implicit none

    ! local type (used for exporting the pointer to node via handle)
    type basenode_ptr
      class(basenode_t), pointer :: p
    end type basenode_ptr

  contains

    module subroutine basetree_Initialize2(this)
      class(basetree_t), intent(inout) :: this
!
! Use a copy of an existing tree to make an empty tree
! (MUST REMAIN INOUT)
!
      this % root => null()
      this % nodes = 0
    end subroutine basetree_Initialize2



    module function basetree_Printcurrentnode(this, handle) result(str)
!
! Display content of the "current" node
!
      class(basetree_t), intent(in) :: this
      integer(DAT_KIND), intent(in) :: handle(:)
      character(len=:), allocatable :: str
      character(len=1000) :: dat
      type(basenode_ptr) :: cp
      cp = transfer(handle, cp)
      if (.not. associated(cp % p)) then
        str='current not allocated...'
      else
        write(dat,*) cp % p % dat
        str='['//trim(adjustl(dat))//']'
      endif
    end function basetree_Printcurrentnode



    module function basetree_Initialize(cfun) result(new)
      type(basetree_t) :: new
      procedure(compare_fun) :: cfun
!
! A compulsory constructor
!
      new % cfun => cfun
    end function basetree_Initialize



    module subroutine basetree_Add(this, dat, ierr)
      class(basetree_t), intent(inout) :: this
      integer(DAT_KIND), intent(in) :: dat(:)
      integer, intent(out), optional :: ierr
!
! Add node (for basetree type only)
!
      integer :: ierr0
      class(basenode_t), pointer :: newnode
      allocate(basenode_t :: newnode)
      call Add2(this, dat, newnode, ierr0)
      if (present(ierr)) then
        ierr = ierr0
      elseif (ierr0 /= ERR_CONT_OK) then
        error stop 'basetree_Add: element already in the tree'
      endif
    end subroutine basetree_Add



    module subroutine Add2(this, dat, newnode, ierr)
      class(basetree_t), intent(inout) :: this
      integer(DAT_KIND), intent(in) :: dat(:)
      class(basenode_t), pointer :: newnode
      integer, intent(out), optional :: ierr
!
! Insert a new node to the tree.
! The "dat" component of "newnode" must not be allocated. 
! If insertion fails due to ERR_CONT_IS, "newnode" is deallocated.
!
      integer :: ierr0

      if (.not. associated(this % cfun)) &
          error stop 'basetree_Insert: cfun procedure pointer not associated'

      ! Prepare new node to be selected to the tree
      ! Node type other than "basenode_t" must be allocated up-stream
      if (.not. associated(newnode)) &
      &   error stop 'basetree_Insert: newnode points nowhere'

      ! Fail if neccessary to prevent inadvertent data overwriting
      if (allocated(newnode % dat)) &
      &   error stop 'basetree_Insert: newnode already contains data'

      newnode % dat = dat
      if (.not. associated(this % root)) then
        ! tree is empty
        this % root => newnode
        if (this % nodes /= 0) &
        &   error stop 'basetree_Insert: nodes /= 0 for the first node'
        ierr0 = ERR_CONT_OK
      else
        call insert_recurse(this, this % root, newnode, this % cfun, ierr0)
      endif

      if (ierr0 == ERR_CONT_OK) then
        this % nodes = this % nodes + 1
      else
        !if (present(newnode)) newnode => null()
        deallocate(newnode)
      endif
      if (present(ierr)) then
        ierr = ierr0
      elseif (ierr0 /= ERR_CONT_OK) then
        error stop 'basetree_Add2: element already in the tree'
      endif
    end subroutine Add2



    recursive subroutine insert_recurse(a, root, new, cfun, ierr0)
      class(basetree_t), intent(inout) :: a
      class(basenode_t), intent(inout), pointer :: root, new
      procedure(compare_fun) :: cfun
      integer, intent(out) :: ierr0

      integer :: ires

      if (.not. associated(root)) then
        error stop "insert_recurse: something got wrong"
      endif

      ires = cfun(new % dat, root % dat)
      select case(ires)
      case(+1) ! "new % key" < "root % key"
        ! Insert node to the left-subtree
        if (associated(root % left)) then
          call insert_recurse(a, root % left, new, cfun, ierr0)
        else
          root % left => new
          new % parent => root
          ierr0 = ERR_CONT_OK
        endif

      case(-1) ! "new % key" > "root % key"
        ! Insert node to the right-subtree
        if (associated(root % right)) then
          call insert_recurse(a, root % right, new, cfun, ierr0)
        else
          root % right => new
          new % parent => root
          ierr0 = ERR_CONT_OK
        endif

      case(0)
        ! Duplicit nodes not allowed at the moment
print *, "insert_recurse: duplicit not ready yet"
        ierr0 = ERR_CONT_IS

      case default
        error stop "insert_recurse: invalid output from cfun"
      end select
    end subroutine insert_recurse



    module subroutine basetree_Update(this, olddat, newdat, ierr)
      class(basetree_t), intent(inout) :: this
      integer(DAT_KIND), intent(in) :: olddat(:), newdat(:)
      integer, intent(out), optional :: ierr

      integer :: ierr0
      class(basenode_t), pointer :: tmp, new

      ! Assert that updated value is not in tree.
      ! If ok, remove old node.
      tmp => Search_node(this, newdat)
      if (associated(tmp)) then
        ierr0 = ERR_CONT_IS
        ! if newdata semms same as olddata, this is not an error
        if (0 == this % cfun(olddat, newdat)) &
        &   call this % Remove(olddat, ierr0)
      else
        ! ierr0 will be ERR_CONT_ISNOT or ERR_CONT_OK
        call this % Remove(olddat, ierr0)
      endif
      if (present(ierr)) then
        ierr = ierr0
      elseif (ierr0 /= ERR_CONT_OK) then
        print *, 'ERROR CODE =', ierr0
        error stop 'basetree_Update: old not found or new value already in tree'
      endif
      if (ierr0 /= ERR_CONT_OK) return

      ! Re-add with a new value
      allocate(basenode_t :: new)
      call Add2(this, newdat, new)
    end subroutine basetree_Update



    module subroutine basetree_Updatecurrent(this, handle, newdat, ierr)
      class(basetree_t), intent(inout) :: this
      integer(DAT_KIND), intent(inout) :: handle(:)
      integer(DAT_KIND), intent(in) :: newdat(:)
      integer, intent(out), optional :: ierr

      integer :: ierr0, ierr1
      type(basenode_ptr) :: cp
      class(basenode_t), pointer :: old, tmp, new

      cp = transfer(handle, cp)
      old => cp % p
      if (associated(old)) then
        ! Assert that updated value is not in tree.
        ! If ok, remove old node.
        tmp => Search_node(this, newdat)
        if (associated(tmp)) then
          ierr0 = ERR_CONT_IS
          ! if newdata semms same as olddata, this is not an error
          if (0 == this % cfun(old % dat, newdat)) &
          &   call this % Remove(old % dat, ierr0)
        else
          call this % Remove(old % dat, ierr1)
          if (ierr1 /= ERR_CONT_OK) &
            error stop 'basetree_Updatecurrent: handled node not in tree'
          ierr0 = ERR_CONT_OK
        endif

      else
        ierr0 = ERR_CONT_END
      endif
      if (present(ierr)) then
        ierr = ierr0
      elseif (ierr0 /= ERR_CONT_OK) then
        error stop 'basetree_Updatecurrent: new value already in tree'
      endif
      if (ierr0 /= ERR_CONT_OK) return

      ! Re-add with a new value
      allocate(basenode_t :: new)
      call Add2(this, newdat, new)
    end subroutine basetree_Updatecurrent



    module function basetree_Isin(this, dat) result(exists)
      logical :: exists
      class(basetree_t), intent(in) :: this
      integer(DAT_KIND), intent(in) :: dat(:)
!
! Return TRUE if the node with the value "dat" is present in the tree.
! Otherwise return FALSE.
!
      class(basenode_t), pointer :: foundnode 

      if (.not. associated(this % cfun)) &
          error stop 'basetree_Exists: cfun procedure pointer not associated'

      foundnode => Search_node(this, dat)
      if (associated(foundnode)) then
        exists = .true.
        !!this % current => foundnode
      else
        exists = .false.
      endif
    end function basetree_Isin



    module function Search_node(this, dat) result(foundnode)
      class(basenode_t), pointer :: foundnode
      class(basetree_t), intent(in) :: this
      integer(DAT_KIND), intent(in) :: dat(:)

      class(basenode_t), pointer :: n
      integer :: ires

      foundnode => null()
      n => this % root
      do
        if (.not. associated(n)) exit
        ires = this % cfun(dat, n % dat)
        select case (ires)
        case(-1) ! "dat % key" > "n % key" 
          n => n % right
        case(1)  ! "dat % key" < "n % key"
          n => n % left
        case(0)  ! node found
          foundnode => n
          exit
        case default
          error stop "Exists: invalid resutl from cfun"
        end select 
      enddo
    end function Search_node



    pure module function basetree_Isempty(this) result(isempty)
      logical :: isempty
      class(basetree_t), intent(in) :: this

      if (.not. associated(this % root) .and. this % nodes == 0) then
        isempty = .true.
      elseif (associated(this % root) .and. this % nodes > 0) then
        isempty = .false.
      else
        error stop 'basetree_Isempty inconsistency found'
      endif
    end function basetree_Isempty



    module function basetree_Read(this, handle, ierr) result(dat)
      class(basetree_t), intent(in) :: this
      integer(DAT_KIND), intent(in) :: handle(:)
      integer, optional, intent(out) :: ierr
      integer(DAT_KIND), allocatable :: dat(:)
!
! Return data of the node that is pointed by "handle".
! If current points nowhere, function fails or flags an error.
!
      integer :: ierr0
      type(basenode_ptr) :: cp

      cp = transfer(handle, cp)

      if (.not. associated(this % root)) then
        !ierr0 = TREE_ERR_EMPTY
        ierr0 = ERR_CONT_END
      elseif (.not. associated(cp % p)) then
        !ierr0 = TREE_ERR_NOCURRENT
        ierr0 = ERR_CONT_END
      else
        !ierr0 = TREE_ERR_OK
        ierr0 = ERR_CONT_OK
        dat = cp % p % dat
      endif

      if (present(ierr)) then
        ierr = ierr0
      elseif (ierr0 /= ERR_CONT_OK) then
        error stop "basetree_Read: current handle is null"
      endif
    end function basetree_Read



    module function basetree_NextRead(this, handle, ierr) result(dat)
      class(basetree_t), intent(in) :: this
      integer(DAt_KIND), intent(inout) :: handle(:)
      integer, optional, intent(out) :: ierr
      integer(DAT_KIND), allocatable :: dat(:)
!
! Move "handle" to a next node and return its data.
! If "handle" is not set, data of a first node are returned.
! If "handle" points to the last node, function fails or flags an error.
!
      integer :: ierr0
      integer(DAT_KIND), parameter :: empty_data(2) = [-99, -99]
      type(basenode_ptr) :: cp

      cp = transfer(handle, cp)

      ierr0 = ERR_CONT_OK
      if (.not. associated(this % root)) then
        ierr0 = ERR_CONT_END
        dat = empty_data
      elseif (.not. associated(cp % p)) then
        cp % p => Leftmost(this % root)
        dat = cp % p % dat
      else
        cp % p => Successor(cp % p)
        if (associated(cp % p)) then
          dat = cp % p % dat
        else
          ierr0 = ERR_CONT_END
          dat = empty_data
        endif
      endif

      handle = transfer(cp, handle)

      if (present(ierr)) then
        ierr = ierr0
      elseif (ierr0 /= ERR_CONT_OK) then
        error stop "basetree_Readnext: End of tree"
      endif
    end function basetree_NextRead



    module function basenode_Parent(n) result(parentf)
      class(basenode_t), pointer :: parentf
      class(basenode_t), intent(in) :: n
!
! Type bound function to allow access to private component of "basenode"
!
      parentf => n % parent
    end function basenode_Parent  



    module function basenode_Grandparent(n) result(grandparent)
      class(basenode_t), pointer :: grandparent
      class(basenode_t), intent(in) :: n

      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        grandparent => p % Parentf()
      else
        grandparent => null()
      endif
    end function basenode_Grandparent



    module function basenode_Leftchild(n) result(leftchild)
      class(basenode_t), pointer :: leftchild
      class(basenode_t), intent(in) :: n

      leftchild => n % left
    end function basenode_Leftchild



    module function basenode_Rightchild(n) result(rightchild)
      class(basenode_t), pointer :: rightchild
      class(basenode_t), intent(in) :: n

      rightchild => n % right
    end function basenode_Rightchild



    module function Sibling(n) result(sb)
      class(basenode_t), pointer :: sb
      class(basenode_t), intent(in), pointer :: n
!
! Note: Unlile other "family" functions, this is not a type bound procedure
!
      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        if (associated(n, p % left)) then
          sb => p % right
        elseif (associated(n, p % right)) then
          sb => p % left
        else
          error stop 'basetree::Sibling ERROR. n is uknown to p'
        endif
      else
        sb => null()
      endif
    end function Sibling



    module function basenode_Uncle(n) result(uncle)
      class(basenode_t), pointer :: uncle
      class(basenode_t), intent(in) :: n

      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        uncle => Sibling(p)
      else
        uncle => null()
      endif
    end function basenode_Uncle



    module subroutine Rotate_left(a, piv)
      class(basetree_t), intent(inout) :: a
      class(basenode_t), pointer :: piv
!
! Rotate left. Fails if pivot is leaf or if pivot's right child is leaf.
!
      class(basenode_t), pointer :: par, rot
      logical :: is_lc, is_rc

      if (.not. associated(piv)) &
      &   error stop "Rotate_left: pivot is not associated"

      ! "par" is pivot's parent, it will be relinked to rotator.
      ! Rotator "rot" is a right child of pivot. It must exists.
      is_lc = Is_left_child(piv)
      is_rc = Is_right_child(piv)
      par => piv % Parentf()
      rot => piv % right
      if (.not. associated(rot)) &
      &   error stop "Rotate_left: rotator is leaf"

      ! Left child of rotator relinked as right child of pivot,
      ! and pivot will be its new parent (if it is not leaf)
      piv % right => rot % left
      if (associated(piv % right)) piv % right % parent => piv

      ! Pivot will be a left child of rotator, and
      ! rotator is new pivot's parent.
      rot % left => piv
      piv % parent => rot

      ! Relink "par <--> piv" now as "par <--> rot"
      if (.not. associated(par)) then
        ! Pivot was a root node, rotator is a new root.
        rot % parent => null()
        a % root => rot
 ! TODO temporary defensive check
 !if (is_lc .or. is_rc) error stop "Rotator_left: Defensive check"

      else
        ! Pivot was a parent's child, rotator takes its place.
        rot % parent => par
        if (is_lc .and. .not. is_rc) then
          par % left => rot
        elseif (is_rc .and. .not. is_lc) then
          par % right => rot
        else
          error stop "Rotator_left: internal error, something very wrong"
        endif
      endif
    end subroutine Rotate_left



    module subroutine Rotate_right(a, piv)
      class(basetree_t), intent(inout) :: a
      class(basenode_t), pointer :: piv
!
! Rotate right. Fails if pivot is leaf or if pivot's left child is leaf.
!
      class(basenode_t), pointer :: par, rot
      logical :: is_lc, is_rc

      if (.not. associated(piv)) &
      &   error stop "Rotate_right: pivot is not associated"

      ! "par" is pivot's parent, it will be relinked to rotator.
      ! Rotator "rot" is a left child of pivot. It must exists.
      is_lc = Is_left_child(piv)
      is_rc = Is_right_child(piv)
      par => piv % Parentf()
      rot => piv % left
      if (.not. associated(rot)) &
      &   error stop "Rotate_right: rotator is a leaf"

      ! Right child of rotator relinked as leff child of pipvot,
      ! and pivot will be its new parent (if it is not leaf)
      piv % left => rot % right
      if (associated(piv % left)) piv % left % parent => piv

      ! Pivot will be a right child of rotator, and rotator is new
      ! pivot's parent
      rot % right => piv
      piv % parent => rot

 !TODO This part duplicates "rotate_left", refactor?
      ! Reconnect rotated part of tree to a up-stream node (par)
      if (.not. associated(par)) then
        ! Pivot was a root node, rotator is a new root
        rot % parent => null()
        a % root => rot
!TODO Temporary defensive check
!if (is_lc .or. is_rc) error stop "Rotator_right: Defensive check"

      else
        ! Pivot was a parent's child, rotator takes its place
        rot % parent => par
        if (is_lc .and. .not. is_rc) then
          par % left => rot
        elseif (is_rc .and. .not. is_lc) then
          par % right => rot
        else
          error stop "Rotator_right: internal error, something very wrong"
        endif
      endif
    end subroutine Rotate_right


    module function Is_left_child(n)
      logical :: Is_left_child
      class(basenode_t), intent(in), pointer :: n
!
! Returns .false. when called with unassociated pointer.
!
      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        if (associated(n, p % left)) then
          is_left_child = .true.
        elseif (associated(n, p % right)) then
          is_left_child = .false.
        else
          error stop 'basetree::Is_left_child ERROR. n is uknown to p'
        endif
      else
        ! node has no parent
        is_left_child = .false.
      endif
    end function Is_left_child



    module function Is_right_child(n)
      logical :: Is_right_child
      class(basenode_t), intent(in), pointer :: n
!
! Returns .false. when called with unassociated pointer.
!
      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        is_right_child = .not. Is_left_child(n)
      else
        ! node has no parent
        is_right_child = .false.
      endif
    end function Is_right_child



    pure module function basetree_Nodes(this)
      integer :: basetree_Nodes
      class(basetree_t), intent(in) :: this
!
! Type-bound function - return number of nodes in the tree
!
      basetree_Nodes = this % nodes
    end function basetree_Nodes



    module subroutine basetree_Firstnode(this, handle, ierr)
      class(basetree_t), intent(in) :: this
      integer(DAT_KIND), allocatable, intent(out) :: handle(:)
      integer, intent(out), optional :: ierr
!
! Set "handle" to the first node in the tree.
! If tree is empty, "handle" is a null pointer.
!
      integer :: ierr0
      type(basenode_ptr) :: cp

      if (associated(this % root)) then
        cp % p => Leftmost(this % root)
        ierr0 = ERR_CONT_OK
      else
        cp % p => null()
        ierr0 = ERR_CONT_END
      endif
      handle = transfer(cp, handle)
      if (present(ierr)) ierr = ierr0
    end subroutine basetree_Firstnode



    module pure subroutine basetree_Resetcurrent(this, handle)
      class(basetree_t), intent(in) :: this
      integer(DAT_KIND), intent(out), allocatable :: handle(:)
!
! Return "handle" pointing in front of the first element in the tree
!
      type(basenode_ptr) :: cp
      cp % p => null()
      handle = transfer(cp, handle)
    end subroutine basetree_Resetcurrent



      module subroutine basetree_Find(this, dat, handle, ierr)
      class(basetree_t), intent(in) :: this
      integer(DAT_KIND), intent(in) :: dat(:)
      integer(DAT_KIND), intent(out), allocatable :: handle(:)
      integer, intent(out), optional :: ierr

      class(basenode_t), pointer :: foundnode
      type(basenode_ptr) :: cp
      integer :: ierr0

      foundnode => Search_node(this, dat)
      ierr0 = ERR_CONT_OK
      if (.not. associated(foundnode)) ierr0 = ERR_CONT_ISNOT
      cp % p => foundnode
      handle = transfer(cp, handle)
      if (present(ierr)) ierr = ierr0
    end subroutine basetree_Find



    module subroutine basetree_Nextnode(this, handle, ierr)
      class(basetree_t), intent(in) :: this
      integer(DAT_KIND), intent(inout) :: handle(:)
      integer, intent(out), optional :: ierr
!
! Moves "handle" to the next node in the tree.
! Ïf "handle" is the last node, "handle" is nullified.
!
      integer :: ierr0
      class(basenode_t), pointer :: n
      type(basenode_ptr) :: cp

      cp = transfer(handle, cp)
      n => cp % p
      if (.not. associated(n)) then
        ierr0 = ERR_CONT_END
      else
        n => Successor(n)
        if (associated(n)) then
          ierr0 = ERR_CONT_OK
        else
          ierr0 = ERR_CONT_END
        endif
        cp % p => n
        handle = transfer(cp, handle)
      endif
      if (present(ierr)) ierr = ierr0
    end subroutine basetree_Nextnode



    function Successor(n)
      class(basenode_t), pointer :: Successor
      class(basenode_t), pointer, intent(in) :: n
!
! Fails if called with null pointer. Returns a pointer to a successor node
! or a null pointer if the current node is the last node
!
      class(basenode_t), pointer :: oldnode

      if (.not. associated(n)) error stop "Successor called with nil node"

      ! If node has a right subtree, the next element is the leftmost element
      ! in this right subtree.
      if (associated(n % right)) then
        Successor => Leftmost(n % right)
        return
      endif

      ! If node does not have a right child, the next element is a parent of
      ! a first left child that is found when going up.
      ! If no such parent is found, the current element is the last one.

      oldnode => n
      do
        Successor => oldnode % Parentf()

        ! no more parents, null pointer is returned
        if (.not. associated(Successor)) exit

        ! parent of a left child is the next node and it is returned
        if (Is_left_child(oldnode)) exit

        ! defensive check, that current node is a right child
        ! (and must have a parent)
        if (.not. Is_right_child(oldnode)) &
        &   error stop "Successor unexpected error: node is not child"

        ! parent of a right child is not the next node, but its
        ! grandparent can be...
        oldnode => oldnode % Parentf()
      enddo
    end function Successor



    module function Leftmost(n) result(lm)
      class(basenode_t), pointer :: lm
      class(basenode_t), pointer, intent(in) :: n
!
! Fails if called with a null pointer, always returns associated pointer.
!
      if (.not. associated(n)) error stop "Leftmost called with nil node"
      lm => n
      do
        if (.not. associated(lm % left)) exit
        lm => lm % left
      enddo
    end function Leftmost



    module function basetree_Isvalid_BST(this) result(isvalid)
      logical :: isvalid
      class(basetree_t), intent(in) :: this
!
! Verify that the tree is a valid BST tree
!
      class(basenode_t), pointer :: minnode, maxnode
      if (.not. associated(this % cfun)) &
          error stop 'Isvalid_BST: cfun procedure pointer not associated'
      call Verify_BST(this % root, this % cfun, isvalid, minnode, maxnode)
    end function basetree_Isvalid_BST



    recursive subroutine Verify_BST(tree, cfun, isvalid, minnode, maxnode)
      class(basenode_t), pointer, intent(in) :: tree
      procedure(compare_fun) :: cfun
      logical, intent(out) :: isvalid
      class(basenode_t), pointer, intent(out) :: minnode, maxnode
!
! Recursively check all subtrees and assert all are valid BST's
!
      class(basenode_t), pointer :: min_left_subtree, min_right_subtree
      class(basenode_t), pointer :: max_left_subtree, max_right_subtree
      logical :: isvalid_left_subtree, isvalid_right_subtree
      integer :: resmin, resmax


      ! Empty tree is a valid tree
      if (.not. associated(tree)) then
        isvalid = .true.
        minnode => null()
        maxnode => null()
        return
      endif

      ! Validate left and right sub-trees
      call Verify_BST(tree % left, cfun, isvalid_left_subtree, &
      &   min_left_subtree, max_left_subtree)
      call Verify_BST(tree % right, cfun, isvalid_right_subtree, &
      &   min_right_subtree, max_right_subtree)

      ! Single node tree is always valid
      if (.not. associated(tree % left) .and. &
      &   .not. associated(tree % right)) then
        isvalid = .true.
        minnode => tree
        maxnode => tree
        return
      endif


      ! Combine maxnode / minnode from both subtrees
      if (associated(tree % left) .and. .not. associated(tree % right)) then
        ! Left child only
        minnode => min_left_subtree
        maxnode => max_left_subtree
      elseif (.not. associated(tree % left) .and. &
      &       associated(tree % right)) then
        ! Right child only
        minnode => min_right_subtree
        maxnode => max_right_subtree
      elseif (associated(tree % left) .and. associated(tree % right)) then
        ! Both childrens
        resmin = cfun(min_left_subtree % dat, min_right_subtree % dat)
        resmax = cfun(max_left_subtree % dat, max_right_subtree % dat)

        select case(resmin)
        case(1)  ! "A" < "B"
           minnode => min_left_subtree
        case(-1) ! "A" > "B"
           minnode => min_right_subtree
        case(0)  ! "A" == "B"
          error stop "Verify_BST: resmin duplicit nodes?"
        case default
          error stop "Verify_BST: resmin invalid result from cfun"
        end select

        select case(resmax)
        case(1)  ! "A" < "B"
           maxnode => max_right_subtree
        case(-1) ! "A" > "B"
           maxnode => max_left_subtree
        case(0)  ! "A" == "B"
          error stop "Verify_BST: resmax duplicit nodes?"
        case default
          error stop "Verify_BST: resmax invalid result from cfun"
        end select

      else
        error stop "Verify_BST: impossible branch"
      endif


      ! Tree is a valid BST if both sub-trees are also valid
      isvalid = isvalid_left_subtree .and. isvalid_right_subtree

      ! Also verify that:
      !   (1)  all lc%key < root%key,
      !   (2)  all rc%key > root%key.
      ! Invalidate "isvalid" if any condition not met
      if (associated(max_left_subtree)) then
        resmax = cfun(max_left_subtree % dat, tree % dat)
      else
        ! With empty left tree, condition (1) must be valid
        resmax = 1
      endif
      if (resmax /= 1) isvalid = .false.

      if (associated(min_right_subtree)) then
        resmin = cfun(tree % dat, min_right_subtree % dat)
      else
        ! With empty right tree, condition (2) must be valid
        resmin = 1
      endif
      if (resmin /= 1) isvalid = .false.

    end subroutine Verify_BST



    module function basetree_Height_range(this) result(ht)
       integer :: ht(2)
       class(basetree_t), intent(in) :: this
       call recurse_HR(this % root, ht(1), ht(2))
    end function basetree_Height_range



    recursive subroutine recurse_HR(n, hmin, hmax)
      class(basenode_t), pointer, intent(in) :: n
      integer, intent(out) :: hmin, hmax

      integer :: hmin_left, hmin_right, hmax_left, hmax_right

      if (.not. associated(n)) then
        hmin = -1
        hmax = -1
        return
      endif
      call recurse_HR(n % Leftchild(), hmin_left, hmax_left)
      call recurse_HR(n % Rightchild(), hmin_right, hmax_right)
      hmin = min(hmin_left, hmin_right)
      hmax = max(hmax_left, hmax_right)

      hmin = hmin + 1
      hmax = hmax + 1
    end subroutine recurse_HR



    !Finalizer for basenode_t objects is not needed at the moment
    !
    !subroutine basenode_Destructor(n)
    !  type(basenode_t), intent(inout) :: n
    !  if (allocated(n % dat)) deallocate(n % dat)
    !end subroutine basenode_Destructor



    module subroutine basetree_Removeall(this)
      class(basetree_t), intent(inout) :: this

      integer :: counter

      counter = 0
      if (associated(this % root)) call delete_recurse(this % root, counter)
      if (this % nodes /= counter) &
      &   error stop "basetree_Removeall: counter mismatch"
      this % root => null()
      this % nodes = 0
    end subroutine basetree_Removeall



    module subroutine basetree_Destructor(this)
      type(basetree_t), intent(inout) :: this
!
! Finalize the "basetree_t" object
!
!TODO Duplicit code with Removeall
      integer :: counter

      counter = 0
!print *, 'In basetree_Destructor...'
      if (associated(this % root)) call delete_recurse(this % root, counter)
!print *, '...deleted nodes = ', counter

      if (this % nodes /= counter) &
      &   error stop "basetree_Delete: counter mismatch"

      ! reset all components (just in case the routine called directly)
      this % nodes = 0
    end subroutine basetree_Destructor



    recursive subroutine delete_recurse(n, counter)
      class(basenode_t), pointer, intent(inout) :: n
      integer, intent(out) :: counter
 !
 ! Fails if "n" is not associated.
 ! Deallocate recursively all nodes in left/right subtrees and then
 ! the node itself.
 !
      integer :: ileft, iright

      if (.not. associated(n)) error stop "delete_recurse: null pointer"

      ileft = 0
      iright = 0

      if (associated(n % left)) call delete_recurse(n % left, ileft)
      if (associated(n % right)) call delete_recurse(n % right, iright)

      ! defensive check that nodes were deallocated
      if (associated(n % left) .or. associated(n % right)) &
      &   error stop "delete_recurse: nodes left allocated"

      deallocate(n)
      counter = ileft + iright + 1
    end subroutine delete_recurse



    subroutine basetree_Display(this)
      class(basetree_t), intent(in) :: this
!
! A graphical expression of the tree (for small trees only)
!
      type basenode_ptr
        class(basenode_t), pointer :: p
      end type
      integer, parameter :: &
          CH = 2,       & ! node width...
          AD = 1,       & ! ... plus one character for / and \ 
          MAXLEVEL = 5, & ! (2**5)=32 nodes at fifth level
          LINE = 96       ! needs 32*(2+1) = 96 cbaracters
      integer :: gap, n, level, i, height(2)
      type(basenode_ptr), allocatable :: queue(:)
      type(basenode_ptr) :: item, lc, rc

      write(*,'(a,i0,a)') 'tree with ', this % count(),' elements >'
      call print_node(null(), (LINE-4)/2-1, 0)
      write(*,'(a)') 'root' 

      allocate(queue(0))
      item % p => this % root
      queue = enqueue(queue, item)
      level = -1
      do
        level = level + 1
        n = 2**(level)
        gap = max((LINE-n*(ch+ad))/(n), 0)
        do i = 1, n
          queue = dequeue(queue, item)
          if (i==1) then
            call print_node(null(), gap/2, 0)
          else
            call print_node(null(), gap, 0)
          endif
          call print_node(item % p, ch, ad)

          if (associated(item % p)) then
            lc % p => item % p % Leftchild()
            rc % p => item % p % Rightchild()
          else
            lc % p => null()
            rc % p => null()
          endif
          queue = enqueue(queue, lc)
          queue = enqueue(queue, rc)
        enddo
        write(*,*) ! end of line
        if (level >= maxlevel) exit
      enddo

      height = this % height_range()
      if (height(2) > MAXLEVEL) write(*, '(a,i0,a,i0,a)') &
         '(only ',MAXLEVEL,' of ',height(2),' levels displayed)'
      write(*,*)

    contains

      function enqueue(pin, new) result(pout)
        type(basenode_ptr), intent(in) :: pin(:)
        type(basenode_ptr), intent(in) :: new
        type(basenode_ptr), allocatable :: pout(:)
        integer :: n
        n = size(pin)
        allocate(pout(n+1))
        pout(1:n) = pin
        pout(n+1) = new
      end function enqueue

      function dequeue(pin, removed) result(pout)
        type(basenode_ptr), intent(in) :: pin(:)
        type(basenode_ptr), intent(out) :: removed
        type(basenode_ptr), allocatable :: pout(:)
        integer :: n
        n = size(pin)
        if (n==0) error stop 'dequeue empty'
        allocate(pout(n-1))
        removed = pin(1)
        pout = pin(2:n)
      end function dequeue
    end subroutine basetree_Display



    subroutine print_node(node, ch, ad)
      class(basenode_t), intent(in), pointer :: node
      integer, intent(in) :: ch, ad
!
! Print node on screen, width is "ch" characters.
! Node can be empty pointer - then print ch+ad spaces.
!
      character(len=*), parameter :: &
      &   BLUE=achar(27)//'[94m', RED=achar(27)//'[95m', &
      &   RESET=achar(27)//'[0m'
      integer, parameter :: MAXBUF = 200
      character(len=MAXBUF) :: prepare
      character(len=:), allocatable :: form, nodechar
      integer :: i

      ! Prepare the format field (works for integers only)
      write(prepare,*) ch
      nodechar = trim(adjustl(prepare))
      form = '(i'//nodechar//'.'//nodechar//')'
      nodechar = ''
      do i=1,MAXBUF
        prepare(i:i) = ' '
      enddo

      if (associated(node)) then
        ! write node value
        write(prepare,fmt=form) node % dat(1)
        select type(node)
        class is (rbnode_t)
          if (node % color == RED_NODE) then
            nodechar = red//trim(adjustl(prepare))//reset
          else
            nodechar = blue//trim(adjustl(prepare))//reset
          endif
        class default
          nodechar = trim(adjustl(prepare))
        end select
        if (Is_left_child(node)) nodechar = nodechar//'/'
        if (Is_right_child(node)) nodechar = '\'//nodechar
        write(*,'(a)',advance='no') nodechar
      else
        ! write "ch" + "ad" spaces
        write(*,'(a)',advance='no') prepare(1:ch+ad)
      endif
    end subroutine print_node



    module subroutine basetree_Delete(this, dat, ierr)
      class(basetree_t), intent(inout) :: this
      integer(DAT_KIND), intent(in) :: dat(:)
      integer, optional, intent(out) :: ierr

      integer :: ierr0
      logical :: lfreed
      class(basenode_t), pointer :: n, ch

      ! Verify that node is in tree, return if not
      n => Search_node(this, dat)
      if (associated(n)) then
        ierr0 = ERR_CONT_OK
      else
        ierr0 = ERR_CONT_ISNOT
      endif
      if (present(ierr)) ierr = ierr0
      if (.not. present(ierr) .and. ierr0 /= ERR_CONT_OK) &
          error stop 'basetree_Delete, element not in the tree'
      if (ierr0 /= ERR_CONT_OK) return

      ! Case 1: If N has two children
      ! - find the successor node, move content of that node to the current
      !   node to be deleted and then delete the successor node
      ! - continue to Case 2
      if (associated(n% Leftchild()) .and. associated(n% Rightchild())) then
        ch => Leftmost(n % Rightchild())
        call move_alloc(ch % dat, n % dat)
        n => ch
        lfreed = .true.
      else
        lfreed = .false.
      endif


      ! Case 2: N now must have one or no children
      ! N can be replaced by CH (if it has one) or be removed
      if (associated(n % Leftchild())) then
        ch => n % Leftchild()
      elseif (associated(n % Rightchild())) then
        ch => n % Rightchild()
      else
        ch => null()
      endif

      ! Link child to a new parent (parent of deleted node)
      if (associated(ch)) ch % parent => n % parent

      ! Link parent of deleted node N to child CH (or null)
      if (.not. associated(n % Parentf())) then
        ! N was root, CH or null is new root
        this % root => ch
      elseif (Is_left_child(n)) then
        n % parent % left => ch
      elseif (Is_right_child(n)) then
        n % parent % right => ch
      else
        error stop 'basetree_Delete: impossible branch'
      endif

      ! N can be now deallocated
      this % nodes = this % nodes - 1
      if (.not. lfreed) deallocate(n % dat)
      deallocate(n)
    end subroutine basetree_Delete



    module subroutine basetree_Copy(aout, bin)
      class(basetree_t), intent(out) :: aout
      class(container_t), intent(in)  :: bin
!
! Copy constructor for binary trees (required by abstract type container_t)
! Source must be tree (compare_function pointer must be copied) 
!
      integer(DAT_KIND), allocatable :: handle(:), dat(:)
      integer :: ierr

      call aout % initialize()
      select type(bin)
      class is (basetree_t)
        aout % cfun => bin % cfun
      class default
        error stop 'basetree_copy: can copy only trees'
      end select

      if (bin % isempty()) return
      call bin % resetcurrent(handle)
      do
        dat = bin % nextread(handle, ierr)
        if (ierr /= ERR_CONT_OK) exit
        call aout % add(dat)
      enddo
    end subroutine basetree_Copy

  end submodule basetree
