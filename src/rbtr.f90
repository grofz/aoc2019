!
! Red black tree implementation (SUBMODULE)
!
  submodule(tree_m) rbtr

    implicit none

    type basenode_ptr
      class(basenode_t), pointer :: p
    end type

  contains

    module function rbtr_Printcurrentnode(this, handle) result(str)
      character(len=:), allocatable :: str
      class(rbtr_t), intent(in) :: this
      integer(DAT_KIND), intent(in) :: handle(:)
      character(len=1000) :: color
      type(basenode_ptr) :: cp

      cp = transfer(handle, cp)
      if (.not. associated(cp % p)) then
        str='current not associated...'
      else
        str = this % basetree_t % Printcurrentnode(handle)
        select type(aa => cp % p)
        class is (rbnode_t)
          write(color,*) aa % color
          str = str//'{'//trim(adjustl(color))//'}'
        end select
      endif
    end function rbtr_Printcurrentnode



    module function rbtr_Initialize(cfun) result(new)
      type(rbtr_t) :: new
      procedure(compare_fun) :: cfun
      new % cfun => cfun
!     allocate(rbnode_t :: new % typet)
    end function rbtr_Initialize



    module subroutine rbtr_Add(this, dat, ierr)
      class(rbtr_t), intent(inout)  :: this
      integer(DAT_KIND), intent(in) :: dat(:)
      integer, intent(out), optional :: ierr
!
! Insert a node to a red-black tree.
! Get a new node as "rbnode_t" and call insertion procedure of a base class.
! Then repair the tree.
!
      integer :: ierr0
      class(basenode_t), pointer :: newnode

      if (.not. associated(this % cfun)) &
          error stop 'rbtr_Insert: cfun procedure pointer not associated'

      ! Default "newnode % color" must be RED_NODE
      allocate(rbnode_t :: newnode)
      call Add2(this, dat, newnode, ierr=ierr0)
      if (ierr0 == ERR_CONT_OK) call insert_repair_tree(this, newnode)

      if (present(ierr)) then
        ierr = ierr0
      elseif (ierr0 /= ERR_CONT_OK) then
        error stop 'rbtree_Add: element already in the tree'
      endif
    end subroutine rbtr_Add



    recursive subroutine insert_repair_tree(a, n)
      class(rbtr_t), intent(inout) :: a
      class(basenode_t), intent(inout), pointer :: n

      class(basenode_t), pointer :: p, u, g
      logical :: uncle_exists, uncle_is_red

      uncle_is_red = .false.
      p => n % Parentf()
      if (.not. associated(p)) then
        ! case 1 - root node mus be black
        call Set_color(n, BLACK_NODE)

      elseif (Is_black(p)) then
        ! case 2 - nothing must be done
        continue

      else
        ! parent is red
        u => n % Uncle()
        uncle_exists = associated(u)
        if (uncle_exists) uncle_is_red = .not. Is_black(u)

        if (uncle_exists .and. uncle_is_red) then
          ! case 3 - repaint parent and uncle black and rerun on grandparent
          g => n % Grandparent()
          call Set_color(p, BLACK_NODE)
          call Set_color(u, BLACK_NODE)
          call Set_color(g, RED_NODE)
          call insert_repair_tree(a, g)
        else

          ! case 4 - parent is red and uncle is black
          g => n % Grandparent()

          ! case 4, step 1
          if (Is_right_child(n) .and. Is_left_child(p)) then
            call Rotate_left(a, p)
            n => p
          elseif (Is_left_child(n) .and. Is_right_child(p)) then
            call Rotate_right(a, p)
            n => p
          endif

          ! case 4, step 2
          p => n % Parentf()
          g => n % Grandparent()

          if (Is_left_child(n)) then
            call Rotate_right(a, g)
          elseif (Is_right_child(n)) then
            call Rotate_left(a, g)
          else
            error stop "insert_repair_tree: internal error"
          endif
          call Set_color(p, BLACK_NODE)
          call Set_color(g, RED_NODE)
        endif
      endif
    end subroutine insert_repair_tree



    module subroutine rbtr_Update(this, olddat, newdat, ierr)
      class(rbtr_t), intent(inout) :: this
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
        error stop 'rbtr_Update: old not found or new value already in tree'
      endif
      if (ierr0 /= ERR_CONT_OK) return

      ! Re-add with a new value
      allocate(rbnode_t :: new)
      call Add2(this, newdat, new)
      call insert_repair_tree(this, new)
    end subroutine rbtr_Update



    module subroutine rbtr_Updatecurrent(this, handle, newdat, ierr)
      class(rbtr_t), intent(inout) :: this
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
            error stop 'rbtr_Updatecurrent: handled node not in tree'
          ierr0 = ERR_CONT_OK
        endif

      else
        ierr0 = ERR_CONT_END
      endif
      if (present(ierr)) then
        ierr = ierr0
      elseif (ierr0 /= ERR_CONT_OK) then
        error stop 'rbtr_Updatecurrent: new value already in tree'
      endif
      if (ierr0 /= ERR_CONT_OK) return

      ! Re-add with a new value
      allocate(rbnode_t :: new)
      call Add2(this, newdat, new)
      call insert_repair_tree(this, new)
    end subroutine rbtr_Updatecurrent



    logical function Is_black(n)
      class(basenode_t), intent(in), pointer :: n
!
! Is node "n" black? Null node is always black.
!
      if (associated(n)) then
        select type(n)
        class is (rbnode_t)
          is_black = n % color == BLACK_NODE
        class default
          error stop "Is_black: node type must be extension of rbnode"
        end select
      else
        is_black = .true.
      endif
    end function Is_black



    subroutine Set_color(n, color)
      class(basenode_t), intent(inout) :: n
      integer(I1B), intent(in) :: color
      select type(n)
      class is (rbnode_t)
        n % color = color
      class default
        error stop "Set_color: node type must be extension of rbnode"
      end select
    end subroutine Set_color



    module function rbtr_Isvalid_rbtree(this) result(isvalid)
      logical :: isvalid
      class(rbtr_t), intent(in) :: this
!
! Verify that the properties of red-black tree are met
!
      integer :: blacks

      if (.not. associated(this % root)) then
        isvalid = .true.
        return
      endif

      select type(tree => this % root)
      class is (rbnode_t)
        call verify_RB(tree, isvalid, blacks)
      class default
        error stop 'Isvalid_rbtree root must be rbnode_t'
      end select

      ! optional verification that the root is black
      if (.not. Is_black(this % root)) isvalid = .false. 

    end function rbtr_Isvalid_rbtree



    recursive subroutine verify_RB(tree, isvalid, blacks)
      class(basenode_t), pointer, intent(in) :: tree
      logical, intent(out) :: isvalid
      integer, intent(out) :: blacks
!
! Recursivelt check all subtrees and assert red-black tree properties
!
      logical :: isvalid_left, isvalid_right
      integer :: blacks_left, blacks_right

      ! Empty tree is a valid tree (leaf nodes are black)
      if (.not. associated(tree)) then
        isvalid = .true.
        blacks = 1
        return
      endif

      ! Validate left and right sub-trees
      call Verify_RB(tree % Leftchild(), isvalid_left, blacks_left)

      call Verify_RB(tree % Rightchild(), isvalid_right, blacks_right)

      ! Validate that red node has only black childrens
      if (Is_black(tree)) then
        isvalid = .true.
      else
        isvalid = Is_black(tree % Leftchild()) .and. &
        &         Is_black(tree % Rightchild())
      endif

      ! Assert that number of black nodes is same in both sub-trees
      ! and return the number of blacks including the current node
      if (blacks_left /= blacks_right) isvalid = .false.

      blacks = blacks_left
      if (Is_black(tree)) blacks = blacks + 1
    end subroutine verify_RB



    module subroutine rbtr_Delete(this, dat, ierr)
      class(rbtr_t), intent(inout) :: this
      integer(DAT_KIND), intent(in) :: dat(:)
      integer, optional, intent(out) :: ierr
!
! Remove node from the red-black tree
!
      logical :: lfreed
      integer :: ierr0
      class(basenode_t), pointer :: n, ch

      n => search_node(this, dat)
      if (associated(n)) then
        ierr0 = ERR_CONT_OK
      else
        ierr0 = ERR_CONT_ISNOT
      endif

      !ierr0 = TREE_ERR_OK
      !if (.not. associated(this % root)) then
      !  ierr0 = TREE_ERR_EMPTY
      !elseif (.not. associated(this % current)) then
      !  ierr0 = TREE_ERR_NOCURRENT
      !endif

      if (present(ierr)) then
        ierr = ierr0
        !if (ierr0 /= TREE_ERR_OK) return
        if (ierr0 /= ERR_CONT_OK) return
      else
        !if (ierr0 /= TREE_ERR_OK) &
        !&   error stop "rbtr_Delete: empty tree or null current pointer"
        if (ierr0 /= ERR_CONT_OK) &
            error stop 'rbtr_Delete, element not in the tree'
      endif
      !n => this % current


      ! CASE I: N has two children
      ! * find the successor node, move content of that node to the current
      !   node to be deleted and then delete the successor node
      ! * continue to the CASE II (one or zero children)
      if (associated(n% Leftchild()) .and. associated(n% Rightchild())) then
        ch => Leftmost(n % Rightchild())
        !deallocate(n % dat)
        !n % dat = ch % dat
        call move_alloc(ch % dat, n % dat)
        n => ch
        lfreed = .true.
      else
        lfreed = .false.
      endif


      ! CASE II: N has one or zero children:
      ! * If N is red, both its children must be leafs.
      !   Node can be removed without violating red-black properties.
      ! * If N is black and its child CH is red, then N can be replaced by
      !   CH, CH is relabeled black and we are done.
      ! * If N is black with no children, removing N will break the
      !   red-black tree properties and it must be rebalanced in
      !   "delete_case1" subroutines.
      if (associated(n % Leftchild())) then
        ch => n % Leftchild()
      elseif (associated(n % Rightchild())) then
        ch => n % Rightchild()
      else
        ch => null()
      endif

      if (.not. associated(ch)) then
        ! N has no children
        if (Is_black(n)) call delete_case1(this, n)

        ! N is red
        if (.not. associated(n % Parentf())) then
          ! N was root
          this % root => null()
        elseif (Is_left_child(n)) then
          n % parent % left => null()
        elseif (Is_right_child(n)) then
          n % parent % right => null()
        else
          error stop 'rbtr_Delete: impossible branch' ! TODO temporary check
        endif

      else
        ! N has one child (N must be black and CH must be red)
        ch % parent => n % parent
        if (.not. associated(n % Parentf())) then
          ! N was root, CH is new root
          this % root => ch
        elseif (Is_left_child(n)) then
          n % parent % left => ch
        elseif (Is_right_child(n)) then
          n % parent % right => ch
        else
          error stop 'rbtr_Delete: impossible branch2' ! TODO temporary check
        endif

        ! Assert N is black and CH is red
        if (.not. (Is_black(n) .and. .not. Is_black(ch))) then
          error stop "rbtr_Delete: assertion failed."
        endif
        call Set_color(ch, BLACK_NODE)
      endif


      ! Now N can be deallocated
      this % nodes = this % nodes - 1

      if (.not. lfreed) deallocate(n % dat)
      deallocate(n)
    end subroutine rbtr_Delete



    subroutine delete_case1(a, m)
      class(rbtr_t), intent(inout) :: a
      class(basenode_t), pointer :: m
!
! M is a black node without children.
! If M is the new root, nothing needs to be done.
! Otherwise proceed to case 2.
!
      if (associated(m % Parentf())) call delete_case2(a, m)
    end subroutine delete_case1



    subroutine delete_case2(a, m)
      class(rbtr_t), intent(inout) :: a
      class(basenode_t), pointer :: m
!
! If S is red:
! * make S black, make P red and
! * rotate left/right around P so S will become grandparent of M
!
      class(basenode_t), pointer :: s, p

      s => Sibling(m)
      p => m % Parentf()

      if (.not. Is_black(s)) then
        call Set_color(p, RED_NODE)
        call Set_color(s, BLACK_NODE)
        if (Is_left_child(m)) then
          call Rotate_left(a, p)
        else
          call Rotate_right(a, p)
        endif
      endif
      call delete_case34(a, m)
    end subroutine delete_case2



    subroutine delete_case34(a, m)
      class(rbtr_t), intent(inout) :: a
      class(basenode_t), pointer :: m
!
! If S, Sleft, Sright and P are black then
! * repaint S red: this compensates the deleted black node in S' subtree
!                  but the whole P->M and P->S sub-trees are one black node 
!                  less than the remaining branches, therefore ...
! * rebalance up-level: use delete_case1 on P
!
! If S, Sleft and Sright are black but P is red then
! * exchange color of S and P and we are done
!
! Otherwise proceed to delete_case5
!
      class(basenode_t), pointer :: s, p

      s => Sibling(m)
      p => m % Parentf()

      ! assert that sibling is not leaf
      if (.not. associated(s)) &
      &   error stop "delete_case34: defensive check, sibling is a leaf:"

      if (Is_black(p) .and. Is_black(s) .and. &
      &   Is_black(s % Leftchild())  .and. Is_black(s % Rightchild())) then
        call Set_color(s, RED_NODE)
        call delete_case1(a, p)

      elseif ( .not. Is_black(p) .and. Is_black(s) .and. &
      &   Is_black(s % Leftchild())  .and. Is_black(s % Rightchild())) then
        call Set_color(s, RED_NODE)
        call Set_color(p, BLACK_NODE)

      else
        call delete_case5(a, m)
      endif
    end subroutine delete_case34



    subroutine delete_case5(a, m)
      class(rbtr_t), intent(inout) :: a
      class(basenode_t), pointer :: m
!
! S is black, S left is red, S right is black and M is the left child
! * rotate right at S so S left is new sibling of M
! * exchange colors of S and its ne parent (it was S left)
!
! Mirrored situation
! S is black, S right is red, S left is black and M is the right child
! * rotate left at S so S right is new sibling of M
! * exchange colort of S and its new parent (it was S right)
!
! At the enf M should have black sibling with red children on the
! outside of the tree and this falls into case 6
!
      class(basenode_t), pointer :: s

      s => Sibling(m)
      ! assert that sibling is not leaf
      if (.not. associated(s)) &
      &   error stop "delete_case5: defensive check, sibling is a leaf:"

      ! assert that sibling is black
      if (.not. Is_black(s)) &
      &   error stop "delete_case5: sibling is red"

      if (Is_left_child(m) .and. Is_black(s % Rightchild())) then
        ! assert S left is red
        if (Is_black(s % Leftchild())) error stop "delete_case5: assert1"

        call Set_color(s, RED_NODE)
        call Set_color(s % Leftchild(), BLACK_NODE)
        call Rotate_right(a, s)

      elseif (Is_right_child(m) .and. Is_black(s % Leftchild())) then
        ! assert S right is red
        if (Is_black(s % Rightchild())) error stop "delete_case5: assert2"

        call Set_color(s, RED_NODE)
        call Set_color(s % Rightchild(), BLACK_NODE)
        call Rotate_left(a, s)
      endif

      call delete_case6(a, m)
    end subroutine delete_case5



    subroutine delete_case6(a, m)
      class(rbtr_t), intent(inout) :: a
      class(basenode_t), pointer :: m
!
! If S is black, its right child is red and M is the left child then
! * rotate left at P so S becomes the parent of P and S's right child
! * exchange colors P and S and make S's right child black
!
! Mirrored situation
! If S is black, its left child is red and M is the right child then
! * rotate right at P so S becomes the parent of P and S's left child
! * exchange colors P and S and make S's left child black
!
! The properties of red-black tree are now restored
!
      class(basenode_t), pointer :: s, p

      s => Sibling(m)
      p => m % Parentf()

      ! assert that sibling is not leaf
      if (.not. associated(s)) &
      &   error stop "delete_case6: defensive check, sibling is a leaf:"

      !s % color = p % color
      if (Is_black(p)) then
        call Set_color(s, BLACK_NODE)
      else
        call Set_color(s, RED_NODE)
      endif
      call Set_color(p, BLACK_NODE)

      if (Is_left_child(m)) then
        call Set_color( s % Rightchild(), BLACK_NODE)
        call Rotate_left(a, p)
      else
        call Set_color( s % Leftchild(), BLACK_NODE)
        call Rotate_right(a, p)
      endif
    end subroutine delete_case6

  end submodule rbtr
