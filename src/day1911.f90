  module day1911_mod
    use tree_m, only : rbtr_t, tree_mold, DAT_KIND
    use abstract_container, only : ERR_CONT_OK, ERR_CONT_ISNOT, ERR_CONT_IS
    implicit none

    integer, parameter :: BLACK = 0, WHITE = 1
    integer, parameter :: DEFAULT_COLOR = BLACK

    type hash_t
      integer :: xy(2), val ! position, color
    end type

    type board_t
      type(rbtr_t) :: map
    end type board_t

  contains
    subroutine board_final(this)
       type(board_t), intent(inout) :: this
       call this%map % Removeall()
     end subroutine board_final

     subroutine board_init(this)
       type(board_t), intent(out) :: this
       this % map = rbtr_t(hash_compare)
     end subroutine board_init



! ==================================
! MAP TO STORE THE STATE OF THE HULL
! ==================================

     pure integer function hash_compare(a, b) result(res)
      integer(DAT_KIND), intent(in) :: a(:), b(:)
  !
  ! user function for the TREE hash
  !
      type(hash_t) :: ahash, bhash
      ahash = transfer(a, ahash)
      bhash = transfer(b, bhash)

      if (ahash%xy(1) < bhash%xy(1)) then
        res = 1
      else if (ahash%xy(1) > bhash%xy(1)) then
        res = -1
      else
        if (ahash%xy(2) < bhash%xy(2)) then
          res = 1
        else if (ahash%xy(2) > bhash%xy(2)) then
          res = -1
        else
          res = 0
        end if
      end if
    end function hash_compare



    integer function hash_get(hash, adr) result(val)
    type(rbtr_t), intent(in) :: hash
    integer, intent(in) :: adr(2)
  !
  ! Ask, if "key" has been stored. Return "val" or "0" (no entry)
  !
      integer(DAT_KIND), allocatable :: handle(:)
      integer :: ierr
      type(hash_t) :: dat

      dat % xy = adr
      call hash % Find(transfer(dat,tree_mold), handle, ierr)
      select case(ierr)
      case(ERR_CONT_OK)
        dat = transfer(hash % Read(handle), dat)
        if (any(dat % xy /= adr)) error stop 'hash_get - wrong key returned'
        val = dat % val
      case(ERR_CONT_ISNOT)
        val = DEFAULT_COLOR
      case default
        error stop 'hash_get - uknown exit code'
      end select
    end function hash_get



    subroutine hash_update(hash, adr, val)
      type(rbtr_t), intent(inout) :: hash
      integer, intent(in) :: adr(2), val
  !
  ! Update pair "key - val" in hash, or store new one.
  !
      integer(DAT_KIND), allocatable :: handle(:)
      type(hash_t) :: olddat, newdat
      integer :: ierr

      olddat % xy = adr
      newdat % xy = adr
      newdat % val = val

      call hash % Find(transfer(olddat,tree_mold), handle, ierr)

      select case(ierr)
      case(ERR_CONT_OK)
        call hash % UpdateCurrent(handle, transfer(newdat,tree_mold))
      case(ERR_CONT_ISNOT)
        call hash % Add(transfer(newdat,tree_mold))
      case default
        error stop 'hash_update - uknown exit code'
      end select
    end subroutine hash_update



    subroutine hash_print(hash)
      type(rbtr_t), intent(in) :: hash
      integer(DAT_KIND), allocatable :: handle(:)

      integer :: ierr
      type(hash_t) :: adat

      ! Iterate all nodes
      if (.not. hash % Isempty()) then
        print *
        call hash % Resetcurrent(handle)
        do
          adat = transfer(hash % NextRead(handle, ierr), adat)
          if (ierr /= 0) exit

          ! We can use value in adat
          print *, 'Node = ', adat % xy, adat % val
          print *, ' ___ = ', hash % Printcurrentnode(handle)
        enddo
      else
        print *
        print *, 'Empty tree: '
      endif
      print *
    end subroutine hash_print


  end module day1911_mod
