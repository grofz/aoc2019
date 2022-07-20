!
! List of strings. A simple implementation using allocatable arrays.
!
! (used in AoC2020 - day21)
! (used in AoC2019 - day14)
!
  module list_mod
     implicit none
     private
     public list_t, CHAR_LEN

     integer, parameter :: CHAR_LEN = 20
     integer, parameter :: NOT_INITIALIZED=-1

     type list_t
       private
       character(len=CHAR_LEN), allocatable :: aa(:)
       integer :: n=NOT_INITIALIZED
     contains
       procedure :: add
       procedure :: remove
       procedure :: find
       procedure :: export => list_export
       procedure :: get => list_get
       procedure :: size => list_size
       procedure :: print => list_print
       procedure :: sort => list_sort
       procedure, private :: list_intersection
       generic :: operator(.and.) => list_intersection
     end type list_t
     interface list_t
       module procedure list_new
       module procedure list_newempty
     end interface

  contains

!
! List must be always initialized.
!
     pure type(list_t)  function list_new(aa) result(this)
       character(len=*), intent(in) :: aa(:)
       this%n = size(aa)
       allocate(this%aa(this%n))
       this%aa = aa
     end function list_new

     pure type(list_t) function list_newempty() result(this)
       integer, parameter :: INIT_SIZE = 2
       this%n = 0
       allocate(this%aa(2))
     end function list_newempty



     pure subroutine add(this, item)
       class(list_t), intent(inout) :: this
       character(len=*), intent(in) :: item
 !
 ! Add item to list, or, if item already in list, do nothing.
 !
       character(len=CHAR_LEN), allocatable :: extra_aa(:)
       if (this%n==NOT_INITIALIZED) error stop 'list_mod - object "list_t" is unitialized'

       ! Do nothing if item is already in list
       if (this%find(item) /= 0) return

       ! Extend array if array is full
       if (this%n == size(this%aa)) then
         allocate(extra_aa(2*size(this%aa)))
         extra_aa(1:this%n) = this%aa
         call move_alloc(extra_aa, this%aa)
       end if

       ! Add item at the end of the list
       this%n = this%n + 1
       this%aa(this%n) = item
     end subroutine



     subroutine remove(this, item)
       class(list_t), intent(inout) :: this
       character(len=*), intent(in) :: item
 !
 ! Remove element from the list, or, if item not in list, do nothing.
 !
       integer :: ind
       logical :: is_shrinking
       character(len=CHAR_LEN), allocatable :: smaller_aa(:)
       if (this%n==NOT_INITIALIZED) error stop 'list_mod - object "list_t" is unitialized'

       ! Do nothing if item is not in the list
       ind = this%find(item)
       if (ind == 0) return

       ! Shrink array if it is 75% not filled
       is_shrinking = .false.
       if (size(this%aa)/this%n >= 4) is_shrinking = .true.

       ! Move all items behind deleted item one position forward
       if (is_shrinking) then
         allocate(smaller_aa(2*this%n))
         smaller_aa(1:ind-1) = this%aa(1:ind-1)
         smaller_aa(ind:this%n-1) = this%aa(ind+1:this%n)
         call move_alloc(smaller_aa, this%aa)
       else
         this%aa(ind:this%n-1) = this%aa(ind+1:this%n)
       end if
       this%n = this%n - 1
     end subroutine



     pure integer function find(this, item) result(ind)
       class(list_t), intent(in) :: this
       character(len=*), intent(in) :: item
 !
 ! Return item position in the array or "0" if item not the list
 !
       if (this%n==NOT_INITIALIZED) error stop 'list_mod - object "list_t" is unitialized'
       ind = 1
       do
         if (ind > this%n) exit
         if (trim(item) == trim(this%aa(ind))) exit
         ind = ind+1
       end do
       if (ind > this%n) ind = 0
     end function



     pure function list_export(this) result(aa)
       class(list_t), intent(in) :: this
       character(len=CHAR_LEN), allocatable :: aa(:)
       if (this%n==NOT_INITIALIZED) error stop 'list_mod - object "list_t" is unitialized'
       allocate(aa(this%n))
       aa = this%aa(1:this%n)
     end function



     pure function list_get(this, ind) result(item)
       class(list_t), intent(in) :: this
       integer, intent(in) :: ind
       character(len=CHAR_LEN) :: item
       if (this%n==NOT_INITIALIZED) error stop 'list_mod - object "list_t" is unitialized'
       if (ind < 1 .or. ind > this%n) error stop 'list_get - index out of range'
       item = this%aa(ind)
     end function



     pure integer function list_size(this)
       class(list_t), intent(in) :: this
       if (this%n==NOT_INITIALIZED) error stop 'list_mod - object "list_t" is unitialized'
       list_size = this%n
     end function



     pure type(list_t) function list_intersection(alist, blist) result(ilist)
       class(list_t), intent(in) :: alist, blist
!
! Intersection of two lists
!
       integer :: i, j
       if (alist%n==NOT_INITIALIZED) error stop 'list_intersection - alist is unitialized'
       if (blist%n==NOT_INITIALIZED) error stop 'list_intersection - blist is unitialized'

       ilist = list_t() ! empty list must be initialized

       ! For every item in A-list, check if there is the same item also in B-list
       do i=1, alist%size()
         j = blist % find(alist % get(i))
         if (j/=0) call ilist % add(alist % get(i))
       end do
     end function



     function list_print(this,delim) result(string)
       class(list_t), intent(in) :: this
       character(len=*), intent(in), optional :: delim
       character(len=:), allocatable :: string
       integer :: i
       character(len=:), allocatable :: delim0
       if (this%n==NOT_INITIALIZED) error stop 'list_mod - object "list_t" is unitialized'
       delim0 = ' '
       if (present(delim)) delim0 = delim
       if (this%n == 0) then
         string = '<empty list>'
         return
       end if

       string = trim(this%aa(1))
       do i=2,this%n
         string = string//delim0//trim(this%aa(i))
       end do
     end function



     subroutine list_sort(list_a, list_b)
       class(list_t), intent(inout) :: list_a
       class(list_t), intent(inout), optional :: list_b
!
! Sort lists using items in list A as keys
!
       character(len=CHAR_LEN) :: atmp, btmp
       integer :: i, j
       if (list_a%n==NOT_INITIALIZED) error stop 'list_mod - object "list_t" is unitialized'

       if (present(list_b)) then
         if (list_b%n==NOT_INITIALIZED) error stop 'list_mod - object "list_t" is unitialized'
         if (list_a%size() /= list_b%size()) error stop 'list_sort - both list must have same size'
       end if

       do i=2, list_a % size()
         atmp = list_a%aa(i)
         if (present(list_b)) btmp = list_b%aa(i)
         j=i-1
         do
           if (j==0) exit
           if (list_a%aa(j)<=atmp) exit
           list_a%aa(j+1)=list_a%aa(j)
           if (present(list_b)) list_b%aa(j+1)=list_b%aa(j)
           j=j-1
         end do
         list_a%aa(j+1)=atmp
         if (present(list_b)) list_b%aa(j+1)=btmp
       end do
     end subroutine list_sort

  end module list_mod
