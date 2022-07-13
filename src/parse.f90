  module parse_mod
    implicit none
    private
    public read_pattern, chop_string, read_numbers, parse_array
    public read_strings, str2int

    integer, parameter :: EXTREME_LINE_LENGTH = 3000 
    integer, parameter :: DEFAULT_LINE_LENGTH = 80   

    ! public just for tests
    public count_lines, count_lines_block

  contains

    function read_strings(file) result(lines)
      character(len=*), intent(in) :: file
      !integer, intent(in) :: line_length
      character(len=EXTREME_LINE_LENGTH), allocatable :: lines(:)
      !character(len=:), allocatable :: lines(:)
      !character(len=line_length), allocatable :: lines(:)
!
! Store each line from the file as an item in the array of characters
!
      integer :: fid, i, n, istat
      character(len=EXTREME_LINE_LENGTH) :: oneline

      open(newunit=fid, file=file, status='old')
      n = count_lines(fid)
      ! some problem with compiler? must have compile-time known length
      !allocate(character(len=line_length0) :: lines(n), stat=istat)
      !allocate(character(len=line_length) :: lines(n), stat=istat)
      allocate( lines(n), stat=istat)
      do i=1, n
        read(fid,'(a)') lines(i)
      end do
      close(fid)
    end function read_strings



    function read_numbers(file) result(a)
      character(len=*), intent(in) :: file
      integer, allocatable :: a(:)
!
! Read from file with a single integer on every line
!
! (just a demo, how use "read_strings" and "str2int" elementals)
!
      character(len=:), allocatable :: lines(:)
      lines = read_strings(file)
      a = str2int(lines)
    end function read_numbers



    function read_pattern(file) result(aa)
      character(len=*), intent(in) :: file
      character(len=1), allocatable :: aa(:,:)
!
! Read "character" 2D matrix from the file.
!
      integer :: fid, nrow, ncol, ios, i
      character(len=5000) :: line

      ! read no of rows, cols, and make sure all rows are of the same
      ! length
      open(newunit=fid, file=file, status='old')
      nrow = 0
      ncol = -1
      do
        read(fid,'(a)',iostat=ios) line
        if (ios /= 0) exit
        if (ncol==-1) ncol = len_trim(line)
        if (len_trim(line) /= ncol) &
          error stop 'read_pattern - not all lines have same length'
        nrow = nrow + 1
      end do

      allocate(aa(ncol,nrow))
      rewind(fid)
      do i=1,nrow
        read(fid,'(*(a))') aa(:,i)
      end do
      close(fid)
      aa = transpose(aa)
    end function read_pattern



    subroutine chop_string(left, reg, right)
      character(len=*), intent(inout) :: left
      character(len=*), intent(in) :: reg
      character(len=*), intent(out) :: right
!
! Chop string in two at "reg"-character: returns left and right part, "reg" character is discarded
!
      integer :: i, n
      i = scan(left, reg)
      n = len(left)
      if (i==0) then
        right = ''
      else
        right = left(i+1:)
        left = left(:i-1)
      end if
    end subroutine chop_string



    function parse_array(line, delim, nlen) result(arr)
      character(len=*), intent(in) :: line
      character(len=1), intent(in) :: delim
      integer, intent(in) :: nlen
      character(len=nlen), allocatable :: arr(:)
!
! Get array of strings from one string
!
      character(len=len(line)) :: line0
      character(len=nlen), allocatable :: wrk(:)
      character(len=len(line)) :: a1, a2
      integer, parameter :: MAX_ITEMS=3500
      integer :: nitems

      allocate(wrk(MAX_ITEMS))
      nitems=0
      line0 = line
      do
        call chop_string(line0, delim, a2)
        if (len_trim(line0) == 0) exit
        a1 = adjustl(line0)
        nitems = nitems+1
        wrk(nitems) = a1

        if(len_trim(a2)==0) exit
        line0 = adjustl(a2)
      end do

      allocate(arr(nitems))
      arr = wrk(1:nitems)
    end function parse_array



    elemental function str2int(s) result(i)
      character(len=*), intent(in) :: s
      integer :: i, ios
      read(s,*,iostat=ios) i
      if (ios /= 0) error stop 'str2int error - invalid string: "'//s//'"'
    end function str2int


 ! =======================
 ! Local helper procedures
 ! =======================

    function count_lines(fid) result(n)
      integer, intent(in) :: fid
      integer :: n
!
! Get number of lines in the file. Empty lines are included in the count.
!
      integer :: ios
      character(len=EXTREME_LINE_LENGTH) :: line
      rewind(fid)
      n = 0
      do
        read(fid,'(a)',iostat=ios) line
        if (ios /= 0) exit
        n = n + 1
      end do
      rewind(fid)
    end function count_lines



    function count_lines_block(fid) result(narr)
      integer, intent(in) :: fid
      integer, allocatable :: narr(:)
!
! Get number of lines for each block. Blocks are separated by an empty line
!
      integer :: ios, iblock
      character(len=EXTREME_LINE_LENGTH) :: line
      rewind(fid)
      narr = [0]
      iblock = 1
      do
        read(fid,'(a)',iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line)==0) then
          iblock = iblock + 1
          narr = [narr, 0]
          cycle
        end if
        narr(iblock) = narr(iblock) + 1
      end do
      rewind(fid)
    end function count_lines_block

  end module parse_mod
