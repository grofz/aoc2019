  module test_parse_mod
    use parse_mod
  contains


    subroutine test_parse2(ispass)
      logical, intent(out) :: ispass
      type(string_t), allocatable :: lines(:), tokens(:)
      character(len=:), allocatable :: file, str
      integer, allocatable :: a(:)

      print *, 'new test'
      !file = 'inp/1901/input.txt'
      file = 'inp/1905/input.txt'

      lines = read_strings(file)
      str ='hello my name is -> peter, i am stuck '
      call split(str,' ',tokens)
      !call split(lines(1)%str,',',tokens)

      do i=1,size(tokens)
        print '(a)', '#'//tokens(i)%str//'#'
      end do
      a = tokens % To_int()

      !10 a = read_numbers(file)
      print '(15(i5,1x))', a
    end subroutine


    subroutine test_parse(ispass)
      logical, intent(out) :: ispass
      integer :: fid
      character(len=*), parameter :: file='inp/1902/input.txt'
      character(len=:), allocatable :: lines(:), items(:)
      integer, allocatable :: a(:)
      integer :: i

      !lines = read_strings(file,12)
      !print *, 'size = ',size(lines)
      !a = str2int(lines)
  !print *, 'aaa...'
      !a = read_numbers(file)
  !print *, '...aaa'

      !do i=1,size(lines)
        !print '(a)', '#'//trim(lines(i))//'#'
      !end do

      !lines = read_strings2(file)
      !items = parse_array(lines(1),',',8)
      !print *, lines(1)
      do i=1,size(items)
       ! print '(a)', '#'//trim(items(i))//'#'
      end do
      !a = str2int(items)


      print '(15(i5,1x))', a



      !open(newunit=fid, file=file, status='old')
      !print *, count_lines(fid)
      !print *, count_lines_block(fid)


      ispass = .false.
    end subroutine test_parse
  end module test_parse_mod

