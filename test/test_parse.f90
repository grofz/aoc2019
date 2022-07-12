  module test_parse_mod
    use parse_mod
  contains

    subroutine test_parse(ispass)
      logical, intent(out) :: ispass
      integer :: fid
      character(len=*), parameter :: file='inp/tests/lines2.txt'
      character(len=:), allocatable :: lines(:)
      integer, allocatable :: a(:)
      integer :: i

      !lines = read_strings(file,12)
      !print *, 'size = ',size(lines)
      !a = str2int(lines)
  !print *, 'aaa...'
      a = read_numbers(file)
  !print *, '...aaa'

      !do i=1,size(lines)
        !print '(a)', '#'//trim(lines(i))//'#'
      !end do
      print '(15(i5,1x))', a



      !open(newunit=fid, file=file, status='old')
      !print *, count_lines(fid)
      !print *, count_lines_block(fid)


      ispass = .false.
    end subroutine test_parse
  end module test_parse_mod

