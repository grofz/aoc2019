  program main
    implicit none
goto 11

01  call day01('inp/1901/input.txt')

02  call day02('inp/1902/input.txt')

03  call day03('inp/1903/input.txt')

04  call day04()

05  call day05('inp/1905/input.txt')

06  continue
    !call day06('inp/1906/sample.txt')
    call day06('inp/1906/input.txt')

07  call day07('inp/1907/input.txt')

08  call day08('inp/1908/input.txt')

09  call day09('inp/1909/input.txt')

10  continue
    call day10('inp/1910/input.txt')
    !call day10('inp/1910/sample3.txt')
    !call day10('inp/1910/sample1.txt')
11  continue
    call day11('inp/1911/input.txt')

  end program main



  subroutine day01(file)
    use day1901_mod
    use parse_mod, only : read_numbers
    implicit none
    character(len=*), intent(in) :: file
    integer, allocatable :: mass(:), fuel(:)
    integer :: tot_fuel

    mass = read_numbers(file)
    !print '("Modules ",i0)', size(mass)
    !print '(12(i6,1x))', mass

    fuel = fuel_required(mass)
    !print '("Fuel required ",i0)', size(mass)
    !print '(12(i6,1x))', fuel

    tot_fuel = sum(fuel)
    print '("Total fuel (1/1) ",i0,l2)', tot_fuel, tot_fuel==3239503

    ! Part Two
    print '("Test cases:")'
    print *, fuel_for_all(14), fuel_for_all(14)==2
    print *, fuel_for_all(1969), fuel_for_all(1969)==966
    print *, fuel_for_all(100756), fuel_for_all(100756)==50346
    fuel = fuel_for_all(mass)
    !print '("Fuel required including fuel mass requirements")'
    !print '(12(i6,1x))', fuel
    tot_fuel = sum(fuel)
    print '("Total fuel (1/2) ",i0,l2)', tot_fuel, tot_fuel==4856390
    print *
  end subroutine day01



  subroutine day02(file)
    !use day1902_mod
    !use day1905_mod 
    use day1907_mod ! Version 2.5 computer from day 7
    use parse_mod, only : read_strings, split, string_t
    implicit none
    character(len=*), intent(in) :: file
    type(string_t), allocatable :: lines(:), items(:)
    integer, allocatable :: state(:)
    type(computer_t) :: zx
    integer :: inp(2), i, j, output
    integer, parameter :: TARGET_RESULT=19690720

    ! Read and parse input
    lines = read_strings(file)
    if (size(lines)/=1) error stop 'day02 - input file has more than one line'
    call split(lines(1)%str, ',', items)
    !items = parse_array(lines(1),',',8)
    state = items % To_int()
    !state = str2int(items)
    !print '(20(i4,1x))', state

    ! Load and run
    !call zx % init(state)
    call zx % Load(state)
    call zx % Legacy_setinput([12,2])
    call zx % Run()
    output = zx % Legacy_getoutput()
    print '("Answer 2/1 ",i0,l2)', output, output==4930687

    ! Part 2 - manual
    goto 100
    do
      write(*,'(a)',advance='no') 'guess the input '
      read(*,*) inp(1), inp(2)
      call zx % Reset()
      call zx % legacy_Setinput(inp)
      call zx % Run()
    output = zx % Legacy_getoutput()
      print *, 'RESULT IS ',output, output-TARGET_RESULT
    end do

    ! Part 2 - automatic
    100 continue
    j = 0
    do i=0, 99
      call zx % Reset()
      call zx % legacy_Setinput([i, j])
      call zx % Run()
      !print '("Iteration ",i2,1x,i2," difference from target ",i9)',i,j,zx%mem(0)-TARGET_RESULT
      if (zx%legacy_Getoutput() > TARGET_RESULT) exit
    end do
    do j=0, 99
      call zx % Reset()
      call zx % legacy_Setinput([i-1, j])
      call zx % Run()
      !print '("Iteration ",i2,1x,i2," difference from target ",i9)',i,j,zx%mem(0)-TARGET_RESULT
      if (zx%legacy_Getoutput() >= TARGET_RESULT) exit
    end do
    print '("Answer 2/1 ",i0,l2)', 100*i+j, zx%legacy_Getoutput()==TARGET_RESULT
    print *
  end subroutine day02



  subroutine day03(file)
    use day1903_mod
    implicit none
    character(len=*), intent(in) :: file
    type(line_t), allocatable :: wire_a(:), wire_b(:)
    integer, allocatable :: xsec(:), ysec(:)
    integer :: ans1, ans2

    call read_from_file(file, wire_a, wire_b)
    call find_intersections(wire_a, wire_b, xsec, ysec, ans1, ans2)
    print '("Valid answer 1? ",l1)', ans1==709 .or. ans1==159 .or. ans1==135
    print '("Valid answer 2? ",l1)', ans2==610 .or. ans2==410 .or. ans2==13836
    print *
  end subroutine day03



  subroutine day04
    use day1904_mod
    implicit none
    character(len=PASSLEN) :: range(2)
    integer :: ans1, ans2

    range(1)='172930'
    range(2)='683082'

    call count_valid(range, 1, ans1)
    print '("Answer 4/1 ",i0,l2)', ans1, ans1==1675
    call count_valid(range, 2, ans2)
    print '("Answer 4/2 ",i0,l2)', ans2, ans2==1142
    print '("Range was ",a,"-",a)',range
    print *
  end subroutine day04



  subroutine day05(file)
    !use day1905_mod
    use day1907_mod ! modernized interpreter
    use parse_mod, only : read_strings, string_t, split
    implicit none
    character(len=*), intent(in) :: file
    type(string_t), allocatable :: lines(:), items(:)
    integer, allocatable :: state(:)
    type(computer_t) :: zx
    integer, allocatable :: outbuf(:)

    ! Read and parse input
    lines = read_strings(file)
    if (size(lines)/=1) error stop 'day05 - input file has more than one line'
    call split(lines(1)%str,',',items)
    state = items % To_int()
    !print '(16(i5,1x))', state

    ! Load and run test (Part 1)
    call zx % Load(state)
    call zx % Reset(1,10)
    call zx % set_inbuf(1)
    call zx % Run()
    outbuf = zx%get_outbuf()
    print '(a,8(i8,1x))', 'Buffer =', outbuf
    print '("Answer 5/1 ",i0,l2)', outbuf(size(outbuf)), outbuf(size(outbuf))==9006673

    ! Part 2
    call zx % Reset(1,1)
    call zx % set_inbuf(5)
    call zx % Run()
    outbuf = zx%get_outbuf()
    print '(a,8(i8,1x))', 'Buffer =', outbuf
    print '("Answer 5/2 ",i0,l2)', outbuf(size(outbuf)), outbuf(size(outbuf))==3629692
    print *
  end subroutine day05



  subroutine day06(file)
    use day1906_mod
    use parse_mod, only : read_strings, string_t, split
    implicit none
    character(len=*), intent(in) :: file
    type(string_t), allocatable :: lines(:)
    type(object_ptr), allocatable :: map(:), you_path(:), san_path(:)
    integer :: nobj, i, j, ans1, you_n, san_n, ans2

    lines = read_strings(file)
    call make_map(lines, map)
    nobj = size(map)

    print '("Unique objects ",i0," Lines of input ",i0)', nobj, size(lines)
    call count_orbits(map, ans1)
    print '("Total number of orbits (Ans 6/1) ",i0,l2)', ans1, ans1==42 .or. ans1==224901

    ! Part Two
    i = object_find(map, 'YOU')
    if (i==0) error stop 'YOU not found'
    call list_to_com(map(i)%ptr, you_path)
    j = object_find(map, 'SAN')
    if (j==0) error stop 'SAN not found'
    call list_to_com(map(j)%ptr, san_path)
    you_n = size(you_path)
    san_n = size(san_path)
    do i=0, min(you_n, san_n)
      if (you_path(you_n-i)%getkey() /= san_path(san_n-i)%getkey()) exit
    end do
    ans2 = (you_n-i-1)+(san_n-i-1)
    print '("Total number of moves (Ans 6/2) ",i0,l2)', ans2, ans2==4 .or. ans1==224901

    ! TODO - deallocate pointers before leaving!!!
  end subroutine day06



  subroutine day07(file)
    use day1907b_mod
    use parse_mod, only : read_strings, string_t, split
    implicit none
    character(len=*), intent(in) :: file
    type(string_t), allocatable :: lines(:), items(:)
    integer, parameter :: CLUSTER_SIZE=5
    type(computer_t) :: cluster(CLUSTER_SIZE)
    integer :: i, ans1, ans2

    ! Read and load Intcode program
    lines = read_strings(file)
    if (size(lines)/=1) error stop 'day07 - input file has more than one line'
    call split(lines(1)%str,',',items)
    do i=1, CLUSTER_SIZE
      call cluster(i) % Load(items % To_int())
    end do
    !print '(16(i5,1x))', items % To_int()

    ! Solve the puzzle
    call solve_day7(cluster, .false., ans1)
    print '("Highest signal value (7/1) is: ",i0,l2)',ans1, ans1==338603
    call solve_day7(cluster, .true., ans2)
    print '("Highest signal value (7/2) is: ",i0,l2)',ans2, ans2==63103596
    print *
  end subroutine day07



  subroutine day08(file)
    use day1908_mod
    use parse_mod, only : read_strings, string_t
    implicit none
    character(len=*), intent(in) :: file
    type(string_t), allocatable :: lines(:)
    character(len=1), allocatable :: pic(:,:)
    integer :: nchar, ans1

    ! Input read into a string
    lines = read_strings(file)
    nchar = len_trim(lines(1)%str)
    print '("Input ",i0" lines of ",i0" characters")',size(lines), nchar
    call decode_image(lines(1)%str, pic, ans1)
    print '("Answer 8/1 ",i0,l2)', ans1, ans1==1792
    print *
  end subroutine day08



  subroutine day09(file)
    use kinds_m, only : I8B
    use intcode_mod, only : computer_t
    use parse_mod, only : read_strings, string_t, split
    implicit none
    character(len=*), intent(in) :: file
    type(computer_t) :: ZX128
    integer(I8B), allocatable :: ans1(:), ans2(:)

    ! Read and load Intcode program
    call ZX128 % Load_from_file(file)

    ! Test Program 1
    !iarr128=[109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] 
    ! Test Program 2
    !iarr128=[1102,34915192,34915192,7,4,7,99,0]
    ! Test Program 3
    !iarr128=[104_I8B,1125899906842624_I8B,99_I8B]
    !call ZX128 % Load(iarr128)

    ! Part 1
    call ZX128 % Reset(1,1)
    call ZX128 % Set_inbuf(1)
    call ZX128 % Run()
    ans1 = ZX128 % Get_outbuf()
    print '(a/,4(i18))', 'BOOST diagnostic code (9/1)', ans1
    print '("Is valid answer ?",l2)', ans1(1)==2457252183_I8B

    ! Part 2
    call ZX128 % Reset(1,1)
    call ZX128 % Set_inbuf(2)
    call ZX128 % Run()
    ans2 = ZX128 % Get_outbuf()
    print '(a/,4(i18))', 'BOOST distress signal coordinates (9/2)', ans2
    print '("Is valid answer ?",l2)', ans2(1)==70634
    print *
  end subroutine day09



  subroutine day10(file)
    use day1910_mod
    implicit none
    character(len=*), intent(in) :: file
    integer, allocatable :: xypos(:,:)
    integer :: idstation, ans1, ans2

    call read_input_from_file(file, xypos)
    call calculate_vectors(xypos,idstation,ans1)
    print '("Answer 10/1 (visible asteroids ",i0,l2)',ans1, ans1==230
    call shoot_asteroids(xypos,idstation,ans2)
  end subroutine day10



  subroutine day11(file)
    use intcode_mod, only : computer_t
    use day1911b_mod, only : robot_t
    use kinds_m, only : I8B
    character(len=*), intent(in) :: file
    integer(I8B) :: inp
    integer(I8b), allocatable :: buff(:)
    integer :: status
    type(computer_t) :: ZX128
    real :: x
    type(robot_t) :: EMIL

    call EMIL % Init(file)
    call EMIL % Walk()

    return
    call ZX128 % Load_from_file(file)
    call ZX128 % Reset(1,2)

    do
    !print *, 'input? '
    !read(*,*) inp
    call random_number(x)
    inp = ceiling(2*x)-1
    if (inp < 0) exit
    call ZX128 % Set_inbuf(inp)
    call ZX128 % Run(status)
    buff = ZX128 % Get_outbuf()
    print '(*(i3,1x))', buff
    if (status==-1) exit
    end do

  end subroutine day11


