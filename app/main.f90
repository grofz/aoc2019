  program main
    implicit none
!goto 05

01  call day01('inp/1901/input.txt')

02  call day02('inp/1902/input.txt')

03  call day03('inp/1903/input.txt')

04  call day04()

05  call day05('inp/1905/input.txt')


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
    use day1905_mod ! Version 2.0 computer from day 5
    use parse_mod, only : read_strings, split, string_t
    implicit none
    character(len=*), intent(in) :: file
    type(string_t), allocatable :: lines(:), items(:)
    integer, allocatable :: state(:)
    type(computer_t) :: zx
    integer :: inp(2), i, j
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
    call zx % Setinput([12,2])
    call zx % Run()
    print '("Answer 2/1 ",i0,l2)', zx%Getoutput(), zx%Getoutput()==4930687

    ! Part 2 - manual
    goto 100
    do
      write(*,'(a)',advance='no') 'guess the input '
      read(*,*) inp(1), inp(2)
      call zx % Reset()
      call zx % Setinput(inp)
      call zx % Run()
      print *, 'RESULT IS ',zx%Getoutput(), zx%Getoutput()-TARGET_RESULT
    end do

    ! Part 2 - automatic
    100 continue
    j = 0
    do i=0, 99
      call zx % Reset()
      call zx % Setinput([i, j])
      call zx % Run()
      !print '("Iteration ",i2,1x,i2," difference from target ",i9)',i,j,zx%mem(0)-TARGET_RESULT
      if (zx%Getoutput() > TARGET_RESULT) exit
    end do
    do j=0, 99
      call zx % Reset()
      call zx % Setinput([i-1, j])
      call zx % Run()
      !print '("Iteration ",i2,1x,i2," difference from target ",i9)',i,j,zx%mem(0)-TARGET_RESULT
      if (zx%Getoutput() >= TARGET_RESULT) exit
    end do
    print '("Answer 2/1 ",i0,l2)', 100*i+j, zx%Getoutput()==TARGET_RESULT
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
    use day1905_mod
    use parse_mod, only : read_strings, string_t, split
    implicit none
    character(len=*), intent(in) :: file
    type(string_t), allocatable :: lines(:), items(:)
    integer, allocatable :: state(:)
    type(computer_t) :: zx

    ! Read and parse input
    lines = read_strings(file)
    if (size(lines)/=1) error stop 'day05 - input file has more than one line'
    call split(lines(1)%str,',',items)
    state = items % To_int()
    !print '(16(i5,1x))', state

    ! Load and run test (Part 1)
    call zx % Load(state)
    call zx % set_inbuf(1)
    call zx % Run()
    print '("Answer 5/1 ",i0,l2)', zx%get_outbuf(), zx%get_outbuf()==9006673

    ! Part 2
    call zx % Reset()
    call zx % set_inbuf(5)
    call zx % Run()
    print '("Answer 5/2 ",i0,l2)', zx%get_outbuf(), zx%get_outbuf()==3629692
    print *
  end subroutine day05
