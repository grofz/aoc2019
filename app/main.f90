  program main
    implicit none
goto 24

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
11  call day11('inp/1911/input.txt')

12  continue
    call day12('inp/1912/input.txt')
    !call day12('inp/1912/sample2.txt')

13  call day13('inp/1913/input.txt')

14  continue
    !call day14('inp/1914/sample4.txt')
    call day14('inp/1914/input.txt')

15  call day15('inp/1915/input.txt')

16  call day16('inp/1916/input.txt')

17  call day17('inp/1917/input.txt')

18  continue
    !call day18('inp/1918/sample4.txt',1)
    !call day18('inp/1918/input.txt',1)
    !call day18('inp/1918/sampleB4.txt',2)
    call day18('inp/1918/inputB.txt',2)

19  call day19('inp/1919/input.txt')

20  continue
   !call day20('inp/1920/sample3.txt')
    call day20('inp/1920/input.txt')

21  call day21('inp/1921/input.txt')

22  call day22('inp/1922/input.txt')

23  call day23('inp/1923/input.txt')

24  continue
   !call day24('inp/1924/sample.txt')
    call day24('inp/1924/input.txt')
stop

25  call day25('inp/1925/input.txt') ! password

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
    use day1911b_mod, only : robot_t
    character(len=*), intent(in) :: file
    type(robot_t) :: EMIL

    ! Part 1
    call EMIL % Init(file)
    call EMIL % Walk(0)

    ! Part 2
    call EMIL % Init(file)
    call EMIL % Walk(1)
  end subroutine day11



  subroutine day12(file)
    use day1912_mod
    use prime_numbers_mod
    use kinds_m, only : I8B
    character(len=*), intent(in) :: file
    integer, allocatable :: x0(:,:), periods(:)
    type(mdsimulation_t) :: sim
    integer(I8b) :: ans
    integer, parameter :: TEND=1000000 !700000

    x0 = positions_from_file(file)
    call sim % Init(x0,'results/nbody.log')
    print '("Simulating ",i0," steps...")', tend
    call sim % Simulate(TEND)
    print '("Simulation completed. Analyzing orbits...")'
    call sim % Analyze(periods)
    print '("Periods of all variables"/,3(i9,:,", "))', periods
    call sim % Closefiles()
    ans = get_lcm(periods)
    print '("Answer is ",i0,l2)', ans, 4686774924_I8B==ans .or. &
        292653556339368_I8B==ans
  end subroutine day12



  subroutine day13(file)
    use day1913_mod, only : cwd_t
    implicit none
    character(len=*), intent(in) :: file
    type(cwd_t) :: GAME
    integer, allocatable :: dout(:)
    integer :: status, cnt, inp, score

    call GAME % Init_cwd(file, .true.)
    !call GAME % Play(.false., 0.0, score)
     call GAME % Play(.true., 0.00, score)
    !call GAME % Play(.true., 0.02, score)
    return


    cnt = 0
    do
      call GAME % Run(status)
      dout = GAME % Get_outbuf()
      print *, dout

      if (size(dout) /= 0 .and. dout(3)==2) cnt=cnt+1
      if (status == -3) cycle
      if (status == -2) then
        write(*,'(a)',advance='no') 'Your input? ', inp
        read(*,*) inp
        call GAME % Set_inbuf(inp)
        cycle
      end if
      print '("Status ",i0)', status
      exit
    end do
    print *, 'counter', cnt
  end subroutine day13



  subroutine day14(file)
    use day1914_mod
    use list_mod, only : list_t
    implicit none
    character(len=*), intent(in) :: file

    type(list_t) :: complist
    integer, allocatable :: stoch(:,:)
    integer(I8B)         :: ans1, ans2
    integer :: i

    call stoch_from_file(file, complist, stoch)

    do i=1,size(stoch,1)
      print '(20i4)',stoch(i,:)
    end do
    print '(a)',  complist % Print()
    print *

    call part1_sol(complist, stoch, 1_I8B, ans1)
    call part2_sol(complist, stoch, 1000000000000_I8B, ans2)
    print '("Ore consumption (14/1)  ",i0,l2)', -ans1,  -857266==ans1
    print '("Fuel produces (14/2)    ",i0,l2)', ans2,  2144702==ans2
    print *
  end subroutine day14



  subroutine day15(file)
    use day1915_mod, only : maze_t, mazenode_t, shortest_path
    use intcode_mod, only : computer_t, SOUTBUF_FULL, SINBUF_EMPTY
    implicit none
    character(len=*), intent(in) :: file

    type(maze_t) :: maze
    type(mazenode_t), allocatable :: nodes(:)
    type(computer_t) :: ZX128
    integer :: status, inp, newinp, leakid, homeid
    integer :: out0
    integer, allocatable :: freedirs(:), dd(:)

    call maze % Init(file)
    call maze % Explore_Maze(nodes, homeid, leakid)
    call maze % Print()
    call shortest_path(nodes, leakid, homeid, dd)
    print *
  end subroutine day15



  subroutine day16(file)
    use day1916_mod
    use kinds_m, only : I1B
    use parse_mod, only : read_strings, string_t
    implicit none
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer, allocatable :: k(:,:), inp(:), out100(:)
    integer(I1B), allocatable :: inplong(:), outlong(:)
    integer :: n, nlong, i, offset, ios
    integer, parameter :: INP_REPEATED=10000
    real :: t0, t1

    call cpu_time(t0)
    lines = read_strings(file)
    if (size(lines)/=1) error stop 'day16 - one line expected in input'
    inp = sig2num(lines(1)%str)
    n = size(inp,1)

    ! input for Part 2
    nlong = n * INP_REPEATED
    allocate(inplong(nlong))
    do i = 1, INP_REPEATED
      inplong((i-1)*n+1:i*n) = int(inp, kind=I1B)
    end do

    print '("Input")'
    print 100, inp
    print '("Input length ",i0)', n

    out100 = fft_phases(inp,100)
    outlong = fft_phases(inplong,100)
    call cpu_time(t1)

    !print '("Output")'
    !print 100, out100

    ! Part 1 - aking for first 8 digits of "out100"
    print '("Ans 16/1 ",8i2," is valid?",l2)',  out100(1:8), &
      all(out100(1:8)==[1,0,1,8,9,3,5,9])

    ! Part 2 - asking about 8 digits after offset
    read(lines(1)%str(1:7),*,iostat=ios) offset
    if (ios/=0) error stop 'day 16 - error conversion'
    if (offset <= size(inplong,1)/2) print *, 'Warning: message in first half'

    print '("Ans 16/2 ",8i2," is valid?",l2)', outlong(offset+1:offset+8), &
        all(outlong(offset+1:offset+8)==[8,0,7,2,2,1,2,6])
    print '("Time taken ",f8.3)', t1-t0
    print *
!return

    ! Example of kernel
    print '("Transformation matrix")'
    k = kernel(32)
    print '(32(i2))', transpose(k)
    print *

    100 format (40(i2))
  end subroutine day16



  subroutine day17(file)
    use intcode_mod, only : computer_t
    use day1917_mod, only : scaffold_t, camera_view, get_crossings, get_fullpath, run_part2, compress_path
    implicit none
    character(len=*), intent(in) :: file

    type(computer_t) :: ZX
    type(scaffold_t) :: scaffold
    integer :: istat, val, ans1
    integer, allocatable :: cpos(:,:)
    character(len=:), allocatable :: fpath

    ! Part 1
    call ZX % Load_from_File(file)
    call ZX % Reset(1,1)
    call scaffold % Init()
    call camera_view(scaffold, ZX)
    !call scaffold % Print('',ismirror=[.false.,.false.])
    call get_crossings(scaffold, ans1, cpos)
    fpath = get_fullpath(scaffold)
    print '(a)', fpath
    print *

    ! Analyze full path and compress it
    ! TODO - done manualy for now

    ! Part 2
    print *, 'Part2'
    call run_part2(file,'inp/1917/path.txt')
    print *

    ! test
    call compress_path(fpath, 'tmp')
  end subroutine day17



  subroutine day18(file, mode)
    use day1918_mod
    use parse_mod, only : read_pattern
    use graph_mod, only : graph_t
    implicit none
    character(len=*), intent(in) :: file
    integer, intent(in) :: mode

    character(len=1), allocatable :: raw(:,:)
    type(graph_t) :: g, g2
    type(state_t) :: s0, sfinal
    type(state_t), allocatable :: snext(:)

    raw = read_pattern(file)
    print '("Labyrinth size ",i0," x ",i0)', size(raw,1), size(raw,2)
    g = graph_from_raw(raw)
    call g % Listvertices()

    print '("Reducing the graph...")'
    g2 = graph2_from_graph(g)
    call g2 % Listvertices()

    select case(mode)
    case(1)
      print '("Solving part 1...")'
      s0 = state_t(g2)
      call search2(s0, g2, 1, sfinal)
      print '("Shortest path (18/1) ", i0)', sfinal%cost
      print '("Validation ?",l2)', sfinal%cost==4770

    case(2)
      print '("Solving part 2...")'
      s0 = state_t(g2)
      call search2(s0, g2, 4, sfinal)
      print '("Shortest path (18/2) ", i0)', sfinal%cost
      print '("Validation ?",l2)', sfinal%cost==1578
    end select
  end subroutine day18



  subroutine day19(file)
    use day1919_mod
    implicit none
    character(len=*), intent(in) :: file

    type(scanner_t) :: SC
    integer :: ans1, ans2

    call SC % Init(file)
    call test_beam(SC, ans1)
    call track_beam(SC, 100, ans2)
  end subroutine day19



  subroutine day20(file)
    use day1920_mod
    use parse_mod, only : read_pattern
    use graph_mod, only : graph_t
    implicit none
    character(len=*), intent(in) :: file

    character(len=1), allocatable :: raw(:,:)
    type(graph_t) :: g0, g1, g2
    integer :: i, ans
    integer, parameter :: NLEV=25
    real :: t0, t1, t2

    raw = read_pattern(file,.true.)
    print '("Labyrinth size ",i0," x ",i0)', size(raw,1), size(raw,2)

    ! Part 1
    print '(a)','Constructing graph...'
    call cpu_time(t0)
    g0 = graph_from_raw(raw,1)
    call cpu_time(t1)
    print '("...done. Time taken ",f8.3," seconds."/)', t1-t0

    print '(a)','Searching shortest path...'
    ans = shortest_distance(g0)
    call cpu_time(t2)
    print '("...done. Time taken ",f8.3," seconds."/)', t2-t1

    print '("Shortest path (20/1) is ",i0,l2)', ans, ans==482

    ! Part 2
    print '(a)','Constructing graph...'
    call cpu_time(t1)
    g0 = graph_from_raw(raw,2)
    call cpu_time(t2)
    print '("...done. Time taken ",f8.3," seconds."/)', t2-t1

    print '(a)','Reducing...'
    call cpu_time(t1)
    g1 = reduced_graph(g0)
    !call g1 % Listvertices()
    call cpu_time(t2)
    print '("...done. Time taken ",f8.3," seconds."/)', t2-t1

    print '(a,i0,a)','Making recursive maze down to level ',NLEV,'...'
    call cpu_time(t1)
    g2 = repeat_graph(g1, NLEV)
    call cpu_time(t2)
    print '("...done. Time taken ",f8.3," seconds."/)', t2-t1

    print '(a)','Searching shortest path...'
    call cpu_time(t1)
    ans = shortest_distance(g2)
    call cpu_time(t2)
    print '("...done. Time taken ",f8.3," seconds."/)', t2-t1

    print '("Shortest path (20/2) is ",i0,l2)', ans, ans==5912
    print '("Total time ",f0.3," seconds."/)', t2-t0
  end subroutine day20



  subroutine day21(file)
    use ascii_mod
    implicit none
    character(len=*), intent(in) :: file

    call run_ascii(file)
  end subroutine day21



  subroutine day22(file)
    use day1922_mod, only : cards_t, IXB, BIG_DECK, SMALL_DECK, NREP
    implicit none
    character(len=*), intent(in) :: file

    integer, parameter :: NCOMB=50
    type(cards_t) :: deck, comb(NCOMB), wrk
    integer(IXB) :: ans, j, rep(NCOMB), todo

    ! Part 1
    deck = cards_t(SMALL_DECK)
    call deck % Deal_file(file)
    ans = deck % Findloc(2019)
    print '("Answer 22/1 is ",i0,l2)', ans, ans==4649

    ! Part 2
    ! "comp(j)" is deck shuffled 2**(j-1) times
    deck = cards_t(BIG_DECK)
    call deck % Deal_file(file)
    rep(1) = 1
    comb(1) = deck ! shuffled once
    do j=2,NCOMB
      !comb(j) = combine(comb(j-1),comb(j-1))
      comb(j) = comb(j-1) .com. comb(j-1)
      rep(j) = 2*rep(j-1)
    enddo

    ! Use "comb" to obtain deck shuffled required amount of ! times
    wrk = cards_t(BIG_DECK)

    todo = NREP
    do j = NCOMB, 1, -1
      if (todo == 0) exit
      if (todo / rep(j) ==  1) then
        todo = todo - rep(j)
        !wrk = combine(comb(j),wrk)
        wrk = comb(j) .com. wrk
      else if (todo / rep(j) == 0) then
        continue
      else
        error stop 'not possible'
      end if
    end do
    ans = wrk % Pick_card(2020_IXB)
    print '("Answer 22/2 is ",i0,l2)', ans, ans==68849657493596_IXB
    print *
  end subroutine day22



  subroutine day23(file)
    use day1923_mod, only : network_t
    implicit none
    character(len=*), intent(in) :: file

    type(network_t) :: network
    integer :: ans1, ans2

    call network % Init(file)
    call network % Run(ans1,ans2)
    print '("Answer 23/1 ",i0,l2)', ans1, ans1==24922
    print '("Answer 23/2 ",i0,l2)', ans2, ans2==19478
    print *
  end subroutine day23



  subroutine day24(file)
    use day1924_mod
    use parse_mod, only : read_pattern
    implicit none
    character(len=*), intent(in) :: file
    character(len=1), allocatable :: tmp(:,:), wrk(:,:), grid(:,:,:)
    integer :: i, nlist, b, ans1, ans2
    integer, allocatable :: list(:)
    logical :: new_layout

    ! Get input
    tmp = read_pattern(file)
    allocate(wrk(0:size(tmp,1)+1,0:size(tmp,2)+1))
    wrk = '.'
    wrk(1:size(tmp,1),1:size(tmp,2)) = tmp
    deallocate(tmp)

    ! Part 1 = grow until pattern repeats
    tmp = wrk
    do i=1,4000
      b = biod(tmp) 
      call add_list(list,nlist,b,new_layout)
      print *
      call print_bugs(tmp)
      print *, b
      if (.not. new_layout) exit
      tmp = one_step(tmp)
    end do
    ans1 = b
    print '("Answer 24/1 is ",i0,l2)', ans1, ans1==30446641

    ! Part 2
    allocate(grid(1:size(wrk,1)-2, 1:size(wrk,2)-2, -500:500))
    grid = '.'
    grid(:,:,0) = wrk(1:size(wrk,1)-2,1:size(wrk,2)-2)
    do i=1,200
      grid = one_step2(grid)
    end do
    ans2 = count(grid==BUG)
    print '("Answer 24/2 is ",i0,l2)', ans2, ans2==1985

  end subroutine day24



  subroutine day25(file)
    use ascii_mod
    implicit none
    character(len=*), intent(in) :: file

    call run_ascii(file)
    ! candy cane, mouse, coin,semiconductor
    ! pass 100667393
    !      100667393
  end subroutine day25
