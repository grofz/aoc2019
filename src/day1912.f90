!
! Day 12 - N-body problem. Molecular dynamics simulation
!
  module day1912_mod
    use parse_mod, only : unique_sort
    implicit none
    private
    public positions_from_file

    integer, parameter :: NO_LOG_FID = 0

    type, public :: mdsimulation_t
      integer, allocatable :: x_init(:,:)
      integer, allocatable :: x(:,:), v(:,:)
      integer, allocatable :: F(:,:)
      integer              :: time=0
      integer :: fidlog = NO_LOG_FID
      integer, allocatable :: fids(:,:), periods(:,:)
    contains
      procedure :: Init, Closefiles
      procedure :: Simulate
      procedure :: Calc_energy
      procedure :: Print => mdsimulation_print
      procedure :: Analyze => analyze_periods
    end type

  contains

    subroutine Simulate(this, tend)
      class(mdsimulation_t), intent(inout) :: this
      integer, intent(in) :: tend

      integer, parameter :: DT = 1

      MAIN: do
        !call this % Print()
        if (this%time==1000) then
           print '("Answer 12/1 is ",i0,l2)', &
           this%Calc_energy(), this%Calc_energy()==7722
        end if

        if (this%fidlog /= NO_LOG_FID) then
          call plot_positions(this%fidlog, this%x, this%time)
        end if
        if (this % time >= tend) exit MAIN

        call calc_forces(this%x, this%F)
        call euler_integer(this%v, this%F, DT)
        call euler_integer(this%x, this%v, DT)
        this % time = this % time + DT
        call log_events(this)
      end do MAIN

    end subroutine Simulate



    subroutine Init(this, x0, filelog)
      class(mdsimulation_t), intent(out) :: this
      integer, intent(in) :: x0(:,:)
      character(len=*), intent(in), optional :: filelog
!
! Set initial positions. Open all log files needed to calculate
! the Part 2
!
      integer :: n, i, j
      character(len=10) :: wrkname
      character(len=*), parameter :: OUTDIR='./results/'
      
      n = size(x0, dim=2)
      if (size(x0, dim=1) /= 3) &
          error stop 'mdsimulation: Init: not 3D positions'
      allocate(this % x(3,n), this % v(3,n), this % F(3,n))
      this % x = x0
      this % v = 0
      this % F = 0
      this % time = 0
      this % x_init = x0

      ! Open logs for analysis in Part 2
      if (present(filelog)) then
        open(newunit=this%fidlog, file=filelog, status='replace')
        allocate(this%fids(3, n))
        do i=1,n
        do j=1,3
          write(wrkname, '("n",i0,"c",i0)') i, j
          open(newunit=this%fids(j,i), &
               file=OUTDIR//trim(wrkname)//'.log', status='replace')
        end do
        end do
      end if
    end subroutine 



    subroutine mdsimulation_final(this)
      type(mdsimulation_t), intent(inout) :: this
      logical :: op
      integer :: i, j
!
! Close all logs we have opened (TODO Delete moon events?)
!
print *, 'Terminator called'
      inquire(unit=this%fidlog, opened=op)
      if (op) close(this%fidlog)
      if (allocated(this%fids)) then
        do i=1,size(this%x, 2)
        do j=1,3
          inquire(unit=this%fids(j,i), opened=op)
          if (op) close(this%fids(j,i))
        end do
        end do
      end if
    end subroutine

    subroutine Closefiles(this)
      class(mdsimulation_t), intent(inout) :: this
      call mdsimulation_final(this)
    end subroutine



    integer function Calc_energy(this) result(tot)
       class(mdsimulation_t), intent(in) :: this

       integer :: pot, kin, i
       associate(x => this%x, v => this%v)
       tot = 0
       do i=1, size(x, 2)
         pot = sum(abs(x(:,i)))
         kin = sum(abs(v(:,i)))
         tot = tot + pot*kin
       end do
       end associate
    end function



    subroutine mdsimulation_print(this)
      class(mdsimulation_t), intent(in) :: this
      integer :: i
      character(len=*), parameter :: fm3d='"[",2(i6,","),i6,"]"'

      print '("Time ",i5,5x,"Energy ",i0)', &
        this % time, this % Calc_energy()
      print '(a)', &
' ******    x    ******    ******    v    ******    ******    F    ******'
      do i=1,size(this%x,dim=2)
        print '('//fm3d//',3x,'//fm3d//',3x,'//fm3d//')', &
          this%x(:,i), this%v(:,i), this%F(:,i)
      end do
      print *
    end subroutine



    subroutine euler_integer(u, f, dt)
      integer, intent(inout) :: u(:,:)  ! state variables 
      integer, intent(in)    :: f(:,:)  ! du/dt = f(u)
      integer, intent(in)    :: dt
!
! Make one step of forward Euler
!               =============================
!               u(t+dt) = u(t) + f[u(t)] * dt
!               =============================
!
      call assert_consistent_arrays(u, f)
      u = u + f*dt
    end subroutine



    subroutine calc_forces(x, F)
      integer, intent(in) :: x(:,:)
      integer, intent(out) :: F(:,:)
!
! Calcualte force beteeen objects
!
      integer :: i, j, n, fpair(3)

      call assert_consistent_arrays(x, F)
      n = size(x, dim=2)
      F = 0
      do i = 1, n-1
      do j = i+1, n
        fpair = x(:,j) - x(:,i)     ! I-->J
        where (fpair /= 0)
          fpair = fpair / abs(fpair)
        else where
          fpair = 0
        end where
        F(:,i) = F(:,i) + fpair
        F(:,j) = F(:,j) - fpair
      end do
      end do
    end subroutine



    subroutine assert_consistent_arrays(aa, bb)
      integer, intent(in), dimension(:,:) :: aa, bb
      integer, parameter :: D=3
      if (size(aa,2) /= size(aa,2)) &
         error stop 'mdsimulation ERROR: arrays are not same'
      if (size(aa,1) /= D .or. size(bb,1) /= D) &
         error stop 'mdsimulation ERROR: first dimention must be 3'
    end subroutine



    function positions_from_file(file) result(x0)
      use parse_mod, only : read_strings, split, string_t
      character(len=*), intent(in) :: file
!
! Read and parse the input file and get initial positions.
!
      integer, allocatable :: x0(:,:)

      type(string_t), allocatable :: lines(:), tok(:), tok1(:)
      character(len=:), allocatable :: wrk
      integer :: i, n, line_len, j
      character(len=1), parameter :: LAB(3) = ['x','y','z']

      lines = read_strings(file)
      n = size(lines)
      print '("Number of objects ",i0)', n
      allocate(x0(3,n))

      do i = 1, n
        ! remove < ... >
        line_len = len_trim(lines(i)%str)
        wrk = lines(i)%str(2:line_len-1)

        ! split to tokens "x=-4"
        call split(wrk, ',', tok)
        if (size(tok) /= 3) &
            error stop 'positions_from_file - 3 tokens expected'
        do j=1,3
          call split(tok(j)%str, '=', tok1)
          if (size(tok1)/=2) error stop 'now two tokens expected'
          if (trim(adjustl(tok1(1)%str)) /= LAB(j)) &
            error stop 'position_from_file = x, y, z - expected'
          read(tok1(2)%str,*) x0(j,i)
        end do
      end do
      print '("Initial positions")'
      do i=1,n
        print '(3(i0,:,","))', x0(:,i)
      end do
    end function


! ==============================================
! PATTERN MATCHING UTILITIES NEEDED FOR PART TEO
! ==============================================



    subroutine plot_positions(fid, u, time)
      integer, intent(in) :: fid
      integer, intent(in) :: u(:,:), time
!
! This file is not needed by the program. 
! Just for movement visualizations.
!
      integer :: n
      n = size(u, 2)
      write(fid, '(*(i0,1x))'), time, u
    end subroutine



    subroutine log_events(this)
      class(mdsimulation_t), intent(inout) :: this
      integer :: i, j, n
      logical :: op
!
! Log times, when a co-oridnate returns back to its initial positions.
! (each moon/component is stored in separate file)
!
      n = size(this%x, 2)
      do i=1,n
      do j=1,3
        if (this%x(j,i)==this%x_init(j,i) ) then
          if (allocated(this%fids)) then
            inquire(unit=this%fids(j,i), opened=op)
            if (op) write(this%fids(j,i), '(i0)') this%time
          end if
        end if
      end do
      end do

    end subroutine log_events



    subroutine analyze_periods(this, allperiods)
      class(mdsimulation_t), intent(inout) :: this
      integer, allocatable, intent(out) :: allperiods(:)
!
! Use files (still opened) with logged events to obtain
! orbit times for each cordinate.
!
      integer :: i, j
      if (.not. allocated(this%fids)) then
        print *, 'Analyze not possible - no logs were opened'
        return
      end if
      if (allocated(this%periods)) deallocate(this%periods)
      allocate(this%periods(3, size(this%x,2)))

      do i=1, size(this%x,2)
      do j=1, 3
        print '(a)', '**********************'
        print '("Moon #",i0,", coordinate #",i0)', i, j
        print '(a)', '**********************'
        call get_largest_period(this%fids(j,i), this%periods(j,i))
      end do
      end do

      ! pack time periods to a single array
      allperiods = reshape(this%periods, [3*size(this%x,2)])
    end subroutine



    subroutine get_largest_period(fid, res)
      integer, intent(in) :: fid
      integer, intent(out) :: res

      logical :: od
      integer, parameter :: MAX_READ = 10000 ! maximum size read from file
      integer :: arrin(MAX_READ), ios, i, n
      integer, allocatable :: modes(:), freq(:), modes1(:), freq1(:)
      integer, allocatable :: pattern(:)

      ! Read events from file
      inquire(unit=fid, opened=od)
      if (.not. od) then
        print *, 'get_largest_period - log file is not opened'
        return
      end if

      rewind(fid)
      do i = 1, MAX_READ
        read(fid, *, iostat=ios) arrin(i)
        if (ios < 0) then
          exit
        elseif (ios > 0) then
          error stop 'get_largesst_period - read errro'
        end if
      end do
      n = i-1 ! last read was EOF or after normal end of loop

      ! Time periods between events (difference)
      !
      !    [a1 a2 a3 a4 aN   ]
      !  - [ 0 a1 a2 a3 aN-1 ]
      !    ===================
      !     d1 d2 d3 d4 dN
      !
      do i = n, 2, -1
        arrin(i) = arrin(i) - arrin(i-1)
      enddo
      print '("Values read from log file ",i0)', n


      ! Match the pattern. 
      ! Note - If it fails try larger sample sizee (increase simulation time
      print '("Matching pattern:")'
      pattern = match_pattern(arrin(1:n))
      print '(10(i5,1x))', pattern

      ! Unique values and their frequencies (just for statistics)
      modes  = unique_sort(arrin(1:n))
      freq  =  frequency(arrin(1:n), modes)
      print '("Event unique differences are ",*(i0,:,", "))', modes
      modes1 = unique_sort(pattern)
      freq1 =  frequency(pattern, modes1)

      ! Sum time differies in pattern -> this is much searched answer
      res = sum(pattern)
      print '("Period for this component ",i0)', res
    end subroutine get_largest_period



    function frequency(vals, unique) result(freq)
      integer, intent(in) :: vals(:), unique(:)
      integer, allocatable :: freq(:)

      integer :: i
      allocate(freq(size(unique)))
      do i=1, size(unique)
        freq(i) = count(vals==unique(i))
       end do
     end function frequency



     function match_pattern(seq) result(pat)
       integer, intent(in) :: seq(:)
       integer, allocatable :: pat(:)
!
! Identify pattern from sample "seq". If it fails, larger
! sample should be tried.
!
       integer, allocatable :: uniq(:), freq(:)
       integer :: piv_ind, piv, ind_a, ind_b, i, ind_c
       logical :: is_fail

       uniq = unique_sort(seq)
       freq = frequency(seq, uniq)

       ! use least occuring item in the sequence as a pivot
       piv_ind = minloc(freq,1)
       piv = uniq(piv_ind)

       ! pattern begins at the position of pivot ...
       ! ... and may end just before pivots next occurrence
       ind_a = findloc(seq, piv, 1)
       ind_b = ind_a + findloc(seq(ind_a+1:), piv, dim=1)
       if (ind_b <= ind_a) error stop 'match_pattern - sample too short'

       ! iteratively move "ind_b" until match has been found
       MAIN: do
         ! test and compare two pattern occurrences
         is_fail = .false.
         do i=1, (ind_b-1)-ind_a+1
           if (ind_b+i > size(seq)) then
             error stop 'match_pattern - sample too short to verify match'
           end if
           if (seq(ind_a+i) == seq(ind_b+i)) cycle
           is_fail = .true.
           exit
         end do
         if (.not. is_fail) exit MAIN

         ! if comparison fails, try increase the pattern
         ind_b = ind_b + findloc(seq(ind_b+1:), piv, dim=1)
         if (ind_b <= ind_a) error stop 'match_pattern - no pattern found'
       end do MAIN

       ! use the remaning of "seq" to verify there is only one pattern
       is_fail = .false.
       ind_c = ind_b
       TEST: do
         ind_c = ind_c + findloc(seq(ind_c+1:), piv, dim=1)
         do i=1, (ind_b-1)-ind_a+1
           if (ind_c+i > size(seq)) then
             exit TEST
           end if
           if (seq(ind_a+i) == seq(ind_c+i)) cycle
           is_fail = .true.
           exit
         end do
         if (is_fail) then
           error stop 'there is more than one pattern'
         end if
       end do TEST

       ! pack the result
       allocate(pat(ind_b-ind_a))
       pat = seq(ind_a:ind_b-1)

     contains
       ! Emergency - old compiler does not have findloc
       integer function findloc(arr,val,dim) result(ind)
         integer, intent(in) :: arr(:), val
         integer, optional :: dim ! not used
         integer :: i
         ind = 0
         do i=1,size(arr)
           if (arr(i)/=val) cycle
           ind = i
           exit
         end do
       end function
     end function match_pattern

  end module day1912_mod
