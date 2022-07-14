  module day1907b_mod
    use day1907_mod, only : computer_t, SHALT, SRUNNING  ! our Intercode computer (V2.5)
    use queue_mod, only : queue_t                        ! buffers
    use permutation_mod, only : permutation_generator_t
    implicit none

  contains

    subroutine solve_day7(cluster, is_day2, maxout)
      type(computer_t), intent(inout) :: cluster(:)
      logical, intent(in) :: is_day2
      integer, intent(out) :: maxout
      integer :: ncluster, i, valout
      integer, allocatable :: phase(:)
      type(permutation_generator_t) :: permutator
      logical :: isok

  print *,'Is part 2? ', is_day2
      ncluster = size(cluster)
      allocate(phase(ncluster))
      do i=1, ncluster
        phase(i) = i-1
      end do
      if (is_day2) phase = phase + 5

      maxout = -huge(maxout)
      call permutator % Init(phase)
      do
        if (permutator % Remaining() <= 0) exit
        call permutator % Next(phase,isok)
        if (.not. isok) error stop 'permutator - did not permutate'
        call test_configuration(cluster, phase, valout)
        if (valout > maxout) then
          maxout = valout
        end if
      end do

    end subroutine solve_day7



    subroutine test_configuration(cluster, phase, val)
      type(computer_t), intent(inout) :: cluster(:)
      integer, intent(in) :: phase(:)
      integer, intent(out) :: val

      integer :: i, ncluster, tmp_io
      integer :: status(size(cluster))

      ! Initialize computers and set their phase
      ncluster = size(cluster)
      do i=1, ncluster
        call cluster(i) % Reset(2,1) ! 2 inputs / 1 output expected
        call cluster(i) % Set_inbuf(phase(i))
      end do
      call cluster(1) % Set_inbuf(0) ! input to the first amplifier

      ! Do main calculation loop
      status = SRUNNING
      do
        do i=1, ncluster
          ! Run the code
          call cluster(i) % Run(status(i))
! print *, 'Computer ',i,status(i)

          ! Move output to the next computer in the chain (if-possible)
          if (i == ncluster) then
            continue
          else
            if (.not. (cluster(i)   % Isempty_outbuf() .or. &
                       cluster(i+1) % Isfull_inbuf())) then
              call cluster(i) % Read_outbuf(tmp_io)
              call cluster(i+1) % Set_inbuf(tmp_io)
            end if
          end if
        end do
        
        ! Exit main loop if there is an output from the last amplifier
        if (.not. cluster(ncluster) % Isempty_outbuf()) exit
      end do
      call cluster(ncluster) % Read_outbuf(val)
 print '("Output ",i0," phasers ",5(i1,1x))', val, phase

    end subroutine test_configuration

  end module day1907b_mod
