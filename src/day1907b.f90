  module day1907b_mod
    use day1907_mod_obs, only : computer_t, SHALT, SRUNNING  ! our Intercode computer (V2.5)
    use queue_aoc19, only : queue_t                        ! buffers
    use permutation_mod, only : permutation_generator_t
    implicit none
    private

    public solve_day7, computer_t

  contains

    subroutine solve_day7(cluster, is_p2, maxout)
      type(computer_t), intent(inout) :: cluster(:)
      logical, intent(in) :: is_p2
      integer, intent(out) :: maxout
      integer :: ncluster, i, valout
      integer, allocatable :: phase(:)
      type(permutation_generator_t) :: permutator
      logical :: isok

      print *,'Solve_day7. Is this part 2 configuration ? ', is_p2
      ncluster = size(cluster)
      allocate(phase(ncluster))
      do i=1, ncluster
        phase(i) = i-1
      end do
      if (is_p2) phase = phase + 5 ! different setting for Part 2 of the puzzle

      maxout = -huge(maxout)
      call permutator % Init(phase)
      do
        if (permutator % Remaining() <= 0) exit
        call permutator % Next(phase,isok)
        if (.not. isok) error stop 'permutator - did not permutate'
        call test_configuration(cluster, phase, is_p2, valout)
        if (valout > maxout) then
          maxout = valout
        end if
      end do

    end subroutine solve_day7



    subroutine test_configuration(cluster, phase, is_p2, val)
      type(computer_t), intent(inout) :: cluster(:)
      integer, intent(in) :: phase(:)
      logical, intent(in) :: is_p2
      integer, intent(out) :: val

      integer :: i, inext, ncluster, tmp_io
      integer :: status(size(cluster))

      ! Reset computers and set their phase parameter
      ncluster = size(cluster)
      do i=1, ncluster
        call cluster(i) % Reset(2,1) ! 2 inputs / 1 output expected
        call cluster(i) % Set_inbuf(phase(i))
      end do

      ! Provide the input to the first amplifier
      call cluster(1) % Set_inbuf(0) 

      ! Do main calculation loop
      status = SRUNNING
      MAIN: do
        do i=1, ncluster
          ! Run the code
          call cluster(i) % Run(status(i))

          ! What is the next computer in the chain?
          if (is_p2) then
            ! circular connections in Part 2
            inext = mod(i, ncluster)+1
          else
            ! linear chain in Part1
            inext = i+1
          end if

          ! Move output to the next computer in the chain (if-possible) ...

          ! ... but make sure that the output from the last unit is not
          ! looped back after code at the first computer has finished
          if (inext==1 .and. status(1)==SHALT) inext = 99

          if (inext <= ncluster) then
            if (.not. (cluster(i)   % Isempty_outbuf() .or. &
                       cluster(inext) % Isfull_inbuf())) then
              call cluster(i) % Read_outbuf(tmp_io)
              call cluster(inext) % Set_inbuf(tmp_io)
            end if
          end if
        end do
        
        ! Exit if all computers has finished (valid for both parts)
        if (all(status==SHALT)) exit MAIN

      end do MAIN

      ! Harvest result from the last computer
      call cluster(ncluster) % Read_outbuf(val)
      !print '("Output ",i0," phasers ",5(i1,1x))', val, phase

    end subroutine test_configuration

  end module day1907b_mod
