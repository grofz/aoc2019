  module day1907_mod
    use intcode23_mod, only : machine_t, STAT_HALT, STAT_WAIT_INPUT, VALUE_KIND
    use permutation_mod, only : permutation_generator_t
    implicit none
    private
    public solve_day7, VALUE_KIND

  contains

    subroutine solve_day7(cluster_size, file, is_p2, maxout)
      integer, intent(in) :: cluster_size
      character(len=*), intent(in) :: file
      logical, intent(in) :: is_p2
      integer(VALUE_KIND), intent(out) :: maxout

      integer :: i
      integer(VALUE_KIND) :: valout
      integer, allocatable :: phase(:)
      type(permutation_generator_t) :: permutator
      logical :: isok

      allocate(phase(cluster_size))
      phase = [(i-1, i=1, cluster_size)]
      if (is_p2) phase = phase + 5 ! different setting for Part 2 of the puzzle

      maxout = -huge(maxout)
      call permutator % Init(phase)
      do
        if (permutator % Remaining() <= 0) exit
        call permutator % Next(phase,isok)
        if (.not. isok) error stop 'permutator - did not permutate'
        call test_configuration(cluster_size, file, phase, is_p2, valout)
        if (valout > maxout) maxout = valout
      end do

    end subroutine solve_day7



    subroutine test_configuration(cluster_size, file, phase, is_p2, val)
      integer, intent(in) :: cluster_size
      character(len=*), intent(in) :: file
      integer, intent(in) :: phase(:)
      logical, intent(in) :: is_p2
      integer(VALUE_KIND), intent(out) :: val

      type(machine_t) :: cluster(cluster_size)
      integer :: i, inext
      integer :: status(size(cluster))

      ! Reset computers and set their phase parameter
      do i=1, size(cluster)
        call cluster(i)%load(file)
        call cluster(i)%pushinput(int(phase(i),VALUE_KIND))
      end do

      ! Provide the input to the first amplifier
      call cluster(1)%pushinput(0_VALUE_KIND)

      ! Do main calculation loop
      MAIN: do
        COMPUTER: do i=1, size(cluster)
          ! Run until halt or waiting for input
          do 
            status(i) = cluster(i)%step()
            if (status(i)==STAT_HALT .or. status(i)==STAT_WAIT_INPUT) exit
          end do

          ! What is the next computer in the chain?
          if (is_p2) then 
            ! circular connections in Part 2
            inext = mod(i, size(cluster))+1
            ! make sure that the output from the last unit is not consumed after
            ! code at the first computer has finished
            if (inext==1 .and. status(1)==STAT_HALT) inext=size(cluster)+1
          else
            ! linear chain in Part 1
            inext = i+1
          end if

          ! Move output to the next computer in the chain
          if (inext <= size(cluster)) & 
              call cluster(inext)%pushinput(cluster(i)%popoutput())
        end do COMPUTER
        
        ! Exit if all computers have finished
        if (all(status==STAT_HALT)) exit MAIN
      end do MAIN

      ! Harvest result from the last computer
      val = cluster(size(cluster))%popoutput()
      !print '("Output ",i0," phasers ",5(i1,1x))', val, phase

    end subroutine test_configuration

  end module day1907_mod
