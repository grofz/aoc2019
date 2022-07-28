  module day1923_mod
    use intcode_mod
    use kinds_m, only : I8B
    implicit none

    integer, parameter :: NCOMP = 50, NULL_MESS=-1

    type connection_t
      integer :: src=-1, dst=-1      ! source, destination [1-50]
      integer :: ptd=0               ! packets to be delivered [2, 1, 0]
      integer(I8B) :: val            ! tmp value
      integer(I8B) :: buf            ! first part of the packet stored
      logical :: requesting=.false.  ! T if computer has empty queue and is not sending
    end type

    type network_t
      type(connection_t) :: conex(NCOMP)
      type(computer_t)   :: ZX(NCOMP)
      integer(I8B)       :: nat(2)=-1,  natprev(2)=-1
    contains
      procedure :: Init => network_new
      procedure :: Run => network_run
    end type

  contains

    subroutine network_new(this, file)
      class(network_t), intent(inout) :: this
      character(len=*), intent(in)  :: file

      integer :: i, istat

      do i=1, NCOMP
        call this%ZX(i) % Load_from_file(file)
        call this%ZX(i) % Reset(20,1)
        call this%ZX(i) % Run(istat)
        if (istat /= SINBUF_EMPTY) error stop 'network_new - unexpected stat'
        call this%ZX(i) % Set_inbuf(i-1) ! provide network address
      end do
      print '(a)', 'Computers booted-up'
    end subroutine network_new


    subroutine network_run(this, ans1, ans2)
      class(network_t), intent(inout) :: this
      integer, intent(out)            :: ans1, ans2

      integer :: i, stat(NCOMP)
      integer(I8B) :: buf

      ans1 = -1

      MLOOP: do

        ! All computers do one step
        do i=1, NCOMP
          call this%ZX(i) % Step(stat(i))

          select case(stat(i))
          case(SHALT)
            error stop 'computer halts'
          case(SRUNNING)
            continue
          case(SINBUF_EMPTY)
            ! Needs input. Provide with "-1" and re-run
            call this%ZX(i) % Set_inbuf(NULL_MESS)
            call this%ZX(i) % Step(stat(i))
            this%conex(i)%requesting = .true.

          case(SOUTBUF_READY)
            ! Is sending. Remove output from buffer to be used later.
            call this%ZX(i) % Read_outbuf(buf)
            this%conex(i)%requesting = .false.
            this%conex(i) % src = i
            this%conex(i) % val = buf

          case default
            print *, stat(i)
            error stop 'unexpected stat'
          end select
        end do

        ! Process network traffic
        do i=1, NCOMP
          if (this%conex(i)% src == -1) cycle
          select case(this%conex(i)% ptd)
          case(0)
            ! connection not yet established - establish connection
            ! (our addresses are "1-50", intcodes are "0-49")
            this%conex(i) % dst = this%conex(i) % val + 1
            this%conex(i) % ptd = 2
            !print '("New connection from ",i0," to ",i0)',&
            !  this%conex(i)%src, this%conex(i)%dst

          case(1)
            ! waiting for last part - deliver packet and close connection
            if (this%conex(i) % dst == 255+1) then
              ! packet for NAT
              this%nat(1) = this%conex(i)%buf
              this%nat(2) = this%conex(i)%val
              print '("NAT packet ",i0,1x,i0)',this%nat
              if (ans1==-1) ans1 = this%nat(2)
            else 
              ! packet between computers
              call this%ZX( this%conex(i)%dst ) % Set_inbuf(this%conex(i)%buf)
              call this%ZX( this%conex(i)%dst ) % Set_inbuf(this%conex(i)%val)
              this%conex( this%conex(i)%dst ) % requesting = .false.
            end if
            this%conex(i) % ptd = 0
            print '("Src=",i0," dst=",i0," Packet = ",i0,1x,i0)', &
              this%conex(i)%src, this%conex(i)%dst, this%conex(i)%buf, this%conex(i)%val
          case(2)
            ! waiting for two parts - store first part
            this%conex(i) % buf = this%conex(i) % val
            this%conex(i) % ptd = 1
            !print '("First part from ",i0," to ",i0," = ",i0)', &
            !  this%conex(i)%src, this%conex(i)%dst, this%conex(i)%val

          case default
            error stop 'invalid ptd value'
          end select
          this%conex(i) % src = -1
        end do

        ! Detect iddle network
        if (count(this%conex%requesting)==NCOMP .and. &
        &   this%nat(1)/=-1 .and. this%nat(2)/=-1) then
          print '("Network idle. NAT restarting ",i0,1x,i0)', this%nat
          this%conex%requesting=.false.
          call this%ZX( 1 ) % Set_inbuf(this%nat(1))
          call this%ZX( 1 ) % Set_inbuf(this%nat(2))

          if (this%natprev(2)==this%nat(2)) then
            print '("Same NAT value for restarting as before. Work done.")'
            ans2 = this%nat(2)
            return
          end if
          this%natprev = this%nat
        end if

      end do MLOOP

    end subroutine

  end module day1923_mod
