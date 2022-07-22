! TODO
! Part 2
!
! 1. Prepare instructions without character limit 
!
! 2. Look for repeating patterns.
!    Identify tokens L, R, 1, 2, 3, ..., 10, 11
!    There will be approximately 80-100 instructions necessary
! Limit 20 characters per line - 10 tokens max (one can be two digits token)
!                                 9 tokens max (three can be two digits)
!                                 8 tokens max (five can be two digits)
!                                 7 tokens max (all can be two digits)
!
! Limit 3 functions called maximum 10 times. Each function should have therefore
! about at minimum 8 instructions.
! We have to find 3 sequences that:
! - are 8-10 instructions long
! - completelly fills the original string
!    Repeat for K-size token sequences from 10 to 8 
!    - find all unique K-size patterns and count them
!    - - traversing the sequence: and (1) add new or (2) increase count 
!    - remove sequences breaking the line length limit
!    - if frequency is more than (3?), mark them and make sure other sequences do not
!      overlap
!
! Start with the beginning and with the end


  module day1917_mod
    use day1911_mod, only : board_t
    use intcode_mod, only : computer_t, SOUTBUF_READY, SINBUF_EMPTY, SHALT
    use parse_mod, only : string_t, split
    implicit none

    type, public, extends(board_t) :: scaffold_t
      integer :: cursor(2)=0
    end type

    integer, parameter :: LF_CHAR=10

  contains

    subroutine camera_view(this, computer)
      class(scaffold_t), intent(inout) :: this
      class(computer_t), intent(inout) :: computer
!
! Transcript ASCII output to the board. Ends when computer stops or when
! two conescutive LF(Ascii=10) are written
!
      integer :: istat, val, nchar, limits(4)
      logical :: last_LF

      this % cursor = [0, 1]
      istat = 0
      nchar = 0
      last_LF = .false.
      do
        call computer % Run(istat)
        if (istat==SOUTBUF_READY) then
          if (computer % Isempty_outbuf()) error stop 'camera_view-buffer empty'
          call computer % read_outbuf(val)
          if (val == LF_CHAR) then ! end of line (LF)
            if (last_LF) then
              istat = 42           ! LF/LF sequence in the output
              exit
            end if
            this % cursor(2) = this % cursor(2) + 1
            this % cursor(1) = 0
            last_LF = .true.
          else if (val >= 32 .and. val <=127) then
            this % cursor(1) = this%cursor(1) + 1
            call this % PaintColor(this % cursor, val)
            nchar = nchar + 1
            last_LF = .false.
          else
            error stop 'camera_view - output value not ASCII'
          end if
        else
          exit
        end if 
      end do
      limits = this % GetLimits()
      print '("Chars written ",i0,"  Cursor at ",i0,",",i0,"  Display size ",i0,",",i0)', &
          nchar, this%cursor, limits(2)-limits(1)+1, limits(4)-limits(3)+1
      print '("Camera_view exit, stat = ",i0)',istat
    end subroutine camera_view



    subroutine get_crossings(this, cal, cpos)
      class(board_t), intent(in) :: this
      integer, intent(out) :: cal
      integer, allocatable, intent(out) :: cpos(:,:)
!
! Identify scaffold crossings and get answer for part 1
!
      integer, allocatable :: map(:,:), tmp(:,:)
      integer :: nx, ny, i, j, ncross

      integer, parameter :: AH=35, MAX_CROSSINGS=100

      map = this % Export(ismirror=[.false.,.false.])
      nx = size(map,1)
      ny = size(map,2)


      ! assume no crossings at the boundary
      ncross = 0
      allocate(tmp(2,MAX_CROSSINGS))
      cal = 0               ! alignment parameter (answer to Part 1)
      do j=2,ny-1
      do i=2,nx-1
        if (map(i,j) /= AH) cycle
        if (map(i-1,j)/=AH .or. map(i+1,j)/=AH .or. &
            map(i,j-1)/=AH .or. map(i,j+1)/=AH) cycle
        ncross = ncross + 1
        tmp(1,ncross) = i
        tmp(2,ncross) = j
        cal = cal + (i-1)*(j-1)
      enddo
      enddo
      allocate(cpos(2,ncross))
      cpos = tmp(:,1:ncross)

      print '("Number of crossings ",i0,"  Alignment parameter ",i0)', ncross, cal
      print '("Validation ? ",l2)', cal==5740
    end subroutine get_crossings



    function get_fullpath(this) result(fpath)
      class(board_t), intent(in) :: this
      character(len=:), allocatable :: fpath

      integer, parameter :: AH=iachar('#'), MAX_LENGTH=1000
      integer, allocatable :: map0(:,:), map(:,:)
      integer :: i, j, nx, ny, nstretch, pos(2), pos1(2), pos2(2), pos3(2)
      integer :: dir ! 1-north, 2-east, 3-south, 4-west
      integer :: dirleft, dirright
      integer, parameter :: dirvec(2,4)=reshape([0,-1, 1,0, 0,1, -1,0], [2,4]) 

      ! "map" has border to avoid "out of array boundary checks later'
      map0 = this % Export(ismirror=[.false.,.false.])
      nx = size(map0,1)
      ny = size(map0,2)
      fpath = ''
      allocate(map(0:nx+1,0:ny+1))
      map = 0
      map(1:nx,1:ny) = map0

      ! find robot position
      pos = 99
      do i=1,nx
      do j=1,ny
        if (map(i,j) == iachar('^')) then
          pos(1) = i; pos(2) = j; dir = 1
          exit
        end if
      end do
      end do
      if (any(pos==99)) error stop 'robot position not found'
        
      nstretch = 0
      MAIN: do
        ! Single stretch (a-find orientation / b-forward / c-identify end)
        dirleft = 4-mod(4-dir+1,4)
        dirright= mod(dir,4)+1
        pos1 = pos + dirvec(:, dir)
        pos2 = pos + dirvec(:, dirleft)
        pos3 = pos + dirvec(:, dirright)
        if (map(pos1(1),pos1(2)) == AH) then ! forward
          nstretch = nstretch + 1
          pos = pos1
        else if (map(pos2(1),pos2(2)) == AH) then ! turning left
          call write_forward_instruction()
          fpath = fpath//'L,'
          dir = dirleft
        else if (map(pos3(1),pos3(2)) == AH) then ! turning right
          call write_forward_instruction()
          fpath = fpath//'R,'
          dir = dirright
        else ! end of road
          call write_forward_instruction()
          exit MAIN
        end if
      end do MAIN

      ! remove last trailing ','
      fpath=fpath(:len(fpath)-1)

    contains
      subroutine write_forward_instruction()
        character(len=2) :: buffer
        if (nstretch > 20) error stop 'stretch is too far'
        if (nstretch == 0) return ! do nothing for the first leg
        write(buffer,'(i2)') nstretch
        fpath = fpath//trim(adjustl(buffer))//','
        nstretch = 0
      end subroutine

    end function get_fullpath



    subroutine run_part2(file, cpath)
      character(len=*), intent(in) :: file, cpath
      integer :: ans
!
! When instruction feed is prepared in file "cpath", run vacuum robot
!
      type(computer_t) :: ZX
      integer :: status, val, fid, i
      character(len=200) :: line

      open(newunit=fid, file=cpath, status='old')
      call ZX % Load_from_file(file)
      call ZX % Reset(100,1)

      ! Wake up robot
      call ZX % Overwrite(0,2)

      do
        call ZX % Run(status)
        select case (status)
        case(SOUTBUF_READY)
          ! Print output directly to terminal
          if (ZX % Isempty_outbuf()) error stop 'run_part2-buffer empty'
          call ZX % read_outbuf(val)
          if (val==LF_CHAR) then ! line-feed
            write(*,*)
          elseif (val>=32 .and. val<127) then
            write(*,'(a)',advance='no') achar(val)
          elseif (val > 10000) then ! we got the answer
            write(*,'(a,i0)') 'Computer says: ', val
            ans = val
          else
            print *, val
            error stop 'unexpected value in output'
          endif

        case(SINBUF_EMPTY)
          ! If asked, feed input by instructions from the file (line by line)
          read(fid,'(A)') line
          write(*,*) '>> '//trim(line)
          do i=1,len_trim(line)
            call ZX % Set_inbuf(iachar(line(i:i)))
          end do
          call ZX % Set_inbuf(LF_CHAR)

        case(SHALT)
          print ('(a)'), 'Program halts'
          exit

        case default
          print *, 'status unexpected ',status
          error stop 'unexpected status'
        end select
      end do
      close(fid)
      print'("Validation (17/2) part two ?",l2)', ans==1022165
    end subroutine run_part2



    subroutine compress_path(fpath, fileout)
      character(len=*), intent(in) :: fpath, fileout

      type(string_t), allocatable :: tokens(:), tmp
      character(len=:), allocatable :: patA, patB, patC, wpath
      integer, allocatable :: ipatA(:)
      integer :: i0, i1, k, a0, a1, j
      integer :: nb, iprev
      logical :: is_ok

      wpath = fpath
      call split(fpath,',',tokens)
      print '("Number of tokens ",i0)', size(tokens)

      ! Find token sequence from the start. Maximum is 10 tokens
      a0 = 1
      allocate(ipatA(0))
      do k=10,6,-1 
        a1 = a0+k-1 ! end of tested sequence
        do i0=a1+1, size(tokens)-k+1 
          is_ok = .true.
          do j=0,k-1
            if (tokens(a0+j)%str==tokens(i0+j)%str) cycle
            is_ok = .false.
            exit
          enddo
          if (.not. is_ok) cycle
          ! match found
          ipatA = [ipatA, i0]
          print '("Match found: ",i0," tokens from ",i0," match start sequence")', &
            k, i0
        enddo
        if (size(ipatA)>=3) exit
        print *, 'Size k=',k,'not succesful. trying next'
        deallocate(ipatA)
        allocate(ipatA(0))
      end do

   !  iprev = 0
   !  allocate(tmp(0))
   !  do j=0, size(ipatA)
   !    if (j==0) then
   !      i0 = 1
   !    else
   !      i0 = ipatA(j)
   !    endif



   !  end do

    end subroutine


  end module day1917_mod
