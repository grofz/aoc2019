!
! Day 3: Crossed Wires
!
  module day1903_mod
    use parse_mod, only : read_strings, parse_array
    implicit none
    private
    public find_intersections, read_from_file, line_t

    type line_t
      private
      integer :: x0(2), x1(2), dx(2)
      integer :: totdis_to_x0
    contains
      procedure :: line_line_intersection
    end type line_t

  contains

    subroutine line_line_intersection(s1,s2,xint,is_intersecting,totdis)
      class(line_t), intent(in) :: s1, s2
      integer, intent(out) :: xint(2), totdis
      logical :: is_intersecting
      integer, dimension(2) :: p10, p11, p20, p21, p1, p2, p3, p4

      is_intersecting = .false.

      ! lines can be either parallel, or perpendicular.
      ! Assuming that input is sane and lines are not ovelaping
      if (dot_product(s1%dx, s2%dx) /= 0) then
        ! lines are parallel
        return
      end if

      ! Segments pX0->pX1 go from lower to higher value
      p10(1) = min(s1%x0(1), s1%x1(1))
      p10(2) = min(s1%x0(2), s1%x1(2))
      p20(1) = min(s2%x0(1), s2%x1(1))
      p20(2) = min(s2%x0(2), s2%x1(2))
      p11(1) = max(s1%x0(1), s1%x1(1))
      p11(2) = max(s1%x0(2), s1%x1(2))
      p21(1) = max(s2%x0(1), s2%x1(1))
      p21(2) = max(s2%x0(2), s2%x1(2))

      if (s1%dx(1)==0) then
        ! segment 1 is vertical (...assuming segment 2 horizontal)
        p3 = p10
        p4 = p11
        p1 = p20
        p2 = p21
      else if(s1%dx(2)==0) then
        ! segment 1 is horizontal (...assuming segment 2 vertical)
        p3 = p20
        p4 = p21
        p1 = p10
        p2 = p11
      else
        error stop 'line_line_intersection - something wrong'
      end if
      !
      ! This is a possible intersection
      !
      !              o P4
      !              |
      !     P1 o-----+-------o P2
      !              |
      !              o P3
      !
      is_intersecting = p1(1) <= p3(1) .and. p3(1) <= p2(1) &
                  .and. p3(2) <= p1(2) .and. p1(2) <= p4(2)
      xint = [p3(1), p1(2)]

      totdis = s1%totdis_to_x0 + sum(abs(s1%x0-xint)) &
             + s2%totdis_to_x0 + sum(abs(s2%x0-xint))
    end subroutine line_line_intersection



    subroutine find_intersections(wire_a, wire_b, xsec, ysec, ans1, ans2)
      class(line_t), intent(in) :: wire_a(:), wire_b(:)
      integer, allocatable, intent(out) :: xsec(:), ysec(:)
      integer, intent(out) :: ans1, ans2
      integer :: xy_best(2), xy_fastest(2)
      integer, parameter :: XY_ORIGIN(2) = [0, 0]
      integer :: ia, ib, xy(2), totdis, dist, dist_best, dist_fastests
      logical :: is_intersecting

      allocate(xsec(0), ysec(0))
      xy_best = [huge(ia), huge(ia)]
      dist_best = huge(ia)
      dist_fastests = huge(ia)
      do ia = 1,size(wire_a)
        do ib = 1,size(wire_b)
          call line_line_intersection(wire_a(ia),wire_b(ib),xy,is_intersecting,totdis)
          if (.not. is_intersecting) cycle
          xsec = [xsec, xy(1)]
          ysec = [ysec, xy(2)]
          dist = abs(xy(1)-XY_ORIGIN(1)) + abs(xy(2)-XY_ORIGIN(2))
          if (dist /= 0 .and. dist < dist_best) then
            xy_best = xy
            dist_best = dist
          end if
          if (totdis /= 0 .and. totdis < dist_fastests) then
            xy_fastest = xy
            dist_fastests = totdis
          end if
        end do
      end do
      print '("Intersections found ",i0,".  Shortest distance (3/1) ",i0)', size(xsec), dist_best
      print '("  Shortest time (3/2) ",i0)', dist_fastests
      print '("Best intersection (distance-wise) ",i0,1x,i0)', xy_best
      print '("Best intersection (time-wise)     ",i0,1x,i0)', xy_fastest
      ans1 = dist_best
      ans2 = dist_fastests
    end subroutine find_intersections



    subroutine read_from_file(file, wire_a, wire_b)
      character(len=*), intent(in) :: file
      type(line_t), intent(out), allocatable :: wire_a(:), wire_b(:)

      character(len=:), allocatable :: lines(:), intervals(:)

      lines = read_strings(file)
      if (size(lines)/=2) error stop 'day 3 - two lines of input expected'
      intervals = parse_array(lines(1),',',10)
      call get_line_intervals(intervals, wire_a)
      intervals = parse_array(lines(2),',',10)
      call get_line_intervals(intervals, wire_b)

print '("Wire A intervals ",i0," and wire B intervals ", i0)', size(wire_a), size(wire_b)
    end subroutine read_from_file



    subroutine get_line_intervals(inst, wire)
      character(len=*), intent(in) :: inst(:)
      type(line_t), allocatable, intent(out) :: wire(:)

      integer :: i, n, pos1(2), pos0(2), dlen, dx(2), tot_dis
      character(len=1) :: dir

      n = size(inst)
      allocate(wire(n))
      pos0 = 0    ! all wires start from [0,0]
      tot_dis = 0 ! total distance traveled from start of wire
      do i=1, n
        dir = inst(i)(1:1)
        read(inst(i)(2:),*) dlen
        select case(dir)
        case('U')
          dx = [0, 1]
        case('D')
          dx = [0, -1]
        case('L')
          dx = [-1, 0]
        case('R')
          dx = [1, 0]
        case default
          error stop 'get_line_intervals - unknown direction'
        end select
        pos1 = pos0 + dx*dlen
        wire(i)%x0 = pos0
        wire(i)%x1 = pos1
        wire(i)%dx = pos1 - pos0
        wire(i)%totdis_to_x0 = tot_dis
        pos0 = pos1
        tot_dis = tot_dis + dlen
      end do
    end subroutine get_line_intervals

  end module day1903_mod
