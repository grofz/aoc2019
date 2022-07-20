  module day1914_mod
    use list_mod, only : list_t, CHAR_LEN
    use parse_mod, only : read_strings, string_t, split
    use kinds_m, only : I8B
    implicit none


  contains

    subroutine part2_sol(complist, smat, nore, ans)
      type(list_t), intent(in) :: complist
      integer, intent(in) :: smat(:,:)
      integer(I8B), intent(in) :: nore  ! ore available
      integer(I8B), intent(out) :: ans  ! fuel produced
!
! Given amount of ore, calculate amount of fuel that can be produced "ans"
! Halving of interval method.
!
      integer(I8B) :: f0, f1, fm, w0, w1, wm

      ! Increase amount of fuel until all ore is consumed.
      f1 = 1
      do
        w0 = w1
        call part1_sol(complist, smat, f1, w1)
        w1 = w1 + nore
        print '("Fuel produced ",i0,". Ore remaining ",i0,".")', f1, w1
        if (w1 > 0) then
          f0 = f1
          f1 = 2 * f1
        else
          exit
        end if
      end do

      ! Divide interval until the smallest excess of ore remains
      if (sign(1_I8B,w0)*sign(1_I8B,w1) > 0) error stop 'solution diverging'
      do
        if ( f1-f0 <=1 ) exit ! solution found
        fm = f0/2 + f1/2
        call part1_sol(complist, smat, fm, wm)
        wm =  wm + nore
        print '("Fuel produced ",i0,". Ore remaining ",i0,".")', fm, wm
        if (wm < 0) then ! ore shortage
          f1 = fm
          w1 = wm
        else
          f0 = fm
          w0 = wm
        end if
      end do
      ans = f0
      print '("Solution found: Fuel produced ",i0,". Ore remaining ",i0,".")', f0, w0
    end subroutine



    subroutine part1_sol(complist, smat, nfuel, ans)
      integer, intent(in) :: smat(:,:)   ! stoichmetry matrix
      type(list_t), intent(in) ::  complist ! list of component names
      integer(I8B), intent(in) ::  nfuel ! fuel required
      integer(I8B), intent(out) :: ans   ! ore neede
!
! For given amount of the fuel, calculate ore consumption ("ans")
!
      integer(I8B), allocatable :: delta_n(:), extent(:) 
      integer, allocatable      :: list(:)
      integer :: iore, ifuel, jfuel, jr, nr, nc, ic, cfuel
      logical :: was_changed

      iore = complist % Find('ORE')
      nr = size(smat,2)
      nc = size(smat,1)

      ! Find by what reaction is fuel produced
      ifuel = complist % Find('FUEL')
      jfuel = find_reaction(smat, ifuel)
      cfuel = smat(ifuel, jfuel)

      allocate(extent(nr))
      extent = 0
      ! Set extent to produce required amount of fuel
      if (cfuel /= 1) error stop 'fuel coefficient must be one'
      extent(jfuel) = cfuel * nfuel
      delta_n = calc_accum(smat, extent)

      MAIN: do
        ! Find which components are missing.
        ! Modify extent until delta_n is nonnegative
        was_changed = .false.

        do ic=1,nc
          ! delta_n(iore) can be negative
          ! delta_n(ifuel) has been set to required amount
          ! ore and fuel components can be ignored
          if (ic == iore .or. ic == ifuel) cycle

          ! Component is in surplus
          if (delta_n(ic) >= 0) cycle

          ! Shortage of component "ic". Increase extent to produce more of it.
          jr = find_reaction(smat, ic)
          associate(d=>delta_n(ic), s=>smat(ic,jr))
            extent(jr) = extent(jr) + (abs(d)/s)
            ! extent truncated down in the divison, round it up if necessary
            if (mod(abs(d),s)/=0) extent(jr) = extent(jr)+1
          end associate
          delta_n = calc_accum(smat, extent)
          was_changed = .true.
        end do
        if (.not. was_changed) exit MAIN
      end do MAIN
      ans = delta_n(iore)

    end subroutine part1_sol



    pure function find_reaction(smat, icomp) result(jreac)
      integer, intent(in) :: smat(:,:), icomp
      integer :: jreac
!
! Find what reaction produces component with position "icomp" in stochmatrix
!
      integer :: nc, nr, jr
      integer, allocatable :: list(:)
      nc = size(smat,1)
      nr = size(smat,2)

      ! Component should be produced in just one reaction.
      ! Line with component should contain just one positive coefficient.
      ! Return column index of this coefficient
      jreac = 0
      list = pack(smat(icomp,:), smat(icomp,:)>0)
      if (size(list)/=1) &
          error stop 'find_reaction - component produced not by just one reaction'
      do jr=1, nr
        if (smat(icomp, jr)>0) then
          jreac = jr
          exit
        end if
      end do
    end function



    pure function calc_accum(stochmat, extent) result(delta_n)
      integer, intent(in) :: stochmat(:,:) 
      integer(I8B), intent(in) :: extent(:)
      integer(I8B), allocatable :: delta_n(:)
!
!                       ==========================
!                       dn_{i} = S_{i,j} * E_{j}'  
!                       ==========================
!   where S_{i,j} is a stoichometric matrix, E_{j} is vector of reaction extents
!   and dn_{i} vector indicates if there is a surplus (>0) or shortfal (<0) 
!   of component  (index i is a component, index j is a reaction)
!   
      integer :: nc, nr

      nc = size(stochmat,1)
      nr = size(stochmat,2)
      if (size(extent,1) /= nr) error stop 'calc_accum - extent array size invalid'
      allocate(delta_n(nc))
      delta_n = matmul(int(stochmat,kind=I8B), extent)
    end function 



    subroutine stoch_from_file(file, complist, stochmat)
      character(len=*), intent(in) :: file
      type(list_t), intent(out) :: complist
      integer, intent(out), allocatable :: stochmat(:,:)

      type(string_t), allocatable :: lines(:), lefrig(:), side(:), item(:)
      integer :: nreac, ncomp, ir, jc, k, m, ios
      character(len=CHAR_LEN) :: word

      ! Every line is one reaction. Assuming "ncomp = nreac + 1"
      lines = read_strings(file)
      nreac = size(lines)
      ncomp = nreac + 1

      ! Stoichometry matrix: Rows => reactions, Cols => components.
      allocate(stochmat(ncomp, nreac))
      stochmat = 0
      complist = list_t()
      LINE: do ir = 1, nreac
        ! Separate left and right sides of the equation
        call split(lines(ir)%str, ' => ', lefrig)
        if (size(lefrig)/=2) error stop 'stochiometry_from_file - error 1'

        do k=1, 2
          ! Split indivdual components
          call split(lefrig(k)%str, ',', side)
          do m=1,size(side)
            ! Split stoichometry coefficient and component name
            call split(trim(adjustl(side(m)%str)), ' ', item)
            if (size(item)/=2) error stop 'stoichometry_from_file - error 2'

            ! Add component to the list (duplicates are ignored)
            word = adjustl(item(2)%str)
            call complist % Add(word)
            jc = complist % Find(word)
            if (jc<1 .or. jc>ncomp) error stop 'stoichometry_from_file - error 3'

            ! Add coefficient to the matrix
            read(item(1)%str, *, iostat=ios) stochmat(jc,ir)
            if (ios/=0) error stop 'conversion error'

            ! Reactants negative stoichometric coefficients
            if (k==1) stochmat(jc,ir) = -stochmat(jc,ir)
          end do
        end do
      end do LINE

      print '("No of reactions ",i0)', nreac
      print '("No of components ",i0)', ncomp

    end subroutine stoch_from_file

  end module day1914_mod
