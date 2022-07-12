!
! Day 1: The Tyranny of the Rocket Equation
!
  module day1901_mod
    implicit none

  contains

    elemental function fuel_required(mass)
      integer, intent(in) :: mass
      integer :: fuel_required
      fuel_required = mass/3 - 2
    end function

    elemental function fuel_for_all(mass)
      integer, intent(in) :: mass
      integer :: fuel_for_all
      integer :: f0
      fuel_for_all = 0
      f0 = fuel_required(mass)
      do
        if (f0 <= 0) exit
        fuel_for_all = fuel_for_all + f0
        f0 = fuel_required(f0)
      end do
    end function

  end module day1901_mod
