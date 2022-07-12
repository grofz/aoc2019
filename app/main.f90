  program main
    implicit none

    call day01('inp/1901/input.txt')

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
    print '("Total fuel (01/1) ",i0,l2)', tot_fuel, tot_fuel==3239503

    ! Part Two
    print '("Test cases:")'
    print *, fuel_for_all(14), fuel_for_all(14)==2
    print *, fuel_for_all(1969), fuel_for_all(1969)==966
    print *, fuel_for_all(100756), fuel_for_all(100756)==50346
    fuel = fuel_for_all(mass)
    !print '("Fuel required including fuel mass requirements")'
    !print '(12(i6,1x))', fuel
    tot_fuel = sum(fuel)
    print '("Total fuel (01/2) ",i0,l2)', tot_fuel, tot_fuel==4856390
    print *
  end subroutine day01
