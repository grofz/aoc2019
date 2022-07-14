!
! Named constants for KIND selection
!
  module kinds_m
    
    integer, parameter :: I8B = selected_int_kind(18)
    integer, parameter :: I4B = selected_int_kind(9)
    integer, parameter :: I2B = selected_int_kind(4)
    integer, parameter :: I1B = selected_int_kind(2)
    !integer, parameter :: SP = kind(1.0)
    !integer, parameter :: DP = kind(1.0d0)
    integer, parameter :: SP = selected_real_kind(6, 37)
    integer, parameter :: DP = selected_real_kind(15, 307)

  end module kinds_m
