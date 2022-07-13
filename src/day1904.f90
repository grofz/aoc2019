  module day1904_mod
    implicit none
    private
    public count_valid

    integer, parameter, public :: PASSLEN=6
  contains

    subroutine count_valid(range, mode, cnt)
      character(len=PASSLEN), intent(in) :: range(2)
      integer, intent(in) :: mode
      integer, intent(out) :: cnt
      character(len=PASSLEN) :: pwd
      pwd=''
      select case(mode)
      case(1)
        cnt = generate_pwds(pwd, range, .false.)
      case(2)
        cnt = generate_pwds(pwd, range, .true.)
      end select
    end subroutine count_valid



    recursive function generate_pwds(pwd, range, isp2) result(cnt)
      character(len=PASSLEN), intent(in) :: pwd, range(2)
      logical, intent(in) :: isp2
      integer :: cnt
      integer :: n, i, begi
      character(len=PASSLEN) :: pwd1

      n = len_trim(pwd)

      ! Easy answers: invalid password, or valid complete password 
      cnt = 0
      if (.not. isvalid(pwd, range)) return
      if (n == PASSLEN) then
        if (isvalid_strict(pwd, isp2)) then
          cnt = 1
!print *, 'Valid password: ',pwd
        else
          cnt = 0
!print *, 'Valid in Part 1, but not in Part 2: ',pwd
        end if
        return
      end if

      ! Recursive answer: add one digit at the end and sum all obtained answers
      !
      !   First digit- the smallest digit from the allowed range.
      !   Second and next digits- rule that digits do not decrease (same or larger as the last digit)
      if (n==0) then
        begi = iachar(range(1)(1:1))-iachar('0')
      else
        begi = iachar(pwd(n:n))-iachar('0')
      end if

      do i = begi, 9
        pwd1 = pwd
        pwd1(n+1:n+1) = achar(i+iachar('0'))
        cnt = cnt + generate_pwds(pwd1, range, isp2)
      end do
    end function generate_pwds



    logical function isvalid(pwd, range)
      character(len=PASSLEN), intent(in) :: pwd, range(2)

      character(len=PASSLEN) :: range0(2)
      integer :: n, i

      n = len_trim(pwd) ! number of digits in the password thus far
      range0 = ''
      range0(1) = range(1)(1:n)
      range0(2) = range(2)(1:n)
      isvalid = .false.

      ! The rules for a valid password
      ! ------------------------------
      ! 1 The value is within the range given by "range(1)-range(2)"
      ! 2 Two adjacent digits are the same (like 22 in 122345).
      ! 3 Going from left to right, the digits never decrease; they only ever increase or
      !   stay the same (like 111123 or 135679).

      if (pwd < range0(1) .or. pwd > range0(2)) return ! rule (1) not met

      ! rule (2) can be enforced only for complete passwords
      if (n == PASSLEN) then
        do i=1,n-1
          if (pwd(i:i)==pwd(i+1:i+1)) then
            isvalid = .true.
            exit
          end if
        end do
      else
        isvalid = .true.
      end if

      ! rule (3) should not be violated (considering the method "pwd" is generated)
      ! therefore this is just "defensive check" that could be removed
      do i=2,n
        if (pwd(i:i)<pwd(i-1:i-1)) then
          !print *, 'Warning - password violatipn of rule (3)'
          error stop 'Warning - password violatipn of rule (3)'
          isvalid = .false.
          exit
        end if
      end do
    end function isvalid



    logical function isvalid_strict(pwd, is_part_two) result(isvalid)
      character(len=PASSLEN), intent(in) :: pwd
      logical, intent(in) :: is_part_two 
      integer :: i, j, freq(0:9)

      if (.not. is_part_two) then
        isvalid = .true.
        return
      end if

      ! Additional rule for Part 2
      ! ==========================
      ! The two adjacent matching digits are not part of a larger group of matching digits.
      !
      ! Examples:
      !  112233 meets these criteria because the digits never decrease and all repeated
      !    digits are exactly two digits long.
      !  123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
      !  111122 meets the criteria (even though 1 is repeated more than twice, it still 
      !  contains a double 22).

      ! Count frequency of every digit in the password
      freq = 0
      do j = 0, 9
        do i = 1, PASSLEN
          if (iachar(pwd(i:i))-iachar('0') == j) freq(j) = freq(j)+1
        end do
      end do

      ! There must be at least one two-digits group
      if (count(freq==2) >= 1) then
        isvalid = .true.
      else
        isvalid = .false.
      end if
    end function isvalid_strict

  end module day1904_mod
