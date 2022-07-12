  program check
    use test_parse_mod
    implicit none
    logical, allocatable :: ispass(:)

    allocate(ispass(0))

10  ispass = [ispass, .false.]
    call test_parse(ispass(size(ispass)))

999 continue
    if (all(ispass)) then
      print '("All tests passed")'
    else
      print '("Some tests failed")'
      print '(*(l3))', ispass
    end if
  end program check
