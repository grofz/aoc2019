program test_incode
  use rbnode_mod, only : allocation_counter
  implicit none
  interface
    subroutine test_incode_sub()
    end subroutine
  end interface

  call test_incode_sub()
  ! check memory leak
  print *, 'Bye test_incode ', allocation_counter
end program


subroutine test_incode_sub()
  use intcode23_mod
  implicit none

  type(memory_t) :: memory, rom

  call memory%readfile('inp/1909/input.txt')
  print '("Size =",i0)', memory%size()
  print '("Value =",i0)', memory%read(0_ADDRESS_KIND)

  call memory%copy(rom)

  call memory%write(0_ADDRESS_KIND, 10_VALUE_KIND)
  print '("Value =",i0)', memory%read(0_ADDRESS_KIND)

 !call memory%restore(rom)
 !print '("Value =",i0)', memory%read(0_ADDRESS_KIND)

 !call memory%write(1000_ADDRESS_KIND, 422_VALUE_KIND)
 !call memory%print()

 !call memory%write(1_ADDRESS_KIND, 30_VALUE_KIND)
 !call memory%write(0_ADDRESS_KIND, 20_VALUE_KIND)
 !print '("Size =",i0)', memory%size()
 !print '("Value =",i0)', memory%read(0_ADDRESS_KIND), memory%read(1_ADDRESS_KIND)

end subroutine test_incode_sub