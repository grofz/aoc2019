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
  use common_mod, only : mold
  implicit none

! type(memory_t) :: memory, rom
  type(machine_t) :: machine
  integer :: istat, input

! call memory%readfile('inp/1909/input.txt')

  do input=1,2
    call machine%load('inp/1909/input.txt')
    call machine%pushinput(int(input,VALUE_KIND))
    do
      istat = machine%step()
      if (istat==STAT_HALT) exit
      if (istat==STAT_OUTPUT_READY) print *, machine%popoutput()
    end do
  end do

! call machine%inp%enqueue(transfer(2_VALUE_KIND,mold))
  !call machine%inp%enqueue(transfer(1_VALUE_KIND,mold))

! print '("Size =",i0)', memory%size()
! print '("Value =",i0)', memory%read(0_ADDRESS_KIND)

! call memory%copy(rom)

! call memory%write(0_ADDRESS_KIND, 10_VALUE_KIND)
! print '("Value =",i0)', memory%read(0_ADDRESS_KIND)

 !call memory%restore(rom)
 !print '("Value =",i0)', memory%read(0_ADDRESS_KIND)

 !call memory%write(1000_ADDRESS_KIND, 422_VALUE_KIND)
 !call memory%print()

 !call memory%write(1_ADDRESS_KIND, 30_VALUE_KIND)
 !call memory%write(0_ADDRESS_KIND, 20_VALUE_KIND)
 !print '("Size =",i0)', memory%size()
 !print '("Value =",i0)', memory%read(0_ADDRESS_KIND), memory%read(1_ADDRESS_KIND)

end subroutine test_incode_sub