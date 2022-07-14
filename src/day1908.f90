!
! Day 8: Space Image Format
!
  module day1908_mod
    implicit none
    private
    public decode_image

  contains

    subroutine decode_image(line, pic, ans1)
      character(len=*), intent(in) :: line
      character(len=1), allocatable, intent(out) :: pic(:,:)
      integer, intent(out) :: ans1

      integer, allocatable :: cube(:,:,:), img(:,:)
      integer, parameter :: WIDTH=25, HEIGHT=6
      integer :: nlayer, nchar, i, j, k, ich
      integer :: ilayer, nzeros_min, nones, ntwos

      nchar = len_trim(line)
      if (mod(nchar, WIDTH*HEIGHT) /= 0) error stop 'day08 - incomplete image?'
      nlayer = nchar/(WIDTH*HEIGHT)
      print '("Layers ",i0)', nlayer

      ! Transform string to 3D matrix of numbers
      allocate(cube(WIDTH, HEIGHT, nlayer))
      do i=1,WIDTH
      do j=1,HEIGHT
      do k=1,nlayer
        ich = 1 + (i-1) + (j-1)*WIDTH + (k-1)*HEIGHT*WIDTH
        read(line(ich:ich),'(i1)') cube(i,j,k)
      end do
      end do
      end do

      ! Looking for a layer with the smallest number of zeros
      nzeros_min = huge(nzeros_min)
      ilayer = -1
      do k=1,nlayer
        associate(i => count(cube(:,:,k)==0))
          if (i >= nzeros_min) cycle
          ilayer = k
          nzeros_min = i
        end associate
      end do
      print '("Layer with minimum of zeros ", i0, " (",i0")")', ilayer, nzeros_min
      nones = count(cube(:,:,ilayer)==1)
      ntwos = count(cube(:,:,ilayer)==2)
      ans1 = nones*ntwos

      ! Part 2 - render the image
      allocate(img(WIDTH, HEIGHT))
      img = 4
      !print '(4(a5,1x))','LAYER', 'BLACK', 'WHITE', 'UNDEF'
      do k=1, nlayer
        where(img==4 .and. cube(:,:,k)==1)
          img = 1
        else where(img==4 .and. cube(:,:,k)==0)
          img = 0
        end where
        !print '(4(i5,1x))', k, count(img==0), count(img==1), count(img==4)
      end do
      pic = int2ch(img)
      print '(25(a1))', pic

    contains
      elemental function int2ch(i) result(ch)
        character(len=1) :: ch
        integer, intent(in) :: i
        ch = '?' 
        if (i==0) then
          ch = ' '
        else if (i==1) then
          ch = '1'
        end if
      end function

    end subroutine decode_image

  end module day1908_mod
