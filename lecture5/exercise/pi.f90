!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c       pi.f90: Calculate pi using Monte Carlo
!C       ("acceptance/rejection" in a circle)
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

Program pi

  implicit none

  integer, dimension(12) :: seed
  real, dimension(2) :: rnd

  real :: area, x, y
  integer :: i, max, pigr, sizer
  integer :: rep, every



  max = 1000
  every = 10

  ! open file to save data, initialize the counter of random number points in the circle
  open(7, File='pigr.dat', Status='Replace')



  ! random numbers generated between -1 and 1
  ! i.e., within a square of side 2 and centered in (0,0):
  ! points falling inside the circle (with x*x+y*y < or = 1) are counted for pigr, 
  ! which is 4*area(circle)/area(square)
  ! i.e. 4*number of points within the circle / total number of points withing the square

  do rep=1, 70

     do i=1,12
        seed(i) = rep*3
     end do
     
     call random_seed( put=seed )

     pigr = 0
     
     do i=1, max

        call random_number(rnd)

        x = rnd(1)
        y = rnd(2)

        if ((x*x + y*y) <= 1) then
           pigr = pigr+1
        endif

        area = 4.0 * real(pigr) / real(i)
        print*, area
        if ( mod(i,every) == 0 ) write(7,*) i, abs(acos(-1.)-area)  !write every 10 points

     end do

  end do
  
  close(7)
  
  stop 'data saved in pigr.dat '
  
End program pi
