module statist

  public :: momentum, correlation
  
  contains


function momentum(xx, len, kk) result(mom)
  real, intent(in) :: xx(:)
  integer, intent(in) :: len, kk
  real :: mom
  integer :: i

  mom = 0

  do i=1, len
     mom = mom + xx(i)**kk
  end do

  mom = mom / real(len)

end function momentum



function correlation(xx, len) result(c)
  real, intent(in) :: xx(:)
  integer, intent(in) :: len
  integer :: i
  real :: c

  c = 0

  do i=1, len-1
     c = c + xx( i ) * xx( i+1 )
  end do

  c = c / real(len)

end function correlation


end module statist



program unif_corr

  use statist
  implicit none
  
  real, dimension(:), allocatable :: x
  ! integer, dimension(:), allocatable :: moms
  real :: corr, r_corr
  !real, dimension(:), allocatable :: momenta, real_momenta
  integer moment
  real computed_moment, r_moment
  integer :: i, iter
  
  print*, "insert moment"
  read(*,*) moment

  r_moment = 1./ real(1 + moment)

  print*, "moment to be computed", moment
  print*, "real_moment", r_moment

  r_corr = 1./4.

  print*, "real_corr", r_corr

  OPEN(unit=1, file="corr_mom.dat", status="replace", action='write')

  do iter=10, 1000, 10
     
     allocate(x(iter))     
     call random_number(x)

     corr = correlation(x, iter)

     computed_moment = momentum(x, iter, moment)

     WRITE(unit=1, fmt=*) iter, corr, abs(r_corr - corr), moment, abs(r_moment - computed_moment)

     deallocate(x)

  end do

  CLOSE(1)


end program unif_corr




