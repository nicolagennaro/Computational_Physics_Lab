! adapted from www.cs.umbbc.edu/~squire/download/gauleg.f90
! gauleg.f90     P145 Numerical Recipes in Fortran
! compute x(i) and w(i)  i=1,n  Legendre ordinates and weights
! on interval -1.0 to 1.0 (length is 2.0)
! use ordinates and weights for Gauss Legendre integration
!

subroutine gaulegf(x1, x2, x, w, n)

  implicit none

  integer, intent(in) :: n
  double precision, intent(in) :: x1, x2
  double precision, dimension(n), intent(out) :: x, w

  integer :: i, j, m

  double precision :: p1, p2, p3, pp, xl, xm, z, z1
  double precision, parameter :: eps=3.d-14
      
  m = (n+1)/2
  xm = 0.5d0*(x2+x1)
  xl = 0.5d0*(x2-x1)

  
  do i=1,m
    z = cos(3.141592654d0*(i-0.25d0)/(n+0.5d0))
    z1 = 0.0

    do while(abs(z-z1) > eps)
      p1 = 1.0d0
      p2 = 0.0d0

      do j=1,n
        p3 = p2
        p2 = p1
        p1 = ((2.0d0*j-1.0d0)*z*p2-(j-1.0d0)*p3)/j
      end do

      pp = n*(z*p1-p2)/(z*z-1.0d0)
      z1 = z
      z = z1 - p1/pp
   end do
   
    x(i) = xm - xl*z
    x(n+1-i) = xm + xl*z
    w(i) = (2.0d0*xl)/((1.0d0-z*z)*pp*pp)
    w(n+1-i) = w(i)
    
 end do
 
end subroutine gaulegf






program gauleg

  implicit none

  integer :: i, j, n_roots
  double precision, dimension(100) :: x, w
  double precision :: sum, a, b
  integer, parameter :: debug=0

  print *, 'test gauleg.f90 on interval -1.0 to 1.0 ordinates, weights'
  do i=1,15
     
     call gaulegf(-1.0d0, 1.0d0, x, w, i)

     sum = 0.0d0

     do j=1,i
        print *, 'x(',j,')=', x(j), '  w(',j,')=', w(j)
        sum = sum + w(j)
     end do

     print *, '             integrate(1.0, from -1.0 to 1.0)= ', sum
     print *, ' '

  end do


  open(1, file='err_cos.dat', status='replace', action='write')
  open(2, file='err_exp.dat', status='replace', action='write')

  
  a = 0.5d0
  b = 1.0d0

  print *, 'test gauleg on integral(sin(x), from ',a,' to ',b,')'

  do i=2,6

     n_roots = 2**i

     call gaulegf(a, b, x, w, n_roots)

     sum = 0.0d0

     do j=1,n_roots
        if(debug>0) then
           print *, 'x(',j,')=', x(j), '  w(',j,')=', w(j)
        end if
        sum = sum +w(j)*sin(x(j))
     end do

     print *, n_roots, ' integral (0.5,1.0) sin(x) dx = ', sum

     write(unit=1, fmt=*) n_roots, abs( (-cos(b)+cos(a) - sum)/(-cos(b)+cos(a)) )

  end do

  print *, '-cos(1.0)+cos(0.5) =', -cos(b)+cos(a)
  print *, 'exact should be: 0.3372802560'
  print *, ' '



  
  a = 0.d0
  b = 1.0d0

  
  print *, 'test gauleg on integral(exp(x), from ',a,' to ',b,')'

  do i=2,6

     n_roots = 2**i

     call gaulegf(a, b, x, w, n_roots)

     sum = 0.0d0

     do j=1,n_roots
        if(debug>0) then
           print *, 'x(',j,')=', x(j), '  w(',j,')=', w(j)
        end if
        sum = sum + w(j)*exp(x(j))
     end do

          write(unit=2, fmt=*) n_roots, abs( (exp(b)-exp(a) - sum)/(exp(b)-exp(a)) )
     
     print *, n_roots, ' integral (0.5,5.0) exp(x) dx = ', sum

  end do

  close(1)
  close(2)

  print *, 'exp(5.0)-exp(0.5) =', exp(b)-exp(a)
  print *, 'exact should be: 146.7644378'
  print *, ' '

end program gauleg
