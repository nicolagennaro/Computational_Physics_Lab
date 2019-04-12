program CTL

  implicit none

  integer n, n_points, n_bins, i, h
  real, dimension(:), allocatable :: v
  integer, dimension(:), allocatable :: hist
  real delta, x, x2, sigma, z2, z4
  

  print*, 'insert n'
  read*, n

  print*, 'insert n_points'
  read*, n_points

  print*, 'insert n_bins'
  read*, n_bins

  
  allocate(v(n))
  allocate(hist(-n_bins/2 : n_bins/2))

  
  do i=1,n
     v(i) = mean_Lorentz(n_points)
  end do

  

  hist = 0

  ! points between -3.0, 3.0
  delta = 6.0/n_bins

  do i=1,n
     h = nint(v(i)/delta)
     if( abs(h) < n_bins/2 ) hist(h) = hist(h) + 1
  end do

  open(1, file='clt_Lorentz.dat', status='replace', action='write')

  do i=-n_bins/2, n_bins/2
     write(1,*) i*delta, hist(i)
  end do

  x = 0
  x2 = 0

  do i=1, n
     x = x + v(i)
     x2 = x2 + v(i)*v(i)
  end do

  print*, 'mean:', x/real(n)
  print*, 'theoretical value:', 0.0
  print*, 'var: ', ( (x2/real(n)) - (x/real(n))**2 )
  print*, 'theoretical value:  inf'
  
  close(1)
  deallocate(v)
  deallocate(hist)

  
contains

  function mean_Lorentz(n_points)
    REAL, PARAMETER :: PI=3.141592
    REAL :: mean_Lorentz
    INTEGER, intent(in) :: n_points
    REAL, dimension(n_points) :: x
    INTEGER :: j
    
    
    mean_Lorentz = 0.0

    call random_number(x)

    x = tan( PI*(x-0.5) )

    do j=1, n_points
       mean_Lorentz = mean_Lorentz + x(j)
    end do

    mean_Lorentz = mean_Lorentz / real(n_points)

  end function mean_Lorentz
  
  
end program CTL
