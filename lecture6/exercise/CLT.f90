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
     v(i) = mean_unif(n_points)
  end do

  

  hist = 0

  delta = 2.0/n_bins

  do i=1,n
     h = nint(v(i)/delta)
     hist(h) = hist(h) + 1
  end do

  open(1, file='clt_unif.dat', status='replace', action='write')

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
  print*, 'theoretical value:', 4.0/12.0/real(n_points)

  sigma =  sqrt(4.0/12.0/real(n_points))


  do i=1, n
     v(i) = v(i)/sigma
  end do

  z2 = 0.0
  z4 = 0.0

  do i=1, n
     z2 = z2 + v(i)**2
     z4 = z4 + v(i)**4
  end do

  print*,
  print*, 'z4:     ', z4/real(n)
  print*, '3 z2**2:', 3.0 * (z2/real(n))**2
  
  close(1)
  deallocate(v)
  deallocate(hist)

  
contains

  function mean_unif(n_points)
    REAL :: mean_unif
    INTEGER, intent(in) :: n_points
    REAL, dimension(n_points) :: x
    INTEGER :: j
    
    
    mean_unif = 0.0

    call random_number(x)

    x = -1.0 + 2.0*x

    do j=1, n_points
       mean_unif = mean_unif + x(j)
    end do

    mean_unif = mean_unif / real(n_points)

  end function mean_unif
  
  
end program CTL
