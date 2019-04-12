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
  allocate(hist(n_bins))

  
  do i=1,n
     v(i) = mean_exp(n_points)
  end do

  

  hist = 0

  ! histogram up to 5.0
  delta = 5.0/n_bins

  do i=1,n
     h = int(v(i)/delta) + 1
     if( h <= n_bins ) hist(h) = hist(h) + 1
  end do

  open(1, file='clt_exp.dat', status='replace', action='write')

  do i=1, n_bins
     write(1,*) i*delta, hist(i)
  end do

  x = 0.0
  x2 = 0.0

  do i=1, n
     x = x + v(i)
     x2 = x2 + v(i)*v(i)
  end do

  print*, 'mean:', x/real(n)
  print*, 'theoretical value:', 1.0
  print*, 'var: ', ( (x2/real(n)) - (x/real(n))**2 )
  print*, 'theoretical value:', 1.0/real(n_points)

  ! sigma =  sqrt(4.0/12.0/real(n_points))


  ! do i=1, n
  !    v(i) = v(i)/sigma
  ! end do

  ! z2 = 0.0
  ! z4 = 0.0

  ! do i=1, n
  !    z2 = z2 + v(i)**2
  !    z4 = z4 + v(i)**4
  ! end do

  ! print*,
  ! print*, 'z4:     ', z4/real(n)
  ! print*, '3 z2**2:', 3.0 * (z2/real(n))**2
  
  close(1)
  deallocate(v)
  deallocate(hist)

  
contains
  
  function mean_exp(n_points)
    REAL :: mean_exp
    INTEGER, intent(in) :: n_points
    REAL, DIMENSION(n_points) :: x
    INTEGER :: j

    call random_number(x)

    x = -log(x)
    
    mean_exp = 0.0

    do j=1, n_points
       mean_exp = mean_exp + x(j)
    end do

    mean_exp = mean_exp / real(n_points)

  end function mean_exp
  
  
end program CTL
