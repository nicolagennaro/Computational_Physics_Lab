program hist


  implicit none
  
  integer:: n_points, n_bins, i, n
  real, dimension(:), allocatable :: x
  real :: delta_r
  integer, dimension(:), allocatable :: histo
  
  
  print*, 'insert n_points'
  read(*,*) n_points
  print*, 'insert n_bins'
  read(*,*) n_bins

  allocate(x(n_points))
  allocate(histo(n_bins))

  histo = 0

  delta_r = 1./n_bins

  print*, delta_r
  
  call random_number(x)

  do n=1, n_points
     i = int(x(n)/delta_r) + 1
     histo(i) = histo(i) + 1
  end do
  
  OPEN(unit=1, file='hist.dat', status="replace", action="write")

  do n=1, n_bins
     WRITE (unit=1, fmt=*) n, histo(n)/real(n_points)*real(n_bins)
  end do

  CLOSE(1)
  print*, "saved in hist.dat"
 
end program hist
