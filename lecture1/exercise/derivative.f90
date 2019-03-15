program derivative

  integer, parameter :: dp = selected_real_kind(13)

  real(kind=dp):: x, true_val, central_diff, forw_diff, back_diff
  real(kind=dp):: h(8)
  integer:: i

  data h /0.5, 0.2, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001 /

  x = 1.
  true_val = cos(1.)

  open(34, file='derivative.dat')
  
  do i=1,8
     central_diff = ( sin( x + h(i) ) - sin( x - h(i) ) ) / (2.*h(i))
     forw_diff = ( sin( x + h(i) ) - sin(x) ) / h(i)
     back_diff = ( sin( x ) - sin( x - h(i) ) ) / h(i)

     print*,
     print*, 
     print*, "h , central, forw, back, true"
     print*, h(i), central_diff, forw_diff, back_diff, true_val
     print*, "errors: central, forw, back"
     print*, abs(central_diff - true_val ), abs(forw_diff - true_val ), abs(back_diff - true_val )
     write(34, *) h(i), central_diff, forw_diff, back_diff, true_val, &
          abs(central_diff - true_val ), abs(forw_diff - true_val ), abs(back_diff - true_val )
  end do
  
  
  end program derivative
