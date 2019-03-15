program overflow_underflow
  implicit none


  real:: over, under
  integer:: i, n
  
  over = 1.
  under = 1.

  print*, "insert number of iterations..."
  read(*,*) n

  print*, "starting with under = ", under
  print*, "starting with over = ", over
  
  do i=1, n
     under = under/2
     over = over*2
  end do

  print*, "after ", n, " iterations"
  print*, "under: ", under
  print*, "over:", over
  
  
end program overflow_underflow

