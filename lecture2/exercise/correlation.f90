program correlation

  implicit none
  
  integer:: n_pairs, i
  real:: x1, x2
  
  print*, "insert n pairs"
  read(*,*) n_pairs

  OPEN(unit=1, file='correlation.dat', status="replace", action="write")
  
  do i=1, n_pairs
     call random_number(x1)
     call random_number(x2)
     WRITE (unit=1, fmt=*) x1, x2
  end do

  CLOSE(1)
  print*, "saved in correlation.dat"
 
end program correlation
