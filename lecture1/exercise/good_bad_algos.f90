module factorial

  public :: fact
  contains
  
recursive function fact(n) result (factorial_result)
  integer, intent (in) :: n
  integer :: factorial_result

  if (n <= 0) then
     factorial_result = 1
  else
     factorial_result = n*fact(n-1)
  end if
end function fact

end module factorial



program good_bad_algos
  use factorial
  
  ! integer, parameter :: dp = selected_real_kind(13)
  
  real:: x
  integer:: i, n_max

  print*, "insert n_max iterations..."
  read(*,*) n_max

  print*, "insert x..."
  read(*,*) x
  
  call my_exp(x, n_max)  

end program good_bad_algos





subroutine my_exp(x, n)

  use factorial
  
  real, intent (in) :: x
  integer, intent (in) :: n
  real:: sum
  integer:: i
  
  sum = 0.

  do i=0, n
     sum = sum + (-1.)**i * (x**i) / fact(i)
  end do

  print*, n, sum, abs( sum - exp(-x) )/ exp(-x)
  
end subroutine my_exp
