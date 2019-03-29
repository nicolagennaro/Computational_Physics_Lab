! rw1d.f90
! A simple random walk program  in 1D.


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






program rw1d

  use factorial

  implicit none

  integer ::         N  ! number of steps
  integer, dimension(:), allocatable :: seed
  integer :: icount1, icount2, icount_rate, ix, irun, istep, nruns, sizer
  real, dimension(:), allocatable :: rnd          ! array of random numbers
  integer, dimension(:), allocatable :: x_N, x2_N ! sum of deviations and 
  ! squares over the runs
  integer, dimension(:), allocatable :: P_N     ! final positions, sum over runs
  real :: n_fact, nx_fact
  

  
  print *, "Enter number of steps >"
  read *, N

  print *, "Enter number of runs >"
  read *, nruns

  print *, "Enter sizer >"
  read *, sizer

  call random_seed(sizer)
  allocate(seed(sizer))

  print *,'Here the seed has ',sizer,' components; insert them (or print "/") >'  

  read *, seed
  call random_seed(put=seed)


  allocate(rnd(N))
  allocate(x_N(N))
  allocate(x2_N(N))
  allocate(P_N(-N:N))
  x_N  = 0
  x2_N = 0
  P_N  = 0

  do irun = 1, nruns
     ix = 0 ! initial position of each run
     call random_number(rnd) ! get a sequence of random numbers
     do istep = 1, N
        if (rnd(istep) < 0.5) then ! random move
           ix = ix - 1 ! left
        else
           ix = ix + 1 ! right
        end if
        x_N (istep) = x_N (istep) + ix
        x2_N(istep) = x2_N(istep) + ix**2 
     end do
     P_N(ix) = P_N(ix) + 1 ! accumulate (only for istep = N)
  end do


  print*, "# N=", N, "  nruns=", nruns
  print*, "# <x_N>   = ", real(x_N(N))/nruns 
  print*, "# <x^2_N> = ", real(x2_N(N))/nruns 
  print*, "# <x^2_N> -  <x_N>^2 = ", real(x2_N(N))/nruns - (real(x_N(N))/nruns)**2 

  open(1, file="P_N.dat", STATUS="REPLACE", ACTION="WRITE")
  write(1,*) "# N=", N , "  nruns=", nruns
  write(1,*) "# <x_N>   = ", real(x_N(N))/nruns 
  write(1,*) "# <x^2_N> = ", real(x2_N(N))/nruns 
  write(1,*) "# <x^2_N> - <x_N>^2 = ", real(x2_N(N))/nruns - (real(x_N(N))/nruns)**2 
  write(1,*) " "

  write(1,*) "# N, mean deviations, mean squared deviations, sigma^2"
  do ix = - N, N
     write(1,*)ix,real(P_N(ix))/nruns
  end do

  close(1)

  print*, ""
  print*, "DELTA: ", abs( (real(x2_N(N))/nruns - (real(x_N(N))/nruns)**2) / real(N) - 1.0  )
  print*, ""


  ! real distribution
  open(2, file="P_N_real.dat", STATUS="REPLACE", ACTION="WRITE")
  
  do ix = -N, N
     n_fact = fact( N )
     nx_fact = fact( (N-ix)/2 )
     n_fact = n_fact / nx_fact
     nx_fact = fact( (N+ix)/2 )
     n_fact = n_fact / nx_fact
     n_fact = n_fact * 0.5**N
     write(2, *) ix, n_fact
  end do

  close(2)
  
  deallocate (rnd, x_N, x2_N, P_N, seed)


  
  stop
end program rw1d
