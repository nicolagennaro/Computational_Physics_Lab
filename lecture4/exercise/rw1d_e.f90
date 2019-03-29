! rw1d.f90
! A simple random walk program  in 1D.

program rw1d_e

  implicit none

  integer :: N, exp, max_exp  ! number of steps
  integer, dimension(:), allocatable :: seed
  integer :: icount1, icount2, icount_rate, ix, irun, istep, nruns, sizer
  real, dimension(:), allocatable :: rnd          ! array of random numbers
  integer :: x_N, x2_N ! sum of deviations and 
  ! squares over the runs
  integer, dimension(:), allocatable :: P_N     ! final positions, sum over runs

  print *, "Enter max_exp"
  read *, max_exp

  if( max_exp .LE. 4 ) then
     print*, "max_exp must be greater than 4..."
     stop 1
  end if
   
  print *, "Enter number of runs >"
  read *, nruns

  print *, "Enter sizer >"
  read *, sizer

  call random_seed(sizer)
  allocate(seed(sizer))

  print *,'Here the seed has ',sizer,' components; insert them (or print "/") >'  

  read *, seed
  call random_seed(put=seed)


  allocate(rnd(2**max_exp))

  
  open(1, file="Nvar.dat", STATUS="REPLACE", ACTION="WRITE")
  
  do exp = 0, max_exp

     N = 2**exp
  
     x_N = 0
     x2_N = 0

     do irun = 1, nruns

        ix = 0 ! initial position of each run

        call random_number(rnd) ! get a sequence of random numbers

        
        do istep = 1, N
           if (rnd(istep) < 0.5) then ! random move
              ix = ix - 1 ! left
           else
              ix = ix + 1 ! right
           end if
        end do

        x_N = x_N + ix
        x2_N = x2_N + ix*ix
        
     end do

     write(1, *) N, real(x2_N)/nruns - (real(x_N)/nruns)**2

  end do

  close(1)

  
  deallocate (rnd, seed)


  
  stop
end program rw1d_e
