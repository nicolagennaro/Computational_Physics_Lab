!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! direct_sampling.f90
!
! DIRECT sampling of several physical observables for the
! hamiltonian:         h = -1/2 \nabla^2 + 1/2 x^2),
! comparison exact expected results with numerical results
! on psi^2(x), with  psi(x) = exp(-x^2/(4 sigma^2)
! (sigma=1  => psi^2(x) = costant * standard gaussian
!  P(x) =  exp(-x**2/(2*sigma**2))/sqrt(2*pi*sigma**2)
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

Module gaussian

  implicit none

  public :: gasdev

contains

  SUBROUTINE gasdev(x)

    REAL, INTENT(OUT) :: x
    REAL :: rsq, v1, v2
    REAL, SAVE :: g
    LOGICAL, SAVE :: gaus_stored=.false.

    if (gaus_stored) then
       x=g
       gaus_stored=.false.
    else
       do
          call random_number(v1)
          call random_number(v2)
          v1 = 2.0*v1 - 1.0
          v2 = 2.0*v2 - 1.0
          rsq = v1**2 + v2**2
          if (rsq > 0.0 .and. rsq < 1.0) exit
       end do
       rsq  = sqrt(- 2.0*log(rsq)/rsq)
       x = v1*rsq
       g = v2*rsq
       gaus_stored = .true.
    end if

  END SUBROUTINE gasdev

end module gaussian






program direct_sampling

  use gaussian

  implicit none

  integer, parameter :: dp=selected_real_kind(13)
  integer :: i,n
  integer, dimension(12) :: seed

  real :: rnd
  real(kind=dp):: sigma
  real(kind=dp):: autocorr_gas1, autocorr_gas2, autocorr_gas3
  real(kind=dp):: autocorr_met1, autocorr_met2, autocorr_met3
  real(kind=dp):: delta, x, x0, xp, expx, expxp, w
  real(kind=dp), dimension(10000) :: vec
  

  print*, "seed >"
  read*,   seed
  print*, "sigma >"
  read*,   sigma
  print*, "x0 >"
  read*,   x0
  print*, "delta >"
  read*,   delta

  open(1, file='autocorr.dat', status='replace')

  
  call random_seed(put=seed)
  
  do n=100, 10000, 100
          
     do i=1,n
        !cccccccccccccccccccccccccc
        call gasdev(rnd)   !
        x=rnd*sigma        ! direct sampling
        !ccccccccccccccccccccccccc!
        vec(i) = x
        
     end do


     autocorr_gas1 = autocorr(vec, n, 1)
     autocorr_gas2 = autocorr(vec, n, 2)
     autocorr_gas3 = autocorr(vec, n, 3)

     x = x0


     ! metropolis
     do i=1,n

        !ccccccccccccccccccccccccccccccc
        expx = - x**2 /(2*sigma**2)    !

        call random_number(rnd)        !

        xp = x + delta * (rnd-0.5_dp)  !

        expxp = - xp**2 /(2*sigma**2)  !   metropolis

        w = exp (expxp-expx)           !   algorithm

        call random_number(rnd)        !

        if (w > rnd) then              !
           x = xp                      !
           !ccccccccccccccccccccccccccccccc
        endif

        vec(i) = x

     end do
     

     autocorr_met1 = autocorr(vec, n, 1)
     autocorr_met2 = autocorr(vec, n, 2)
     autocorr_met3 = autocorr(vec, n, 3)


     
!     write(unit=*,fmt=*)"Results (simulation vs. exact result):"
     write(unit=1, fmt=*) n, autocorr_gas1, autocorr_met1, autocorr_gas2, &
          autocorr_met2, autocorr_gas3, autocorr_met3

  end do

  
  close(1)

  stop

contains
  

  function autocorr( x, n, j )

    real(kind=dp), dimension(n), intent(in) :: x

    integer, intent(in) :: n
    integer, intent(in) :: j
    integer :: ind

    real(kind=dp) :: x1, x11, x2, autocorr


    x1 = 0.0_dp
    x11 = 0.0_dp
    x2 = 0.0_dp


    do ind=1, n-j
       x1 = x1 + x(ind)
       x2 = x2 + x(ind)**2
       x11 = x11 + x(ind)*x(ind + j)
    end do

    autocorr = ( x11/(n-j) - (x1/(n-j))**2 ) / ( x2/(n-j) - (x1/(n-j))**2 )

  end function autocorr



  
end program direct_sampling




     
  
