!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     int.f90: 
!     integrates f(x)=exp(x) in the interval [vmin,vamx]=[0,1]
!     using trapezoidal and Simpson rule
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

module ex2mod

  implicit none
  
  public :: f, mcm, imp_samp, expdev

  integer, parameter :: rkind = selected_real_kind(15)

contains

  ! function to be integrated
  !
  function f(x)
    
    implicit none

    real(rkind) :: f
    real(rkind), intent(in) :: x

    f = exp( - x**2 )

    return

  end function f



  
  function mcm(n_points)

    implicit none

    real(rkind) :: mcm
    integer, intent(in) :: n_points
    integer :: i
    real(rkind) :: rnd, ff, fi, fi2, sigma

    mcm = 0.0
    fi = 0.0
    fi2 = 0.0

    do i=1, n_points
       call RANDOM_NUMBER(rnd)
       ff = f(rnd)
       mcm = mcm + ff
       fi = fi + ff
       fi2 = fi2 + ff**2
    end do

    fi = fi / real(n_points)
    fi2 = fi2 / real(n_points)

    sigma = sqrt( fi2 - fi**2 )

    print*, "mcm: ", n_points, sigma, sigma / sqrt( real(n_points) )
    
    mcm = mcm / real(n_points)

    return

  end function mcm




  function imp_samp(n_points)
    
    real(rkind) :: imp_samp

    integer, intent(in) :: n_points
    integer :: i

    real(rkind) :: rnd, fi, pi, fp
    real(rkind) :: fi2, fi_mean
    real(rkind) :: integral
    real(rkind) :: sigma

    imp_samp = 0.0
    fi2 = 0.0
    fi_mean = 0.0

    integral = (1.0 - exp( -1.0 ))

    do i=1, n_points

       call expdev(pi)
       fi = f(pi)
       pi = exp( -pi )
       fp = fi/pi
       imp_samp = imp_samp + fp

       fi_mean = fi_mean + fp
       fi2 = fi2 + (fp)**2
       
    end do

    fi_mean = (fi_mean * integral) / real(n_points)
    fi2 = (fi2 * integral**2) / real(n_points)

    sigma = sqrt( fi2 - fi_mean**2 )
    
    print*, "imp_samp: ", n_points, sigma, sigma / sqrt( real(n_points) )
    
    imp_samp = imp_samp / real(n_points) * integral

       
  end function imp_samp
  


  subroutine expdev(x)
    REAL(rkind), intent (out) :: x
    REAL(rkind) :: r

    do
       call random_number(r)
       x = -log(r)
       if( x .GE. 0.0 .AND. x .LE. 1.0 ) exit
    end do
    
  end subroutine expdev





  
end module ex2mod





program ex2

  use ex2mod

  implicit none

  real(rkind) :: r1, r2, theo, vmin, vmax
  integer :: i, n
  real(rkind), parameter :: PI = 4.0*atan(1.0)
  

  vmin = 0.0
  vmax = 1.0

  theo = sqrt(PI) * erf( 1.0 ) / 2.0

  print*,' exact value =',theo

  open(unit=7,file='ex2.dat',status='unknown')
  
  write(7,*)"# N,  exact,  mcm, imp_samp"

  do i = 10,20

     n = 2**i

     r1 = mcm(n)
!     r1 = (r1-theo)
     

     r2 = imp_samp(n)
!     r2 = (r2-theo)

     write(7,'(i8, 3(2x,f20.16))') n, theo, r1, r2
!     write(7,'(i4,4(2x,f10.6))') n, 1./n, theo, r1, r2

  end do

  close(7)

  stop

end program ex2
