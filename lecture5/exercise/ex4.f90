!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     int.f90: 
!     integrates f(x)=exp(x) in the interval [vmin,vamx]=[0,1]
!     using trapezoidal and Simpson rule
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

module ex4mod

  implicit none
  
  public :: f, mcm

  integer, parameter :: rkind = selected_real_kind(15)

contains

  ! function to be integrated
  !
  function f(x)
    
    implicit none

    real(rkind) :: f
    real(rkind), intent(in) :: x

    f = sqrt(1.0_rkind - x**2)

    return

  end function f



  
  function mcm(n_points)

    implicit none

    real(rkind) :: mcm
    integer, intent(in) :: n_points
    integer :: i
    real(rkind) :: rnd, ff, fi, fi2, sigma_n2

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

    sigma_n2 = fi2 - fi**2
    
    mcm = mcm / real(n_points)

    print*, "mcm: ", n_points, mcm, sigma_n2
    
    return

  end function mcm


  
end module ex4mod





program ex4

  use ex4mod

  implicit none

  real(rkind) :: r1, r2, theo, vmin, vmax, mm, mm2
  integer :: i, n, m
  real(rkind), parameter :: PI = acos(-1.0)

  vmin = 0.0
  vmax = 1.0

  theo = PI / 4.0_rkind

  print*, 
  print*,' exact value = ',theo
  print*, 
  
  do i=2, 4

     n = 10**i

     r1 = mcm(n)

     print*, 'n           mcm(n)'
     print*, n, r1

  end do


  print*,
  print*,
  print*,
  print*,

  
  n = 10**4

  mm = 0.0
  mm2 = 0.0
  
  do m=1, 10
     
     r1 = mcm(n)
     mm = mm + r1
     mm2 = mm2 + r1*r1

  end do

  print*, "mean: ", mm/10.0
  print*, "sigma_m2: ", mm2/10.0 - (mm/10.0)**2
  print*, "sigma_m: ", sqrt( mm2/10.0 - (mm/10.0)**2 )

  
  print*, 
  print*,
  print*,
  print*,
  print*,


  n = 10**3

  mm = 0.0
  mm2 = 0.0
  
  do m=1, 10
     
     r1 = mcm(n)
     mm = mm + r1
     mm2 = mm2 + r1*r1

  end do

  
  print*, "mean: ", mm/10.0
  print*, "sigma_s2: ", mm2/10.0 - (mm/10.0)**2
  print*, "sigma_s/sqrt(s): ", sqrt( (  (mm2/10.0 - (mm/10.0)**2)  )  / 10.0 )

  print*, 

  
  stop

end program ex4
