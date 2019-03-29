! rw2d.f90
! A simple random walk program  in 2D. 

PROGRAM drunk

  IMPLICIT NONE

  INTEGER :: i, N, sizer
  INTEGER :: nruns, run, exp
  REAL :: xN, yN, x2N, y2N
  REAL :: phi, rnd
  INTEGER, DIMENSION(:), allocatable :: seed

  REAL, DIMENSION(2) :: x, pos
  ! REAL :: x=0.0, y=0.0        ! Put drunk initially at the origin
  INTEGER, PARAMETER :: out=1 ! Set output unit

  REAL, PARAMETER :: step=1.0 ! twopi=2.0*3.1415926 ! step size and constants
  CHARACTER(LEN=15) :: filein
  CHARACTER(LEN=15), SAVE :: FORMAT1 = "(1i5,1x,2F14.7)"
  
  PRINT*, "Enter number of runs: "
  READ*, nruns

  PRINT*,"Enter file for data"
  READ*, filein

  OPEN(out, FILE=filein, STATUS="REPLACE", ACTION="WRITE")

  PRINT*, "Enter sizer"
  READ*, sizer

  allocate(seed(sizer))

  print *,'Here the seed has ',sizer,' components; insert them (or print "/") >'  

  read(*,*)seed

  CALL RANDOM_SEED(PUT=seed)

  i = 0
  ! WRITE(UNIT=out,FMT=FORMAT1)i,x,y


  DO exp = 3, 10

     N = 2**exp

     
     
     xN = 0
     yN = 0
     x2N = 0
     y2N = 0
     
     DO run = 1, nruns

        pos = 0
     
        DO  i=1, N
           CALL compute_xy_2(x, step)
           pos = pos + x
           print*, x
        END DO

        xN = xN + pos(1)
        x2N = x2N + pos(1)*pos(1)
        yN = yN + pos(2)
        y2N = y2N + pos(2)*pos(2)

     END DO

     WRITE(UNIT=out, FMT=FORMAT1) N, real( x2N )/nruns + real( y2N )/nruns - real( xN / nruns )**2 - real( yN / nruns )**2
  END DO

  CLOSE(out)

  deallocate(seed)

  stop
  
END PROGRAM drunk


SUBROUTINE compute_xy( x, step )
  REAL, DIMENSION(2), INTENT(INOUT) :: x
  REAL, INTENT(IN) :: step
  REAL :: rnd, phi
  REAL, PARAMETER :: twopi = 2.0*3.1415926
  
  CALL RANDOM_NUMBER(rnd)
  phi = twopi * rnd
  x(1) = step * COS(phi)
  x(2) = step * SIN(phi)

END SUBROUTINE compute_xy




SUBROUTINE compute_xy_1( x, step )
  REAL, DIMENSION(2), INTENT(INOUT) :: x
  REAL, INTENT(IN) :: step
  REAL :: random
  REAL :: rnd_plus
  
  CALL RANDOM_NUMBER(random)
  CALL RANDOM_NUMBER(rnd_plus)
  x(1) = step * ( -1.0 + random*2.0 )
  x(2) = step * ( SQRT( 1.0  - x(1)**2 ) )

  if( rnd_plus > 0.5 )  x(2) = -x(2)

END SUBROUTINE compute_xy_1




SUBROUTINE compute_xy_2( x, step )
  REAL, DIMENSION(2), INTENT(OUT) :: x
  REAL, INTENT(IN) :: step
  REAL :: random1, random2, x2

  
  CALL RANDOM_NUMBER(random1)
  CALL RANDOM_NUMBER(random2)

  IF ( random1 == 0.0 .OR. random2 == 0.0 ) then
     print*, "error"
     stop
  END IF

  
  x(1) = step * ( -1.0 + 2 * random1 )
  x(2) = step * ( -1.0 + 2 * random2 )

  x2 = SQRT( x(1)*x(1) + x(2)*x(2) )  

  x(1) = x(1) / x2
  x(2) = x(2) / x2


END SUBROUTINE compute_xy_2
