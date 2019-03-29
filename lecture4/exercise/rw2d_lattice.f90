! rw2d.f90
! A simple random walk program  in 2D. 

PROGRAM drunk

  IMPLICIT NONE

  INTEGER :: i, N, sizer
  INTEGER :: nruns, run, exp, step
  INTEGER, DIMENSION(4) :: moves
  REAL :: xN, yN, x2N, y2N
  REAL :: phi, rnd
  INTEGER, DIMENSION(:), allocatable :: seed

  REAL, DIMENSION(2) :: x
  ! REAL :: x=0.0, y=0.0        ! Put drunk initially at the origin
  INTEGER, PARAMETER :: out=1 ! Set output unit

  ! REAL, PARAMETER :: step=1.0 ! twopi=2.0*3.1415926 ! step size and constants
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

        moves = 0
     
        DO  i=1, N
           CALL compute_step_drift( step )
           moves(step) = moves(step) + 1
        END DO
        
        ! moves(1) = right
        ! moves(2) = left
        ! moves(3) = up
        ! moves(4) = down
        
        xN = xN + ( moves(1) - moves(2) )
        x2N = x2N + ( moves(1) - moves(2) )*( moves(1) - moves(2) )
        yN = yN + ( moves(3) - moves(4) )
        y2N = y2N + ( moves(3) - moves(4) )*( moves(3) - moves(4) )

     END DO

     WRITE(UNIT=out, FMT=FORMAT1) N, real( x2N )/nruns + real( y2N )/nruns - real( xN / nruns )**2 - real( yN / nruns )**2
  END DO

  CLOSE(out)

  deallocate(seed)

  stop
  
END PROGRAM drunk





SUBROUTINE compute_step( step )
  INTEGER, INTENT(OUT) :: step
  REAL :: rnd
  
  CALL RANDOM_NUMBER(rnd)
  step = floor( rnd*4 ) + 1

END SUBROUTINE compute_step



SUBROUTINE compute_step_drift( step )
  INTEGER, INTENT(OUT) :: step
  REAL :: rnd
  
  CALL RANDOM_NUMBER(rnd)

  IF( rnd .LE. 0.4 ) THEN
     step = 1
  else if( rnd .GE. 0.4 .AND. rnd .LE. 0.6 ) THEN
     step = 2
  else if( rnd .GE. 0.6 .AND. rnd .LE. 0.8 ) THEN
     step = 3
  else
     step = 4
  end if
  
     
     
END SUBROUTINE compute_step_drift
