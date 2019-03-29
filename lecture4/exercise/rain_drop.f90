! rw2d.f90
! A simple random walk program  in 2D. 

PROGRAM rain_drop

  IMPLICIT NONE

  INTEGER :: i, N, sizer
  INTEGER :: nruns, run, time, step, height, h, mean_time
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


  DO height = 2, 50

     mean_time = 0
     
     DO run = 1, nruns

        h = height
        time = 0
        
        DO WHILE( h .GE. 0 )
           CALL compute_step( step )
           h = h + step
           time = time + 1
        END DO
        
        ! moves(1) = right
        ! moves(2) = left
        ! moves(3) = up
        ! moves(4) = down
        
        mean_time = mean_time + time
        
     END DO

     WRITE(UNIT=out, FMT=FORMAT1) height, real(mean_time)/nruns, real(height) / mean_time * nruns
  END DO

  CLOSE(out)

  deallocate(seed)

  stop
  
END PROGRAM rain_drop






SUBROUTINE compute_step( step )
  INTEGER, INTENT(OUT) :: step
  REAL :: rnd
  
  CALL RANDOM_NUMBER(rnd)

  IF( rnd .LE. 0.6 ) THEN
     step = -1
  else if( rnd .GE. 0.6 .AND. rnd .LE. 0.7 ) THEN
     step = 1
  else
     step = 0
  end if
     
END SUBROUTINE compute_step
