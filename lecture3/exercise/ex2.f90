
program ex2

  implicit none

  real :: x, delta
  real, parameter :: pi = 4.*atan(1.)
  integer :: i, n, nbin, ibin, sizer, discarded, tmp_disc
  integer, dimension(:), allocatable :: histo1, histo2, seed
    
  print*, "Insert sizer"
  read(*,*) sizer
  ! call random_seed(sizer)
  allocate(seed(sizer))


  print *,'Here the seed has ',sizer,' components; insert them (or print "/") >'  
  read(*,*)seed
  print*, 'seed: ', seed
  
  call random_seed(put=seed)

  print *," insert length of the sequence >"
  read *, n
  print *," Insert number of bins in the histogram (must be odd)>"
  read *, nbin

  ! IF( mod(nbin, 2) .NE. 1 ) THEN
  !    print*, 'nbin must be odd'
  !    STOP
  ! END IF

  print*, 'nbin:   ', nbin
  ! print*, 'nbin/2: ', nbin/2
  
  delta = 2./nbin

  print*, 'delta:  ', delta

  allocate (histo1(nbin))
  allocate (histo2(nbin))

  histo1 = 0
  histo2 = 0

  DO i = 1,n
     CALL ITM(x)
     ! ibin = int ((x+1)/delta) + 1
     ibin = int ((x+1)/delta) + 1
     IF (ibin .LE. nbin) THEN
        histo1(ibin) = histo1(ibin) + 1
     ELSE
        print*, 'the x caused a problem ', x, ibin
     END IF
  END DO

  ! now the rejection method
  
  discarded = 0
  
  DO i=1, n 
     call REJEC(x, tmp_disc)
     discarded = discarded + tmp_disc
     ibin = int ((x+1)/delta) + 1
     IF (ibin .LE. nbin) THEN
        histo2(ibin) = histo2(ibin) + 1
     ELSE
        print*, 'the x caused a problem ', x, ibin
     END IF
  END DO

  print*, 'rejection method discarded: ', discarded
  print*, 'fraction of discarded:      ', discarded / float( n + discarded )
  
  open (unit=7,file="ex2.dat",status="replace",action="write")
  
  do ibin=1, nbin
     write(unit=7,fmt=*) -1 + (ibin-0.5)*delta, histo1(ibin)/float(n)/delta, histo2(ibin)/float(n)/delta
  end do





  
contains

  subroutine ITM(x)

    REAL, intent (out) :: x
    REAL :: u

    call random_number(u)

    x = sin( pi*( 2.*u - 1) )

  end subroutine ITM




  subroutine REJEC(x, disc)

    REAL, intent (out) :: x
    INTEGER, intent (out) :: disc
    REAL :: u, v, u2, v2

    disc = 0

    DO
       call random_number(u)
       call random_number(v)

       u2 = u*u
       v2 = v*v
       
       IF( u2 + v2 <= 1. ) THEN
          x = ( u2 - v2 ) / ( u2 + v2 )
          EXIT
       END IF
       
       disc = disc + 1
       
    END DO

  end subroutine REJEC




end program ex2
