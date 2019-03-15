!
!     generation of pseudorandom numbers - linear congruential method
!
program random_lcm

  implicit  none
  !
  !     declaration of variables
  integer :: i, number, old, seed, x, a,m,c
  Character(50) file_name
  logical :: period_found
  
  ! 
  !     supply initial values of some variables:
  !     seed:   to start; a: 
  !     number: how many numbers we want to generate   

  period_found = .FALSE.
  
  print*,'Use LCM: x_(i+1)=mod(a*x_i+c,m)'
  print*,'Insert seed (=x_0), a, m, c >'
  read (*,*) seed, a, m, c


  
  print*,' How many numbers do you want to generate ?'
  read(*,*)number
  !

!  file_name = 'random_'//trim(str(seed))//'.dat'

  if( seed<10 ) then
     WRITE( file_name, '(A7,I1,A4)' ) "random_", seed, ".dat"
  else
     WRITE( file_name, '(A7,I2,A4)' ) "random_", seed, ".dat"
  end if

  
  OPEN(unit=1, file=file_name, status="replace", action="write")
  old = seed
  !
  do i = 1, number

     x = mod ((a*old+c), m)

     WRITE (unit=1,fmt=*) x

     old = x

     if( old == seed .AND. .NOT. period_found ) then
        print*,
        print*, 'period: ', i
        print*,
        period_found = .TRUE.
     end if
     
  end do
  close(1)
  print*,' data saved in ', file_name
  stop

end program random_lcm



