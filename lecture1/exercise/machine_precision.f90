program machine_precision

  real:: eps, unit
  integer:: count
  logical:: verbose

  verbose = .TRUE.
  unit = 1.
  eps = 1.

  count = 0
  
  do while( unit < (unit + eps) )
     count = count + 1
     eps = eps/2.
     if( verbose ) then
       print*, count, eps
     end if
  end do

  print*, 
  print*, "stopped after ", count, "iterations"
  
end program machine_precision
