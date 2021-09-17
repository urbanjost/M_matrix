program test_func_array
   implicit none

! First you should create a type that contain only a procedure pointer
! that doesn't pass any argument. and then create an array of that type.
  type pp
    procedure(func) ,pointer ,nopass :: f =>null()
  end type pp

! Now define an interface that matches your procedures

   interface
      function func(x)
         real :: func
         real, intent (in) :: x
      end function func
   end interface

! create an array of function pointers
   type(pp) :: func_array(4)

! point your pointers to the functions
   func_array(1)%f => exp
   func_array(2)%f => tan
   func_array(3)%f => cos
   func_array(4)%f => sin

   print*,func_array(1)%f(1.)
   print*,func_array(2)%f(1.)
   print*,func_array(3)%f(0.)
   print*,func_array(4)%f(0.)  

end program test_func_array
