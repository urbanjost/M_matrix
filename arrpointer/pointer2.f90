program test_func_array
implicit none
integer,parameter :: size_of_elemental=100

! First you should create a type that contain only a procedure pointer
! that doesn't pass any argument. and then create an array of that type...
  type lala_function_pointer
    character(len=63)                :: name=' '
    procedure(func) ,pointer ,nopass :: func =>null()
  end type lala_function_pointer

! Now define an interface that matches your procedures

   interface
      function func(x)
         real :: func
         real, intent (in) :: x
      end function func
   end interface

! create an array of function pointers
type(lala_function_pointer),save :: elemental_func_array(size_of_elemental)

! point your pointers to the functions
   elemental_func_array(1)%name = 'exp'; elemental_func_array(1)%func => exp
   elemental_func_array(2)%name = 'tan'; elemental_func_array(2)%func => tan
   elemental_func_array(3)%name = 'cos'; elemental_func_array(3)%func => cos
   elemental_func_array(4)%name = 'sin'; elemental_func_array(4)%func => sin

   print*,elemental_func_array(1)%func(1.)
   print*,elemental_func_array(2)%func(1.)
   print*,elemental_func_array(3)%func(0.)
   print*,elemental_func_array(4)%func(0.)  

end program test_func_array
