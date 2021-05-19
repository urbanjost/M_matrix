program try_matz
use M_matrix, only : mat88, get_from_mat88, put_into_mat88
implicit none
integer,parameter             :: lda=10
integer                       :: m,n, i,j, ierr
doubleprecision               :: arr(lda,lda)
real                          :: vec(lda)
integer                       :: ivec(lda)
integer                       :: whole
doubleprecision,allocatable   :: dble_array(:,:)
doubleprecision,allocatable   :: dble_vector(:)
doubleprecision,allocatable   :: scalar

   ! pass some commands to mat88
   call mat88( 'b=<1 2 3 4; 5 6 7 8>;') ! create some values in mat88(3f)
   ! the commands may be an array
   call mat88( [character(len=80) :: &
    & 'a=magic(4);', &
    & 'c=3**3;', &
    & '']) 

   RUN: block
       ! put some values from the program into mat88
       ! note not testing ierr
       call put_into_mat88('C',(30.0,40.0),ierr) ! a complex scalar
       call put_into_mat88('I',20,ierr)          ! an integer scalar
       call put_into_mat88('R',10.0,ierr)        ! a real scalar
       ivec=[000,100,200,300,400,500,600,700,800,900]
       call put_into_mat88('IVEC',ivec,ierr)     ! an integer vector
       vec=[0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]
       call put_into_mat88('VEC',vec,ierr)       ! a real vector
       call put_into_mat88('string',"this is a string",ierr) ! string
       call put_into_mat88('strings', &
       & [character(len=30) ::     &
       & "this is a string",       &
       & "this is another string"] &
       & ,ierr) ! string array

       ! now this program generates an array
       do i=1,lda
          do j=1,lda
             arr(i,j)=(i-1)*lda+j
          enddo
       enddo

       ! The matrix ARR is sent to the mat88() stack 
       call put_into_mat88('ARR',arr,ierr)

       if (ierr .ne. 0) then
          write(*,*)'<ERROR> could not store array, ERR=',ierr
          exit run
       endif

       ! The call to mat88() will transpose our matrix, put the result
       ! X on the stack and go back to our program.
       call mat88([character(len=80) :: &
       & "X=ARR'", &
       & "ARR,R,C,I,IVEC,VEC,display(string),display(strings)'", &
       & 'who', &
       & ''])

       ! now enter mat88() interactively to look around
       ! once interactive mode is exited get some values back out of mat88()
       write(*,*)'entering interactive mode. Enter "help" for a description'
       write(*,*)'Entering "return" will return back to the main program.'
       call mat88()

       ! at least for now, return values should be allocatable, primarily
       ! because mat88() can change the sive of arrays. Might change this
       ! of make another procedure that fails unless the requested array
       ! matches in size.
       call get_from_mat88('a',dble_array,ierr); call checkit()

       call get_from_mat88('b',dble_array,ierr); call checkit()
       call get_from_mat88('b',dble_vector,ierr); write(*,*)'as a vector',dble_vector

       call get_from_mat88('c',dble_array,ierr); call checkit()
       call get_from_mat88('unknown',dble_array,ierr); call checkit()
       call get_from_mat88('X',dble_array,ierr); call checkit()
       !call get_from_mat88('c',whole,ierr); call checkit()

       ! The next call to mat88() will again place you in interactive mode in mat88().
       ! Entering "return" will return back to the main program.
       call mat88()

   endblock RUN
contains

subroutine checkit()
   if (ierr .ne. 0)then
      write(*,*)'<ERROR> retrieving variable, ERR=',ierr
   else
      m=size(dble_array,dim=1)
      n=size(dble_array,dim=2)
      write(*,*)'BACK IN THE CALLING PROGRAM. THE VALUES ARE'
      write(*,*)'<INFO>',new_line('A'),(( int(dble_array(i,j)),i=1,m),new_line('A'),j=1,n ),&
      & ' SIZE=',size(dble_array),'ROWS=',m,'COLS=',n,'IERR=',ierr
   endif
end subroutine checkit

end program try_matz
