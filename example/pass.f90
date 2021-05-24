program try_matz
use M_matrix, only : laff, get_from_laff, put_into_laff, ifin_laff
implicit none
integer,parameter             :: lda=10
integer                       :: m,n, i,j, ierr
doubleprecision               :: arr(lda,lda)
real                          :: vec(lda)
integer                       :: ivec(lda)
integer                       :: whole
character(len=:),allocatable  :: string
character(len=:),allocatable  :: strings(:)
doubleprecision,allocatable   :: dble_array(:,:)
doubleprecision,allocatable   :: dble_vector(:)
doubleprecision,allocatable   :: scalar

   ! pass some commands to laff
   call laff( 'b=<1 2 3 4; 5 6 7 8>;') ! create some values in laff(3f)
   ! the commands may be an array
   call laff( [character(len=80) :: &
    & 'a=magic(4);', &
    & 'c=3**3;', &
    & 'FRED=3**3;', &
    & '']) 
    write(*,*)'a',ifin_laff('a')
    write(*,*)'b',ifin_laff('b')
    write(*,*)'c',ifin_laff('c')
    write(*,*)'d',ifin_laff('d')
    write(*,*)'reallylongname_reallylongname_really_long_name_really_longname', &
    & ifin_laff('reallylongname_reallylongname_really_long_name_really_longname')
    write(*,*)'///',ifin_laff('///')

   RUN: block
       ! put some values from the program into laff
       ! note not testing ierr
       call put_into_laff('C',(30.0,40.0),ierr) ! a complex scalar
       call put_into_laff('I',20,ierr)          ! an integer scalar
       call put_into_laff('R',10.0,ierr)        ! a real scalar
       ivec=[000,100,200,300,400,500,600,700,800,900]
       call put_into_laff('IVEC',ivec,ierr)     ! an integer vector
       vec=[0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]
       call put_into_laff('VEC',vec,ierr)       ! a real vector
       call put_into_laff('string',"this is a string",ierr) ! string
       call put_into_laff('strings', &
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

       write(*,*)'THE ORIGINAL ARR ARRAY IN THE USER PROGRAM'
       dble_array=arr;call checkit

       ! The matrix ARR is sent to the laff() stack 
       call put_into_laff('ARR',arr,ierr)

       if (ierr .ne. 0) then
          write(*,*)'<ERROR> could not store array, ERR=',ierr
          exit run
       endif

       ! The call to laff() will transpose our matrix, put the result
       ! X on the stack and go back to our program.
       call laff([character(len=80) :: &
       & "X=ARR';", &
       & "// ARR,R,C,I,IVEC,VEC,display(string),display(strings)'", &
       & 'who', &
       & ''])

       ! now enter laff() interactively to look around
       ! once interactive mode is exited get some values back out of laff()
       write(*,*)'entering interactive mode. Enter "help" for a description'
       write(*,*)'Entering "return" will return back to the main program.'
       call laff()

       ! at least for now, return values should be allocatable, primarily
       ! because laff() can change the sive of arrays. Might change this
       ! of make another procedure that fails unless the requested array
       ! matches in size.
       call get_from_laff('a',dble_array,ierr); call checkit()

       call get_from_laff('b',dble_array,ierr); call checkit()
       call get_from_laff('b',dble_vector,ierr); write(*,*)'as a vector',dble_vector

       call get_from_laff('c',dble_array,ierr); call checkit()
       call get_from_laff('unknown',dble_array,ierr); call checkit()
       write(*,*)'ARR retrieved'
       call get_from_laff('ARR',dble_array,ierr=ierr); call checkit()
       !NO: NOT FIXED SIZE:call get_from_laff('ARR',ARR,err=ierr); call checkit()
       write(*,*)'X retrieved'
       call get_from_laff('X',dble_array,ierr=ierr); call checkit()
       call get_from_laff('c',whole,ierr); write(*,*)'as a scalar',whole
       call get_from_laff('string',string,ierr); write(*,*)string
       call get_from_laff('strings',strings,ierr); write(*,'(a)')strings
       call get_from_laff('strings',strings,ierr); write(*,*)'strings(2)=',strings(2)

       ! The next call to laff() will again place you in interactive mode in laff().
       ! Entering "return" will return back to the main program.
       call laff()

   endblock RUN
contains

subroutine checkit()
integer                       :: m,n
   if (ierr .ne. 0)then
      write(*,*)'<ERROR> retrieving variable, ERR=',ierr
   else
      m=size(dble_array,dim=1)
      n=size(dble_array,dim=2)
      write(*,'(*(g0))')'BACK IN THE CALLING PROGRAM. THE VALUES ARE:SIZE:',size(dble_array),':ROWS:',m,':COLS:',n,':IERR:',ierr
      write(*,'(*(g0,1x))')'<INFO>',new_line('A'),(int(dble_array(j,:)),new_line('A'),j=1,m)
   endif
end subroutine checkit

end program try_matz
