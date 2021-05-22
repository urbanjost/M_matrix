program try_matz
use M_matrix, only : laff, get_from_laff, put_into_laff
implicit none
integer                       :: i,j, ierr
doubleprecision,allocatable   :: dble_array(:,:)
doubleprecision,allocatable   :: dble_vector(:)
   ! pass some commands to laff
   call laff( 'b=<1 2 3 4 5 6 7 8>') ! create some values in laff(3f)
   call put_into_laff('B', [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0] ,ierr) 
   if (ierr .ne. 0) then
      write(*,*)'<ERROR> could not store array, ERR=',ierr
   endif
   allocate(dble_array(3,5))
   dble_array(1,:)=[11,12,13,14,15]
   dble_array(2,:)=[21,22,23,24,25]
   dble_array(3,:)=[31,32,33,34,35]
   call checkit()
   call put_into_laff('C',dble_array,ierr)

   call laff([character(len=80) :: &
    & "", &
    & "", &
    & "", &
    & "", &
    & "", &
    & 'who', &
    & ''])
   write(*,*)'entering interactive mode. Enter "help" for a description'
   write(*,*)'Entering "return" will return back to the main program.'
   call laff()
   call get_from_laff('B',dble_array,ierr); call checkit()
   call get_from_laff('b',dble_vector,ierr); write(*,'(*(g0,1x))')'as a vector',nint(dble_vector)
   call get_from_laff('b',dble_array,ierr); write(*,'(*(g0,1x))'); call checkit()
   call laff()

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
