program config
use M_matrix, only : lala, get_from_lala, put_into_lala
implicit none

! variables to read from config file
real,allocatable             :: table(:,:)
character(len=:),allocatable :: title
real                         :: pi

character(len=*),parameter   :: gen='(*(g0,1x))'
integer                      :: i
integer                      :: ierr

   ! read config file
   call lala("semi;exec('data/xin');return")

   call get_from_lala('table',table,ierr) ! get the array as a REAL array
   call get_from_lala('pi',pi,ierr)
   call get_from_lala('title',title,ierr)

   write(*,gen)'in calling program table shape =',shape(table)
   write(*,gen)(table(i,:),new_line('A'),i=1,size(table,dim=1))
   write(*,*)'title=',title
   write(*,*)'pi=',PI
end program config
