program config_file
use M_matrix, only : laff, get_from_laff, put_into_laff, ifexists_laff
! create contents for a test file
character(len=*),parameter:: sample(*)=[character(len=80) :: &
"// my data definition file                                         ", &
"                                                                   ", &
"title='this is my title';                                          ", &
"                                                                   ", &
"A=[1 2 3; 4 5 6; 7 8 9];                                           ", &
"                                                                   ", &
"//                                                                 ", &
"// sin table                                                       ", &
"//                                                                 ", &
"PI=atan(1)*4;                                                      ", &
"step=0.05; myarray=[sin(0)];                                       ", &
"for I=step:step:(2*PI), myarray=[myarray,sin(I)];                  ", &
"                                                                   ", &
"//                                                                 ", &
"// read file if environment variable is set                        ", &
"//                                                                 ", &
"MATRC=getenv('MATRC')                                              ", &
"if MATRC <> ' ', ...                                               ", &
"   display('reading your personal configuration file'), ...        ", &
"   exec(getenv('MATRC')), ...                                      ", &
"end;                                                               ", &
"                                                                   ", &
"//                                                                 ", &
"// display information if environment variable VERBOSE='TRUE'      ", &
"//                                                                 ", &
"if getenv('VERBOSE') = 'TRUE', ...                                 ", &
"   plot(myarray);                                                  ", &
"   display(['TITLE IS ' title]); ...                               ", &
"   display(['HOME IS  ' getenv('HOME')]); ...                      ", &
"   A, myarray, long; PI, short; ...                                ", &
"   who ...                                                         ", &
"end                                                                ", &
""]
character(len=:),allocatable :: mytitle
integer :: ierr

   open(file='scr_test2.mat',newunit=lun)
   write(lun,'(a)')sample
   close(lun)

   ! read the test file
   !WORKS!call laff( 'title="set the title with a command";')
   call put_into_laff('title','from a put',ierr=ierr)
   write(*,*)'IERR=',ierr
   !call laff( 'exec("scr_test2.mat");')
   ! get some data now set in laff()
   call get_from_laff('title',mytitle,ierr=ierr)
   write(*,*)'In program title is now ['//trim(mytitle)//']'
   call laff()

   open(file='scr_test2.mat',newunit=lun)
   close(lun,status='delete')

end program config_file
