program config_file
use M_matrix, only : lala, get_from_lala, put_into_lala, ifin_lala
! create contents for a test file
character(len=*),parameter:: sample(*)=[character(len=80) :: &
"// my data definition file                                         ", &
"                                                                   ", &
"title='this is my title from the file';                            ", &
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


   ! preset a LALA variable using a command
   call lala( 'title2="set with a command";')
   ! preset a LALA variable using a PUT
   call put_into_lala('set with a PUT','from a put',ierr=ierr)
   write(*,*)'IERR=',ierr

   ! load file. Note the "return" is currently required
   call lala(" exec('scr_test2.mat');return;")

   ! get some data now set in lala()
   call get_from_lala('title',mytitle,ierr=ierr)
   write(*,*)'In program title is now ['//trim(mytitle)//']'

   ! interactively enter LALA
   call lala()

   open(file='scr_test2.mat',newunit=lun)
   close(lun,status='delete')

end program config_file
