program bigmat
use m_matrix, only : mat88
   call mat88(-1,' ') ! quiet initialization
   call mat88( 2,'a=<1 2 3 4; 5 6 7 8>;quit')
   call mat88( 1,'save("file1");quit')
   call mat88( 2,'help INTRO;quit')
   call mat88( 1,' ')
end program bigmat
