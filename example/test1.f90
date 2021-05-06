program bigmat
use m_matrix, only : mat88
   call mat88(4000) ! quiet initialization
   call mat88( 'a=<1 2 3 4; 5 6 7 8>;quit')
   call mat88( 'save("file1");quit')
   call mat88( 'help INTRO;quit')
   call mat88( ' ')
end program bigmat
