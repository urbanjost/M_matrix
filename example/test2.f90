program bigmat
use m_matrix, only : mat88
   call mat88(-1,' ')
   call mat88( 2,'a=<1 2 3 4; 5 6 7 8>')
   call mat88( 2,'save("file1")')
   call mat88( 2,'what')
   call mat88( 2,'who')
   stop
end
