program bigmat
use m_matrix, only : mat88
   call mat88( 'a=<1 2 3 4; 5 6 7 8>')
   call mat88( 'save("file1")')
   call mat88( 'what')
   call mat88( 'who')
   stop
end
