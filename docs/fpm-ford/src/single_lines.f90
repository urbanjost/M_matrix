program bigmat
use m_matrix, only : lala
   call lala(4000) ! small memory footprint initialization
   call lala( 'a=<1 2 3 4; 5 6 7 8>;quit')
   call lala( 'save(''file1'');quit')
   call lala( 'help;quit')
   call lala( ' ')
end program bigmat
