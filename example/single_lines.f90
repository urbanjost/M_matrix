program bigmat
use m_matrix, only : laff
   call laff(4000) ! small memory footprint initialization
   call laff( 'a=<1 2 3 4; 5 6 7 8>;quit')
   call laff( 'save("file1");quit')
   call laff( 'help;quit')
   call laff( ' ')
end program bigmat
