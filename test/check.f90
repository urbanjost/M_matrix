program testit
use M_matrix, only : mat88, mat88_get, mat88_put
implicit none
integer,parameter :: lda=10
integer           :: m,n, i,j, ierr
doubleprecision   :: arr(lda,lda),x(lda,lda)
logical           :: logs
   logs=.false.
   call test_magic()
   call test_ones()
   call test_zeros()
contains

subroutine test_magic()
   call mat88(-1,'') ! iquiet initialization of  mat88().
   call mat88( 2,'tally=[0];N=10') 
   if(logs)call mat88( 2,'diary("magic.log");') 
   call mat88( 2,'a=magic(N);') 
   call mat88( 2,'b=sum(a);')     
   call mat88( 2,&
   & 'if size(a) = [N,N] ,&
   &    display("magic SIZE OK");&
   &    tally=[tally,0];&
   & else,&
   &    display("magic SIZE BAD");&
   &    size(a),&
   &    tally=[tally,1];')     
   call mat88( 2,&
   & 'if b = 5050, &
   &    display("magic SUM OK"), &
   &    tally=[tally,0]; &
   & else, &
   &    display("magic SUM FAILED"); &
   &    size(a), &
   &    tally=[tally,1]; &
   & end')
   call mat88( 2,'if sum(tally) = 0,display("magic PASSED"),else,display("magic FAILED");tally')
end subroutine test_magic

subroutine test_ones()
   call mat88(-1,'') ! iquiet initialization of  mat88().
   call mat88( 2,'tally=[0];') 
   if(logs)call mat88( 2,'diary("ones.log");') 
   call mat88( 2,'a=ones(30,40);') 
   call mat88( 2,'b=sum(a);') 
   call mat88( 2, &
   & 'if b = 1200,display("ones SUM OK"),tally=[tally,0];else,display("ones SUM FAILED");size(a),tally=[tally,1];end')
   call mat88( 2,&
   & 'if size(a) = [30,40] ,display("ones SIZE OK");tally=[tally,0];else,display("ones SIZE BAD");size(a),tally=[tally,1];')     
   call mat88( 2,&
   & 'if sum(a-ones(30,40)) = 0, &
   &    display("ones DELTA OK"), &
   &    tally=[tally,0]; &
   & else, &
   &    display("ones DELTA FAILED"); &
   &    tally=[tally,1]; &
   & end')
   call mat88( 2,'if sum(tally) = 0,display("ones PASSED"),else,display("ones FAILED");tally')
end subroutine test_ones

subroutine test_zeros()
   call mat88(-1,'') ! iquiet initialization of  mat88().
   call mat88( 2,'tally=[0];') 
   if(logs)call mat88( 2,'diary("zeros.log");') 
   call mat88( 2,'a=zeros(30,40);') 
   call mat88( 2,'b=sum(a);') 
   call mat88( 2, &
   & 'if b = 0,display("zeros SUM OK"),tally=[tally,0];else,display("zeros SUM FAILED");size(a),tally=[tally,1];end')
   call mat88( 2,&
   & 'if size(a) = [30,40] ,display("zeros SIZE OK");tally=[tally,0];else,display("zeros SIZE BAD");size(a),tally=[tally,1];')     
   call mat88( 2,&
   & 'if sum(a-zeros(30,40)) = 0, &
   &    display("zeros DELTA OK"), &
   &    tally=[tally,0]; &
   & else, &
   &    display("zeros DELTA FAILED"); &
   &    tally=[tally,1]; &
   & end')
   call mat88( 2,'if sum(tally) = 0,display("zeros PASSED"),else,display("zeros FAILED");tally')
end subroutine test_zeros

end program testit
