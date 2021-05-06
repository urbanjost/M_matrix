program testit
use M_matrix, only : mat88, mat88_get, mat88_put
implicit none
integer,parameter :: lda=10
integer           :: m,n, i,j, ierr
doubleprecision   :: arr(lda,lda),x(lda,lda)
logical           :: logs
   logs=.true.
   logs=.false.
   call test_abs ()     ! abs   abs(X) is the absolute value, or complex modulus, of the
   call test_ans ()     ! ans   Variable created automatically when expressions are not
   call test_atan ()    ! atan  atan(X) is the arctangent of X . See HIGH .
   call test_base ()    ! base  base(X,B) is a vector containing the base B representation
   call test_char ()    ! char  "char(K)" requests an input line containing a single
   call test_chol ()    ! chol  Cholesky factorization. "chol(X)" uses only the diagonal
   call test_chop ()    ! chop  Truncate arithmetic. "chop(P)" causes P places to be chopped
   call test_clear ()   ! clear  Erases all variables, except "eps", "flop", "eye" and "rand".
   call test_cond ()    ! cond  Condition number in 2-norm. "cond(X)" is the ratio of the
   call test_conjg ()   ! conjg  "conjg(X)" is the complex conjugate of X .
   call test_cos ()     ! cos   cos(X) is the cosine of X . See HIGH .
   call test_debug ()   ! debug  "debu(1)" turns on verbose low-level debugging for the developer,
   call test_det ()     ! det   "det(X)" is the determinant of the square matrix X .
   call test_diag ()    ! diag  If V is a row or column vector with N components,
   call test_diary ()   ! diary  "diary('file')" causes a copy of all subsequent terminal input and
   call test_display () ! display  "display(X)" prints X in a compact format.
   call test_doc ()     ! doc   does nothing at the moment
   call test_eig ()     ! eig   Eigenvalues and eigenvectors.
   call test_else ()    ! else  Used with "if".
   call test_end ()     ! end   Terminates the scope of "for", "while" and "if" statements.
   call test_eps ()     ! eps   Floating point relative accuracy. A permanent variable
   call test_exec ()    ! exec  "exec('file',k)" obtains subsequent MAT88 input from an
   call test_exit ()    ! exit  Causes termination of a "for" or "while" loop.
   call test_exp ()     ! exp   exp(X) is the exponential of X , e to the X . See HIGH.
   call test_eye ()     ! eye   Identity matrix. "eye(N)" is the N by N identity matrix.
   call test_flops ()   ! flops  Count of floating point operations.
   call test_for ()     ! for   Repeat statements a specific number of times.
   call test_help ()    ! help  topic|SECTION_NAME
   call test_hess ()    ! hess  Hessenberg form. The Hessenberg form of a matrix is zero
   call test_if ()      ! if    Conditionally execute statements
   call test_imag ()    ! imag  "imag(X)" is the imaginary part of X .
   call test_invh ()    ! invh  Inverse Hilbert matrix. "invh(N)" is the inverse of a N_by_N
   call test_inv ()     ! inv   "inv(X)" is the inverse of the square matrix X . A warning
   call test_kron ()    ! kron  "kron(X,Y)" is the Kronecker tensor product of X and Y. It
   call test_lala ()    ! lala  A placeholder for a new command.
   call test_lines ()   ! lines  An internal count is kept of the number of lines of output
   call test_load ()    ! load  "load('file')" retrieves all the variables from the file .
   call test_log ()     ! log   log(X) is the natural logarithm of X. See HIGH.
   call test_long ()    ! long   See "short" also.
   call test_lu ()      ! lu    Factors from Gaussian elimination. <L,U> = LU(X) stores a
   call test_magic ()   ! magic  Magic square. "magic(N)" is an N by N matrix constructed
   call test_norm ()    ! norm  For matrices..
   call test_ones ()    ! ones  All ones. "ones(N)" is an N by N matrix of ones. "ones(M,N)"
   call test_orth ()    ! orth  Orthogonalization. "Q = orth(X)" is a matrix with
   call test_pinv ()    ! pinv  Pseudoinverse.
   call test_plot ()    ! plot  "plot(X,Y)" produces a plot of the elements of Y against
   call test_poly ()    ! poly  Characteristic polynomial.
   call test_print ()   ! print  "print('file',X)" prints X on the file using the current
   call test_prod ()    ! prod  "prod(X)" is the product of all the elements of X .
   call test_qr ()      ! qr    Orthogonal-triangular decomposition.  "<Q,R> = qr(X)" produces an
   call test_quit ()    ! quit  From the terminal, causes return to the operating system
   call test_rand ()    ! rand  Random numbers and matrices. "rand(N)" is an N by N matrix
   call test_rank ()    ! rank  Rank. "K = rank(X)" is the number of singular values of X
   call test_rat ()     ! rat   An experimental function which attempts to remove the
   call test_rcond ()   ! rcond  "rcond(X)" is an estimate for the reciprocal of the
   call test_real ()    ! real  "real(X)" is the real part of X.
   call test_roots ()   ! roots  Find polynomial roots. "roots(C)" computes the roots of the
   call test_round ()   ! round  "round(X)" rounds the elements of X to the nearest integers.
   call test_rref ()    ! rref  "rref(A)" is the reduced row echelon form of the rectangular
   call test_save ()    ! save  "save('file')" stores all the current variables in a file.
   call test_schur ()   ! schur  Schur decomposition. "<U,T> = schur(X)" produces an upper
   call test_semi ()    ! semi  "semi" toggles the action of semicolons at the end of lines.
   call test_short ()   ! short  See "long" also.
   call test_sh ()      ! sh    Starts the command shell interactively, using the command defined by
   call test_sin ()     ! sin   sin(X) is the sine of X. See HIGH.
   call test_size ()    ! size  If X is an M by N matrix, then size(X) is <M, N> .
   call test_sqrt ()    ! sqrt  sqrt(X) is the square root of X. See HIGH. Complex
   call test_sum ()     ! sum   "sum(X)" is the sum of all the elements of X.
   call test_svd ()     ! svd   Singular value decomposition. "<U,S,V> = svd(X)" produces a
   call test_tril ()    ! tril  Lower triangle. "tril(X)" is the lower triangular part of X.
   call test_triu ()    ! triu  Upper triangle. "triu(X)" is the upper triangular part of X.
   call test_user ()    ! user  Allows personal Fortran subroutines to be linked into
   call test_what ()    ! what  does nothing for now
   call test_while ()   ! while  Repeat statements an indefinite number of times.
   call test_who ()     ! who   Lists current variables.
   call test_zeros ()   ! zeros
!       ! call test_{ ()
!       ! call test_} ()
!       ! call test_) () (     ( ) or { } are used to indicate precedence in arithmetic expressions
!       ! call test_[ ()
!       ! call test_] ()
!       ! call test_> () <     < > or [ ] are brackets used in forming vectors and matrices.
!       ! call test_= () =     Used in assignment statements and to mean equality in "while"
!       ! call test_; () ;     Used inside brackets to end rows.
!       ! call test_, () ,     Used to separate matrix subscripts and function arguments.
!       ! call test_' () '     Transpose. X' is the complex conjugate transpose of X .
!       ! call test_/ () /     Slash or matrix right division. B/A is roughly the same
!       ! call test_- () -     Subtraction. X - Y . X and Y must have the same
!       ! call test_\ () \     Backslash or matrix left division. A\B is roughly the
!       ! call test_+ () +     Addition. X + Y . X and Y must have the same dimensions.
!       ! call test_: () :     Colon. Used in subscripts, "for" iterations and possibly
!       ! call test_. () .     Decimal point. 314/100, 3.14 and .314E1 are all the
!       ! call test_* () *     Matrix multiplication, X*Y . Any scalar (1 by 1 matrix)
!       ! call test_< ()
!       ! call test_( ()
!       ! !     If the first character of a line the rest of the line is
!       ! }     see "(".
!       ! )     See "(" .
!       ! [     See "<"
!       ! ]     See "<"
!       ! >     See "<" . Also see MACROS.
!       ! {     see "(".
contains
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_magic()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help magic')
   call mat88( 'tally=[0];N=10')
   if(logs)call mat88( 'diary("magic.log");')
   call mat88( 'a=magic(N);')
   call mat88( 'b=sum(a);')
   call mat88( &
   & 'if size(a) = [N,N] ,&
   &    display("magic SIZE OK");&
   &    tally=[tally,0];&
   & else,&
   &    display("magic SIZE BAD");&
   &    size(a),&
   &    tally=[tally,1];')
   call mat88( &
   & 'if b = 5050, &
   &    display("magic SUM OK"), &
   &    tally=[tally,0]; &
   & else, &
   &    display("magic SUM FAILED"); &
   &    size(a), &
   &    tally=[tally,1]; &
   & end')
   call mat88( 'if sum(tally) = 0,display("magic PASSED"),else,display("magic FAILED");tally')
end subroutine test_magic
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_ones()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help ones')
   call mat88( 'tally=[0];')
   if(logs)call mat88( 'diary("ones.log");')
   call mat88( 'a=ones(30,40);')
   call mat88( 'b=sum(a);')
   call mat88(  &
   & 'if b = 1200,display("ones SUM OK"),tally=[tally,0];else,display("ones SUM FAILED");size(a),tally=[tally,1];end')
   call mat88( &
   & 'if size(a) = [30,40] ,display("ones SIZE OK");tally=[tally,0];else,display("ones SIZE BAD");size(a),tally=[tally,1];')
   call mat88( &
   & 'if sum(a-ones(30,40)) = 0, &
   &    display("ones DELTA OK"), &
   &    tally=[tally,0]; &
   & else, &
   &    display("ones DELTA FAILED"); &
   &    tally=[tally,1]; &
   & end')
   call mat88( 'if sum(tally) = 0,display("ones PASSED"),else,display("ones FAILED");tally')
end subroutine test_ones
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_zeros()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help zeros')
   call mat88( 'tally=[0];')
   if(logs)call mat88( 'diary("zeros.log");')
   call mat88( 'a=zeros(30,40);')
   call mat88( 'b=sum(a);')
   call mat88(  &
   & 'if b = 0,display("zeros SUM OK"),tally=[tally,0];else,display("zeros SUM FAILED");size(a),tally=[tally,1];end')
   call mat88( &
   & 'if size(a) = [30,40] ,display("zeros SIZE OK");tally=[tally,0];else,display("zeros SIZE BAD");size(a),tally=[tally,1];')
   call mat88( &
   & 'if sum(a-zeros(30,40)) = 0, &
   &    display("zeros DELTA OK"), &
   &    tally=[tally,0]; &
   & else, &
   &    display("zeros DELTA FAILED"); &
   &    tally=[tally,1]; &
   & end')
   call mat88( 'if sum(tally) = 0,display("zeros PASSED"),else,display("zeros FAILED");tally')
end subroutine test_zeros
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_sum()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help sum')
   call mat88( 'tally=[0];')
   if(logs)call mat88( 'diary("sum.log");')
   call mat88( 'a=<1 2 3; 4 5 6; 7 8 9>;')
   call mat88( 'b=sum(magic(3));')
   call mat88( 'c=sum(a);')
   call mat88(  &
   & 'if c = 45,display("sum SUM OF ""a"" OK"),tally=[tally,0];else,display("sum SUM OF ""a"" FAILED");size(a),tally=[tally,1];end')
   call mat88( &
   & 'if size(c) = [1,1] ,display("sum SIZE OK");tally=[tally,0];else,display("sum SIZE BAD");size(a),tally=[tally,1];')
   call mat88( &
   & 'if sum(a) + b = 90, &
   &    display("sum ARRAY OK"), &
   &    tally=[tally,0]; &
   & else, &
   &    display("sum ARRAY FAILED"); &
   &    tally=[tally,1]; &
   & end')
   call mat88( 'if sum(tally) = 0,display("sum PASSED"),else,display("sum FAILED");tally')
end subroutine test_sum
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_abs ()
   call mat88( "display(ones(80,1)'*46)")
   if(logs)call mat88( 'diary("abs.log");')
   call mat88( [ character(len=256) :: &
     & 'help abs                                                                 ', &
     & 'tally=[0];                                                               ', &
     & 'a=<1 2 3; 4 5 6; 7 8 9>;b=-a;                                            ', &
     & 'if a+b=zeros(a), tally=[tally,0];display("a-b is zero       ");else,tally=[tally,1];display("a-b is NOT zero");      ', &
     & 'if sum(b)=-45,   tally=[tally,0];display("expected sum is OK");else,tally=[tally,1];display("unexpected sum"),sum(b);', &
     & 'if a=-b,         tally=[tally,0];display("a = -b as expected");else,tally=[tally,1];display("a is NOT equal to -b"); ', &
     & 'if sum(tally)=0,display("abs PASSED");else,display("abs FAILED");tally ', &
     & ''])
end subroutine test_abs
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_ans ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help ans')
   call mat88( 'tally=[0];')
end subroutine test_ans
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_atan ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help atan')
   call mat88( 'tally=[0];')
end subroutine test_atan
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_base ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help base')
   call mat88( 'tally=[0];')
end subroutine test_base
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_char ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help char')
   call mat88( 'tally=[0];')
end subroutine test_char
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_chol ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help chol')
   call mat88( 'tally=[0];')
end subroutine test_chol
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_chop ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help chop')
   call mat88( 'tally=[0];')
end subroutine test_chop
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_clear ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help clear')
   call mat88( 'tally=[0];')
end subroutine test_clear
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_cond ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help cond')
   call mat88( 'tally=[0];')
end subroutine test_cond
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_conjg ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help conjg')
   call mat88( 'tally=[0];')
end subroutine test_conjg
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_cos ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help cos')
   call mat88( 'tally=[0];')
end subroutine test_cos
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_debug ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help debug')
   call mat88( 'tally=[0];')
end subroutine test_debug
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_det ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help det')
   call mat88( 'tally=[0];')
end subroutine test_det
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_diag ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help diag')
   call mat88( 'tally=[0];')
end subroutine test_diag
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_diary ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help diary')
   call mat88( 'tally=[0];')
end subroutine test_diary
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_display ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help display')
   call mat88( 'tally=[0];')
end subroutine test_display
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_doc ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help doc')
   call mat88( 'tally=[0];')
end subroutine test_doc
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_eig ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help eig')
   call mat88( 'tally=[0];')
end subroutine test_eig
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_else ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help else')
   call mat88( 'tally=[0];')
end subroutine test_else
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_end ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help end')
   call mat88( 'tally=[0];')
end subroutine test_end
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_eps ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help eps')
   call mat88( 'tally=[0];')
end subroutine test_eps
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_exec ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help exec')
   call mat88( 'tally=[0];')
end subroutine test_exec
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_exit ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help exit')
   call mat88( 'tally=[0];')
end subroutine test_exit
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_exp ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help exp')
   call mat88( 'tally=[0];')
end subroutine test_exp
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_eye ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help eye')
   call mat88( 'tally=[0];')
end subroutine test_eye
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_flops ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help flops')
   call mat88( 'tally=[0];')
end subroutine test_flops
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_for ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help for')
   call mat88( 'tally=[0];')
end subroutine test_for
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_help ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help help')
   call mat88( 'tally=[0];')
end subroutine test_help
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_hess ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help hess')
   call mat88( 'tally=[0];')
end subroutine test_hess
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_if ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help if')
   call mat88( 'tally=[0];')
end subroutine test_if
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_imag ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help imag')
   call mat88( 'tally=[0];')
end subroutine test_imag
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_invh ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help invh')
   call mat88( 'tally=[0];')
end subroutine test_invh
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_inv ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help inv')
   call mat88( 'tally=[0];')
end subroutine test_inv
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_kron ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help kron')
   call mat88( 'tally=[0];')
end subroutine test_kron
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_lala ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help lala')
   call mat88( 'tally=[0];')
end subroutine test_lala
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_lines ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help lines')
   call mat88( 'tally=[0];')
end subroutine test_lines
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_load ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help load')
   call mat88( 'tally=[0];')
end subroutine test_load
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_log ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help log')
   call mat88( 'tally=[0];')
end subroutine test_log
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_long ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help long')
   call mat88( 'tally=[0];')
end subroutine test_long
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_lu ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help lu')
   call mat88( 'tally=[0];')
end subroutine test_lu
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_norm ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help norm')
   call mat88( 'tally=[0];')
end subroutine test_norm
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_orth ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help orth')
   call mat88( 'tally=[0];')
end subroutine test_orth
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_pinv ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help pinv')
   call mat88( 'tally=[0];')
end subroutine test_pinv
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_plot ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help plot')
   call mat88( 'tally=[0];')
end subroutine test_plot
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_poly ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help poly')
   call mat88( 'tally=[0];')
end subroutine test_poly
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_print ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help print')
   call mat88( 'tally=[0];')
end subroutine test_print
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_prod ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help prod')
   call mat88( 'tally=[0];')
end subroutine test_prod
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_qr ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help qr')
   call mat88( 'tally=[0];')
end subroutine test_qr
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_quit ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help quit')
   call mat88( 'tally=[0];')
end subroutine test_quit
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_rand ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help rand')
   call mat88( 'tally=[0];')
end subroutine test_rand
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_rank ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help rank')
   call mat88( 'tally=[0];')
end subroutine test_rank
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_rat ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help rat')
   call mat88( 'tally=[0];')
end subroutine test_rat
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_rcond ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help rcond')
   call mat88( 'tally=[0];')
end subroutine test_rcond
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_real ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help real')
   call mat88( 'tally=[0];')
end subroutine test_real
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_roots ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help roots')
   call mat88( 'tally=[0];')
end subroutine test_roots
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_round ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help round')
   call mat88( 'tally=[0];')
end subroutine test_round
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_rref ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help rref')
   call mat88( 'tally=[0];')
end subroutine test_rref
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_save ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help save')
   call mat88( 'tally=[0];')
end subroutine test_save
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_schur ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help schur')
   call mat88( 'tally=[0];')
end subroutine test_schur
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_semi ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help semi')
   call mat88( 'tally=[0];')
end subroutine test_semi
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_short ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help short')
   call mat88( 'tally=[0];')
end subroutine test_short
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_sh ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help sh')
   call mat88( 'tally=[0];')
end subroutine test_sh
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_sin ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help sin')
   call mat88( 'tally=[0];')
end subroutine test_sin
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_size ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help size')
   call mat88( 'tally=[0];')
end subroutine test_size
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_sqrt ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help sqrt')
   call mat88( 'tally=[0];')
end subroutine test_sqrt
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_svd ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help svd')
   call mat88( 'tally=[0];')
end subroutine test_svd
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_tril ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help tril')
   call mat88( 'tally=[0];')
end subroutine test_tril
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_triu ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help triu')
   call mat88( 'tally=[0];')
end subroutine test_triu
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_user ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help user')
   call mat88( 'tally=[0];')
end subroutine test_user
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_what ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help what')
   call mat88( 'tally=[0];')
end subroutine test_what
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_while ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help while')
   call mat88( 'tally=[0];')
end subroutine test_while
!-------------------------------------------------------------------------------------------------------------------------------------
subroutine test_who ()
   call mat88( "display(ones(80,1)'*46)")
   call mat88( 'help who')
   call mat88( 'tally=[0];')
end subroutine test_who
!-------------------------------------------------------------------------------------------------------------------------------------
end program testit
