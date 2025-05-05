program testit
use M_matrix, only : lala, get_from_lala, put_into_lala
implicit none
logical           :: logs=.false.
integer           :: sumtally

   !!logs=.true.
   logs=.false.
   !!call lala(20000,echo=.true.)

   if(logs)call lala( 'diary(''test_suite_M_matrix.log'');')
   logs=.false.

   call lala( [ character(len=256) :: &
     & 'sumtally=0;', &
     & 'who'])
      call test_abs ()     ! abs   abs(X) is the absolute value, or complex modulus, of the
      call test_all ()     ! all   all(X) returns 1 if all elmements are non-zero.
   call test_ans ()     ! ans   Variable created automatically when expressions are not
      call test_any ()     ! any   any(X) returns 1 if any elmement is non-zero,
      call test_atan ()    ! atan  atan(X) is the arctangent of X . See HIGH .
   call test_base ()    ! base  base(X,B) is a vector containing the base B representation
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
   call test_display () ! display  'display(X)" prints X in a compact format.
   call test_delete ()  ! delete delete named file
   call test_eig ()     ! eig   Eigenvalues and eigenvectors.
   call test_else ()    ! else  Used with "if".
   call test_end ()     ! end   Terminates the scope of "for", "while" and "if" statements.
      call test_eps ()     ! eps   Floating point relative accuracy. A permanent variable
   call test_lt ()      ! lt   elementally return true(1) or false(0) for relational operator
   call test_le ()      ! le   elementally return true(1) or false(0) for relational operator
   call test_eq ()      ! eq   elementally return true(1) or false(0) for relational operator
   call test_ge ()      ! ge   elementally return true(1) or false(0) for relational operator
   call test_gt ()      ! gt   elementally return true(1) or false(0) for relational operator
   call test_ne ()      ! ne   elementally return true(1) or false(0) for relational operator
      call test_maxval ()  ! maxval  maximum real value
      call test_minval ()  ! minval  minimum real value
      call test_maxloc ()  ! maxloc  location of maximum real value
      call test_minloc ()  ! minloc  location of minimum real value
   call test_exec ()    ! exec  "exec('file',k)" obtains subsequent LALA input from an
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
      call test_pow ()     ! pow   "pow(X,P)" raises each elment of X to the power P
   call test_print ()   ! print  "print('file',X)" prints X on the file using the current
      call test_prod ()    ! prod  "prod(X)" is the product of all the elements of X .
   call test_qr ()      ! qr    Orthogonal-triangular decomposition.  "<Q,R> = qr(X)" produces an
   call test_quit ()    ! quit  From the terminal, causes return to the operating system
   call test_randu ()   ! randu  uniform Random numbers and matrices. "randu(N)" is an N by N matrix
   call test_randn ()   ! randn  normal distribution of Random numbers and matrices. "randn(N)" is an N by N matrix
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
      call test_shape ()    ! shape  If X is an M by N matrix, then shape(X) is <M, N> .
   call test_sqrt ()    ! sqrt  sqrt(X) is the square root of X. See HIGH. Complex
      call test_sum ()     ! sum   "sum(X)" is the sum of all the elements of X.
   call test_svd ()     ! svd   Singular value decomposition. "<U,S,V> = svd(X)" produces a
   call test_tril ()    ! tril  Lower triangle. "tril(X)" is the lower triangular part of X.
   call test_triu ()    ! triu  Upper triangle. "triu(X)" is the upper triangular part of X.
   call test_user ()    ! user  Allows personal Fortran subroutines to be linked into
   call test_while ()   ! while  Repeat statements an indefinite number of times.
   call test_who ()     ! who   Lists current variables.

   call test_doc ()     ! doc   does nothing at the moment
   call test_what ()    ! what  does nothing for now
   call test_lala ()    ! lala  A placeholder for a new command.

      call test_zeros ()   ! zeros
      call test_general_avg ()
      call test_general_expr()     ! basic expressions
      call test_general_dots()     ! basic expressions
      call test_general_pascal()   ! basic expressions
   call test_general_char ()       ! basic string manipulations
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
!       ! call test_\ () \     Backslash or matrix left division. A\B is roughly the
      call test_addition_and_subtraction()    ! Addition. X + Y . X and Y must have the same dimensions unless scalar.
                                              ! Subtraction. X - Y . X and Y must have the same dimensions unless scalar.
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

      call test_set_theory ()   ! package from M_sets

   write(*,*)'TOTAL FAILED:',abs(sumtally)
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_magic()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help magic')
   call lala( 'tally=[0];N=10')
   if(logs)call lala( 'diary(''magic.log'');')
   call lala( 'a=magic(N);')
   call lala( 'b=sum(a);')
   call lala( &
   & 'display(ones(80,1)''*95);       &
   & if all(eq(shape(a),[N,N]))=1,    &
   &    display(''magic shape OK'');  &
   &    tally=[tally,0];              &
   & else,                            &
   &    display(''magic shape BAD''); &
   &    shape(a),                     &
   &    tally=[tally,1];             ')
   call lala( &
   & 'if b = 5050,                     &
   &    display(''magic SUM OK''),     &
   &    tally=[tally,0];               &
   & else,                             &
   &    display(''magic SUM FAILED''); &
   &    shape(a),                      &
   &    tally=[tally,1];               &
   & end                              ')
   call lala( 'if sum(tally) = 0,display(''magic PASSED''),else,display(''magic FAILED'');tally')
   call wrapup()
end subroutine test_magic
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_ones()
   call lala( 'display(ones(80,1)''*61); help ones; display(ones(80,1)''*95)')
   call lala( 'tally=[0];')
   if(logs)call lala( 'diary(''ones.log'');')
   call lala( 'a=ones(30,40);')
   call lala( 'b=sum(a);')
   call lala(  &
   & 'if b = 1200,display(''ones SUM OK''),tally=[tally,0];else,display(''ones SUM FAILED'');shape(a),tally=[tally,1];end')
   call lala( &
   & 'if all(eq(shape(a),[30,40]))=1 ,display(''ones shape OK'');tally=[tally,0];&
   & else,display(''ones shape BAD'');shape(a),tally=[tally,1];')
   call lala( &
   & 'if sum(a-ones(30,40)) = 0, &
   &    display(''ones DELTA OK''), &
   &    tally=[tally,0]; &
   & else, &
   &    display(''ones DELTA FAILED''); &
   &    tally=[tally,1]; &
   & end')
   call lala( 'if sum(tally) = 0,display(''ones PASSED''),else,display(''ones FAILED'');tally')
   call wrapup()
end subroutine test_ones
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_zeros()
  call lala( 'display(ones(80,1)''*61); help zeros; display(ones(80,1)''*95)')
  call lala( 'tally=[0];')
  if(logs)call lala( 'diary(''zeros.log'');')
  call lala( 'a=zeros(30,40);')
  call lala( 'b=sum(a);')
  call lala(  &
  & 'if b = 0,display(''zeros SUM OK''),tally=[tally,0];else,display(''zeros SUM FAILED'');shape(a),tally=[tally,1];end')
  call lala( &
  & 'if all(eq(shape(a),[30,40]))=1 ,display(''zeros shape OK'');tally=[tally,0];&
  & else,display(''zeros shape BAD'');shape(a),tally=[tally,1];')
  call lala( &
  & 'if sum(a-zeros(30,40)) = 0, &
  &    display(''zeros DELTA OK''), &
  &    tally=[tally,0]; &
  & else, &
  &    display(''zeros DELTA FAILED''); &
  &    tally=[tally,1]; &
  & end')
  call lala( 'if sum(tally) = 0,display(''zeros PASSED''),else,display(''zeros FAILED'');tally')
  call wrapup()
end subroutine test_zeros
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_pow()
  call lala( 'display(ones(80,1)''*61); help pow; display(ones(80,1)''*95)')
  call lala( 'tally=[0];')
  if(logs)call lala( 'diary(''pow.log'');')
  call lala( 'a=<1 2 3; 4 5 6; 7 8 9>;')
  call lala( 'b=magic(3);')
  call lala( 'c=pow(b,3);')
  call lala('answ=[ 512 1 216 ; 27 125 343 ; 64 729 8 ]')
  call lala(  &
  & "if all(eq(round(c),answ))=1,display('pow OF ''a'' OK'),tally=[tally,0];&
  & else,display('pow OF ''a'' FAILED');shape(a),tally=[tally,1];end")
  call lala( &
& 'if all(eq(shape(c),[3,3]))=1,display(''pow shape OK'');tally=[tally,0];&
& else,display(''pow shape BAD'');shape(c),tally=[tally,1];')
  call lala( &
  & 'if all(eq(round(pow(a,2)),a.*a))=1, &
  &    display(''pow ARRAY OK''),        &
  &    tally=[tally,0];                  &
  & else,                                &
  &    display(''pow ARRAY FAILED'');    &
  &    tally=[tally,1];                  &
  & end')
  call lala( 'if sum(tally) = 0,display(''pow PASSED''),else,display(''pow FAILED'');tally')
  call wrapup()
end subroutine test_pow
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_lt()
  call lala( 'display(ones(80,1)''*61); help lt; display(ones(80,1)''*95)')
  call lala( 'tally=[0];')
  if(logs)call lala( 'diary(''lt.log'');')
  call lala( 'a=<1 2 3; 4 5 6; 7 8 9>;')
  call lala( 'b=magic(3);')
  call lala( 'c=lt(a,b);')
  call lala( &
& 'if all(eq(shape(c),[3,3]))=1,display(''lt shape OK'');tally=[tally,0];else,display(''lt shape BAD'');shape(c),tally=[tally,1];')
  call lala( 'if sum(tally) = 0,display(''lt PASSED''),else,display(''lt FAILED'');tally')
  call wrapup()
end subroutine test_lt
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_le()
  call lala( 'display(ones(80,1)''*61); help le; display(ones(80,1)''*95)')
  call lala( 'tally=[0];')
  if(logs)call lala( 'diary(''le.log'');')
  call lala( 'a=<1 2 3; 4 5 6; 7 8 9>;')
  call lala( 'b=magic(3);')
  call lala( 'c=le(a,b);')
  call lala( &
& 'if all(eq(shape(c),[3,3]))=1,display(''le shape OK'');tally=[tally,0];else,display(''le shape BAD'');shape(c),tally=[tally,1];')
  call lala( 'if sum(tally) = 0,display(''le PASSED''),else,display(''le FAILED'');tally')
  call wrapup()
end subroutine test_le
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_eq()
  call lala( 'display(ones(80,1)''*61); help eq; display(ones(80,1)''*95)')
  call lala( 'tally=[0];')
  if(logs)call lala( 'diary(''eq.log'');')
  call lala( 'a=<1 2 3; 4 5 6; 7 8 9>;')
  call lala( 'b=magic(3);')
  call lala( 'c=eq(a,b);')
  call lala( &
& 'if all(eq(shape(c),[3,3]))=1,display(''eq shape OK'');tally=[tally,0];else,display(''eq shape BAD'');shape(c),tally=[tally,1];')
!  call lala( &
!  & 'if eq(a,b) + b = 90, &
!  &    display(''eq ARRAY OK''), &
!  &    tally=[tally,0]; &
!  & else, &
!  &    display(''eq ARRAY FAILED''); &
!  &    tally=[tally,1]; &
!  & end')
  call lala( 'if sum(tally) = 0,display(''eq PASSED''),else,display(''eq FAILED'');tally')
  call wrapup()
end subroutine test_eq
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_ge()
  call lala( 'display(ones(80,1)''*61); help ge; display(ones(80,1)''*95)')
  call lala( 'tally=[0];')
  if(logs)call lala( 'diary(''ge.log'');')
  call lala( 'a=<1 2 3; 4 5 6; 7 8 9>;')
  call lala( 'b=magic(3);')
  call lala( 'c=ge(a,b);')
  call lala( &
& 'if all(eq(shape(c),[3,3]))=1,display(''ge shape OK'');tally=[tally,0];else,display(''ge shape BAD'');shape(c),tally=[tally,1];')
  call lala( 'if sum(tally) = 0,display(''ge PASSED''),else,display(''ge FAILED'');tally')
  call wrapup()
end subroutine test_ge
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_gt()
  call lala( 'display(ones(80,1)''*61); help gt; display(ones(80,1)''*95)')
  call lala( 'tally=[0];')
  if(logs)call lala( 'diary(''gt.log'');')
  call lala( 'a=<1 2 3; 4 5 6; 7 8 9>;')
  call lala( 'b=magic(3);')
  call lala( 'c=gt(a,b);')
  call lala( &
& 'if all(eq(shape(c),[3,3]))=1,display(''gt shape OK'');tally=[tally,0];else,display(''gt shape BAD'');shape(c),tally=[tally,1];')
  call lala( 'if sum(tally) = 0,display(''gt PASSED''),else,display(''gt FAILED'');tally')
  call wrapup()
end subroutine test_gt
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_ne()
  call lala( 'display(ones(80,1)''*61); help ne; display(ones(80,1)''*95)')
  call lala( 'tally=[0];')
  if(logs)call lala( 'diary(''ne.log'');')
  call lala( 'a=<1 2 3; 4 5 6; 7 8 9>;')
  call lala( 'b=magic(3);')
  call lala( 'c=ne(a,b);')
  call lala( &
& 'if all(eq(shape(c),[3,3]))=1,display(''ne shape OK'');tally=[tally,0];else,display(''ne shape BAD'');shape(c),tally=[tally,1];')
  call lala( 'if sum(tally) = 0,display(''ne PASSED''),else,display(''ne FAILED'');tally')
  call wrapup()
end subroutine test_ne
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_maxloc()
  call lala( 'display(ones(80,1)''*61); help maxloc; display(ones(80,1)''*95)')
  call lala( 'tally=[0];')
  if(logs)call lala( 'diary(''maxloc.log'');')
  call lala( 'a=<1 2 3; 4 5 6; 7 8 9>;')
  call lala( 'b=-magic(11);')
  call lala( 'c=maxloc(a);')
  call lala(  "if all(eq(c,[3,3]))=1,display('maxloc OF ''a'' OK'),tally=[tally,0];&
  &else,display('maxloc OF ''a'' FAILED');c,tally=[tally,1];end")
  call lala( &
& 'if all(eq(shape(c),[1,2]))=1,display(''maxloc shape OK'');tally=[tally,0];&
& else,display(''maxloc shape BAD'');shape(c),tally=[tally,1];')
  call lala( &
  &'l= maxloc(b); &
  & if b(l(1),l(2))=maxval(b), &
  &    display(''maxloc of ''''b'''' OK''), &
  &    tally=[tally,0]; &
  & else, &
  &    display(''maxloc of ''''b'''' FAILED''); &
  &    tally=[tally,1]; &
  & end')
  call lala( 'if sum(tally) = 0,display(''maxloc PASSED''),else,display(''maxloc FAILED'');tally')
  call wrapup()
end subroutine test_maxloc
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_minloc()
  call lala( 'display(ones(80,1)''*61); help minloc; display(ones(80,1)''*95)')
  call lala( 'tally=[0];')
  if(logs)call lala( 'diary(''minloc.log'');')
  call lala( 'a=<1 2 3; 4 5 6; 7 8 9>;')
  call lala( 'b=-magic(11);')
  call lala( 'c=minloc(a);')
  call lala("if all(eq(c,[1,1]))=1, display('minloc OF ''a'' OK'),tally=[tally,0];&
  &else,display('minloc OF ''a'' FAILED');c,tally=[tally,1];end")
  call lala( &
& 'if all(eq(shape(c),[1,2]))=1,display(''minloc shape OK'');tally=[tally,0];&
& else,display(''minloc shape BAD'');shape(c),tally=[tally,1];')
  call lala( &
  & 'l=minloc(b); &
  & if b(l(1),l(2))=minval(b), &
  &    display(''minloc of ''''b'''' OK''), &
  &    tally=[tally,0]; &
  & else,  &
  &    display(''minloc of ''''b'''' FAILED''); &
  &    tally=[tally,1]; &
  & end')
  call lala( 'if sum(tally) = 0,display(''minloc PASSED''),else,display(''minloc FAILED'');tally')
  call wrapup()
end subroutine test_minloc
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_minval()
  call lala( 'display(ones(80,1)''*61); help minval; display(ones(80,1)''*95)')
  call lala( 'tally=[0];')
  if(logs)call lala( 'diary(''minval.log'');')
  call lala( 'a=<1 2 3; 4 5 6; 7 8 9>;')
  call lala( 'b=-magic(11);')
  call lala( 'c=minval(a);')
  call lala(  &
  & "if c = 1,display('minval OF ''a'' OK'),tally=[tally,0];else,display('minval OF ''a'' FAILED');c,tally=[tally,1];end")
  call lala( &
& 'if all(eq(shape(c),[1,1]))=1,display(''minval shape OK'');tally=[tally,0];&
& else,display(''minval shape BAD'');shape(c),tally=[tally,1];')
  call lala( &
  & 'if minval(b)  = -121, &
  &    display(''minval ARRAY OK''), &
  &    tally=[tally,0]; &
  & else, &
  &    display(''minval ARRAY FAILED''); &
  &    tally=[tally,1]; &
  & end')
  call lala( 'if sum(tally) = 0,display(''minval PASSED''),else,display(''minval FAILED'');tally')
  call wrapup()
end subroutine test_minval
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_maxval()
  call lala( 'display(ones(80,1)''*61); help maxval; display(ones(80,1)''*95)')
  call lala( 'tally=[0];')
  if(logs)call lala( 'diary(''maxval.log'');')
  call lala( 'a=<1 2 3; 4 5 6; 7 8 9>;')
  call lala( 'b=magic(11);')
  call lala( 'c=maxval(a);')
  call lala(  &
  & "if c = 9,display('maxval OF ''a'' OK'),tally=[tally,0];else,display('maxval OF ''a'' FAILED');c,tally=[tally,1];end")
  call lala( &
& 'if all(eq(shape(c),[1,1]))=1 ,display(''maxval shape OK'');tally=[tally,0];&
& else,display(''maxval shape BAD'');shape(c),tally=[tally,1];')
  call lala( &
  & 'if maxval(b)  = 121, &
  &    display(''maxval ARRAY OK''), &
  &    tally=[tally,0]; &
  & else, &
  &    display(''maxval ARRAY FAILED''); &
  &    tally=[tally,1]; &
  & end')
  call lala( 'if sum(tally) = 0,display(''maxval PASSED''),else,display(''maxval FAILED'');tally')
  call wrapup()
end subroutine test_maxval
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_sum()
  call lala( 'display(ones(80,1)''*61); help sum; display(ones(80,1)''*95)')
  if(logs)call lala( 'diary(''sum.log'');')
  call lala( 'tally=[0];')
  call lala( 'a=<1 2 3; 4 5 6; 7 8 9>;')
  call lala( 'b=sum(magic(3));')
  call lala( 'c=sum(a);')
  call lala(  &
  & "if c = 45,display('sum: SUM OF ''a'' OK'),tally=[tally,0];&
  &else,display('sum: SUM OF ''a'' FAILED');shape(a),tally=[tally,1];end")
  call lala( &
& 'if all(eq(shape(c),[1,1]))=1 ,display(''sum: shape OK'');tally=[tally,0];&
& else,display(''sum: shape BAD'');shape(c),tally=[tally,1];')
  call lala( &
  & 'if sum(a) + b = 90, &
  &    display(''sum: SUM ARRAY OK''), &
  &    tally=[tally,0]; &
  & else, &
  &    display(''sum: SUM ARRAY FAILED''); &
  &    tally=[tally,1]; &
  & end')

  call lala( '&
  & matrix=[  1,    2,    3,    4; &
  &           10,   20,   30,   40; &
  &           100,  200,  300,  400  ]; &
  & s=sum(matrix); &
  & sumcols=sum(matrix,1); &
  & sumrows=sum(matrix,2); &
  & if s = 1110, &
  &    display(''sum: MATRIX OK''), tally=[tally,0]; &
  & else, &
  &    display(''sum: MATRIX FAILED''); tally=[tally,1]; &
  & end; &
  & if all(eq(sumcols,[111;222;333;444]))=1, &
  & display(''sum: SUM COLS OK''),tally=[tally,0];else, display(''sum: SUM COLS NOT OK''),tally=[tally,1];end;&
  & if all(eq(sumrows,[10;100;1000]))=1, &
  & display(''sum: SUM ROWS OK''),tally=[tally,0];else, display(''sum: SUM ROWS NOT OK''),tally=[tally,1];end;&
  & ')

  call lala( 'if sum(tally) = 0,display(''sum PASSED''),else,display(''sum FAILED'');tally')
  call wrapup()
end subroutine test_sum
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_any()
  call lala( 'display(ones(80,1)''*61); help any; display(ones(80,1)''*95)')
  call lala( 'tally=[0];')
  if(logs)call lala( 'diary(''any.log'');')
  call lala( [ character(len=267) :: &

'a=<0 0 0; 0 0 0; 0 0 0>;', &
'b=any(a);', &
"if b = 0,display('any(''a'') OK'),tally=[tally,0];else,display('any(''a'') FAILED');shape(a),tally=[tally,1];end", &

'a=<0 0 0; 0 0 0; 1 0 0>;', &
'b=any(a);', &
"if b = 1,display('any(''a'') OK'),tally=[tally,0];else,display('any(''a'') FAILED');shape(a),tally=[tally,1];end", &

'if all(eq(shape(a),[3,3]))=1 ,display(''any(a) shape OK'');tally=[tally,0];', &
'if all(eq(shape(a),[3,3]))=0 ,display(''any(a) shape BAD'');shape(a),tally=[tally,1];', &
'if any(tally) = 0,display(''any PASSED''),else,display(''any FAILED'');tally', &
''])

  call wrapup()
end subroutine test_any
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_all()
  call lala( 'display(ones(80,1)''*61); help all; display(ones(80,1)''*95)')
  call lala( 'tally=[0];')
  if(logs)call lala( 'diary(''all.log'');')
  call lala( [ character(len=267) :: &
'a=<1 2 3; 4 5 6; 7 8 9>;', &
'b=all(a);', &
"if b = 1,display('all(''a'') OK'),tally=[tally,0];else,display('all(''a'') FAILED');shape(a),tally=[tally,1];end", &
'if all(eq(all(a),[1,1]))=1 ,display(''all(a) shape OK'')          ;tally=[tally,0];', &
'if all(eq(all(a),[1,1]))=0 ,display(''all(a) shape BAD'');shape(a),tally=[tally,1];', &
'if all(tally) = 0,display(''all PASSED''),else,display(''all FAILED'');tally', &
''])
  call wrapup()
end subroutine test_all
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_abs ()
   call lala( 'display(ones(80,1)''*61); help abs; display(ones(80,1)''*95)')
   if(logs)call lala( 'diary(''abs.log'');')
   call lala( [ character(len=256) :: &
'tally=[0];                                                               ', &
'a=<1 2 3; 4 5 6; 7 8 9>;b=-a;                                            ', &
'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero '');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
'if sum(b)=-45,tally=[tally,0];display(''expected sum is OK'');else,tally=[tally,1];display(''unexpected sum''),sum(b);', &
'if all(eq(a,-b))=1,tally=[tally,0];display(''a = -b as expected'');else,tally=[tally,1];display(''a is NOT equal to -b''); ', &
'if sum(tally)=0,display(''abs PASSED'');else,display(''abs FAILED'');tally ', &
''])
  call wrapup()
end subroutine test_abs
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_atan ()
  call lala( 'display(ones(80,1)''*61)')
  call lala( 'tally=[0];')
  call lala( [ character(len=256) :: &
'help atan; display(ones(80,1)''*95)', &
'PI=atan(1)*4;A=cos(PI);B=sin(PI);', &
'if A-1<eps,tally=[tally,0];display(''test if near PI OK'');else,tally=[tally,1];display(''test if near PI FAILED'');', &
'if B<eps,tally=[tally,0];display(''2nd test if near PI OK'');else,tally=[tally,1];display(''2nd test if near PI FAILED'');', &
'if sum(tally)=0,display(''atan PASSED'');else,display(''atan FAILED'');tally ', &
''])
  call wrapup()
end subroutine test_atan
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_cos ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
'help cos; display(ones(80,1)''*95)', &
'                                                                         ', &
'PI=atan(1)*4;P=cos(PI);PP=cos(2*PI);Z=cos(0);HP=cos(PI/2);', &
'if abs(HP)<eps,tally=[tally,0];display(''HALF-PI OK'');else,tally=[tally,1];display(''HALF-PI FAILED'');', &
'if Z=1,tally=[tally,0];display(''ZERO OK'');else,tally=[tally,1];display(''ZERO FAILED'');', &
'if P=-1,tally=[tally,0];display(''PI OK'');else,tally=[tally,1];display(''PI FAILED'');', &
'if PP=1,tally=[tally,0];display(''TWO PI OK'');else,tally=[tally,1];display(''TWO PI FAILED'');', &
'if cos(-2*PI)=1,tally=[tally,0];display(''-TWO PI OK'');else,tally=[tally,1];display(''-TWO PI FAILED'');', &
'if cos(-2000*PI)=1,tally=[tally,0];display(''-2000 PI OK'');else,tally=[tally,1];display(''-2000 PI FAILED'');', &
'PI,P,PP,Z,HP                                                                         ', &
'if sum(tally)=0,display(''cos PASSED'');else,display(''cos FAILED'');tally ', &
''])
  call wrapup()
end subroutine test_cos
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_round ()
   call lala( [ character(len=256) :: &
'// test round()                                                                ', &
'clear                                                                          ', &
'display(ones(80,1)''*61)                                                       ', &
'help round; display(ones(80,1)''*95)                                           ', &
'tally=[0];                                                                     ', &
'a=magic(10)+randu(10)*ones(10)*0.49;                                           ', &
'a=magic(5);                // an array of whole numbers                        ', &
'b=randu(5)-ones(5)*0.49999; // array with everything 0.5 < x > -0.5            ', &
'c=(a+b);                   // values of a randomly changed by less than +-1/2  ', &
'                                                                               ', &
'if all(ne(c,a))=1 , if all(eq(round(c),a))=1, ..                               ', &
'   display(''round of array plus random small fraction PASSED''), ..           ', &
'   tally=[tally,0], ..                                                         ', &
'else, ..                                                                       ', &
'   display(''round of array FAILED''), ..                                      ', &
'   tally=[tally,1], ..                                                         ', &
'end;                                                                           ', &
'                                                                               ', &
'if all(eq(round(a-c),0))=1, ..                                                 ', &
'   display(''round delta of original and randomized PASSED''), ..              ', &
'   tally=[tally,0], ..                                                         ', &
'else, ..                                                                       ', &
'   display(''round delta of original and randomized FAILED''), ..              ', &
'   tally=[tally,1], ..                                                         ', &
'end;                                                                           ', &
'                                                                               ', &
'if sum(tally)=0,display(''round PASSED'');else,display(''round FAILED'')       ', &
'<M,N>=shape(tally)                                                             ', &
'display(tally(2:N),1)                                                          ', &
''])
  call wrapup()
end subroutine test_round
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_shape ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help shape')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
'a=10;b=magic(4);c=ones(11,5);                                                                                             ', &
'<X,Y>=shape(c);                                                                                                           ', &
'if X=11,display(''X is 11''),else,display(''X is NOT 11'');X                                                              ', &
'if Y= 5,display(''Y is  5''),else,display(''Y is NOT  5'');Y                                                              ', &
'if all(eq(shape(a),1))=1,display(''shape of a OK'');tally=[tally,0];&
& else,display(''shape of a BAD'');shape(a),tally=[tally,1];       ', &
'if all(eq(shape(b),[ 4, 4]))=1,display(''shape of b OK'');tally=[tally,0];&
& else,display(''shape of b BAD'');shape(b),tally=[tally,1]; ', &
'if all(eq(shape(c),[11,5]))=1,display(''shape of c OK'');tally=[tally,0];&
& else,display(''shape of c BAD'');shape(c),tally=[tally,1];', &
'if sum(tally)=0,display(''shape PASSED'');else,display(''shape FAILED'');tally ', &
''])
  call wrapup()
end subroutine test_shape
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_hess ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help hess')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
     & 'A=magic(5);                                                                  ', &
     & '<P,H>=hess(A);                                                               ', &
     & 'B=P*H*P'';                                                                   ', &
     & 'if all(eq(A,round(B)))=1, ...                                                ', &
     & '   tally=[tally,0];display(''got back the original'');  ...                  ', &
     & 'else,  ...                                                                   ', &
     & '   tally=[tally,1];display(''does not match original'');                     ', &
     & 'if sum(tally)=0,display(''hess PASSED'');else,display(''hess FAILED'');tally ', &
     & ''])
  call wrapup()
end subroutine test_hess
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_if ()
   call lala( [ character(len=256) :: &
     & 'display(ones(80,1)''*61)                                                 ', &
     & 'help if                                                                  ', &
     & 'display(ones(80,1)''*95)                                                 ', &
     & 'tally=[0];                                                               ', &
     & '                                                                         ', &
     & ' n=5;                                                                    ', &
     & '                                                                         ', &
     & ' for i = 1:n, for j = 1:n, ...                                           ', &
     & '    if i = j, a(i,j) = 2; else, if abs(i-j) = 1, a(i,j) = -1; ...        ', &
     & '    else, a(i,j) = 0;                                                    ', &
     & '                                                                         ', &
     & ' // An easier way to accomplish the same thing is                        ', &
     & ' b = 2*eye(n);                                                           ', &
     & ' for i = 1:n-1, b(i,i+1) = -1; b(i+1,i) = -1;                            ', &
     & '                                                                         ', &
     & 'if all(eq(a,b))=1, tally=[tally,0];display(''matches'');else,tally=[tally,1];display(''does not match'');', &
     & 'if sum(tally)=0,display(''if PASSED'');else,display(''if FAILED'');tally ', &
     & ''])
  call wrapup()
end subroutine test_if
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_norm ()
   call lala( 'display(ones(80,1)''*61);help norm;display(ones(80,1)''*95)')
   if(logs)call lala( 'diary(''norm.log'');')
   call lala( [ character(len=256) :: &
   &'tally=[0]'';                                                                                                              ',&
   &'//diary(''norm'')                                                                                                         ',&
   &'long                                                                                                                      ',&
   &'X=magic(5);                                                                                                               ',&
   &'XX=X(:);                                                                                                                  ',&
   &'// A is answer, E is expected                                                                                             ',&
   &'A=norm(XX)      ;E=74.330343736592525   ; if abs(A/E)-1<=2*eps,tally=[tally,0];else,tally=[tally,x];A,E,A-E,eps,abs(A/E)-1',&
   &'A=norm(X,''inf'') ;E=65.0                 ; if abs(A/E)-1<=2*eps,tally=[tally,0];else,tally=[tally,x];A,E,A-E,eps,abs(A/E)-1',&
   &'A=norm(XX,''inf'');E=25.0                 ; if abs(A/E)-1<=2*eps,tally=[tally,0];else,tally=[tally,x];A,E,A-E,eps,abs(A/E)-1',&
   &'A=norm(X,1)     ;E=65.0                 ; if abs(A/E)-1<=2*eps,tally=[tally,0];else,tally=[tally,x];A,E,A-E,eps,abs(A/E)-1',&
   &'A=norm(X,2)     ;E=65.0                 ; if abs(A/E)-1<=2*eps,tally=[tally,0];else,tally=[tally,x];A,E,A-E,eps,abs(A/E)-1',&
   &'A=norm(X,''fro'') ;E=74.330343736592525   ; if abs(A/E)-1<=2*eps,tally=[tally,0];else,tally=[tally,x];A,E,A-E,eps,abs(A/E)-1',&
   &'A=norm(XX,1)    ;E=325.0                ; if abs(A/E)-1<=2*eps,tally=[tally,0];else,tally=[tally,x];A,E,A-E,eps,abs(A/E)-1',&
   &'A=norm(XX,''inf'');E=25.0                 ; if abs(A/E)-1<=2*eps,tally=[tally,0];else,tally=[tally,x];A,E,A-E,eps,abs(A/E)-1',&
   &'A=norm(XX,2)    ;E=74.330343736592525   ; if abs(A/E)-1<=2*eps,tally=[tally,0];else,tally=[tally,x];A,E,A-E,eps,abs(A/E)-1',&
   &'A=norm(XX,3)    ;E=47.270359729914041   ; if abs(A/E)-1<=2*eps,tally=[tally,0];else,tally=[tally,x];A,E,A-E,eps,abs(A/E)-1',&
   &'A=norm(XX,1/2)  ;E=7.333144324200370d+03; if abs(A/E)-1<=2*eps,tally=[tally,0];else,tally=[tally,x];A,E,A-E,eps,abs(A/E)-1',&
   &'A=norm(XX,1/3)  ;E=1.757406593784711d+05; if abs(A/E)-1<=2*eps,tally=[tally,0];else,tally=[tally,x];A,E,A-E,eps,abs(A/E)-1',&
   &'A=norm(-XX,3)   ;E=47.270359729914041   ; if abs(A/E)-1<=2*eps,tally=[tally,0];else,tally=[tally,x];A,E,A-E,eps,abs(A/E)-1',&
   &'A=norm(-XX,-3)  ;E=0.940699519035135    ; if abs(A/E)-1<=2*eps,tally=[tally,0];else,tally=[tally,x];A,E,A-E,eps,abs(A/E)-1',&
   &'A=norm(-XX,1/3) ;E=1.757406593784711d+05; if abs(A/E)-1<=2*eps,tally=[tally,0];else,tally=[tally,x];A,E,A-E,eps,abs(A/E)-1',&
   &'if sum(tally)=0,display(''norm PASSED'');else,display(''norm FAILED'')                                                    ',&
   &'display(tally,1);                                                                                                         ',&
   &'quit                                                                                                                      ',&
   &'//==============================================================================                                          ',&
   &'// YOU SHOULD GET THE FOLLOWING ERROR MESSAGE                                                                             ',&
   &'// norm(-X,-3)                                                                                                            ',&
   &'//              /\--ERROR:Only 1, 2 or INF norm of matrix                                                                 ',&
   &'//==============================================================================                                          ',&
   &'// MACROS DID NOT WORK, ACTED LIKE ELSE COMMANDS NOT PRESENT 20210510                                                     ',&
   &"//T= 'if abs(A - E)  =     0, display(''PASSED''),else,display(''FAILED''),A,E,A-E,eps,abs(A/E)-1';                       ",&
   &"//T= 'if abs(A - E) <=   eps, display(''PASSED''),else,display(''FAILED''),A,E,A-E,eps,abs(A/E)-1';                       ",&
   &"//T= 'if abs(A/E)-1 <= 2*eps, display(''PASSED''),else,display(''FAILED''),A,E,A-E,eps,abs(A/E)-1';                       ",&
   &'display(T,1)                                                                                                              ',&
   &"//==============================================================================                                          ",&
   &""])
  call wrapup()
end subroutine test_norm
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_save ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help save')
   call lala( [ character(len=256) :: &
'clear                                 // clear out user variables                                             ', &
'A=magic(4); b=ones(3,4); c=12**2;     // define some variables                                                ', &
'test_Variable=1234567890;                                                                                     ', &
'save(''__saved'');                    // save user variables to a file                                        ', &
'who; clear; who                       // list variables clear and they should be gone                         ', &
'load(''__saved'')                     // load the variables back in                                           ', &
'who                                   // should see them now                                                  ', &
'tally=[0];                            // test they are expected values and sizes                              ', &
'if all(eq(A,magic(4)))=1,  tally=[tally,0];display(''save of A PASSED'');else,tally=[tally,1];display(''save of A FAILED''); ', &
'if all(eq(b,ones(3,4)))=1, tally=[tally,0];display(''save of b PASSED'');else,tally=[tally,1];display(''save of b FAILED''); ', &
'if c=12**2,     tally=[tally,0];display(''save of c PASSED'');else,tally=[tally,1];display(''save of c FAILED''); ', &
'if test_Variable=1234567890, ...                                                                              ', &
'   tally=[tally,0];...                                                                                        ', &
'   display(''save of test_variable PASSED'');...                                                              ', &
'   else,...                                                                                                   ', &
'   tally=[tally,1];...                                                                                        ', &
'   display(''save of test_variable FAILED'');...                                                              ', &
'end;                                                                                                          ', &
'if sum(tally)=0,display(''save PASSED'');else,display(''save FAILED'');tally                                  ', &
'delete(''__saved'')                     // delete the scratch file                                            ', &
''])
  call wrapup()
end subroutine test_save
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_load ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help load')
   call lala( [ character(len=256) :: &
'clear                                                                                                         ', &
'A=magic(4); b=ones(3,4); c=12**2;                                                                             ', &
'test_Variable=1234567890;                                                                                     ', &
'save(''__saved'');                                                                                            ', &
'who; clear; who                                                                                               ', &
'load(''__saved'')                                                                                             ', &
'who                                                                                                           ', &
'tally=[0];                                                                                                    ', &
'if all(eq(A,magic(4)))=1,  tally=[tally,0];display(''load of A PASSED'');else,tally=[tally,1];display(''load of A FAILED''); ', &
'if all(eq(b,ones(3,4)))=1, tally=[tally,0];display(''load of b PASSED'');else,tally=[tally,1];display(''load of b FAILED''); ', &
'if c=12**2,     tally=[tally,0];display(''load of c PASSED'');else,tally=[tally,1];display(''load of c FAILED''); ', &
'if test_Variable=1234567890, ...                                                                              ', &
'   tally=[tally,0];...                                                                                        ', &
'   display(''load of test_variable PASSED'');...                                                              ', &
'   else,...                                                                                                   ', &
'   tally=[tally,1];...                                                                                        ', &
'   display(''load of test_variable FAILED'');...                                                              ', &
'end;                                                                                                          ', &
'if sum(tally)=0,display(''load PASSED'');else,display(''load FAILED'');tally                                  ', &
'delete(''__saved'')                     // delete the scratch file                                            ', &
''])
  call wrapup()
end subroutine test_load
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_invh ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help invh')
   call lala( 'tally=[0];')

   call lala( [ character(len=256) :: &
 & '                                                                         ', &
 & '// generate the Hilbert matrix of order N.                               ', &
 & 'N=5                                                                      ', &
 & 'for i = 1:N, for j = 1:N, A(i,j) = 1/(i+j-1);                            ', &
 & '// generate the inverse Hilbert matrix                                   ', &
 & 'C=invh(N);                                                               ', &
 & 'expected=[                                                               ', &
 & '   25     -300     1050    -1400      630;                               ', &
 & ' -300     4800   -18900    26880   -12600;                               ', &
 & ' 1050   -18900    79380  -117600    56700;                               ', &
 & '-1400    26880  -117600   179200   -88200;                               ', &
 & '  630   -12600    56700   -88200    44100;                               ', &
 & '];                                                                       ', &
 & '                                                                         ', &
 & 'if all(eq(C,expected))=1,tally=[tally,0];display(''inverse Hilbert PASSED'');&
 & else,tally=[tally,1];display(''inverse Hilbert FAILED'');', &
 & 'if sum(tally)=0,display(''invh PASSED'');else,display(''invh FAILED'');tally ', &
 & ''])
  call wrapup()
end subroutine test_invh
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_kron ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help kron')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
     & '                                                                         ', &
     & 'lines(888888)                                                            ', &
     & '//  C = Kronecker product of A and B                                     ', &
     & 'A=randu(4);                                                              ', &
     & 'B=randu(4);                                                              ', &
     & '// ==========================================                            ', &
     & '// first, the hard way                                                   ', &
     & '[m, n] = shape(A);                                                       ', &
     & 'for i = 1:m, ...                                                         ', &
     & '   ci = A(i,1)*B; ...                                                    ', &
     & '   for j = 2:n, ci = [ci A(i,j)*B]; end ...                              ', &
     & '   if i = 1, C = ci; else, C = [C; ci];                                  ', &
     & '// ==========================================                            ', &
     & '// then the easy ways                                                    ', &
     & 'D=kron(A,B);                                                             ', &
     & 'E=A .*. B;                                                               ', &
     & '// ==========================================                            ', &
     & '// subtract the values from the expected results                         ', &
     & '// and sum the deltas. Could just compare C to E                         ', &
     & '// and C to D, of course.                                                ', &
     & 'S1=sum(abs(C-E))                                                         ', &
     & 'S2=sum(abs(C-D))                                                         ', &
     & '                                                                         ', &
     & 'if S1=0, tally=[tally,0];display(''kron(A,B) check PASSED'');else,tally=[tally,1];display(''kron(A,B) check FAILED'');  ', &
     & 'if S2=0, tally=[tally,0];display(''A .*. B check PASSED'');else,tally=[tally,1];display(''A .*. B check FAILED'');      ', &
     & 'if sum(tally)=0,display(''kron PASSED'');else,display(''kron FAILED'');tally ', &
     & ''])
  call wrapup()
end subroutine test_kron
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_prod ()
   call lala( [ character(len=256) :: &
     & 'display(ones(80,1)''*61);help prod;clear;tally=[0];                                                                ', &
     & 'a = < 1  2  3 ; 4  5  6 ; 7  8  9 >;                                                                              ', &
     & 'expected=362880;                                                                                                  ', &
     & 'c=a''*2;                                                                                                           ', &
     & 'd=prod(c)/2**9;                                                                                                   ', &
     & '                                                                                                                  ', &
     & 'if prod(a) = expected, tally=[tally,0];display(''prod(a) PASSED'');else,tally=[tally,1];display('' prod(a) FAILED''); ', &
     & 'if expected = d, tally=[tally,0];display(''d PASSED'');else,tally=[tally,1];display(''d FAILED''             ', &
     & 'if sum(tally)=0,display(''prod PASSED'');else,display(''prod FAILED'');tally                                          ', &
     & ''])
  call wrapup()
end subroutine test_prod
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_eps ()
   if(logs)call lala( 'diary(''eps.log'');')
   call lala( [ character(len=256) :: &
     & ' display(ones(80,1)''*''='');help eps;display(ones(80,1)''*''_'')              ', &
     & 'tally=[0];                                                               ', &
     & ' // find the eps for this programming environment by brute force         ', &
     & ' myeps = 1;                                                              ', &
     & ' while 1 + myeps > 1, myeps = myeps/2;                                   ', &
     & ' myeps = 2*myeps                                                         ', &
     & '// compare it to the eps used by LALA                                   ', &
     & 'if myeps=eps, ...                                                        ', &
     & '   tally=[tally,0];display(''eps matches expected value''); ...            ', &
     & 'else, ...                                                                ', &
     & '   tally=[tally,1];display(''eps is NOT the expected value''), ...         ', &
     & 'end;                                                                     ', &
     & 'if sum(tally)=0,display(''eps PASSED'');else,display(''eps FAILED'');tally   ', &
     & ''])
  call wrapup()
end subroutine test_eps
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_ans ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help ans')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
     & '                                                                         ', &
     & '                                                                         ', &
     & '                                                                         ', &
     & 'if sum(tally)=0,display(''ans PASSED'');else,display(''ans FAILED'');tally ', &
     & ''])
  call wrapup()
end subroutine test_ans
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_base ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help base')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''base PASSED'');else,display(''base FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_base
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_chol ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help chol')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''chol PASSED'');else,display(''chol FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_chol
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_chop ()
   call lala( 'display(ones(80,1)''*61);help chop')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''chop PASSED'');else,display(''chop FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_chop
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_clear ()
   call lala( 'display(ones(80,1)''*61);help clear')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''clear PASSED'');else,display(''clear FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_clear
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_cond ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help cond')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''cond PASSED'');else,display(''cond FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_cond
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_conjg ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help conjg')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''conjg PASSED'');else,display(''conjg FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_conjg
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_debug ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help debug')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''debug PASSED'');else,display(''debug FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_debug
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_det ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help det')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''det PASSED'');else,display(''det FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_det
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_diag ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help diag')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''diag PASSED'');else,display(''diag FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_diag
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_diary ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( [ character(len=256) :: &
   & 'help diary                                                                 ', &
   & 'tally=[0];                                                                 ', &
   & 'if sum(tally)=0,display(''diary PASSED'');else,display(''diary FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_diary
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_display ()
   if(logs)call lala( 'diary(''display.log'');')
   call lala( [ character(len=256) :: &
   & 'display([27,91,''H'',27,91,''2J'']) // clear and home cursor on ANSI device  ', &
   & 'display(ones(80,1)''*61)         // make a line                              ', &
   & 'help display                                                                 ', &
   & 'display(ones(80,1)''*95)         // make a line                              ', &
   & 'tally=[0];                                                                   ', &
   & '                                                                             ', &
   & 'display(<                       // multiple lines, all the same length       ', &
   & '''#--------------#'';                                                        ', &
   & '''|              |'';                                                        ', &
   & '''|    WARNING   |'';                                                        ', &
   & '''|              |'';                                                        ', &
   & '''#--------------#'';                                                        ', &
   & '>)                                                                           ', &
   & '                                                                             ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''display PASSED'');else,display(''display FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_display
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_delete ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help delete')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''delete PASSED'');else,display(''delete FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_delete
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_doc ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help doc')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''doc PASSED'');else,display(''doc FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_doc
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_eig ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help eig')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''eig PASSED'');else,display(''eig FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_eig
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_else ()
   if(logs)call lala( 'diary(''else.log'');')
   call lala( 'display(ones(80,1)''*61);help else;tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''ELSE PASSED'');else,display(''ELSE FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_else
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_end ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help end')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''END PASSED'');else,display(''END FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_end
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_exec ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help exec')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''exec PASSED'');else,display(''exec FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_exec
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_exit ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help exit')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''exit PASSED'');else,display(''exit FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_exit
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_exp ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help exp')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''exp PASSED'');else,display(''exp FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_exp
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_eye ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help eye')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & 'a=magic(5);                                                                ', &
   & 'b=a*eye(a);                                                                ', &
   & 'c=a*eye*2;                                                                 ', &
   & 'd=eye(5);                                                                  ', &
   & 'e=eye(5,6);                                                                ', &
   & 'if sum(tally)=0,display(''eye PASSED'');else,display(''eye FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_eye
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_flops ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help flops')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''flops PASSED'');else,display(''flops FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_flops
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_for ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help for')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''for PASSED'');else,display(''for FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_for
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_help ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help help')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''help PASSED'');else,display(''help FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_help
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_imag ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help imag')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & 'i=sqrt(-1);                                                              ', &
   & 'c=3+4*i;                                                                 ', &
   & '                                                                         ', &
   & 'if imag(c)=4, tally=[tally,0];display(''imag OK '');else,tally=[tally,1];display(''imag not OK'');', &
   & 'if sum(tally)=0,display(''imag PASSED'');else,display(''imag FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_imag
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_inv ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help inv')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''inv PASSED'');else,display(''inv FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_inv
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_lala ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help lala')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''lala PASSED'');else,display(''lala FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_lala
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_lines ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help lines')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''lines PASSED'');else,display(''lines FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_lines
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_log ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help log')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''log PASSED'');else,display(''log FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_log
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_long ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help long')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''long PASSED'');else,display(''long FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_long
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_lu ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help lu')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''lu PASSED'');else,display(''lu FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_lu
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_orth ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help orth')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''orth PASSED'');else,display(''orth FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_orth
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_pinv ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help pinv')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''pinv PASSED'');else,display(''pinv FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_pinv
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_plot ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help plot')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''plot PASSED'');else,display(''plot FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_plot
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_poly ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help poly')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''poly PASSED'');else,display(''poly FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_poly
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_print ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help print')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''print PASSED'');else,display(''print FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_print
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_qr ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help qr')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''qr PASSED'');else,display(''qr FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_qr
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_quit ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help quit')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''quit PASSED'');else,display(''quit FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_quit
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_randn ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help randn')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''randn PASSED'');else,display(''randn FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_randn
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_randu ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help randu')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''randu PASSED'');else,display(''randu FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_randu
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_rank ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help rank')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''rank PASSED'');else,display(''rank FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_rank
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_rat ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help rat')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''rat PASSED'');else,display(''rat FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_rat
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_rcond ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help rcond')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''rcond PASSED'');else,display(''rcond FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_rcond
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_real ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help real')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & 'i=sqrt(-1);                                                              ', &
   & 'c=3+4*i;                                                                 ', &
   & '                                                                         ', &
   & 'if real(c)=3, tally=[tally,0];display(''real OK '');else,tally=[tally,1];display(''real not OK'');', &
   & 'if sum(tally)=0,display(''real PASSED'');else,display(''real FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_real
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_roots ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help roots')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''roots PASSED'');else,display(''roots FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_roots
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_rref ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help rref')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''rref PASSED'');else,display(''rref FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_rref
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_schur ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help schur')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''schur PASSED'');else,display(''schur FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_schur
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_semi ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help semi')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''semi PASSED'');else,display(''semi FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_semi
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_short ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help short')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''short PASSED'');else,display(''short FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_short
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_sh ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help sh')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''sh PASSED'');else,display(''sh FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_sh
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_sin ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help sin')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''sin PASSED'');else,display(''sin FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_sin
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_sqrt ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help sqrt')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''sqrt PASSED'');else,display(''sqrt FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_sqrt
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_svd ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help svd')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
   & '                                                                         ', &
   & '                                                                         ', &
   & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
   & 'if sum(tally)=0,display(''svd PASSED'');else,display(''svd FAILED'');tally ', &
   & ''])
  call wrapup()
end subroutine test_svd
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_tril ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help tril')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
  & '', &
  & 'a=magic(6);', &
  & '', &
  & 'answer=tril(a);', &
  & 'expected =<35 0 0 0 0 0; 3 32 0 0 0 0; 31 9 2 0 0 0; 8 28 33 17 0 0; 30 5 34 12 14 0; 4 36 29 13 18 11>;', &
  & 'if all(eq(answer,expected))=1, tally=[tally,0], else, tally=[tally,1];', &
  & '', &
  & 'answer=tril(a,2);', &
  & 'expected = <35 1 6 0 0 0; 3 32 7 21 0 0; 31 9 2 22 27 0; 8 28 33 17 10 15; 30 5 34 12 14 16; 4 36 29 13 18 11>;', &
  & 'if all(eq(answer,expected))=1, tally=[tally,0], else, tally=[tally,2];', &
  & '', &
  & 'answer=tril(a,-1);', &
  & 'expected =< 0 0 0 0 0 0; 3 0 0 0 0 0; 31 9 0 0 0 0; 8 28 33 0 0 0; 30 5 34 12 0 0; 4 36 29 13 18 0>;', &
  & 'if all(eq(answer,expected))=1, tally=[tally,0], else, tally=[tally,3];', &
  & '                                                                         ', &
  & 'if sum(tally)=0,display(''tril PASSED'');else,display(''tril FAILED'');tally ', &
  & ''])
  call wrapup()
end subroutine test_tril
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_triu ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help triu')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
  & '                                                                         ', &
  & '                                                                         ', &
  & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
  & 'if sum(tally)=0,display(''triu PASSED'');else,display(''triu FAILED'');tally ', &
  & ''])
  call wrapup()
end subroutine test_triu
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_user ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help user')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
  & '                                                                         ', &
  & '                                                                         ', &
  & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
  & 'if sum(tally)=0,display(''user PASSED'');else,display(''user FAILED'');tally ', &
  & ''])
  call wrapup()
end subroutine test_user
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_what ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help what')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
  & '                                                                         ', &
  & '                                                                         ', &
  & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
  & 'if sum(tally)=0,display(''what PASSED'');else,display(''what FAILED'');tally ', &
  & ''])
  call wrapup()
end subroutine test_what
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_while ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help while')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
  & '                                                                         ', &
  & '                                                                         ', &
  & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero''); ', &
  & 'if sum(tally)=0,display(''while PASSED'');else,display(''while FAILED'');tally ', &
  & ''])
  call wrapup()
end subroutine test_while
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_who ()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'help who')
   call lala( 'tally=[0];')
   call lala( [ character(len=256) :: &
  & '                                                                         ', &
  & '                                                                         ', &
  & '                                                                         ', &
 !& 'if all(eq(a+b,zeros(a)))=1, tally=[tally,0];display(''a-b is zero'');else,tally=[tally,1];display(''a-b is NOT zero'');', &
  & 'if sum(tally)=0,display(''who PASSED'');else,display(''who FAILED'');tally ', &
  & ''])
  call wrapup()
end subroutine test_who
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_general_char ()
   if(logs)call lala( 'diary(''test_general_char.log'');')
   call lala( [ character(len=256) :: &
'display(ones(80,1)''*''=''); // display a line of equals across display             ', &
'tally=[0];                                                                          ', &
'display(0:126) // display printable ASCII characters                                ', &
'// on an ANSI terminal or terminal emulator                                         ', &
'clr=''display([27,91,''''H'''',27,91,''''2J''''])'' // home cursor and clear screen ', &
'>clr                                                                                ', &
'//if ''ABCabc''=[65 66 67 97 98 99],tally=[tally,0];display(''ABCabc PASSED'');else,tally=[tally,1];display(''ABCabc FAILED'');', &
'if sum(tally)=0,display(''general char PASSED'');else,display(''general char FAILED'');tally ', &
''])
  call wrapup()
end subroutine test_general_char
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_general_dots()
   if(logs)call lala( 'diary(''test_general_dots.log'');')
   call lala( [ character(len=256) :: &
   & 'display(ones(80,1)''*61);display(''general expression dots'');               ', &
   & 'tally=[0];                                                                   ', &
   & 'a=magic(3);b=ones(3)*2;                                                      ', &
   & '                                                                             ', &
   & 'if all(eq(a*2,a.*b))=1, tally=[tally, 0], else, tally=[tally, -1];           ', &
   & '                                                                             ', &
   & 'if sum(abs(tally)) = 0,display(''general expression dots PASSED''),else,display(''general expression dots FAILED'');tally'])
   !!logs=.false.
  call wrapup()
end subroutine test_general_dots
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_general_pascal()
   if(logs)call lala( 'diary(''test_general_pascal.log'');')
   call lala( [ character(len=256) :: &
   & 'display(ones(80,1)''*61);display(''general pascal test'');                                ', &
   & 'tally=[0];                                                                                ', &
   & '                                                                                          ', &
   & '// In mathematics, particularly matrix theory and combinatorics, the Pascal               ', &
   & '// matrix is an infinite matrix containing the binomial coefficients                      ', &
   & '// as its elements. There are three ways to achieve this: as either an                    ', &
   & '// upper-triangular matrix, a lower-triangular matrix, or a symmetric                     ', &
   & '// matrix. The 5 × 5 truncations of these are shown below.                                ', &
   & '                                                                                          ', &
   & '// Lower triangular:                                                                      ', &
   & 'L5 = <                                                                                    ', &
   & ' 1 0 0 0 0;                                                                               ', &
   & ' 1 1 0 0 0;                                                                               ', &
   & ' 1 2 1 0 0;                                                                               ', &
   & ' 1 3 3 1 0;                                                                               ', &
   & ' 1 4 6 4 1; >                                                                             ', &
   & '// Symmetric:                                                                             ', &
   & 'S5 = <                                                                                    ', &
   & '  1 1  1  1  1;                                                                           ', &
   & '  1 2  3  4  5;                                                                           ', &
   & '  1 3  6 10 15;                                                                           ', &
   & '  1 4 10 20 35;                                                                           ', &
   & '  1 5 15 35 70; >                                                                         ', &
   & '// Upper triangular:                                                                      ', &
   & 'U5 = <                                                                                    ', &
   & '  1 1 1 1 1;                                                                              ', &
   & '  0 1 2 3 4;                                                                              ', &
   & '  0 0 1 3 6;                                                                              ', &
   & '  0 0 0 1 4;                                                                              ', &
   & '  0 0 0 0 1; >                                                                            ', &
   & '                                                                                          ', &
   & '// These matrices have the pleasing relationship Sn = LnUn. From this it is               ', &
   & '// easily seen that all three matrices have determinant 1, as the determinant             ', &
   & '// of a triangular matrix is simply the product of its diagonal elements,                 ', &
   & '// which are all 1 for both Ln and Un. In other words, matrices Sn, Ln,                   ', &
   & '// and Un are unimodular, with Ln and Un having trace n.                                  ', &
   & '                                                                                          ', &
   & 'L=[1]                                                                                     ', &
   & '     //Generate next Lower triangular Pascal matrix:                                      ', &
   & '     for I=1:4,...                                                                        ', &
   & '        [k,k] = shape(L);...                                                              ', &
   & '        k = k + 1;...                                                                     ', &
   & '        L(k,1:k) = [L(k-1,:) 0] + [0 L(k-1,:)];...                                        ', &
   & '        L,...                                                                             ', &
   & '     end                                                                                  ', &
   & '                                                                                          ', &
   & 'L5-L                                                                                      ', &
   & '                                                                                          ', &
& 'if sum(abs(L5-L)) =0, tally=[tally, 0],display(''pascal L5 passed''), else, tally=[tally, -1],display(''pascal L5 FAILED''); ', &
& '                                                                                          ', &
& 'if sum(abs(tally)) = 0,display(''general pascal PASSED''),else,display(''general pascal FAILED'');tally'])
   !!logs=.false.
  call wrapup()
end subroutine test_general_pascal
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_general_expr()
   if(logs)call lala( 'diary(''test_general_expr.log'');')
   call lala( [ character(len=256) :: &
   & 'display(ones(80,1)''*61);display(''general expression tests'');              ', &
   & 'tally=[0];                                                                   ', &
   & 'a=3+4;                                                                       ', &
   & 'if a=7,tally=[tally,0],else,tally=[tally,1];                                 ', &
   & 'if a<7,tally=[tally,2],else,tally=[tally,0];                                 ', &
   & 'if a>7,tally=[tally,3],else,tally=[tally,0];                                 ', &
   & 'if a=>7,tally=[tally,0],else,tally=[tally,4];                                ', &
   & 'if a<=7,tally=[tally,0],else,tally=[tally,5];                                ', &
   & 'if a<>7,tally=[tally,6],else,tally=[tally,0];                                ', &
   & 'if a=7,tally=[tally,0],else,tally=[tally,7];                                 ', &
   & 'if a=7,tally=[tally,0],else,tally=[tally,8];                                 ', &
   & 'b=44-30.0;                                                                   ', &
   & 'if b=14,tally=[tally,0],else,tally=[tally,9];                                ', &
   & 'c=7*8;                                                                       ', &
   & 'if c=56,tally=[tally,0],else,tally=[tally,10];                               ', &
   & 'd=90/30;                                                                     ', &
   & 'if d=3,tally=[tally,0],else,tally=[tally,11];                                ', &
   & 'e=2**8;                                                                      ', &
   & 'if e=256,tally=[tally,0],else,tally=[tally,12];                              ', &
   & '                                                                             ', &
   & 'answers=[   a, b, c,d,  e]                                                   ', &
   & 'expected=[7d0,14,56,3,256]                                                   ', &
   & 'tally=[tally,answers-expected];                                              ', &
   & '[rows,cols]=shape(tally);                                                    ', &
   & 'for i=1:cols, if abs(tally(i)) < 2*eps,tally(i)=0;else,tally(i)=1;           ', &
   & 'if sum(abs(tally)) = 0,display(''general expr  PASSED''),else,display(''general expr  FAILED'');tally', &
   & 'tally'])
   !!logs=.false.
  call wrapup()
end subroutine test_general_expr
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_general_avg()
   call lala( 'display(ones(80,1)''*61)')
   call lala( 'display(''general tests: avg'')')
   if(logs)call lala( 'diary(''test-general-avg.log'');')
   call lala( [ character(len=256) :: &
   & 'tally=[0];                                                              ', &
   & 'a=magic(8); n=3;                                                        ', &
   & 'for i = 2:2:n, for j=2:2:n,t = (a(i-1,j-1)+a(i-1,j)+a(i,j-1)+a(i,j))/4; ', &
   & 'if t = 32.5, tally=[tally, 0], else, tally=[tally, -1];                 ', &
   & 'if sum(tally) = 0,display(''avg PASSED''),else,display(''avg FAILED'');tally'])
  call wrapup()
end subroutine test_general_avg
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_addition_and_subtraction()
  call lala( 'display(ones(80,1)''*61); help +; display(ones(80,1)''*95)')
  if(logs)call lala( 'diary(''addition.log'');')
  call lala(  [ character(len=128) :: &
"tally=[0];                                                                      ",&
"a=<1 2 3; 4 5 6; 7 8 9>;                                                        ",&
"b=3;                                                                            ",&
"d=ones(a)*b+a;D=b+a;                                                            ",&
"e=ones(a)*b-a;E=b-a;                                                            ",&
"f=a+ones(a)*b;F=a+b;                                                            ",&
"g=a-ones(a)*b;G=a-b;                                                            ",&
"if all(eq(d,D))=1,display('addition:case D:1 OK'),tally=[tally,0]; else,display('addition:case D:1 FAILED');tally=[tally,1];end",&
"if all(eq(e,E))=1,display('addition:case E:1 OK'),tally=[tally,0]; else,display('addition:case E:1 FAILED');tally=[tally,1];end",&
"if all(eq(f,F))=1,display('addition:case F:1 OK'),tally=[tally,0]; else,display('addition:case F:1 FAILED');tally=[tally,1];end",&
"if all(eq(g,G))=1,display('addition:case G:1 OK'),tally=[tally,0]; else,display('addition:case G:1 FAILED');tally=[tally,1];end",&
"b=-3;                                                                           ",&
"d=ones(a)*b+a;D=b+a;                                                            ",&
"e=ones(a)*b-a;E=b-a;                                                            ",&
"f=a+ones(a)*b;F=a+b;                                                            ",&
"g=a-ones(a)*b;G=a-b;                                                            ",&
"if all(eq(d,D))=1,display('addition:case D:2 OK'),tally=[tally,0]; else,display('addition:case D:2 FAILED');tally=[tally,1];end",&
"if all(eq(e,E))=1,display('addition:case E:2 OK'),tally=[tally,0]; else,display('addition:case E:2 FAILED');tally=[tally,1];end",&
"if all(eq(f,F))=1,display('addition:case F:2 OK'),tally=[tally,0]; else,display('addition:case F:2 FAILED');tally=[tally,1];end",&
"if all(eq(g,G))=1,display('addition:case G:2 OK'),tally=[tally,0]; else,display('addition:case G:2 FAILED');tally=[tally,1];end",&
"// literal values ",&
"d=ones(a)*3+a;D=3+a;                                                            ",&
"e=ones(a)*3-a;E=3-a;                                                            ",&
"f=a+ones(a)*3;F=a+3;                                                            ",&
"g=a-ones(a)*3;G=a-3;                                                            ",&
"if all(eq(d,D))=1,display('addition:case D:3 OK'),tally=[tally,0]; else,display('addition:case D:3 FAILED');tally=[tally,1];end",&
"if all(eq(e,E))=1,display('addition:case E:3 OK'),tally=[tally,0]; else,display('addition:case E:3 FAILED');tally=[tally,1];end",&
"if all(eq(f,F))=1,display('addition:case F:3 OK'),tally=[tally,0]; else,display('addition:case F:3 FAILED');tally=[tally,1];end",&
"if all(eq(g,G))=1,display('addition:case G:3 OK'),tally=[tally,0]; else,display('addition:case G:3 FAILED');tally=[tally,1];end",&
"d=ones(a)*(-3)+a;D=(-3)+a;                                                            ",&
"e=ones(a)*(-3)-a;E=(-3)-a;                                                            ",&
"f=a+ones(a)*(-3);F=a+(-3);                                                            ",&
"g=a-ones(a)*(-3);G=a-(-3);                                                            ",&
"if all(eq(d,D))=1,display('addition:case D:4 OK'),tally=[tally,0]; else,display('addition:case D:4 FAILED');tally=[tally,1];end",&
"if all(eq(e,E))=1,display('addition:case E:4 OK'),tally=[tally,0]; else,display('addition:case E:4 FAILED');tally=[tally,1];end",&
"if all(eq(f,F))=1,display('addition:case F:4 OK'),tally=[tally,0]; else,display('addition:case F:4 FAILED');tally=[tally,1];end",&
"if all(eq(g,G))=1,display('addition:case G:4 OK'),tally=[tally,0]; else,display('addition:case G:4 FAILED');tally=[tally,1];end",&
"twice=<2 4 6; 8 10 12; 14 16 18>;                                                        ",&
"if all(eq(twice,a+a))=1,display('addition: array OK'),tally=[tally,0]; else,display('addition:array FAILED');tally=[tally,1];end",&
"display('addition: completed');"])

  call lala( 'if sum(tally) = 0,display(''addition PASSED''),else,display(''addition FAILED'');tally')
  call wrapup()
end subroutine test_addition_and_subtraction
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_set_theory ()
   if(logs)call lala( 'diary(''set_theory.log'');')
   call lala( [ character(len=256) :: &
'// test set_theory()                                                                     ', &
'clear                                                                                    ', &
'display(ones(80,1)''*61)                                                                 ', &
'// help SET THEORY; display(ones(80,1)''*95)                                             ', &
'tally=[0];                                                                               ', &
'semi;                                                                                    ', &
'//--------------------------------------------------------------------------------                        ',&
'help set%intersect                                                                                        ',&
'A= [ 7 1 7 7 4 ];                                                                                         ',&
'B= [ 7 0 4 4 0 ];                                                                                         ',&
'expected=[7 4];                                                                                           ',&
'got=set%intersect(A,B);                                                                                   ',&
"if all(eq(expected,got))=1,display('set%intersect OK'),tally=[tally,0];else,display('set%intersect FAILED');tally=[tally,1];end",&
'//--------------------------------------------------------------------------------                        ',&
'help set%ismember                                                                                         ',&
'A= [ 5 3 4 2 ];                                                                                           ',&
'B= [ 2 4 4 4 6 8 ];                                                                                       ',&
'expected = [0 0 1 1];                                                                                     ',&
'got = set%ismember(A,B);                                                                                  ',&
"if all(eq(expected,got))=1,display('set%ismember OK'),tally=[tally,0];else,display('set%ismember FAILED');tally=[tally,1];end",&
'//--------------------------------------------------------------------------------                        ',&
'help set%issorted                                                                                         ',&
'A= [ 10 -10 0 1 2 3 3 2 1 -10 ];                                                                          ',&
'expected = 0;                                                                                             ',&
'got = set%issorted(A);                                                                                    ',&
"if all(eq(expected,got))=1,display('set%issorted OK'),tally=[tally,0];else,display('set%issorted FAILED');tally=[tally,1];end",&
'A= [ -10 10 100 201 ];                                                                                    ',&
'expected = 1;                                                                                             ',&
'got = set%issorted(A);                                                                                    ',&
"if all(eq(expected,got))=1,display('set%issorted OK'),tally=[tally,0];else,display('set%issorted FAILED');tally=[tally,1];end",&
'//--------------------------------------------------------------------------------                        ',&
'help set%setdiff                                                                                          ',&
'A= [ 3 6 2 1 5 1 1 ];                                                                                     ',&
'B= [ 2 4 6 ];                                                                                             ',&
'expected = [ 3 1 5 ] ;                                                                                    ',&
'got = set%setdiff(A,B);                                                                                   ',&
"if all(eq(expected,got))=1,display('set%setdiff OK'),tally=[tally,0];else,display('set%setdiff FAILED');tally=[tally,1];end",&
'//--------------------------------------------------------------------------------                        ',&
'help set%setxor                                                                                           ',&
'A = [5,1,3,3,3];                                                                                          ',&
'B = [4,1,2];                                                                                              ',&
'expected = [5 3 4 2 ];                                                                                    ',&
'got = set%setxor(A,B);                                                                                    ',&
"if all(eq(expected,got))=1,display('set%setxor OK'),tally=[tally,0];else,display('set%setxor FAILED');tally=[tally,1];end",&
'//--------------------------------------------------------------------------------                        ',&
'help set%union                                                                                            ',&
'A= [5 7 1];                                                                                               ',&
'B= [3 1 1];                                                                                               ',&
'got = set%union(A,B);                                                                                     ',&
'expected = [5 7 1 3 ];                                                                                    ',&
"if all(eq(expected,got))=1,display('set%union OK'),tally=[tally,0];else,display('set%union FAILED');tally=[tally,1];end",&
'A= [5 5 3];                                                                                               ',&
'B= [1 2 5];                                                                                               ',&
'expected = [5 3 1 2];                                                                                     ',&
'got= set%union(A,B);                                                                                      ',&
"if all(eq(expected,got))=1,display('set%union OK'),tally=[tally,0];else,display('set%union FAILED');tally=[tally,1];end",&
'//--------------------------------------------------------------------------------                        ',&
'help set%unique                                                                                           ',&
'A = [44, 33, 33, 33, 22, 11, 33, 44, 55, 33];                                                             ',&
'expected = [44, 33, 22, 11, 55];                                                                          ',&
'got = set%unique(A);                                                                                      ',&
"if all(eq(expected,got))=1,display('set%unique OK'),tally=[tally,0];else,display('set%unique FAILED');tally=[tally,1];end",&
'//--------------------------------------------------------------------------------                        ',&
'if sum(tally)=0,display(''set_theory PASSED'');else,display(''set_theory FAILED'')       ', &
'<M,N>=shape(tally)                                                                       ', &
'display(tally(2:N),1)                                                                    ', &
''])
  call wrapup()
end subroutine test_set_theory
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine wrapup()
integer                      :: inc
integer                      :: ierr
   call lala('sumtally=sum(tally);')
   call get_from_lala('sumtally',inc,ierr)
   sumtally=sumtally+inc
   call lala( [ character(len=256) :: &
'                                                                        ', &
'                                                                        ', &
'diary(0)                                                                ', &
'clear                                                                   '])
end subroutine wrapup
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
!-----------------------------------------------------------------------------------------------------------------------------------
end program testit
