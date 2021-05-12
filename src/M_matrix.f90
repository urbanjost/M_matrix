module M_matrix
use,intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
use M_strings, only : value_to_string, lower, v2s, s2v
use M_journal, only : journal
use M_help, only    : help_command
use M_history, only : redo
!>
!!##SYNTAX DIAGRAMS (9)
!!
!!    A formal description of the language acceptable to MAT88, as well as
!!    a flow chart of the mat88 program, is provided by the syntax diagrams
!!    or syntax graphs of wirth [6]. There are eleven non-terminal symbols
!!    in the language:
!!
!!       LINE, STATEMENT, CLAUSE, EXPRESSION, TERM,
!!       FACTOR, NUMBER, INTEGER, NAME, COMMAND, TEXT .
!!
!!    The diagrams define each of the non-terminal symbols using the others
!!    and the terminal symbols:
!!
!!       LETTER -- A THROUGH Z,
!!       DIGIT  -- 0 THROUGH 9,
!!       CHAR   -- ( ) ; : + - * / \ = . , < >
!!       QUOTE  -- '
!!
!!    LINE
!!
!!           |-----> STATEMENT >----|
!!           |                      |
!!           |-----> CLAUSE >-------|
!!           |                      |
!!    -------|-----> EXPR >---------|------>
!!         | |                      | |
!!         | |-----> COMMAND >------| |
!!         | |                      | |
!!         | |-> > >-> EXPR >-> < >-| |
!!         | |                      | |
!!         | |----------------------| |
!!         |                          |
!!         |        |-< ; <-|         |
!!         |--------|       |---------|
!!                  |-< , <-|
!!
!!    STATEMENT
!!
!!         |-> NAME >--------------------------------|
!!         |          |                              |
!!         |          |         |--> : >---|         |
!!         |          |         |          |         |
!!         |          |-> ( >---|-> EXPR >-|---> ) >-|
!!         |                  |              |       |
!!    -----|                  |-----< , <----|       |--> = >--> EXPR >--->
!!         |                                         |
!!         |       |--< , <---|                      |
!!         |       |          |                      |
!!         |-> < >---> NAME >---> > >----------------|
!!
!!    CLAUSE
!!
!!         |---> FOR   >---> NAME >---> = >---> EXPR >--------------|
!!         |                                                        |
!!         | |-> WHILE >-|                                          |
!!         |-|           |-> EXPR >----------------------           |
!!         | |-> IF    >-|          |   |   |   |   |   |           |
!!    -----|                        <   <=  =   <>  >=  >           |---->
!!         |                        |   |   |   |   |   |           |
!!         |                        ----------------------> EXPR >--|
!!         |                                                        |
!!         |---> ELSE  >--------------------------------------------|
!!         |                                                        |
!!         |---> END   >--------------------------------------------|
!!
!!    EXPR
!!
!!           |-> + >-|
!!           |       |
!!    -------|-------|-------> TERM >---------->
!!           |       |    |             |
!!           |-> - >-|    |  |-< + <-|  |
!!                        |  |       |  |
!!                        |--|-< - <-|--|
!!                           |       |
!!                           |-< : <-|
!!
!!    TERM
!!
!!    ---------------------> FACTOR >---------------------->
!!            |                                   |
!!            |             |-< * <-|             |
!!            |  |-------|  |       |  |-------|  |
!!            |--|       |--|-< / <-|--|       |--|
!!               |-< . <-|  |       |  |-< . <-|
!!                          |-< \ <-|
!!
!!    FACTOR
!!
!!         |----------------> NUMBER >---------------|
!!         |                                         |
!!         |-> NAME >--------------------------------|
!!         |          |                              |
!!         |          |         |--> : >---|         |
!!         |          |         |          |         |
!!         |          |-> ( >---|-> EXPR >-|---> ) >-|
!!         |                  |              |       |
!!         |                  |-----< , <----|       |
!!         |                                         |
!!    -----|------------> ( >-----> EXPR >-----> ) >-|-|-------|----->
!!         |                                         | |       | |
!!         |                  |--------------|       | |-> ' >-| |
!!         |                  |              |       |           |
!!         |------------> < >-|---> EXPR >---|-> > >-|           |
!!         |                    |          |         |           |
!!         |                    |--<   <---|         |           |
!!         |                    |          |         |           |
!!         |                    |--< ; <---|         |           |
!!         |                    |          |         |           |
!!         |                    |--< , <---|         |           |
!!         |                                         |           |
!!         |------------> > >-----> EXPR >-----> < >-|           |
!!         |                                         |           |
!!         |-----> FACTOR >---> ** >---> FACTOR >----|           |
!!         |                                                     |
!!         |------------> ' >-----> TEXT >-----> ' >-------------|
!!
!!    NUMBER
!!
!!        |----------|                          |-> + >-|
!!        |          |                          |       |
!!    -----> INT >-----> . >---> INT >-----> E >---------> INT >---->
!!                 |                   | |      |       |        |
!!                 |                   | |      |-> - >-|        |
!!                 |                   | |                       |
!!                 |---------------------------------------------|
!!
!!    INT
!!
!!    ------------> DIGIT >----------->
!!              |           |
!!              |-----------|
!!
!!    NAME
!!
!!                      |--< LETTER <--|
!!                      |              |
!!    ------> LETTER >--|--------------|----->
!!                      |              |
!!                      |--< DIGIT  <--|
!!
!!    COMMAND
!!
!!                            |--> NAME >--|
!!                            |            |
!!    --------> NAME >--------|------------|---->
!!                            |            |
!!                            |--> CHAR >--|
!!                            |            |
!!                            |---> ' >----|
!!
!!    TEXT
!!
!!                    |-> LETTER >--|
!!                    |             |
!!                    |-> DIGIT >---|
!!    ----------------|             |-------------->
!!                |   |-> CHAR >----|   |
!!                |   |             |   |
!!                |   |-> ' >-> ' >-|   |
!!                |                     |
!!                |---------------------|
!>
!! Originally based on a routine called MATLAB, although heavily modified
!! since. The original stated ...
!!
!!    MATLAB stands for MATrix LABoratory. It is a FORTRAN package
!!    developed by Argonne National Laboratories for in-house use. It
!!    provides comprehensive vector and tensor operations in a package
!!    which may be programmed, either through a macro language or through
!!    execution of script files.
!!
!!    Matlab is reentrant and recursive. Functions supported include (but
!!    are not by any means limited to) sin, cos, tan, arc functions, upper
!!    triangular, lower triangular, determinants, matrix multiplication,
!!    identity, Hilbert matrices, eigenvalues and eigenvectors, matrix
!!    roots and products, inversion and so on and so forth.
!!
!!    The file available on the bulletin board as Matlab.arc contains an
!!    Amiga-ized executable copy of MATLAB and the online help file, as
!!    well as this intro.
!!
!!    If you want the source code (over 300K) and a manual, or if your
!!    bulletin board only has this message and not the package, send $5.00
!!    and a 3.5" disk to:
!!
!!                               Jim Locker
!!                               4443 N. Hyland Ave.
!!                               Dayton, OH 45424
!!
!!    The package is public domain, but of course postage and reproduction
!!    cost money. Believe me, this package is a bargain at the price.
!!    Please feel free to distribute the package.
!!
!!    The source was taken off a VAX 11/780. It ran without modification
!!    (except the file handler and some minor error handling) on an Amiga
!!    1000 using ABSoft Fortran v2.2.  It will run in 512K environment.
!!    I have seen it on IBM mainframes and IBM PCs.
!!
!!    Subsequent changes per John S. Urban: see change log and git(1) history
implicit none
!private
public mat88
public mat88_get
!!public :: name_exists, get, geti, getr, getc, geti ! maybe a kind type or second parameter and returned value is of same type(?)
!!! scalar and array?
public mat88_put

! for other routines
public mat_flop
public mat_wasum
public mat_wdotcr
public mat_wdotci
! till get rid of type mismatches, the following are public

integer,parameter,private:: sp=kind(1.0),dp=kind(1.0d0)
!==================================================================================================================================!
! program limits
integer,parameter        :: GG_LINELEN=1024
integer,parameter        :: GG_MAX_NUMBER_OF_NAMES=480
integer,save             :: G_BIGMEM=-1
integer,parameter        :: GG_MAX_NAME_LENGTH=32       ! <WARNING> just began changing this to a constant

integer,parameter        :: GG_PAD(*)=[36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36]
!==================================================================================================================================!
character(len=GG_LINELEN),allocatable,save :: G_PSEUDO_FILE(:) ! allow for input to be passed from program instead of from file
logical                  :: G_PROMPT              ! companion for G_PSEUDO_FILE
logical,save             :: G_MARKDOWN=.false.    ! only process lines from ```mat* to ```
logical,save             :: G_ECHO=.false.        ! echo input lines

integer,parameter        :: G_EOL=99
integer                  :: G_LIN(GG_LINELEN)

integer                  :: G_LHS ! number of arguments on LHS
integer                  :: G_RHS ! number of arguments on RHS
integer                  :: G_FIN
integer                  :: G_FUN
integer                  :: G_FMT
integer                  :: G_RIO
integer                  :: G_INPUT_LUN
integer                  :: G_OUTPUT_LUN
integer                  :: G_PTZ
integer                  :: G_SYM
integer                  :: G_SYN(GG_MAX_NAME_LENGTH)
!==================================================================================================================================!
integer                  :: G_CURRENT_RANDOM_SEED
integer                  :: G_CURRENT_RANDOM_TYPE     ! [0] uniform distribution
                                                      ! [*] normal distribution
integer                  :: G_FLOP_COUNTER(2)
integer                  :: G_DEBUG_LEVEL             ! select which debug messages to display. zero (0) is off
logical                  :: G_FILE_OPEN_ERROR         ! flag whether file open error occurred or not
integer                  :: G_ERR
integer                  :: G_LINECOUNT(4)            ! [1] lines displayed since count started
                                                      ! [2] line limit before warning (ie. page length+1)
                                                      ! [3] 0 or 1 for "semi" mode to be on or off
                                                      ! [4] flag from "exec" command, and ...

integer                  :: G_BUF(GG_LINELEN)
!==================================================================================================================================!
! PARSING
integer,parameter        :: G_PSIZE=32                        ! stack size for pseudo-recursion
integer                  :: G_IDS(GG_MAX_NAME_LENGTH,G_PSIZE)
integer                  :: G_PSTK(G_PSIZE)
integer                  :: G_RSTK(G_PSIZE)
integer                  :: G_PT

integer                  :: G_CHRA ! current character in line
integer                  :: G_LINE_POINTER(6) ! [1] first character to process in current line
                                              ! [2] last character to process in current line
                                              ! [3]
                                              ! [4] pointer into current character in current line being processed
                                              ! [5]
                                              ! [6]
!==================================================================================================================================!

integer                     :: G_STACK_IDS(GG_MAX_NAME_LENGTH, GG_MAX_NUMBER_OF_NAMES)
integer                     :: G_STACK_ID_LOC(GG_MAX_NUMBER_OF_NAMES)
integer                     :: G_STACK_ROWS(GG_MAX_NUMBER_OF_NAMES)
integer                     :: G_STACK_COLS(GG_MAX_NUMBER_OF_NAMES)
doubleprecision,allocatable :: G_STACK_REALS(:), G_STACK_IMAGS(:)   ! set to size of G_BIGMEM

integer                     :: G_TOP_OF_SAVED, G_BOTTOM_OF_SCRATCH_IN_USE

!   Two large real arrays, G_STACK_REALS and G_STACK_IMAGS (for real and imaginary parts), are used to store all
!   the matrices. Four integer arrays (G_STACK_IDS, G_STACK_ROWS, G_STACK_COLS, G_STACK_ID_LOC) are used to store the names,
!   the row and column dimensions, and the pointers into the real stacks. The following diagram illustrates this storage scheme.
!
!    TOP        IDSTK     MSTK NSTK LSTK               STKR      STKI
!     --      -- -- -- --   --   --   --              --------   --------
!    |  |--->|  |  |  |  | |  | |  | |  |----------->|        | |        |
!     --      -- -- -- --   --   --   --              --------   --------
!            |  |  |  |  | |  | |  | |  |            |        | |        |
!             -- -- -- --   --   --   --              --------   --------
!                  .         .    .    .                  .          .
!                  .         .    .    .                  .          .
!                  .         .    .    .                  .          .
!             -- -- -- --   --   --   --              --------   --------
!    BOT     |  |  |  |  | |  | |  | |  |            |        | |        |
!     --      -- -- -- --   --   --   --              --------   --------
!    |  |--->| X|  |  |  | | 2| | 1| |  |----------->|  3.14  | |  0.00  |
!     --      -- -- -- --   --   --   --              --------   --------
!            | A|  |  |  | | 2| | 2| |  |---------   |  0.00  | |  1.00  |
!             -- -- -- --   --   --   --          \   --------   --------
!            | E| P| S|  | | 1| | 1| |  |-------   ->| 11.00  | |  0.00  |
!             -- -- -- --   --   --   --        \     --------   --------
!            | F| L| O| P| | 1| | 2| |  |------  \   | 21.00  | |  0.00  |
!             -- -- -- --   --   --   --       \  \   --------   --------
!            | E| Y| E|  | |-1| |-1| |  |---    \ |  | 12.00  | |  0.00  |
!             -- -- -- --   --   --   --    \   | |   --------   --------
!            | R| A| N| D| | 1| | 1| |  |-   \  | |  | 22.00  | |  0.00  |
!             -- -- -- --   --   --   --  \  |  \ \   --------   --------
!                                         |  \   \ ->| 1.E-15 | |  0.00  |
!                                         \   \   \   --------   --------
!                                          \   \   ->|  0.00  | |  0.00  |
!                                           \   \     --------   --------
!                                            \   \   |  0.00  | |  0.00  |
!                                             \   \   --------   --------
!                                              \   ->|  1.00  | |  0.00  |
!                                               \     --------   --------
!                                                --->| URAND  | |  0.00  |
!                                                     --------   --------
!
!   The top portion of the stack is used for temporary variables and the bottom portion for saved variables. The figure shows the
!   situation after the line
!
!      A = [11,12; 21,22],  x = [3.14, sqrt(-1)]'
!
!   has been processed. The four permanent names, "eps", "flop", "rand"
!   and "eye", occupy the last four positions of the variable stacks.
!   RAND has dimensions 1 by 1, but whenever its value is requested,
!   a random number generator is used instead. "eye" has dimensions -1
!   by -1 to indicate that the actual dimensions must be determined
!   later by context. The two saved variables have dimensions 2 by 2
!   and 2 by 1 and so take up a total of 6 locations.
!
!   Subsequent statements involving A and x will result in
!   temporary copies being made in the top of the stack for use in
!   the actual calculations. Whenever the top of the stack reaches
!   the bottom, a message indicating memory has been exceeded is
!   printed, but the current variables are not affected.
!
!   This modular structure makes it possible to implement MAT88
!   on a system with a limited amount of memory. The object code for
!   the MATFN's and the LINPACK-EISPACK subroutines is rarely needed.
!   Although it is not standard, many Fortran operating systems
!   provide some overlay mechanism so that this code is brought into
!   the main memory only when required. The variables, which occupy
!   a relatively small portion of the memory, remain in place, while
!   the subroutines which process them are loaded a few at a time.
!==================================================================================================================================!
!
! CHARACTER SET
integer,parameter        :: G_CHARSET_SIZE=87      ! number of characters in character set

!            0       10       20       30       40       50
!
!     0      0        A        K        U   COLON  :  LESS   <
!     1      1        B        L        V   PLUS   +  GREAT  >
!     2      2        C        M        W   MINUS  -
!     3      3        D        N        X   STAR   *
!     4      4        E        O        Y   SLASH  /
!     5      5        F        P        Z   BSLASH \
!     6      6        G        Q  BLANK     EQUAL  =
!     7      7        H        R  LPAREN (  DOT    .
!     8      8        I        S  RPAREN )  COMMA  ,
!     9      9        J        T  SEMI   ;  QUOTE  '

! unused: `~!#%^&_?

character(len=G_CHARSET_SIZE),parameter ::  G_DEFINE_CHARSET=&
                            &"0123456789abcdefghijklmnopqrstuvwxyz ();:+-*/\=.,'<>ABCDEFGHIJKLMNOPQRSTUVWXYZ`~!#%^&_?"
! ALTERNATE CHARACTER SET     0---------1---------2---------3---------4---------5---------6---------7-------
!                             012345678901234567890123456789012345678901234567890123456789012345678901234567890123456

character(len=G_CHARSET_SIZE),parameter ::  G_DEFINE_ALT_CHARSET=&
                            &'0123456789abcdefghijklmnopqrstuvwxyz {};|+-*/$=@,"[]ABCDEFGHIJKLMNOPQRSTUVWXYZ`~!#%^&_?'
! CHARACTER SET               0---------1---------2---------3---------4---------5---------6---------7-------
!                             012345678901234567890123456789012345678901234567890123456789012345678901234567890123456

integer,parameter :: score=85
integer,parameter :: blank=36
integer,parameter :: bslash=45
integer,parameter :: colon=40
integer,parameter :: comma=48
integer,parameter :: dot=47
integer,parameter :: dstar=54
integer,parameter :: equal=46
integer,parameter :: great=51
integer,parameter :: less=50
integer,parameter :: lparen=37
integer,parameter :: minus=42
integer,parameter :: plus=41
integer,parameter :: quote=49
integer,parameter :: rparen=38
integer,parameter :: semi=39
integer,parameter :: slash=44
integer,parameter :: star=43

integer,parameter :: zero=0
integer,parameter :: name=1
integer,parameter :: num=0

integer,parameter :: a_low=10
integer,parameter :: a_up=52
integer,parameter :: d_low=13
integer,parameter :: d_up=55
integer,parameter :: e_low=14
integer,parameter :: e_up=56
integer,parameter :: z_low=35
integer,parameter :: z_up=77

integer                  :: G_CHARSET(G_CHARSET_SIZE)      ! G_DEFINE_CHARSET converted to "Hollerith" values
integer                  :: G_ALT_CHARSET(G_CHARSET_SIZE)  ! G_DEFINE_ALT_CHARSET converted to "Hollerith" values

character(len=:),allocatable :: G_HELP_TEXT(:)

interface mat88
   module procedure mat88_init
   module procedure mat88_cmd
   module procedure mat88_cmds
end interface mat88
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    MAT88(3f) - [M_matrix] initialize and/or pass commands to matrix
!!    laboratory interpreter
!!##SYNOPSIS
!!
!!
!!   subroutine MAT88(init,cmd)
!!
!!    integer,intent(in),optional :: init
!!    character(len=*),intent(in),optional :: cmd
!!       or
!!    character(len=*),intent(in),optional :: cmd(:)
!!
!!##DESCRIPTION
!!    MAT88(3f) is modeled on MATLAB(3f) (MATrix LABoratory), a FORTRAN
!!    package developed by Argonne National Laboratories for in-house use.
!!    It provides comprehensive vector and tensor operations in a package
!!    which may be programmed, either through a macro language or through
!!    execution of script files.
!!
!!    MAT88(3f) is reentrant and recursive. Functions supported include (but
!!    are not by any means limited to) sin, cos, tan, arcfunctions, upper
!!    triangular, lower triangular, determinants, matrix multiplication,
!!    identity, Hilbert matrices, eigenvalues and eigenvectors, matrix
!!    roots and products, inversion and so on and so forth.
!!
!!    The HELP command describes using the interpreter.
!!
!!##OPTIONS
!!    INIT    indicate size of scratch space to allocate and (re)initialize
!!            MAT88.
!!
!!    CMD     MAT88 command(s) to perform. May be CHARACTER scalar or vector
!!
!!    INIT and CMD cannot be combined on a single call.
!!
!!    The first call may be an initialization declaring the number of
!!    doubleprecision values to allocate for the combined scratch and
!!    variable storage area. This is a required initial command. It may be
!!    repeated. A size of zero will deallocate any allocated storage (after
!!    which the routine cannot be called with commands until reallocated by
!!    another call to mat88()).
!!
!!    If no parameters are supplied interactive mode is entered.
!!
!!    If a CMD is passed and no previous initialization call was made the
!!    scratch space will be allocated to 200000.
!!
!!##EXAMPLE
!!
!!
!!   Sample program
!!
!!       program demo_MAT88
!!       use M_matrix, only : mat88
!!
!!          write(*,'(a)')'optionally initialize scratch area size'
!!          call MAT88(20000)
!!
!!          write(*,'(a)')'do some commands'
!!          call MAT88([character(len=80) :: &
!!          & 'semi;                         ',&
!!          & 'a=magic(4),b=-a               ',&
!!          & 'a+b;a;b                       ',&
!!          & 'display("That is all Folks!") '])
!!
!!          write(*,'(a)')'do a single command'
!!          call MAT88('who')
!!
!!          write(*,'(a)')'enter interactive mode'
!!          call MAT88()
!!
!!          write(*,'(a)')'ending program'
!!       end program demo_MAT88
!!
!!   Example 2:
!!
!!    program bigmat
!!    use M_matrix, only : mat88
!!       ! pass strings to MAT88 but do not enter interactive mode
!!       call mat88(20000)                  ! initialize silently
!!       call mat88( 'a=[1 2 3 4; 5 6 7 8]')
!!       call mat88( [character(len=80) :: &
!!        & 'semi;lines(9999)                                      ',&
!!        & '// create a magic square and add 100 to all the values',&
!!        & 'A=magic(4),<X,Y>=size(A)                              ',&
!!        & 'B=A+ones(X,Y)*100                                     ',&
!!        & '// save all current values to a file                  ',&
!!        & 'save("sample.mat")                                    ',&
!!        & '// clear all user values                              ',&
!!        & 'clear                                                 ',&
!!        & '// show variable names, load values from file         ',&
!!        & '// and show again to show the variables are restored  ',&
!!        & 'who;load("sample.mat");who                            '])
!!    end program bigmat
!!
!!   Sample program with custom user function
!!
!!       program sample_user
!!       use M_matrix, only : mat88
!!          call MAT88(20000)
!!          call MAT88(' ')
!!       end program
!!
!!       !-------------------------------------------------------------
!!       SUBROUTINE mat88_user(A,M,N,S,T)  ! sample mat88_user routine
!!       ! Allows personal  Fortran  subroutines  to  be  linked  into
!!       ! MAT88. The subroutine should have the heading
!!       !
!!       !               SUBROUTINE mat88_user(A,M,N,S,T)
!!       !               DOUBLEPRECISION A(M,N),S,T
!!       !
!!       ! The MAT88 statement Y = USER(X,s,t) results in a call to
!!       ! the subroutine with a copy of the matrix X stored in the
!!       ! argument A, its column and row dimensions in M and N,
!!       ! and the scalar parameters S and T stored in S and T.
!!       ! If S and T are omitted, they are set to 0.0. After
!!       ! the return, A is stored in Y. The dimensions M and
!!       ! N may be reset within the subroutine. The statement Y =
!!       ! USER(K) results in a call with M = 1, N = 1 and A(1,1) =
!!       ! FLOAT(K). After the subroutine has been written, it must
!!       ! be compiled and linked to the MAT88 object code within the
!!       ! local operating system.
!!       !
!!       integer M,N
!!       DOUBLEPRECISION A(M,N),S,T
!!       !
!!       if(s.eq.0.and.t.eq.0)then
!!          ! print statistics for matrix, for example
!!          write(*,*)'m=',m
!!          write(*,*)'n=',n
!!          write(*,*)'s=',s
!!          write(*,*)'t=',t
!!
!!          DO i10 = 1, M
!!             write(*,*)(a(i10,i20),i20=1,n)
!!          enddo
!!       else  ! a(i,j)=a(i,j)*s+t  in a linear fashion
!!          DO i30 = 1, M
!!             DO i40 = 1, N
!!                a(i30,i40)=a(i30,i40)*S+T
!!             enddo
!!          enddo
!!       endif
!!       END SUBROUTINE mat88_user
!!
!!       ! end program sample_user
!!
!!  Example inputs
!!
!!     :avg:
!!
!!     for i = 2:2:n, for j = 2:2:n, t = (a(i-1,j-1)+a(i-1,j)+a(i,j-1)+a(i,j))/4; ...
!!     a(i-1,j-1) = t; a(i,j-1) = t; a(i-1,j) = t; a(i,j) = t;
!!
!!     :cdiv:
!!
!!     // ======================================================
!!     // cdiv
!!     a=sqrt(random(8))
!!     ar = real(a); ai = imag(a); br = real(b); bi = imag(b);
!!     p = bi/br;
!!     t = (ai - p*ar)/(br + p*bi);
!!     cr = p*t + ar/br;
!!     ci = t;
!!     p2 = br/bi;
!!     t2 = (ai + p2*ar)/(bi + p2*br);
!!     ci2 = p2*t2 - ar/bi;
!!     cr2 = t2;
!!     s = abs(br) + abs(bi);
!!     ars = ar/s;
!!     ais = ai/s;
!!     brs = br/s;
!!     bis = bi/s;
!!     s = brs**2 + bis**2;
!!     cr3 = (ars*brs + ais*bis)/s;
!!     ci3 = (ais*brs - ars*bis)/s;
!!     [cr ci; cr2 ci2; cr3 ci3]
!!     // ======================================================
!!
!!     :exp:
!!
!!     t = 0*x + eye; s = 0*eye(x); n = 1;
!!     while abs(s+t-s) > 0, s = s+t, t = x*t/n, n = n + 1
!!
!!     :four:
!!      n
!!      pi = 4*atan(1);
!!      i = sqrt(-1);
!!      w = exp(2*pi*i/n);
!!      F = [];
!!      for k = 1:n, for j = 1:n, F(k,j) = w**((j-1)*(k-1));
!!      F = F/sqrt(n);
!!      alpha = r*pi;
!!      rho = exp(i*alpha);
!!      S = log(rho*F)/i - alpha*eye;
!!      serr = norm(imag(S),1);
!!      S = real(S);
!!      serr = serr + norm(S-S',1)
!!      S = (S + S')/2;
!!      ferr = norm(F-exp(i*S),1)
!!
!!      :gs:
!!      for k = 1:n, for j = 1:k-1, d = x(k,:)*x(j,:)'; x(k,:) = x(k,:) - d*x(j,:); ...
!!      end, s = norm(x(k,:)), x(k,:) = x(k,:)/s;
!!
!!      :jacobi:
!!      [n, n] = size(A);
!!      X = eye(n);
!!      anorm = norm(A,'fro');
!!      cnt = 1;
!!      while cnt > 0, ...
!!        cnt = 0; ...
!!        for p = 1:n-1, ...
!!          for q = p+1:n, ...
!!            if anorm + abs(a(p,q)) > anorm, ...
!!              cnt = cnt + 1; ...
!!              exec('jacstep'); ...
!!            end, ...
!!          end, ...
!!        end, ...
!!        display(rat(A)), ...
!!      end
!!
!!      :jacstep:
!!
!!      d = (a(q,q)-a(p,p))*0.5/a(p,q);
!!      t = 1/(abs(d)+sqrt(d*d+1));
!!      if d < 0, t = -t; end;
!!      c = 1/sqrt(1+t*t);  s = t*c;
!!      R = eye(n); r(p,p)=c; r(q,q)=c; r(p,q)=s; r(q,p)=-s;
!!      X = X*R;
!!      A = R'*A*R;
!!
!!      :kron:
!!
!!      //  C = Kronecker product of A and B
!!      [m, n] = size(A);
!!      for i = 1:m, ...
!!         ci = a(i,1)*B; ...
!!         for j = 2:n, ci = [ci a(i,j)*B]; end ...
!!         if i = 1, C = ci; else, C = [C; ci];
!!
!!      :lanczos:
!!
!!      [n,n] = size(A);
!!      q1 = rand(n,1);
!!      ort
!!      alpha = []; beta = [];
!!      q = q1/norm(q1); r = A*q(:,1);
!!      for j = 1:n, exec('lanstep',0);
!!
!!      :lanstep:
!!
!!      alpha(j) = q(:,j)'*r;
!!      r = r - alpha(j)*q(:,j);
!!      if ort <> 0, for k = 1:j-1, r = r - r'*q(:,k)*q(:,k);
!!      beta(j) = norm(r);
!!      q(:,j+1) = r/beta(j);
!!      r = A*q(:,j+1) - beta(j)*q(:,j);
!!      if j > 1, T = diag(beta(1:j-1),1); T = diag(alpha) + T + T'; eig(T)
!!
!!      :mgs:
!!
!!      for k = 1:n, s = norm(x(k,:)), x(k,:) = x(k,:)/s; ...
!!         for j = k+1:n, d = x(j,:)*x(k,:)'; x(j,:) = x(j,:) - d*x(k,:);
!!
!!      :net:
!!
!!      C = [
!!      1   2   15  .   .   .
!!      2   1   3   .   .   .
!!      3   2   4   11  .   .
!!      4   3   5   .   .   .
!!      5   4   6   7   .   .
!!      6   5   8   .   .   .
!!      7   5   9   30  .   .
!!      8   6   9   10  11  .
!!      9   7   8   30  .   .
!!      10  8   12  30  31  34
!!      11  3   8   12  13  .
!!      12  10  11  34  36  .
!!      13  11  14  .   .   .
!!      14  13  15  16  38  .
!!      15  1   14  .   .   .
!!      16  14  17  20  35  37
!!      17  16  18  .   .   .
!!      18  17  19  .   .   .
!!      19  18  20  .   .   .
!!      20  16  19  21  .   .
!!      21  20  22  .   .   .
!!      22  21  23  .   .   .
!!      23  22  24  35  .   .
!!      24  23  25  39  .   .
!!      25  24  .   .   .   .
!!      26  27  33  39  .   .
!!      27  26  32  .   .   .
!!      28  29  32  .   .   .
!!      29  28  30  .   .   .
!!      30  7   9   10  29  .
!!      31  10  32  .   .   .
!!      32  27  28  31  34  .
!!      33  26  34  .   .   .
!!      34  10  12  32  33  35
!!      35  16  23  34  36  .
!!      36  12  35  38  .   .
!!      37  16  38  .   .   .
!!      38  14  36  37  .   .
!!      39  24  26  .   .   .
!!      ];
!!      [n, m] = size(C);
!!      A = 0*ones(n,n);
!!      for i=1:n, for j=2:m, k=c(i,j); if k>0, a(i,k)=1;
!!      check = norm(A-A',1), if check > 0, quit
!!      [X,D] = eig(A+eye);
!!      D = diag(D);  D = D(n:-1:1)
!!      X = X(:,n:-1:1);
!!      [x(:,1)/sum(x(:,1)) x(:,2) x(:,3) x(:,19)]
!!
!!      :pascal:
!!
!!      //Generate next Pascal matrix
!!      [k,k] = size(L);
!!      k = k + 1;
!!      L(k,1:k) = [L(k-1,:) 0] + [0 L(k-1,:)];
!!
!!      :pdq:
!!
!!      alpha = []; beta = 0; q = []; p = p(:,1)/norm(p(:,1));
!!      t = A'*p(:,1);
!!      alpha(1) = norm(t);
!!      q(:,1) = t/alpha(1);
!!      X = p(:,1)*(alpha(1)*q(:,1))'
!!      e(1) = norm(A-X,1)
!!      for j = 2:r, exec('pdqstep',ip); ...
!!         X = X + p(:,j)*(alpha(j)*q(:,j)+beta(j)*q(:,j-1))', ...
!!         e(j) = norm(A-X,1)
!!
!!      :pdqstep:
!!
!!      t = A*q(:,j-1) - alpha(j-1)*p(:,j-1);
!!         if ort>0, for i = 1:j-1, t = t - t'*p(:,i)*p(:,i);
!!      beta(j) = norm(t);
!!      p(:,j) = t/beta(j);
!!      t = A'*p(:,j) - beta(j)*q(:,j-1);
!!         if ort>0, for i = 1:j-1, t = t - t'*q(:,i)*q(:,i);
!!      alpha(j) = norm(t);
!!      q(:,j) = t/alpha(j);
!!
!!      :pop:
!!
!!      y = [ 75.995   91.972  105.711  123.203   ...
!!           131.669  150.697  179.323  203.212]'
!!      t = [ 1900:10:1970 ]'
!!      t = (t - 1940*ones(t))/40;   [t y]
!!      n = 8;  A(:,1) = ones(t);  for j = 2:n, A(:,j) = t .* A(:,j-1);
!!      A
!!      c = A\y
!!
!!      :qr:
!!
!!      scale = s(m);
!!      sm = s(m)/scale; smm1 = s(m-1)/scale; emm1 = e(m-1)/scale;
!!      sl = s(l)/scale; el = e(l)/scale;
!!      b = ((smm1 + sm)*(smm1 - sm) + emm1**2)/2;
!!      c = (sm*emm1)**2;
!!      shift = sqrt(b**2+c); if b < 0, shift = -shift;
!!      shift = c/(b + shift)
!!      f = (sl + sm)*(sl-sm) - shift
!!      g = sl*el
!!      for k = l: m-1, exec('qrstep',ip)
!!      e(m-1) = f
!!
!!      :qrstep:
!!
!!      exec('rot');
!!      if k <> l, e(k-1) = f
!!      f = cs*s(k) + sn*e(k)
!!      e(k) = cs*e(k) - sn*s(k)
!!      g = sn*s(k+1)
!!      s(k+1) = cs*s(k+1)
!!      exec('rot');
!!      s(k) = f
!!      f = cs*e(k) + sn*s(k+1)
!!      s(k+1) = -sn*e(k) + cs*s(k+1)
!!      g = sn*e(k+1)
!!      e(k+1) = cs*e(k+1)
!!
!!      :rho:
!!
!!      //Conductivity example.
!!      //Parameters ---
!!         rho       //radius of cylindrical inclusion
!!         n         //number of terms in solution
!!         m         //number of boundary points
!!      //initialize operation counter
!!         flop = [0 0];
!!      //initialize variables
!!         m1 = round(m/3);   //number of points on each straight edge
!!         m2 = m - m1;       //number of points with Dirichlet conditions
!!         pi = 4*atan(1);
!!      //generate points in Cartesian coordinates
!!         //right hand edge
!!         for i = 1:m1, x(i) = 1; y(i) = (1-rho)*(i-1)/(m1-1);
!!         //top edge
!!         for i = m2+1:m, x(i) = (1-rho)*(m-i)/(m-m2-1); y(i) = 1;
!!         //circular edge
!!         for i = m1+1:m2, t = pi/2*(i-m1)/(m2-m1+1); ...
!!            x(i) = 1-rho*sin(t);  y(i) = 1-rho*cos(t);
!!      //convert to polar coordinates
!!         for i = 1:m-1, th(i) = atan(y(i)/x(i));  ...
!!            r(i) = sqrt(x(i)**2+y(i)**2);
!!         th(m) = pi/2;  r(m) = 1;
!!      //generate matrix
!!         //Dirichlet conditions
!!         for i = 1:m2, for j = 1:n, k = 2*j-1; ...
!!            a(i,j) = r(i)**k*cos(k*th(i));
!!         //Neumann conditions
!!         for i = m2+1:m, for j = 1:n, k = 2*j-1; ...
!!            a(i,j) = k*r(i)**(k-1)*sin((k-1)*th(i));
!!      //generate right hand side
!!         for i = 1:m2, b(i) = 1;
!!         for i = m2+1:m, b(i) = 0;
!!      //solve for coefficients
!!         c = A$b
!!      //compute effective conductivity
!!         c(2:2:n) = -c(2:2:n)
!!         sigma = sum(c)
!!      //output total operation count
!!         ops = flop(2)
!!
!!      :rogers.exec:
!!
!!      exec('d.boug');                        // reads data
!!      [g,k] = size(p);               // p is matrix of gene frequencies
!!      wv = ncen/sum(ncen);           // ncen contains population sizes
!!      pbar = wv*p;                   // weighted average of p
!!      p = p - ones(g,1)*pbar;        // deviations from mean
!!      p = sqrt(diag(wv)) * p;        // weight rows of p by sqrt of pop size
!!      h = diag(pbar); h = h*(eye-h); // diagonal contains binomial variance: p*(1-p)
!!      r = p*inv(h)*p'/k;             // normalized covariance matrix
!!      eig(r)'
!!
!!      :rosser:
!!
!!      A  = [
!!        611.  196. -192.  407.   -8.  -52.  -49.   29.
!!        196.  899.  113. -192.  -71.  -43.   -8.  -44.
!!       -192.  113.  899.  196.   61.   49.    8.   52.
!!        407. -192.  196.  611.    8.   44.   59.  -23.
!!         -8.  -71.   61.    8.  411. -599.  208.  208.
!!        -52.  -43.   49.   44. -599.  411.  208.  208.
!!        -49.   -8.    8.   59.  208.  208.   99. -911.
!!         29.  -44.   52.  -23.  208.  208. -911.   99.  ];
!!
!!      :rot:
!!
!!      // subexec rot(f,g,cs,sn)
!!         rho = g; if abs(f) > abs(g), rho = f;
!!         cs = 1.0; sn = 0.0; z = 1.0;
!!         r = norm([f g]); if rho < 0, r = -r; r
!!         if r <> 0.0, cs = f/r
!!         if r <> 0.0, sn = g/r
!!         if abs(f) > abs(g), z = sn;
!!         if abs(g) >= abs(f), if cs <> 0, z = 1/cs;
!!         f = r;
!!         g = z;
!!
!!      :rqi:
!!
!!      rho = (x'*A*x)
!!      x = (A-rho*eye)\x;
!!      x = x/norm(x)
!!
!!      :setup:
!!
!!      diary('xxx')
!!      !tail -f xxx > /dev/tty1 &
!!      !tail -f xxx > /dev/tty2 &
!!
!!      :sigma:
!!
!!      RHO = .5  M = 20  N = 10   SIGMA =  1.488934271883534
!!      RHO = .5  M = 40  N = 20   SIGMA =  1.488920312974229
!!      RHO = .5  M = 60  N = 30   SIGMA =  1.488920697912116
!!
!!      :strut.mat:
!!
!!      // Structure problem, Forsythe, Malcolm and Moler, p. 62
!!      s =  sqrt(2)/2;
!!      A = [
!!      -s  .  .  1  s   .  .  .  .  .  .  .  .  .  .  .  .
!!      -s  . -1  . -s   .  .  .  .  .  .  .  .  .  .  .  .
!!       . -1  .  .  .   1  .  .  .  .  .  .  .  .  .  .  .
!!       .  .  1  .  .   .  .  .  .  .  .  .  .  .  .  .  .
!!       .  .  . -1  .   .  .  1  .  .  .  .  .  .  .  .  .
!!       .  .  .  .  .   . -1  .  .  .  .  .  .  .  .  .  .
!!       .  .  .  . -s -1  .  .  s  1  .  .  .   .  .  .  .
!!       .  .  .  .  s   .  1  .  s  .  .  .  .  .  .  .  .
!!       .  .  .  .  .   .  . -1 -s  .  .  1  s  .  .  .  .
!!       .  .  .  .  .   .  .  . -s  . -1  . -s  .  .  .  .
!!       .  .  .  .  .   .  .  .  . -1  .  .  .  1  .  .  .
!!       .  .  .  .  .   .  .  .  .  .  1  .  .  .  .  .  .
!!       .  .  .  .  .   .  .  .  .  .  . -1  .  .  .  s  .
!!       .  .  .  .  .   .  .  .  .  .  .  .  .  . -1 -s  .
!!       .  .  .  .  .   .  .  .  .  .  .  . -s -1  .  .  1
!!       .  .  .  .  .   .  .  .  .  .  .  .  s  .  1  .  .
!!       .  .  .  .  .   .  .  .  .  .  .  .  .  .  . -s -1];
!!      b = [
!!       .  .  . 10  .   .  . 15  .  .  .  .  .  .  . 10  .]';
!!
!!      :test1:
!!
!!      // -----------------------------------------------------------------
!!      // start a new log file
!!      sh rm -fv log.txt
!!      diary('log.txt')
!!      // -----------------------------------------------------------------
!!      titles=['GNP deflator'
!!       'GNP         '
!!       'Unemployment'
!!       'Armed Force '
!!       'Population  '
!!       'Year        '
!!       'Employment  '];
!!      data = ...
!!      [ 83.0  234.289  235.6  159.0  107.608  1947  60.323
!!        88.5  259.426  232.5  145.6  108.632  1948  61.122
!!        88.2  258.054  368.2  161.6  109.773  1949  60.171
!!        89.5  284.599  335.1  165.0  110.929  1950  61.187
!!        96.2  328.975  209.9  309.9  112.075  1951  63.221
!!        98.1  346.999  193.2  359.4  113.270  1952  63.639
!!        99.0  365.385  187.0  354.7  115.094  1953  64.989
!!       100.0  363.112  357.8  335.0  116.219  1954  63.761
!!       101.2  397.469  290.4  304.8  117.388  1955  66.019
!!       104.6  419.180  282.2  285.7  118.734  1956  67.857
!!       108.4  442.769  293.6  279.8  120.445  1957  68.169
!!       110.8  444.546  468.1  263.7  121.950  1958  66.513
!!       112.6  482.704  381.3  255.2  123.366  1959  68.655
!!       114.2  502.601  393.1  251.4  125.368  1960  69.564
!!       115.7  518.173  480.6  257.2  127.852  1961  69.331
!!       116.9  554.894  400.7  282.7  130.081  1962  70.551];
!!      short
!!      X = data;
!!      [n,p] = size(X)
!!      mu = ones(1,n)*X/n
!!      X = X - ones(n,1)*mu;  X = X/diag(sqrt(diag(X'*X)))
!!      corr = X'*X
!!      y = data(:,p); X = [ones(y) data(:,1:p-1)];
!!      long e
!!      beta = X\y
!!      expected = [ ...
!!         -3.482258634594421D+03
!!          1.506187227124484D-02
!!         -3.581917929257409D-02
!!         -2.020229803816908D-02
!!         -1.033226867173703D-02
!!         -5.110410565317738D-02
!!          1.829151464612817D+00
!!      ]
!!      display('EXPE and BETA should be the same')
!!
!!      :tryall:
!!
!!      diary('log.txt')
!!      a=magic(8)
!!      n=3
!!      exec('avg')
!!      b=random(8,8)
!!      exec('cdiv')
!!      exec('exp')
!!      exec('four')
!!      exec('gs')
!!      exec('jacobi')
!!      // jacstep
!!      exec('kron')
!!      exec('lanczos')
!!      // lanstep
!!      exec('longley')
!!      exec('mgs')
!!      exec('net')
!!      exec('pascal')
!!      exec('pdq')
!!      // pdqstep
!!      exec('pop')
!!      exec('qr')
!!      // qrstep
!!      exec('rho')
!!      exec('rosser')
!!      // rot
!!      exec('rqi')
!!      exec('setup')
!!      exec('sigma')
!!      exec('strut.mat')
!!      exec('w5')
!!      exec('rogers.exec
!!      exec('rogers.load
!!
!!      :w5:
!!
!!      w5 = [
!!              1.     1.      0.      0.      0.
!!            -10.     1.      1.      0.      0.
!!             40.     0.      1.      1.      0.
!!            205.     0.      0.      1.      1.
!!            024.     0.      0.      0.     -4.
!!           ]
subroutine MAT88_init(init,echo,markdown)

! ident_1="@(#)M_matrix::mat88(3f): initialize and/or pass commands to matrix laboratory interpreter"

integer,intent(in)          :: init
logical,intent(in),optional :: echo
logical,intent(in),optional :: markdown
doubleprecision             :: s,t
integer,parameter           :: EPS(GG_MAX_NAME_LENGTH)=   [14,25,28,36,36,GG_PAD(2:)]
integer,parameter           :: FLOPS(GG_MAX_NAME_LENGTH)= [15,21,24,25,28,GG_PAD(2:)]
integer,parameter           :: EYE(GG_MAX_NAME_LENGTH)=   [14,34,14,36,36,GG_PAD(2:)]
integer,parameter           :: RAND(GG_MAX_NAME_LENGTH)=  [27,10,23,13,36,GG_PAD(2:)]

   if(present(echo)) G_ECHO=echo
   if(present(markdown)) G_MARKDOWN=markdown

   G_PROMPT=.true.

   if(allocated(G_PSEUDO_FILE))deallocate(G_PSEUDO_FILE)
   allocate(G_PSEUDO_FILE(0))

   G_BIGMEM=INIT
   if(G_BIGMEM.lt.0)G_BIGMEM=200000
   if(allocated(G_STACK_REALS) )deallocate(G_STACK_REALS)
   if(allocated(G_STACK_IMAGS) )deallocate(G_STACK_IMAGS)
   allocate(G_STACK_REALS(G_BIGMEM),G_STACK_IMAGS(G_BIGMEM))              ! set to size of G_BIGMEM

   G_INPUT_LUN = STDIN                                                    ! unit number for terminal input
   call mat_files(G_INPUT_LUN,G_BUF)
   G_RIO = G_INPUT_LUN                                                    ! current file to read commands from
   G_OUTPUT_LUN = STDOUT                                                  ! unit number for terminal output
   call mat_files(G_OUTPUT_LUN,G_BUF)

   call mat_help_text()                                                   ! initialize help text
   G_CURRENT_RANDOM_SEED = 0                                              ! random number seed
   G_CURRENT_RANDOM_TYPE = 0                                              ! set the type of random numbers to compute
   G_LINECOUNT(2) = 23                                                    ! initial line limit for paging output

   call mat_str2buf(G_DEFINE_CHARSET,G_CHARSET,G_CHARSET_SIZE)            ! convert character set string to hollerith
   call mat_str2buf(G_DEFINE_ALT_CHARSET,G_ALT_CHARSET,G_CHARSET_SIZE)    ! convert character set string to hollerith

   G_TOP_OF_SAVED = GG_MAX_NUMBER_OF_NAMES-3  ! move up to allow room for the built-in values eps, flops, eye, rand

   call mat_wset(5,0.0D0,0.0d0,G_STACK_REALS(G_BIGMEM-4),G_STACK_IMAGS(G_BIGMEM-4),1)

   call mat_putid(G_STACK_IDS(1,GG_MAX_NUMBER_OF_NAMES-3),EPS)
   G_STACK_ID_LOC(GG_MAX_NUMBER_OF_NAMES-3) = G_BIGMEM-4
   G_STACK_ROWS(GG_MAX_NUMBER_OF_NAMES-3) = 1
   G_STACK_COLS(GG_MAX_NUMBER_OF_NAMES-3) = 1
   ! interesting way to calculate the epsilon value of a machine
   s = 1.0d0
   SET_ST: do
      s = s/2.0D0
      t = 1.0d0 + s
      if (t .LE. 1.0d0) exit
   enddo SET_ST

   G_STACK_REALS(G_BIGMEM-4) = 2.0d0*s

   call mat_putid(G_STACK_IDS(1,GG_MAX_NUMBER_OF_NAMES-2),flops)
   G_STACK_ID_LOC(GG_MAX_NUMBER_OF_NAMES-2) = G_BIGMEM-3
   G_STACK_ROWS(GG_MAX_NUMBER_OF_NAMES-2) = 1
   G_STACK_COLS(GG_MAX_NUMBER_OF_NAMES-2) = 2

   call mat_putid(G_STACK_IDS(1,GG_MAX_NUMBER_OF_NAMES-1), eye)
   G_STACK_ID_LOC(GG_MAX_NUMBER_OF_NAMES-1) = G_BIGMEM-1
   G_STACK_ROWS(GG_MAX_NUMBER_OF_NAMES-1) = -1
   G_STACK_COLS(GG_MAX_NUMBER_OF_NAMES-1) = -1
   G_STACK_REALS(G_BIGMEM-1) = 1.0D0

   call mat_putid(G_STACK_IDS(1,GG_MAX_NUMBER_OF_NAMES), rand)
   G_STACK_ID_LOC(GG_MAX_NUMBER_OF_NAMES) = G_BIGMEM
   G_STACK_ROWS(GG_MAX_NUMBER_OF_NAMES) = 1
   G_STACK_COLS(GG_MAX_NUMBER_OF_NAMES) = 1

   G_FMT = 1
   G_FLOP_COUNTER(1) = 0
   G_FLOP_COUNTER(2) = 0
   G_DEBUG_LEVEL = 0
   G_PTZ = 0
   G_PT = G_PTZ
   G_ERR = 0

end subroutine MAT88_init
!==================================================================================================================================
subroutine MAT88_cmd(input_string,echo,markdown)

! ident_2="@(#)M_matrix::mat88(3f): run a single command in matrix laboratory interpreter and return to calling program"

character(len=*),intent(in) :: input_string
logical,intent(in),optional :: echo
logical,intent(in),optional :: markdown

   call mat88_cmds( [input_string],echo=echo,markdown=markdown)

end subroutine MAT88_cmd
!==================================================================================================================================
subroutine MAT88_cmds(pseudo_file,echo,markdown)

! ident_3="@(#)M_matrix::mat88(3f): run an array of commands in matrix laboratory interpreter and return to calling program"

character(len=*),intent(in),optional :: pseudo_file(:)
logical,intent(in),optional          :: echo
logical,intent(in),optional          :: markdown

   if(present(echo)) G_ECHO=echo
   if(present(markdown)) G_MARKDOWN=markdown

   if(G_BIGMEM.LT.0)then
      call mat88_init(200000)
   else
      G_PROMPT=.true.
   endif

   if(present(pseudo_file))then
      G_PSEUDO_FILE=[character(len=GG_LINELEN) :: pseudo_file,'quit']
      G_PROMPT=.false.
   endif

   PARSE_LINE : do
      call mat_parse()
      select case(G_FUN)
      case(1) ; call mat_matfn1()
      case(2) ; call mat_matfn2()
      case(3) ; call mat_matfn3()
      case(4) ; call mat_matfn4()
      case(5) ; call mat_matfn5()
      case(6) ; call mat_matfn6()
      case(21); call mat_matfn1()
      case(99); exit PARSE_LINE
      case default
      end select
   enddo PARSE_LINE

end subroutine MAT88_cmds
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_err(n)

! ident_4="@(#)M_matrix::mat_err(3fp): given error number, write associated error message and set G_ERR"

integer,intent(in)   :: n

integer              :: i
integer              :: k
integer              :: lb
integer              :: lt
character(len=255)   :: msg

   G_ERR = n
   select case(n)
    case(1);  msg='Improper multiple assignment'
    case(2);  msg='Improper factor'
    case(3);  msg='Expected right parenthesis'
    case(4);
      G_BUF(:GG_MAX_NAME_LENGTH) = G_CHARSET(G_IDS(:GG_MAX_NAME_LENGTH,G_PT+1)+1) ! extract variable name into buffer
      call mat_buf2str(msg,G_BUF,GG_MAX_NAME_LENGTH)                              ! convert buffer to string
      msg='Undefined variable: '//msg(1:GG_MAX_NAME_LENGTH)
    case(5);  msg='Column lengths do not match'
    case(6);  msg='Row lengths do not match'
    case(7);  msg='Text too long'
    case(8);  msg='Incompatible for ADDITION'
    case(9);  msg='Incompatible for SUBTRACTION'
    case(10); msg='Incompatible for MULTIPLICATION'
    case(11); msg='Incompatible for RIGHT DIVISION'
    case(12); msg='Incompatible for LEFT DIVISION'
    case(13); msg='Improper assignment to PERMANENT VARIABLE'
    case(14); msg='EYE-dentity undefined by CONTEXT'
    case(15); msg='Improper assignment to submatrix'
    case(16); msg='Improper command'
    case(17)
      lb = G_BIGMEM - G_STACK_ID_LOC(G_TOP_OF_SAVED) + 1
      lt = g_err + G_STACK_ID_LOC(G_TOP_OF_SAVED)
      call journal(' Too much memory required')
      write(msg,'(1X,I7,'' Variables,'',I7,'' Temporaries,'',I7,'' Available.'')') lb,lt,G_BIGMEM
    case(18); msg='Too many names'
    case(19); msg='Matrix is singular to working precision'
    case(20); msg='Matrix must be square'
    case(21); msg='Subscript out of range'
    case(22); write(msg, "(1X,'Recursion difficulties',*(I4))") (G_RSTK(I),I=1,G_PT)
    case(23); msg='Only 1, 2 or INF norm of matrix'
    case(24); msg='No convergence'
    case(25); msg='Can not use function name as variable'
    case(26); msg='Too complicated (STACK OVERFLOW)'
    case(27); msg='Division by zero is a NO-NO'
    case(28); msg='Empty macro'
    case(29); msg='Not positive definite'
    case(30); msg='Improper exponent'
    case(31); msg='Improper string'
    case(32); msg='Singularity of LOG or ATAN'
    case(33); msg='Too many colons'
    case(34); msg='Improper FOR clause'
    case(35); msg='Improper WHILE or IF clause'
    case(36); msg='Argument out of range'
    case(37); msg='Improper MACROS'
    case(38); msg='Improper file name'
    case(39); msg='Incorrect number of arguments'
    case(40); msg='Expecting statement terminator'
    case default
       write(msg,'("*mat_err* internal error: unknown error code =",i0)')n
       call journal()
       return
   end select

   k = max(1,G_LINE_POINTER(2) - G_LINE_POINTER(1)) ! number of spaces to shift arrow by
   call journal(' '//repeat(' ',k)//'/\--ERROR:'//msg)

end subroutine mat_err
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_files(lunit,iname,status)
integer                      :: lunit             ! logical unit number
                                                  ! if LUNIT is zero, return
                                                  ! if LUNIT = standard input, return
                                                  ! if LUNIT = standard output, return
                                                  ! if LUNIT is positive, open the unit to file name INAME
                                                  ! if LUNIT is negative, close the unit number
integer                      :: iname(GG_LINELEN) ! INAME = FILE NAME, 1 character per word
                                                  ! how to know length of iname?
character(len=1024)          :: name
character(len=1024)          :: temp1
character(len=*),optional    :: status
character(len=20)            :: status_local
integer                      :: ios
   if(present(status))then
      status_local=status
   else
      status_local='UNKNOWN'
   endif

   !  Amiga dependent stuff squeezes the NAME from one CHAR per word to one per byte
   if (G_DEBUG_LEVEL .eq. 1) then
      call journal('sc','*MLFILES* LUNIT=', LUNIT)
      name(1:10)='*MLFILES* INAME='
      call mat_buf2str(name(11:),iname,GG_LINELEN)
      call journal(name)
   endif

   G_FILE_OPEN_ERROR=.false.
   select case(lunit)
    case(0)      ! error catcher
    case(stdin)  ! if unit is standard input return
    case(stdout) ! if unit is standard output return
    case(8)      ! diary file
       call mat_buf2str(name,iname,GG_LINELEN)
       call journal('O',trim(name)) ! open up trail file
    case(:-1)
      if(lunit.eq.-8)then
         call journal('O','')                                        ! close trail file
      else                                                           ! if LUNIT is negative, close the unit
         flush(unit=-lunit,iostat=ios)
         close(unit=-lunit,iostat=ios)
      endif
    case default                                                     !  ALL OTHER FILES
      call mat_buf2str(name,iname,GG_LINELEN)
      open(unit=lunit,file=name,status=status_local,iostat=ios)      ! open a file
      if(ios.ne.0)then                                               ! general file open failure
         call journal('OPEN failed on file '//name)
         G_FILE_OPEN_ERROR=.true.                                    ! set the flag a file error occurred
         G_RIO=G_INPUT_LUN                                           ! set current file to read input lines from/to G_INPUT_LUN
      else
         G_FILE_OPEN_ERROR=.false.                                   ! set the flag a file error did not occurr
      endif
   end select
end subroutine mat_files
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_getsym()

! ident_5="@(#)M_matrix::mat_getsym(3fp): get a symbol"

character(len=80) :: message

doubleprecision   :: syv
doubleprecision   :: s

integer      :: SIGN
integer      :: CHCNT
integer      :: SS
integer      :: i
!.......................................................................
   INFINITE : do
      if (G_CHRA .ne. blank) exit INFINITE
      call mat_getch() ! get next character
   enddo INFINITE
!.......................................................................
   G_LINE_POINTER(2) = G_LINE_POINTER(3)
   G_LINE_POINTER(3) = G_LINE_POINTER(4)
   if (G_CHRA .le. 9) then
      call mat_getval(syv)
      if (G_CHRA .ne. dot) goto 60
      call mat_getch() ! get next character
   elseif (G_CHRA.le.z_low .or. (G_CHRA.ge.a_up.and.G_CHRA.le.z_up).or.G_CHRA.eq.score) then     ! alphameric (0-9a-zA-Z_)
      ! name
      G_sym = name
      G_syn(1) = G_CHRA
      chcnt = 1

      do
         call mat_getch() ! get next character
         chcnt = chcnt+1
         if ( .not.(G_CHRA .ge. a_up .and. G_CHRA.le.z_up .or. G_CHRA.eq.score ) ) then ! alternate case letter
            if (G_CHRA .gt. z_low) exit ! a control character not alphanumeric and not special like eol
         endif
         if (chcnt .le. GG_MAX_NAME_LENGTH) G_syn(chcnt) = G_CHRA
      enddo

      if (chcnt .le. GG_MAX_NAME_LENGTH) then
         do i = chcnt, GG_MAX_NAME_LENGTH
            G_syn(i) = blank
         enddo
      endif
      goto 90
   else ! special character
      ss = G_sym
      G_sym = G_CHRA
      call mat_getch() ! get next character
      if (G_sym .ne. dot) goto 90
   !
   !     is dot part of number or operator
      syv = 0.0d0
      if (G_CHRA .gt. 9) then ! a number character
         if (G_CHRA.eq.star.or.G_CHRA.eq.slash.or.G_CHRA.eq.bslash) goto 90
         if (ss.eq.star .or. ss.eq.slash .or. ss.eq.bslash) goto 90
      endif
   endif

   ! number
   chcnt = G_LINE_POINTER(4)
   call mat_getval(s)
   chcnt = G_LINE_POINTER(4) - chcnt
   if (G_CHRA .eq. G_EOL) chcnt = chcnt+1
   syv = syv + s/10.0d0**chcnt

60 continue

   if (.not.(G_CHRA.ne.d_low .and. G_CHRA.ne.e_low .and. G_CHRA.ne.d_up .and. G_CHRA.ne.e_up) )then
      call mat_getch() ! get next character
      sign = G_CHRA
      if (sign.eq.minus .or. sign.eq.plus) call mat_getch() ! get next character
      call mat_getval(s)
      if (sign .ne. minus) syv = syv*10.0d0**s
      if (sign .eq. minus) syv = syv/10.0d0**s
   endif
   G_STACK_IMAGS(G_BIGMEM) = mat_flop(syv)
   G_sym = num

90 continue

   if (G_CHRA .eq. blank) then
      call mat_getch() ! get next character
      goto 90
   endif

   if (G_DEBUG_LEVEL .ne. 1) return

   if (G_sym.gt.name .and. G_sym.lt.G_CHARSET_SIZE) then
      call journal(char(G_CHARSET(G_sym+1)))
   endif

   if (G_sym .ge. G_CHARSET_SIZE) call journal('eol')

   if (G_sym .eq. name) call mat_print_id(G_syn,1)

   if (G_sym .eq. num) then
      write(message,'(1x,g8.2)') syv
      call journal(message)
   endif

end subroutine mat_getsym
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_str2buf(string,buf,lrecl)

! ident_6="@(#)M_matrix::mat_str2buf(3fp): convert string to hollerith"

! g95 compiler does not support Hollerith, this is a KLUDGE to give time to think about it

character(len=*),intent(in) :: string
integer,intent(in)          :: lrecl
integer,intent(out)         :: buf(:)
integer                     :: i

   buf=ichar(' ')+538976304-48
   do i=1,min(lrecl,len_trim(string),size(buf))
      buf(i)=ichar(string(i:i))+538976304-48
   enddo

end subroutine mat_str2buf
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_buf2str(string,buf,lrecl)

! ident_7="@(#)M_matrix::mat_buf2string(3fp): convert hollerith to string"

integer,intent(in)     :: lrecl
integer,intent(in)     :: buf(:)
character(len=*)       :: string
integer                :: i
integer                :: ilen
   string(:)=' '
   ilen=len(string)
   do i=1,min(lrecl,ilen,size(buf))
      string(i:i)=char(buf(i)-538976304+48)
   enddo
end subroutine mat_buf2str
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine ints2str(ints,string,ierr)

! ident_8="@(#)M_matrix::ints2str(3f) convert mat88 integers to a character variable"

! temporary procedure while writing ASCII-based upgrade

integer,intent(in)                       :: ints(:)
character(len=:),allocatable,intent(out) :: string
integer,intent(out)                      :: ierr
integer                                  :: i

   ierr=0
   if(allocated(string))deallocate(string)
   allocate(character(len=size(ints)) :: string)
   string(:)=' '

   do i=1,size(ints)
      if( ints(i).lt.len(G_DEFINE_CHARSET) .and. ints(i).ge.0 )then
         string(i:i)=G_DEFINE_CHARSET(ints(i)+1:ints(i)+1)
      else
         call journal('sc',' function name contains unacceptable characters:',ints(i))
         ierr=ierr+1
      endif
   enddo

end subroutine ints2str
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_matfn6()
!
! ident_9="@(#)M_matrix::mat_matfn6(3f):evaluate utility functions"
!
integer :: i, j, k
integer :: ia
integer :: ib
integer :: ja
integer :: jb
integer :: l
integer :: la
integer :: lb
integer :: ld
integer :: lj
integer :: ll
integer :: ls
integer :: m
integer :: ma
integer :: mn
integer :: n
integer :: na
integer :: nn
integer,parameter :: unifor(GG_MAX_NAME_LENGTH) =[30,23,18,15,24,27,22,GG_PAD(4:)]
integer,parameter :: normal(GG_MAX_NAME_LENGTH) =[23,24,27,22,10,21,36,GG_PAD(4:)]
integer,parameter :: seed(GG_MAX_NAME_LENGTH)   =[28,14,14,13,36,36,36,GG_PAD(4:)]
integer           :: id(GG_MAX_NAME_LENGTH)
doubleprecision   :: eps0,eps,s,sr,si,t
character(len=80) :: message
character(len=GG_LINELEN) :: string_buf
!

   if (G_DEBUG_LEVEL .eq. 1) call journal('sc','*MATFN6* ',G_FIN)


   l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
   m = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
   n = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)

!  functions/G_FIN
!  magi diag sum  prod user eye  rand ones chop size kron  tril triu zeros
!    1    2    3    4    5    6    7    8    9   10  11-13  14   15   16

   FUN6: select case(G_FIN)
!===================================================================================================================================
   case(1) ! COMMAND::MAGIC
      N = MAX(int(G_STACK_REALS(L)),0)
      IF (N .EQ. 2) N = 0
      IF (N .GT. 0) call mat_magic(G_STACK_REALS(L),N,N)
      call mat_rset(N*N,0.0D0,G_STACK_IMAGS(L),1)
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = N
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = N
!===================================================================================================================================
   case(11,12,13) !  COMMAND::KRONECKER PRODUCT
      if (G_RHS .ne. 2) then
         call mat_err(39) ! Incorrect number of arguments
         return
      endif
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE - 1
      L = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
      MA = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
      NA = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
      LA = L + MAX(M*N*MA*NA,M*N+MA*NA)
      LB = LA + MA*NA

      if(too_much_memory(LB + M*N - G_STACK_ID_LOC(G_TOP_OF_SAVED)) )return

!     MOVE A AND B ABOVE RESULT
      call mat_wcopy(MA*NA+M*N,G_STACK_REALS(L),G_STACK_IMAGS(L),1,G_STACK_REALS(LA),G_STACK_IMAGS(LA),1)
      DO JA = 1, NA
        DO J = 1, N
          LJ = LB + (J-1)*M
          DO IA = 1, MA
!           GET J-TH COLUMN OF B
            call mat_wcopy(M,G_STACK_REALS(LJ),G_STACK_IMAGS(LJ),1,G_STACK_REALS(L),G_STACK_IMAGS(L),1)
!           ADDRESS OF A(IA,JA)
            LS = LA + IA-1 + (JA-1)*MA
            DO I = 1, M
!             A(IA,JA) OP B(I,J)
              IF (G_FIN .EQ. 11) &
              call mat_wmul( G_STACK_REALS(LS), G_STACK_IMAGS(LS), &
                             G_STACK_REALS(L),  G_STACK_IMAGS(L),  &
                             G_STACK_REALS(L),  G_STACK_IMAGS(L))
              IF (G_FIN .EQ. 12) &
              call mat_wdiv( G_STACK_REALS(LS), G_STACK_IMAGS(LS), &
                             G_STACK_REALS(L),  G_STACK_IMAGS(L),  &
                             G_STACK_REALS(L),  G_STACK_IMAGS(L))
              IF (G_FIN .EQ. 13)  &
              call mat_wdiv( G_STACK_REALS(L),  G_STACK_IMAGS(L),  &
                             G_STACK_REALS(LS), G_STACK_IMAGS(LS), &
                             G_STACK_REALS(L),  G_STACK_IMAGS(L))
              IF (G_ERR .GT. 0) return
              L = L + 1
            ENDDO
          enddo
        enddo
      enddo
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = M*MA
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = N*NA
!===================================================================================================================================
   case(9) ! COMMAND::CHOP

      eps0 = 1.0d0
      do                                                                  ! recalculate epsilon
         eps0 = eps0/2.0d0
         t = mat_flop(1.0d0 + eps0)
         if (t .le. 1.0d0) exit
      enddo
      eps0 = 2.0d0*eps0

      G_FLOP_COUNTER(2) = int(G_STACK_REALS(L))
      if (G_SYM .ne. SEMI) then
         write(message,'(''CHOP '',I2,'' PLACES.'')') G_FLOP_COUNTER(2)
         call journal(message)
      endif

      eps = 1.0d0
      do                                                                  ! recalculate epsilon
         eps = eps/2.0d0
         t = mat_flop(1.0d0 + eps)
         if (t .le. 1.0d0) exit
      enddo
      eps = 2.0d0*eps

      t = G_STACK_REALS(G_BIGMEM-4)
      if (t.lt.eps .or. t.eq.eps0) G_STACK_REALS(G_BIGMEM-4) = eps
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
!===================================================================================================================================
   case(3) ! COMMAND::SUM
      sr = 0.0d0
      si = 0.0d0
      mn = m*n
      do i = 1, mn
         ls = l+i-1
         sr = mat_flop(SR+G_STACK_REALS(LS))
         si = mat_flop(SI+G_STACK_IMAGS(LS))
      enddo
      G_STACK_REALS(l) = sr
      G_STACK_IMAGS(l) = si
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
!===================================================================================================================================
   case(4) ! COMMAND::PROD
      SR = 1.0D0
      SI = 0.0D0
      MN = M*N
      DO I = 1, MN
         LS = L+I-1
         call mat_wmul(G_STACK_REALS(LS),G_STACK_IMAGS(LS),SR,SI,SR,SI)
      enddo
      G_STACK_REALS(L) = SR
      G_STACK_IMAGS(L) = SI
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
!===================================================================================================================================
   case(5) ! COMMAND::USER
      ! The MAT88 statement "Y = user(X,s,t)" results in a call to the
      ! subroutine with a copy of the matrix X stored in the argument A,
      ! its column and row dimensions in M and N, and the scalar parameters
      ! s and t stored in S and T. If s and t are omitted, they are set
      ! to 0.0. After the return, A is stored in Y. The dimensions M and
      ! N may be reset within the subroutine. The statement Y = "user(K)"
      ! results in a call with M = 1, N = 1 and A(1,1) = "float(K)".

      if(G_RHS.gt.2)then
         t = G_STACK_REALS(L)
      else
         t = 0.0D0
      endif

      if(G_RHS.eq.2)then
         s = G_STACK_REALS(L)
      else
         s = 0.0D0
      endif

      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE-1
      l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
      m = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
      n = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)

      call mat88_user(G_STACK_REALS(L),m,n,s,t)
      call mat_rset(M*N,0.0D0,G_STACK_IMAGS(l),1)
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = m
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
!===================================================================================================================================
   case(10) ! COMMAND::SIZE
      ! store the two output values onto stack
      G_STACK_REALS(L) = M
      G_STACK_IMAGS(L) = 0.0D0
      G_STACK_REALS(L+1) = N
      G_STACK_IMAGS(L+1) = 0.0D0
      if(G_LHS.eq.1)then
         ! output is a 1x2 array so store values indicating the size of the new stack value
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 2
      else
         ! output is two scalars
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1

         G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE + 1
         G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) = L+1
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      endif
!===================================================================================================================================
   case(2,14,15) ! COMMAND::DIAG=2
                 ! COMMAND::TRIL=14
                 ! COMMAND::TRIU=15
      k = 0
      if (G_RHS .eq. 2) then
         k = int(G_STACK_REALS(l))
         G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE-1
         l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
         m = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
         n = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
      endif

      if (G_FIN .ge. 14) then ! COMMAND::TRIL, COMMAND::TRIU
            do j = 1, n
               ld = l + j - k - 1 + (j-1)*m
               select case(G_FIN)
               case(14)
                        ll = j - k - 1
                        ls = ld - ll
               case(15)
                        ll = m - j + k
                        ls = ld + 1
               end select
               if (ll .gt. 0) call mat_wset(ll, 0.0d0, 0.0d0, G_STACK_REALS(ls), G_STACK_IMAGS(ls), 1)
            enddo
      elseif (m .eq. 1 .or. n .eq. 1) then
         n = max(m,n)+iabs(k)

         if(too_much_memory( l+n*n - G_STACK_ID_LOC(G_TOP_OF_SAVED)) )return

         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
         do jb = 1, n
            do ib = 1, n
               j = n+1-jb
               i = n+1-ib
               sr = 0.0d0
               si = 0.0d0
               if (k.ge.0) ls = l+i-1
               if (k.lt.0) ls = l+j-1
               ll = l+i-1+(j-1)*n
               if (j-i .eq. k) sr = G_STACK_REALS(ls)
               if (j-i .eq. k) si = G_STACK_IMAGS(ls)
               G_STACK_REALS(LL) = sr
               G_STACK_IMAGS(LL) = si
            enddo
         enddo
      else
         if (k.ge.0) mn=min(m,n-k)
         if (k.lt.0) mn=min(m+k,n)
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = max(mn,0)
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
         if (mn .le. 0) exit FUN6
         do i = 1, mn
            if (k.ge.0) ls = l+(i-1)+(i+k-1)*m
            if (k.lt.0) ls = l+(i-k-1)+(i-1)*m
            ll = l+i-1
            G_STACK_REALS(ll) = G_STACK_REALS(ls)
            G_STACK_IMAGS(ll) = G_STACK_IMAGS(ls)
         enddo
      endif
      exit FUN6
!-----------------------------------------------------------------------------------------------------------------------------------
   case(6,7,8,16) ! COMMAND::EYE,
                  ! COMMAND::RAND,
                  ! COMMAND::ONES,
                  ! COMMAND::ZEROS
      if (.not.(m.gt.1 .or. G_RHS.eq.0)) then

         if (G_RHS .eq. 2) then
            nn = int(G_STACK_REALS(L))
            G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE-1
            l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
            n = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
         endif

         if (G_FIN.eq.7.and.n.lt.GG_MAX_NAME_LENGTH)then        ! a call to RAND might be RAND('UNIFORM'|'SEED'|'NORMAL')
            id=blank
            do i = 1, min(GG_MAX_NAME_LENGTH,n)  ! in case it is one of these words store it in the ID array to test if it matches
               ls = l+i-1
               id(i) = int(G_STACK_REALS(ls))
            enddo
            if(mat_eqid(id,unifor).or.mat_eqid(id,normal))then ! SWITCH UNIFORM AND NORMAL(if a matrix just happens to match, a bug)
               G_CURRENT_RANDOM_TYPE = id(1) - unifor(1)        ! set random type to generate by seeing if first letter is a "u"
               G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
               exit FUN6
            elseif (mat_eqid(id,seed)) then                     ! if a matrix just happens to match "seed" , a bug)
               if (G_RHS .eq. 2) G_CURRENT_RANDOM_SEED = nn
               G_STACK_REALS(l) = G_CURRENT_RANDOM_SEED
               G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
               if (G_RHS .eq. 2) G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
               G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
               exit FUN6
            endif
         endif

         if (n .le. 1) then
            m = max(int(G_STACK_REALS(l)),0)
            if (G_RHS .eq. 2) n = max(nn,0)
            if (G_RHS .ne. 2) n = m

            if(too_much_memory( l+m*n - G_STACK_ID_LOC(G_TOP_OF_SAVED))) return

            G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = m
            G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
            if (m*n .eq. 0) exit FUN6
         endif

      endif

      do j = 1, n
         do i = 1, m

           ll = l+i-1+(j-1)*m             ! location to place value

           G_STACK_IMAGS(ll) = 0.0d0      ! all of these functions set imaginary values to zero

           select case(G_FIN)
           case( 6 ) !::EYE
              if(i.eq.j)then               ! on the diagonal
                 G_STACK_REALS(ll) = 1.0d0
              else
                 G_STACK_REALS(ll) = 0.0d0
              endif
           case( 7 ) !::RAND
              IF(G_CURRENT_RANDOM_TYPE.EQ.0) then
                 G_STACK_REALS(ll)=mat_flop(mat_urand(G_CURRENT_RANDOM_SEED))
              else
                 do
                    sr = 2.0d0*mat_urand(G_CURRENT_RANDOM_SEED)-1.0d0
                    si = 2.0d0*mat_urand(G_CURRENT_RANDOM_SEED)-1.0d0
                    t = sr*sr + si*si
                    if (t .le. 1.0d0) exit
                 enddo

                 G_STACK_REALS(ll) = mat_flop(sr*dsqrt((-(2.0d0*dlog(t)))/t))
              endif
           case( 8 ) !::ONES
              G_STACK_REALS(ll) = 1.0d0
           case( 16) !::ZEROS
              G_STACK_REALS(ll) = 0.0d0
           case default
              call journal('should not get here: internal error')
           end select
         enddo
      enddo
      exit FUN6
!===================================================================================================================================
   case(17) ! COMMAND::GETENV JSU
      GETENV : block
      character(len=:),allocatable :: answers(:)
      character(len=GG_LINELEN)    :: varname
      character(len=:),allocatable :: env_value
      allocate(character(len=0)    :: answers(0) )
      write(*,*)'GOT HERE A: M=',M,' N=',N,' G_RHS=',G_RHS,' L=',L
      ! sort out what to do with an array of input later, for now concatenating into one string
      if (m.lt.1 .or. G_RHS.eq.0)then
         call journal('sc','<ERROR>GETENV:NEEDS AN ARGUMENT:ROWS=',m,' ARG_COUNT=',G_RHS)
         G_ERR=999
         return
      endif
      if (G_RHS.gt.1)then
         call journal('sc','<ERROR>GETENV:TOO MANY ARGUMENTS:ARG_COUNT=',G_RHS)
         G_ERR=999
         return
      endif

      ll=l
      do j=1,m
         id=blank
         id(1:n)=int(G_STACK_REALS(ll:ll+N-1))
         varname=''
         do i=1,n
            if(id(i).le.0)exit
            if(id(i).lt.len(G_DEFINE_CHARSET))then
               varname(i:i)=G_DEFINE_CHARSET(id(i)+1:id(i)+1)
            else
               call journal('sc',' function name contains unacceptable characters:',id(i))
               return
            endif
         enddo
         ll=ll+n
         env_value=system_getenv(varname)
         answers=[character(len=max(len(answers),len_trim(env_value))) :: answers,env_value]
      enddo
      write(*,*)'GOT HERE C:VARNAME=',trim(varname),' ANSWERS=',answers,len(answers)

      m=size(answers,dim=1)
      n=len(answers)
      if(too_much_memory( l+m*n - G_STACK_ID_LOC(G_TOP_OF_SAVED)) )return
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = m
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
      if (m*n .eq. 0) exit FUN6

      write(*,*)'GOT HERE D:M=',m,' N=',n,'L=',L
      ! so starting at G_STACK_REALS(l) convert the characters to numbers and store the M x N number of characters
      do j = 1, n
         do i = 1, m
           ll = l+i-1+(j-1)*m             ! location to place value
           G_STACK_IMAGS(ll) = 0.0d0      ! all of these functions set imaginary values to zero
           nn=index(G_DEFINE_CHARSET,answers(m)(j:j))
           if(nn.gt.0)then
              G_STACK_REALS(ll) = real(nn-1)
           else
              call journal('sc','bad character')
              G_STACK_REALS(ll) = 0.0d0
           endif
         enddo
      enddo
      endblock GETENV
      exit FUN6
!===================================================================================================================================
   case(18) ! COMMAND::DAT
      DATETIME: block
      integer :: time_values(8)
      ! store the two output values onto stack
      call date_and_time(values=time_values)
      G_STACK_REALS(L:L+8-1) = dble(time_values)
      G_STACK_IMAGS(L:L+8-1) = 0.0D0
      ! output is a 1x8 array so store values indicating the size of the new stack value
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 8
      endblock DATETIME
!===================================================================================================================================
   end select FUN6
end subroutine mat_matfn6
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_funs(id)

! ident_10="@(#)M_matrix::ml_funcs(3fp):scan function list"

integer,intent(in)               :: id(GG_MAX_NAME_LENGTH)
integer                          :: selector
character(len=GG_MAX_NAME_LENGTH) :: name
integer                          :: i

   !  print function names and return
   if (id(1).eq.0) then
      call help_command(G_HELP_TEXT,'SUMMARY',merge(G_LINECOUNT(:2),[0,huge(0)],G_PROMPT))
      return
   endif

   name=''
   do i=1,size(id)
      if(id(i).le.0)exit
      if(id(i).lt.len(G_DEFINE_CHARSET))then
         name(i:i)=G_DEFINE_CHARSET(id(i)+1:id(i)+1)
      else
         call journal('sc',' function name contains unacceptable characters:',name,'...')
         G_fin = 0
         return
      endif
   enddo
   !
   !  find value for given function name to determine what to call for each name.
   !     o first digit indicates which routine to call (SUBROUTINE MAT_MATFN[1-6])
   !     o remaining digits indicate nth number in computed goto in called routine
   select case(name)
   case('eps');             selector=000
   case('flop');            selector=000

   case('inv');             selector=101
   case('det');             selector=102
   case('rcond');           selector=103
   case('lu');              selector=104
   case('invh','inverse_hilbert','invhilb');  selector=105
   case('chol');            selector=106
   case('rref');            selector=107

   case('sin');             selector=201
   case('cos');             selector=202
   case('atan');            selector=203
   case('exp');             selector=204
   case('sqrt');            selector=205
   case('log');             selector=206
   case('eig');             selector=211
   case('schur');           selector=212
   case('hess');            selector=213
   case('poly');            selector=214
   case('roots');           selector=215
   case('abs');             selector=221  !  calling  codes  corresponding  to  the  function  names
   case('round');           selector=222
   case('real');            selector=223
   case('imag');            selector=224
   case('conjg');           selector=225

   case('svd');             selector=301
   case('pinv');            selector=302
   case('cond');            selector=303
   case('norm');            selector=304
   case('rank');            selector=305

   case('qr');              selector=401
   case('orth');            selector=402

   case('exec');            selector=501
   case('save');            selector=502
   case('load');            selector=503
   case('print');           selector=504
   case('diary');           selector=505
   case('disp','display','echo');  selector=506
   case('base');            selector=507
   case('lines');           selector=508
   case('char');            selector=509
   case('plot');            selector=510
   case('rat');             selector=511
   case('debug');           selector=512
   case('doc');             selector=513

   case('magic');           selector=601
   case('diag');            selector=602
   case('sum');             selector=603
   case('prod');            selector=604
   case('user');            selector=605
   case('eye');             selector=606
   case('rand','random');   selector=607
   case('ones');            selector=608
   case('chop');            selector=609
   case('size');            selector=610
   case('kron');            selector=611
   case('tril');            selector=614
   case('triu');            selector=615
   case('zeros');           selector=616
   case('getenv');          selector=617
   case('dat','date_and_time');   selector=618

   case default !  function name was not found
      G_fin = 0
      return
   end select

!  found name so get G_FIN and G_FUN value from corresponding code

   G_fin = mod(selector,100) ! which routine to call (SUBROUTINE MAT_MATFN[1-6])
   G_FUN = selector/100      ! which case to select in called procedure

   if (G_rhs.eq.0 .and. selector.eq.606) G_fin = 0
   if (G_rhs.eq.0 .and. selector.eq.607) G_fin = 0
end subroutine mat_funs
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_putid(x,y)

! ident_11="@(#)M_matrix::mat_putid(3fp): copy a name to allow an easy way to store a name"

integer,intent(out) :: x(GG_MAX_NAME_LENGTH)
integer,intent(in)  :: y(GG_MAX_NAME_LENGTH)
integer             :: i
      do i = 1, GG_MAX_NAME_LENGTH
         x(i) = y(i)
      enddo
end subroutine mat_putid
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_getval(s)

! ident_12="@(#)M_matrix::mat_getval(3fp): form numerical value from string of integer characters"

doubleprecision,intent(out) :: s
      s = 0.0d0
      INFINITE: do
         if (G_CHRA .gt. 9) exit INFINITE
         s = 10.0d0*s + dble(G_CHRA)
         call mat_getch() ! get next character
      enddo INFINITE
end subroutine mat_getval
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_getch()

! ident_13="@(#)M_matrix::mat_getch(3f): get next character from input line into G_CHRA"

   G_CHRA = G_LIN(G_LINE_POINTER(4))
   if (G_CHRA .ne. G_EOL) G_LINE_POINTER(4) = G_LINE_POINTER(4) + 1

end subroutine mat_getch
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wdotur(n,xr,xi,incx,yr,yi,incy)
integer,intent(in) :: n
doubleprecision    :: xr(*)
doubleprecision    :: xi(*)
integer            :: incx
doubleprecision    :: yr(*)
doubleprecision    :: yi(*)
integer            :: incy

doubleprecision    :: s
integer            :: ix
integer            :: iy
integer            :: i

   s = 0.0d0
   if (n .gt. 0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do i = 1, n
         s = mat_flop(s + xr(ix)*yr(iy) - xi(ix)*yi(iy))
         ix = ix + incx
         iy = iy + incy
      enddo
   endif

   mat_wdotur = s

end function mat_wdotur
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_appnum(rval,string,ilen,ierr)

! ident_14="@(#)M_matrix::mat_appnum(3fp): subroutine returns a string given a prefix string and a real value"

!     Input string should have at least 20 blank characters at end
!     03/16/87 J. S. Urban
!

real,intent(in)                :: rval           ! input value to convert to characters and append to STRING
character(len=*)               :: string         ! string to append string value of RVAL to
integer,intent(out)            :: ilen           ! new length of STRING on output
integer,intent(out)            :: ierr           ! flag to indicate if error occurred

intrinsic                      :: len_trim

character(len=20)              :: chars          ! scratch string to store string representation of RVAL in
integer                        :: ilen2          ! length of string created by converting RVAL to a string

   ierr=0                                        ! initialize error flag to indicate no errors
   chars=' '                                     ! initialize scratch string to all blanks
   ilen=len_trim(string(:len(string)))           ! find last non-blank character in initial input string

   call value_to_string(rval,chars,ilen2,ierr)   ! convert RVAL to a string in CHARS
   if(ilen+ilen2.gt.len(string))then
      call journal('sc','*mat_appnum* error: input string variable too short to store output string')
      call journal('sc','*mat_appnum* '//string,rval)
      ierr=-1
   else
      string=string(:ilen)//chars(:ilen2)        ! append CHARS to STRING
      ilen=ilen+ilen2                            ! calculate length of new string
   endif
end subroutine mat_appnum
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wcopy(number_of_values,xr,xi,incx,yr,yi,incy)
integer,intent(in)          :: number_of_values
doubleprecision,intent(in)  :: xr(*)
doubleprecision,intent(in)  :: xi(*)
integer,intent(in)          :: incx
doubleprecision,intent(out) :: yr(*)
doubleprecision,intent(out) :: yi(*)
integer,intent(in)          :: incy

integer                     :: ix
integer                     :: iy
integer                     :: i
   if (number_of_values .gt. 0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-number_of_values+1)*incx + 1
      if (incy.lt.0) iy = (-number_of_values+1)*incy + 1
      do i = 1, number_of_values
         yr(iy) = xr(ix)
         yi(iy) = xi(ix)
         ix = ix + incx
         iy = iy + incy
      enddo
   endif
end subroutine mat_wcopy
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wdiv(ar,ai,br,bi,cr,ci)

! ident_15="@(#)M_matrix::mat_wdiv(3fp): c = a/b"

doubleprecision :: ar
doubleprecision :: ai
doubleprecision :: br
doubleprecision :: bi
doubleprecision :: cr
doubleprecision :: ci

doubleprecision :: s
doubleprecision :: d
doubleprecision :: ars
doubleprecision :: ais
doubleprecision :: brs
doubleprecision :: bis

   s = dabs(br) + dabs(bi)
   if (s .eq. 0.0d0) then
      call mat_err(27)
      return
   endif
   ars = ar/s
   ais = ai/s
   brs = br/s
   bis = bi/s
   d = brs**2 + bis**2
   cr = mat_flop((ars*brs + ais*bis)/d)
   ci = (ais*brs - ars*bis)/d
   if (ci .ne. 0.0d0) ci = mat_flop(ci)
end subroutine mat_wdiv
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wset(n,xr,xi,yr,yi,incy)

! ident_16="@(#)M_matrix::mat_set(3f):"

integer,intent(in)         :: n     ! number of Y values to set
doubleprecision,intent(in) :: xr    ! constant to assign Y real values to
doubleprecision,intent(in) :: xi    ! constant to assing Y imaginary values to
doubleprecision            :: yr(*) ! Y real component to set to XR
doubleprecision            :: yi(*) ! Y imaginary component to set to XI
integer                    :: incy  ! stride to take while setting output values

integer         :: iy
integer         :: i
   iy = 1
   if (n .le. 0 ) return
   do i = 1,n
      yr(iy) = xr
      yi(iy) = xi
      iy = iy + incy
   enddo
end subroutine mat_wset
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_base(x,b,eps,s,n)

! ident_17="@(#)M_matrix::mat_base(3fp): store base b representation of x in s(1:n)"

doubleprecision :: x
doubleprecision :: b
doubleprecision :: eps
doubleprecision :: s(*)
integer         :: n

doubleprecision :: t

integer      :: l
integer      :: j
integer      :: k
integer      :: m

   l = 1
   if (x .ge. 0.0d0)then
      s(l) = plus
   else
      s(l) = minus
   endif
   s(l+1) = zero
   s(l+2) = dot
   x = dabs(x)
   if (x .ne. 0.0d0) then
      k = dlog(x)/dlog(b)
   else
      k = 0
   endif
   if (x .gt. 1.0d0) k = k + 1
   x = x/b**k
   if (b*x .ge. b) k = k + 1
   if (b*x .ge. b) x = x/b
   if (eps .ne. 0.0d0)then
      m = (-1)*dlog(eps)/dlog(b) + 4
   else
      m = 54
   endif
   do l = 4, m
      x = b*x
      j = int(x)
      s(l) = dble(j)
      x = x - s(l)
   enddo
   s(m+1) = comma
   if (k .ge. 0) s(m+2) = plus
   if (k .lt. 0) s(m+2) = minus
   t = dabs(dble(k))
   n = m + 3
   if (t .ge. b) n = n + int(dlog(t)/dlog(b))
   l = n
   INFINITE: do
      j = int(dmod(t,b))
      s(l) = dble(j)
      l = l - 1
      t = t/b
      if (l .lt. m+3) exit
   enddo INFINITE
end subroutine mat_base
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wswap(n,xr,xi,incx,yr,yi,incy)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: yr(*)
doubleprecision :: yi(*)
integer         :: incy

doubleprecision :: t

integer         :: i
integer         :: ix
integer         :: iy

   if (n .le. 0) return
   ix = 1
   iy = 1
   if (incx.lt.0) ix = (-n+1)*incx + 1
   if (incy.lt.0) iy = (-n+1)*incy + 1
   do i = 1, n
      t = xr(ix)
      xr(ix) = yr(iy)
      yr(iy) = t
      t = xi(ix)
      xi(ix) = yi(iy)
      yi(iy) = t
      ix = ix + incx
      iy = iy + incy
   enddo
end subroutine mat_wswap
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_print(ID,K)

! ident_18="@(#)M_matrix::mat_print(3fp): primary output routine"

integer           :: id(GG_MAX_NAME_LENGTH)
integer           :: k

character(len=81) :: message
character(len=80) :: form
character(len=1)  :: ls_char

doubleprecision   :: s
doubleprecision   :: tr
doubleprecision   :: ti
doubleprecision   :: pr(12)
doubleprecision   :: pi(12)
integer           :: sig(12)
integer           :: typ
integer           :: f
integer           :: l,m,n,mn
integer           :: ks
integer           :: i
integer           :: ios
integer           :: istep
integer           :: j
integer           :: j1
integer           :: j2
integer           :: j3
integer           :: jinc
integer           :: jm
integer           :: ls
integer,save      :: fno(11)= [11,12,21,22,23,24,31,32,33,34,-1]
integer,save      :: fnl(11)= [12, 6, 8, 4, 6, 3, 4, 2, 3, 1, 1]
integer           :: itype


! FORMAT NUMBERS AND LENGTHS
! G_FMT   1       2       3       4       5
!       SHORT   LONG   SHORT E  LONG E    Z
! TYP   1       2       3
!    INTEGER  REAL   COMPLEX
!.......................................................................
   if (G_LINECOUNT(1) .lt. 0) goto 99
!.......................................................................
   l = G_STACK_ID_LOC(k)
   m = G_STACK_ROWS(k)
   n = G_STACK_COLS(k)
   mn = m*n
   typ = 1
   s = 0.0d0
   itype=-9999
   do i = 1, mn
      ls = l+i-1
      tr = G_STACK_REALS(ls)
      ti = G_STACK_IMAGS(ls)
      s = dmax1(s,dabs(tr),dabs(ti))
      if (mat_round(tr) .ne. tr) typ = max(2,typ)
      if (ti .ne. 0.0d0) typ = 3
   enddo
   if (s .ne. 0.0d0) s = dlog10(s)
   ks = int(s)
   if (-2 .le. ks .and. ks .le. 1) ks = 0
   if (ks .eq. 2 .and. G_FMT .eq. 1 .and. typ .eq. 2) ks = 0

   f=0                          ! initialize to bad value
   if (typ .eq. 1 )then         ! if output type is integer
      if( ks .le. 2 )then
         f = 1
      else
         f = 2
      endif
   endif
   if (typ .eq. 1 .and. ks .gt. 9) typ = 2  !change type from integer to real

   if (typ .eq. 2) f = G_FMT + 2   ! if type is real
   if (typ .eq. 3) f = G_FMT + 6   ! if type is complex
   if(f.eq.0)then
      call journal('*mat_print* internal error - bad type')
      goto 99
   endif

   if (mn.eq.1 .and. ks.ne.0 .and. G_FMT.lt.3 .and. typ.ne.1) f = f+2

   if (G_FMT .eq. 5) f = 11

   jinc = fnl(f)
   f = fno(f)

   s = 1.0d0
   if (f.eq.21 .or. f.eq.22 .or. f.eq.31 .or. f.eq.32) s = 10.0D0**ks
   ls = ((n-1)/jinc+1)*m + 2
!.......................................................................
   IF (G_LINECOUNT(1) + LS .gt. G_LINECOUNT(2)) then
      G_LINECOUNT(1) = 0

      if(G_PROMPT)then
         WRITE(message, "(' AT LEAST ',I5,' MORE LINES.','  ENTER BLANK LINE TO CONTINUE OUTPUT.')") LS
         call journal(message)

         READ(G_INPUT_LUN,'(a1)',END=19) LS_CHAR  ! read response to pause from standard input
         IF (LS_CHAR .EQ. ' ') goto 20      ! if blank or a return display the values
         G_LINECOUNT(1) = -1
         goto 99
      else
         LS_CHAR = ' '
         goto 20
      endif
   19 continue
      call mat_files(-G_INPUT_LUN,G_BUF)
   endif
   20 continue
!.......................................................................
   call journal(' ')
   call mat_print_id(ID,-1)
   G_LINECOUNT(1) = G_LINECOUNT(1)+2
   if (s .ne. 1.0d0)then
      write(message,'(''  '',1PD9.1," *")') s
      if(G_OUTPUT_LUN.eq.STDOUT)then
         call journal(message)
      else
         write(G_OUTPUT_LUN,'(a)')message(1:80)
      endif
   endif
   do j1 = 1, n, jinc
      j2 = min(n, j1+jinc-1)
      if (n .gt. jinc)then
         write(message,'(''     COLUMNS'',I6,'' THRU'',I6)') j1,j2
         if(G_OUTPUT_LUN.eq.STDOUT)then
            call journal(message)
         else
            write(G_OUTPUT_LUN,'(a)')message(1:80)
         endif
      endif
      do i = 1, m
         jm = j2-j1+1
         do j = 1, jm
            ls = l+i-1+(j+j1-2)*m
            pr(j) = G_STACK_REALS(ls)/s
            pi(j) = dabs(G_STACK_IMAGS(ls)/s)
            sig(j) = G_CHARSET(plus+1)
            if (G_STACK_IMAGS(ls) .lt. 0.0d0) sig(j) = G_CHARSET(minus+1)
         enddo

         select case(F)
         case(11)
            form='(1X,12F6.0)'          ! integer
            istep=12
            itype= 777
         case(12)
            form='(1X,6F12.0)'          ! integer
            istep=6
            itype= 777
         case(21)
            form='(1X,F9.4,7F10.4)'     ! 8 numbers
            istep=8
            itype= 999
         case(22)
            form='(1X,F19.15,3F20.15)'  ! 4 numbers
            istep=4
            itype= 999
         case(23)
            form='(1X,1P6D13.4)'        ! 6 numbers
            istep=6
            itype= 999
         case(24)
            form='(1X,1P3D24.15)'       ! 3 numbers
            istep=3
            itype= 999
         case(31)
            form='(1X,4(F9.4,1X,A1,F7.4,''i''))'                       ! 4x3
            istep=12
            itype= 888
         case(32)
            form='(1X,F19.15,A1,F18.15,''i'',F20.15,A1,F18.15,''i'')'  ! 6
            istep=6
            itype= 888
         case(33)
            form='(1X,3(1PD13.4,1X,A1,1PD10.4,''i''))'                 ! 9
            istep=9
            itype= 888
         case(34)
            form='(1X,1PD24.15,1X,A1,1PD21.15,''i'')'                  ! 3
            istep=3
            itype= 888
         case(-1)
            call mat_formz(G_OUTPUT_LUN,G_STACK_REALS(ls),G_STACK_IMAGS(ls))
            istep=-1
            itype=-1
         case default
            call journal('*internal error*')
            goto 99
         end select

         ! print data based on type
         if(itype.gt.0)then
            do j3=1,jm,istep
               select case(itype)
               case(777); write(message,form)(pr(j),j=j3,min(j3+istep-1,jm))
               case(999); write(message,form)(pr(j),j=j3,min(j3+istep,jm))
               case(888); write(message,form)(pr(j),sig(j),pi(j),j=j3,min(j3+istep-1,jm))
               end select
               if(G_OUTPUT_LUN.eq.STDOUT)then
                  call journal(message)
               else
                  write(G_OUTPUT_LUN,'(a)')message(1:80)
               endif
            enddo
         endif

         G_LINECOUNT(1) = G_LINECOUNT(1)+1
      enddo
   enddo

99 continue
   if(G_OUTPUT_LUN.ne.STDOUT)flush(unit=G_OUTPUT_LUN,iostat=ios)

end subroutine mat_print
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wsqrt(xr,xi,yr,yi)

! ident_19="@(#)M_matrix::mat_wsqrt(3fp): y = sqrt(x) with yr .ge. 0.0 and sign(yi) .eq. sign(xi)"

doubleprecision,intent(in)  :: xr
doubleprecision,intent(in)  :: xi
doubleprecision,intent(out) :: yr
doubleprecision,intent(out) :: yi
doubleprecision             :: s
doubleprecision             :: tr
doubleprecision             :: ti
!
   tr = xr
   ti = xi
   s = dsqrt(0.5d0*(mat_pythag(tr,ti) + dabs(tr)))
   if (tr .ge. 0.0d0) yr = mat_flop(s)
   if (ti .lt. 0.0d0) s = -s
   if (tr .le. 0.0d0) yi = mat_flop(s)
   if (tr .lt. 0.0d0) yr = mat_flop(0.5d0*(ti/yi))
   if (tr .gt. 0.0d0) yi = mat_flop(0.5d0*(ti/yr))
end subroutine mat_wsqrt
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wlog(in_real,in_imag,out_real,out_imag)

! ident_20="@(#)M_matrix::mat_wlog(3fp): y = log(x)"

doubleprecision :: in_real, in_imag
doubleprecision :: out_real, out_imag
doubleprecision :: t
doubleprecision :: r
   r = mat_pythag(in_real,in_imag)

   if (r .eq. 0.0d0) then
      call mat_err(32) !  Singularity of LOG or ATAN
   else
      t = datan2(in_imag,in_real)
      if (in_imag.eq.0.0d0 .and. in_real.lt.0.0d0) t = dabs(t)
      out_real = dlog(r)
      out_imag = t
   endif

end subroutine mat_wlog
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_formz(lunit,x,y)

! ident_21="@(#)M_matrix::mat_formz: system dependent routine to print with z format"

integer                    :: lunit
doubleprecision,intent(in) :: x,y

character(len=36)          :: mline

   if (y .ne. 0.0d0) then
      write(mline,'(2z18)') x,y
   else
      write(mline,'(z18)') x
   endif

   call journal(mline)

end subroutine mat_formz
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_prompt(pause)

! ident_22="@(#)M_matrix::mat_prompt(3f): issue interactive prompt with optional pause"

integer,intent(in) :: pause
character(len=1)   :: dummy

   if(.not.G_PROMPT)return ! in batch mode
   ! paranoid checks
   if(G_OUTPUT_LUN.le.0)then
      call journal('*mat_prompt* internal error: G_OUTPUT_LUN <= 0')
   elseif(G_INPUT_LUN.lt.0)then
      call journal('*mat_prompt* internal error: G_INPUT_LUN <= 0')
   else
      ! write prompt using format that stays on current line
      if(G_OUTPUT_LUN.eq.STDOUT)then
         WRITE(G_OUTPUT_LUN,'(''<>'')',advance='no')   ! write prompt to interactive input
      endif
      if (pause .eq. 1) read(G_INPUT_LUN,'(a1)') dummy
   endif

end subroutine mat_prompt
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rswap(n,x,incx,y,incy)
integer         :: n
doubleprecision :: x(*)
integer         :: incx
doubleprecision :: y(*)
integer         :: incy

doubleprecision :: t
integer         :: ix
integer         :: iy
integer         :: i

   if (n .le. 0) return
   ix = 1
   iy = 1
   if (incx.lt.0) ix = (-n+1)*incx+1
   if (incy.lt.0) iy = (-n+1)*incy+1
   do i = 1, n
      t = x(ix)
      x(ix) = y(iy)
      y(iy) = t
      ix = ix + incx
      iy = iy + incy
   enddo
end subroutine mat_rswap
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wrscal(n,s,xr,xi,incx)
integer         :: n
doubleprecision :: s
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx

integer         :: ix
integer         :: i
   if (n .le. 0) return
   ix = 1
   do i = 1, n
      xr(ix) = mat_flop(s*xr(ix))
      if (xi(ix) .ne. 0.0d0) xi(ix) = mat_flop(s*xi(ix))
      ix = ix + incx
   enddo
end subroutine mat_wrscal
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wscal(n,sr,si,xr,xi,incx)
integer,intent(in)         :: n
doubleprecision,intent(in) :: sr
doubleprecision,intent(in) :: si
doubleprecision            :: xr(*)
doubleprecision            :: xi(*)
integer                    :: incx
integer                    :: ix
integer                    :: i
   if (n .gt. 0) then
      ix = 1
      do i = 1, n
         call mat_wmul(sr,si,xr(ix),xi(ix),xr(ix),xi(ix))
         ix = ix + incx
      enddo
   endif
end subroutine mat_wscal
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wmul(ar,ai,br,bi,cr,ci)

! ident_23="@(#)M_matrix::mat_wmul(3fp) c = a*b"

doubleprecision,intent(in)  :: ar
doubleprecision,intent(in)  :: ai
doubleprecision,intent(in)  :: br
doubleprecision,intent(in)  :: bi
doubleprecision,intent(out) :: cr
doubleprecision,intent(out) :: ci

doubleprecision :: t
   t = ar*bi + ai*br
   if (t .ne. 0.0d0) t = mat_flop(t)
   cr = mat_flop(ar*br - ai*bi)
   ci = t
end subroutine mat_wmul
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_stack1(op)

! ident_24="@(#)M_matrix::mat_stack1(3f): Unary Operations"

integer           :: op
integer,parameter :: quote=49
integer           :: i
integer           :: j
integer           :: l
integer           :: ll
integer           :: ls
integer           :: m
integer           :: mn
integer           :: n

   if (G_DEBUG_LEVEL .eq. 1) call journal('sc','mat_stack1 ',op)
   l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
   m = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
   n = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
   mn = m*n
   if (mn .eq. 0) then
   elseif (op .ne. quote) then                                 ! unary minus
      call mat_wrscal(MN,-1.0D0,G_STACK_REALS(L),G_STACK_IMAGS(L),1)
   else                                                        ! transpose
      ll = l + mn

      if(too_much_memory( ll+mn - G_STACK_ID_LOC(G_TOP_OF_SAVED)) )return

      call mat_wcopy(MN,G_STACK_REALS(L),G_STACK_IMAGS(l),1,G_STACK_REALS(ll),G_STACK_IMAGS(ll),1)
      M = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
      N = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = m
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
      do i = 1, m
         do j = 1, n
            ls = l+mn+(j-1)+(i-1)*n
            ll = l+(i-1)+(j-1)*m
            G_STACK_REALS(ll) = G_STACK_REALS(ls)
            G_STACK_IMAGS(ll) = -G_STACK_IMAGS(ls)
         enddo
      enddo
   endif
end subroutine mat_stack1
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rrot(n,dx,incx,dy,incy,c,s)

! ident_25="@(#)M_matrix::mat_rrot(3f): Applies a plane rotation."

integer         :: n
doubleprecision :: dx(*)
integer         :: incx
doubleprecision :: dy(*)
integer         :: incy
doubleprecision :: c
doubleprecision :: s

doubleprecision :: dtemp
integer         :: i
integer         :: ix
integer         :: iy
!
   if (n.gt.0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do i = 1,n
           dtemp = mat_flop(c*dx(ix) + s*dy(iy))
           dy(iy) = mat_flop(c*dy(iy) - s*dx(ix))
           dx(ix) = dtemp
           ix = ix + incx
           iy = iy + incy
      enddo
   endif
end subroutine mat_rrot
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rset(n,dx,dy,incy)

! ident_26="@(#)M_matrix::mat_rset(3f): copies a scalar, dx, to a vector, dy."

integer         :: n
doubleprecision :: dx,dy(*)
integer         :: incy
integer         :: i
integer         :: iy

   if (n.gt.0) then
      iy = 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do i = 1,n
         dy(iy) = dx
         iy = iy + incy
      enddo
   endif
end subroutine mat_rset
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_print_id(id,argcnt)

! ident_27="@(#)M_matrix::mat_print_id(3fp): print table of variable id names (up to) eight per line"

!     ID     Is array of GG_MAX_NAME_LENGTH character IDs to print
!     ARGCNT is number of IDs to print
!            If = -1, print one ID with an "  =" suffix
!
integer            :: id(GG_MAX_NAME_LENGTH,*)
integer            :: argcnt
integer,parameter  ::  hollerith_blank=ichar(' ')+538976304-48
integer,parameter  ::  hollerith_equal=ichar('=')+538976304-48
integer            :: id_counter                               !
integer            :: i, j, k
integer            :: line_position                            ! pointer into output line being built
integer            :: linebuf(8*GG_MAX_NAME_LENGTH+2*8+1)      ! scratch buffer for building up line
character(len=(8*GG_MAX_NAME_LENGTH+2*8+1)) :: mline           ! scratch space for building line to print

   id_counter = 1                                         ! which ID to start the line with
   INFINITE : do
      linebuf(1)=hollerith_blank                          ! put a space at beginning of line
      line_position = 2
      do j = id_counter,min(id_counter+7,iabs(argcnt))    ! copy up to eight names into buffer
         do i = 1, GG_MAX_NAME_LENGTH                     ! copy one name into buffer
            k = id(i,j)+1                                 ! this is the kth letter of the set
            linebuf(line_position) = G_CHARSET(k)
            if(linebuf(line_position).ne.hollerith_blank)line_position = line_position+1   ! increment pointer into output
         enddo
         linebuf(line_position+0)=hollerith_blank         ! put two spaces between names
         linebuf(line_position+1)=hollerith_blank
         line_position=line_position+2
      enddo
      if (argcnt .eq. -1) then                            ! special flag to print one word and  =
         linebuf(line_position) = hollerith_equal         ! put value for equal sign into buffer
      else
         line_position=line_position-3                    ! was prepared for another ID with two blanks
      endif

      call mat_buf2str(mline,linebuf,line_position)       ! write LINEBUF(1:line_position) line to a character variable
      if(G_OUTPUT_LUN.eq.STDOUT)then
         call journal(mline)                              ! print the line to stdout
      else
         write(G_OUTPUT_LUN,'(a)')trim(mline)             ! print the line to a file
      endif

      id_counter = id_counter+8                           ! prepare to get up to eight more IDs
      if (id_counter .gt. iabs(argcnt)) exit INFINITE     ! if not done do another line
   enddo INFINITE
end subroutine mat_print_id
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine MAT_STACK_PUT(id)

! ident_28="@(#)M_matrix::mat_stack_put(3fp): put variables into storage"

integer                    :: id(GG_MAX_NAME_LENGTH)
character(len=GG_LINELEN)  :: mline
integer             :: i
integer             :: ib
integer             :: j
integer             :: k
integer             :: km1
integer             :: l
integer             :: l1
integer             :: l2
integer             :: li
integer             :: lj
integer             :: lk
integer             :: ll
integer             :: ls
integer             :: lt
integer             :: m
integer             :: m1
integer             :: m2
integer             :: mk
integer             :: mn
integer             :: mn1
integer             :: mn2
integer             :: mnk
integer             :: mt
integer             :: n
integer             :: nk
integer             :: nt

   if (G_DEBUG_LEVEL .eq. 1) then
      write(mline,'('' STACKP '',*(i4))') id
      call journal(mline)
   endif

   if (G_BOTTOM_OF_SCRATCH_IN_USE .le. 0) then
      call mat_err(1)
      return
   endif

   call mat_funs(id)
   if (G_fin .ne. 0) then
      call mat_err(25)
      return
   endif

   m = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
   n = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
   if (m .gt. 0) l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)

   if (m .lt. 0) then
      call mat_err(14)
      return
   endif

   if (m .eq. 0 .and. n .ne. 0) goto 99
   mn = m*n
   lk = 0
   mk = 1
   nk = 0
   lt = 0
   mt = 0
   nt = 0
   !
   ! DOES VARIABLE ALREADY EXIST
   call mat_putid(G_STACK_IDS(1,G_TOP_OF_SAVED-1),id)
   k = GG_MAX_NUMBER_OF_NAMES+1
05 continue
   k = k-1
   if (.not.mat_eqid(G_STACK_IDS(1,k),id)) goto 05
   if (k .eq. G_TOP_OF_SAVED-1) goto 30
   lk = G_STACK_ID_LOC(k)
   mk = G_STACK_ROWS(k)
   nk = G_STACK_COLS(k)
   mnk = mk*nk
   if (G_rhs .eq. 0) goto 20
   if (G_rhs .gt. 2) then
      call mat_err(15)
      return
   endif
   mt = mk
   nt = nk
   lt = l + mn

   if(too_much_memory( lt + mnk - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

   call mat_wcopy(mnk,G_STACK_REALS(lk),G_STACK_IMAGS(lk),1,G_STACK_REALS(lt),G_STACK_IMAGS(lt),1)
!
!     DOES IT FIT
20 continue
   if (G_rhs.eq.0 .and. mn.eq.mnk) goto 40
   if (k .ge. GG_MAX_NUMBER_OF_NAMES-3) then
      call mat_err(13)
      return
   endif
!
!     SHIFT STORAGE
   if (k .eq. G_TOP_OF_SAVED) goto 25
   ls = G_STACK_ID_LOC(G_TOP_OF_SAVED)
   ll = ls + mnk
   call mat_wcopy(lk-ls,G_STACK_REALS(ls),G_STACK_IMAGS(ls),-1,G_STACK_REALS(ll),G_STACK_IMAGS(ll),-1)
   km1 = k-1
   do ib = G_TOP_OF_SAVED, km1
      i = G_TOP_OF_SAVED+km1-ib
      call mat_putid(G_STACK_IDS(1,i+1),G_STACK_IDS(1,i))
      G_STACK_ROWS(i+1) = G_STACK_ROWS(i)
      G_STACK_COLS(i+1) = G_STACK_COLS(i)
      G_STACK_ID_LOC(i+1) = G_STACK_ID_LOC(i)+mnk
   enddo
!
!  DESTROY OLD VARIABLE
25 continue
   G_TOP_OF_SAVED = G_TOP_OF_SAVED+1
!
!  CREATE NEW VARIABLE
30 continue
   if (mn .eq. 0) goto 99
   if (G_TOP_OF_SAVED-2 .le. G_BOTTOM_OF_SCRATCH_IN_USE) then
      call mat_err(18)
      return
   endif
   k = G_TOP_OF_SAVED-1
   call mat_putid(G_STACK_IDS(1,k), id)
   if (G_rhs .eq. 1) goto 50
   if (G_rhs .eq. 2) goto 55
!
!  STORE
40 continue
   if (k .lt. GG_MAX_NUMBER_OF_NAMES) G_STACK_ID_LOC(k) = G_STACK_ID_LOC(k+1) - mn
   G_STACK_ROWS(k) = m
   G_STACK_COLS(k) = n
   lk = G_STACK_ID_LOC(k)
   call mat_wcopy(mn,G_STACK_REALS(l),G_STACK_IMAGS(l),-1,G_STACK_REALS(lk),G_STACK_IMAGS(lk),-1)
   goto 90
!===================================================================================================================================
!
!  VECT(ARG)
50 continue
   if (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE-1) .lt. 0) goto 59
   mn1 = 1
   mn2 = 1
   l1 = 0
   l2 = 0
   if (n.ne.1 .or. nk.ne.1) goto 52
   l1 = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE-1)
   m1 = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE-1)
   mn1 = m1*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE-1)
   m2 = -1
   goto 60
!===================================================================================================================================
52 continue
   if (m.ne.1 .or. mk.ne.1) then
      call mat_err(15)
      return
   endif
   l2 = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE-1)
   m2 = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE-1)
   mn2 = m2*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE-1)
   m1 = -1
   goto 60
!===================================================================================================================================
!     matrix(arg,arg)
55 continue
   if (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE-1).lt.0 .and. G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE-2).lt.0) goto 59
   l2 = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE-1)
   m2 = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE-1)
   mn2 = m2*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE-1)
   if (m2 .lt. 0) mn2 = n
   l1 = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE-2)
   m1 = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE-2)
   mn1 = m1*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE-2)
   if (m1 .lt. 0) mn1 = m
   goto 60
!===================================================================================================================================
59 continue
   if (mn .ne. mnk) then
      call mat_err(15)
      return
   endif
   lk = G_STACK_ID_LOC(k)
   call mat_wcopy(mn,G_STACK_REALS(l),G_STACK_IMAGS(l),-1,G_STACK_REALS(lk),G_STACK_IMAGS(lk),-1)
   goto 90
!===================================================================================================================================
60 continue
   if (mn1.ne.m .or. mn2.ne.n) then
      call mat_err(15)
      return
   endif
   ll = 1
   if (m1 .lt. 0) goto 62
   do i = 1, mn1
      ls = l1+i-1
      mk = max(mk,int(G_STACK_REALS(ls)))
      ll = min(ll,int(G_STACK_REALS(ls)))
   enddo
62 continue
   mk = max(mk,m)
   if (m2 .lt. 0) goto 64
   do i = 1, mn2
      ls = l2+i-1
      nk = max(nk,int(G_STACK_REALS(ls)))
      ll = min(ll,int(G_STACK_REALS(ls)))
   enddo
64 continue
   nk = max(nk,n)
   if (ll .lt. 1) then
      call mat_err(21)
      return
   endif
   mnk = mk*nk
   lk = G_STACK_ID_LOC(k+1) - mnk

   if(too_much_memory( lt + mt*nt - lk) )return

   G_STACK_ID_LOC(k) = lk
   G_STACK_ROWS(k) = mk
   G_STACK_COLS(k) = nk
   call mat_wset(mnk,0.0d0,0.0d0,G_STACK_REALS(lk),G_STACK_IMAGS(lk),1)
   if (nt .lt. 1) goto 67
   do j = 1, nt
      ls = lt+(j-1)*mt
      ll = lk+(j-1)*mk
      call mat_wcopy(mt,G_STACK_REALS(ls),G_STACK_IMAGS(ls),-1,G_STACK_REALS(ll),G_STACK_IMAGS(ll),-1)
   enddo
67 continue
   do j = 1, n
      do i = 1, m
         li = l1+i-1
         if (m1 .gt. 0) li = l1 + int(G_STACK_REALS(li)) - 1
         lj = l2+j-1
         if (m2 .gt. 0) lj = l2 + int(G_STACK_REALS(lj)) - 1
         ll = lk+li-l1+(lj-l2)*mk
         ls = l+i-1+(j-1)*m
         G_STACK_REALS(ll) = G_STACK_REALS(ls)
         G_STACK_IMAGS(ll) = G_STACK_IMAGS(ls)
      enddo
   enddo
   goto 90
!===================================================================================================================================
! PRINT IF DESIRED AND POP STACK
90 continue
   if (G_sym.ne.semi .and. G_LINECOUNT(3).eq.0) call mat_print(id,k) ! if not a semi-colon and "semi" mode print
   if (G_sym.eq.semi .and. G_LINECOUNT(3).eq.1) call mat_print(id,k) ! if a semi-colon and "semi" mode off print
   if (k .eq. G_TOP_OF_SAVED-1) G_TOP_OF_SAVED = G_TOP_OF_SAVED-1
99 continue
   if (m .ne. 0) G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE - 1 - G_rhs
   if (m .eq. 0) G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE - 1
end subroutine MAT_STACK_PUT
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##THE PARSER-INTERPRETER (10)
!!
!!    The structure of the parser-interpreter is similar to that of Wirth's
!!    compiler [6] for his simple language, PL/0 , except that MAT88 is
!!    programmed in Fortran, which does not have explicit recursion. The
!!    interrelation of the primary subroutines is shown in the following
!!    diagram.
!!
!!          MAIN
!!            |
!!          MAT88     |--CLAUSE
!!            |       |    |
!!          PARSE-----|--EXPR----TERM----FACTOR
!!                    |    |       |       |
!!                    |    |-------|-------|
!!                    |    |       |       |
!!                    |  STACK1  STACK2  STACKG
!!                    |
!!                    |--STACKP--PRINT
!!                    |
!!                    |--COMAND
!!                    |
!!                    |
!!                    |          |--CGECO
!!                    |          |
!!                    |          |--CGEFA
!!                    |          |
!!                    |--MATFN1--|--CGESL
!!                    |          |
!!                    |          |--CGEDI
!!                    |          |
!!                    |          |--CPOFA
!!                    |
!!                    |
!!                    |          |--IMTQL2
!!                    |          |
!!                    |          |--HTRIDI
!!                    |          |
!!                    |--MATFN2--|--HTRIBK
!!                    |          |
!!                    |          |--CORTH
!!                    |          |
!!                    |          |--COMQR3
!!                    |
!!                    |
!!                    |--MATFN3-----CSVDC
!!                    |
!!                    |
!!                    |          |--CQRDC
!!                    |--MATFN4--|
!!                    |          |--CQRSL
!!                    |
!!                    |
!!                    |          |--FILES
!!                    |--MATFN5--|
!!                               |--SAVLOD
!!
!!    Subroutine PARSE controls the interpretation of each statement. It
!!    calls subroutines that process the various syntactic quantities such
!!    as command, expression, term and factor. A fairly simple program
!!    stack mechanism allows these subroutines to recursively "call"
!!    each other along the lines allowed by the syntax diagrams. The four
!!    STACK subroutines manage the variable memory and perform elementary
!!    operations, such as matrix addition and transposition.
!!
!!    The four subroutines MATFN1 though MATFN4 are called whenever "serious"
!!    matrix computations are required. They are interface routines which
!!    call the various LINPACK and EISPACK subroutines. MATFN5 primarily
!!    handles the file access tasks.
SUBROUTINE mat_parse()
integer            :: id(GG_MAX_NAME_LENGTH)
integer            :: excnt
integer            :: pts
integer,parameter  :: ans(GG_MAX_NAME_LENGTH) =[10,23,28,36,GG_PAD(:)]
integer,parameter  :: ennd(GG_MAX_NAME_LENGTH)=[14,23,13,36,GG_PAD(:)]
integer,parameter  :: else(GG_MAX_NAME_LENGTH)=[14,21,28,14,GG_PAD(:)]
integer            :: p
integer            :: r
character(len=80)  :: mline
integer            :: i5
integer            :: ierr
integer            :: ilen
integer            :: j
integer            :: k
integer            :: l
integer            :: ls
integer            :: n
character(len=:),allocatable :: symbol
!
   01 continue
      r = 0
      if (G_ERR .gt. 0) G_PTZ = 0
      if (G_ERR.le.0 .and. G_PT.gt.G_PTZ) r = G_RSTK(G_PT)

      if (G_DEBUG_LEVEL .eq. 1) then
         write(mline,'('' PARSE'',*(I4))') G_PT,r,G_PTZ,G_ERR
         call journal(mline)
      endif

      if (r.eq.15) goto 93
      if (r.eq.16 .or. r.eq.17) goto 94
      G_SYM = G_EOL
      G_BOTTOM_OF_SCRATCH_IN_USE = 0
      if (G_RIO .ne. G_INPUT_LUN) call mat_files(-G_RIO,G_BUF)
      G_RIO = G_INPUT_LUN
      G_LINECOUNT(3) = 0
      G_LINECOUNT(4) = 2
      G_LINE_POINTER(1) = 1
   10 continue
      if (G_SYM.eq.G_EOL.and.mod(G_LINECOUNT(4)/2,2).eq.1) call mat_prompt(G_LINECOUNT(4)/4)
      if (G_SYM .eq. G_EOL) call mat_getlin()
      G_ERR = 0
      G_PT = G_PTZ

   15 continue
      excnt = 0
      if (G_DEBUG_LEVEL .eq. 1) then
         mline='STATE'
         call mat_appnum(real(G_PT),mline,ilen,ierr)
         call mat_appnum(real(G_BOTTOM_OF_SCRATCH_IN_USE),mline,ilen,ierr)
         call journal(mline)
      endif
      G_LHS = 1
      call mat_putid(id,ans)
      call mat_getsym()
      if (G_SYM.eq.colon .and. G_CHRA.eq.G_EOL) G_DEBUG_LEVEL = 1-G_DEBUG_LEVEL
      if (G_SYM .eq. colon) call mat_getsym()
      if (G_SYM.eq.SEMI .or. G_SYM.eq.COMMA .or. G_SYM.eq.G_EOL) goto 80
      if (G_SYM .eq. name) then
         ! lhs begins with name
         call ints2str(G_SYN,symbol,ierr)              ! convert ID to a character variable
         call mat_comand(symbol)
         IF (G_ERR .GT. 0) goto 01
         IF (G_FUN .EQ. 99) goto 95
         IF (G_FIN .EQ. -15) goto 80
         IF (G_FIN .LT. 0) goto 91
         IF (G_FIN .GT. 0) goto 70
         ! if name is a function, must be rhs
         G_RHS = 0
         call mat_funs(G_SYN)
         IF (G_FIN .NE. 0) goto 50
         ! peek one character ahead
         IF (G_CHRA.EQ.SEMI .OR. G_CHRA.EQ.COMMA .OR. G_CHRA.EQ.G_EOL) call mat_putid(ID,G_SYN)
         IF (G_CHRA .EQ. EQUAL) then
            ! lhs is simple variable
            call mat_putid(ID,G_SYN)
            call mat_getsym()
            call mat_getsym()
            goto 50
         endif
         IF (G_CHRA .EQ. LPAREN) then
            ! lhs is name(...)
            G_LINE_POINTER(5) = G_LINE_POINTER(4)
            call mat_putid(ID,G_SYN)
            call mat_getsym()
            goto 32
         endif
         goto 50
      endif
      if (G_SYM .eq. less) goto 40
      if (G_SYM .eq. great) goto 45
      goto 50
!.......................................................................
!     lhs is name(...)
   32 continue
      call mat_getsym()
      excnt = excnt+1
      G_PT = G_PT+1
      call mat_putid(G_IDS(1,G_PT), id)
      G_PSTK(G_PT) = excnt
      G_RSTK(G_PT) = 1
!     *call* expr
      goto 92
!.......................................................................
   35 continue
      call mat_putid(id,G_IDS(1,G_PT))
      excnt = G_PSTK(G_PT)
      G_PT = G_PT-1
      if (G_SYM .eq. comma) goto 32
      if (G_SYM .ne. rparen) then
         call mat_err(3)
         goto 01
         return  ! ???? cannot unconditionally goto and return
      endif
      if (G_SYM .eq. rparen) call mat_getsym()
      if (G_SYM .eq. equal) goto 50
!     lhs is really rhs, forget scan just done
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE - excnt
      G_LINE_POINTER(4) = G_LINE_POINTER(5)
      G_CHRA = lparen
      G_SYM = name
      call mat_putid(G_SYN,id)
      call mat_putid(id,ans)
      excnt = 0
      goto 50
!.......................................................................
!     multiple lhs
   40 continue
      G_LINE_POINTER(5) = G_LINE_POINTER(4)
      pts = G_PT
      call mat_getsym()
   41 continue
      if (G_SYM .ne. name)then
         goto 43
      endif
      call mat_putid(id,G_SYN)
      call mat_getsym()
      if (G_SYM .eq. great)then
         call mat_getsym()
         if (G_SYM .eq. equal) goto 50
         goto 43
      endif
      if (G_SYM .eq. comma) call mat_getsym()
      G_PT = G_PT+1
      G_LHS = G_LHS+1
      G_PSTK(G_PT) = 0
      call mat_putid(G_IDS(1,G_PT),id)
      goto 41
!.......................................................................
   43 continue
      G_LINE_POINTER(4) = G_LINE_POINTER(5)
      G_PT = pts
      G_LHS = 1
      G_SYM = less
      G_CHRA = G_LIN(G_LINE_POINTER(4)-1)
      call mat_putid(id,ans)
      goto 50
!.......................................................................
!     macros string
   45 continue
      call mat_getsym()
      if (G_DEBUG_LEVEL .eq. 1) then
         mline='MACROS'
         call mat_appnum(real(G_PT),mline,ilen,ierr)
         call mat_appnum(real(G_BOTTOM_OF_SCRATCH_IN_USE),mline,ilen,ierr)
      endif
      if (G_SYM.eq.less .and. G_CHRA.eq.G_EOL) call mat_err(28)
      if (G_ERR .gt. 0) goto 01
      G_PT = G_PT+1
      G_RSTK(G_PT) = 20
!     *call* expr
      goto 92
!.......................................................................
   46 continue
      G_PT = G_PT-1
      if (G_SYM.ne.less .and. G_SYM.ne.G_EOL) call mat_err(37) ! Improper MACROS
      if (G_ERR .gt. 0) goto 01
      if (G_SYM .eq. less) call mat_getsym()
      k = G_LINE_POINTER(6)
      G_LIN(k+1) = G_LINE_POINTER(1)
      G_LIN(k+2) = G_LINE_POINTER(2)
      G_LIN(k+3) = G_LINE_POINTER(6)
      G_LINE_POINTER(1) = k + 4
!     transfer stack to input line
      k = G_LINE_POINTER(1)
      l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
      n = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
      do j = 1, n
         ls = l + j-1
         G_LIN(k) = int(G_STACK_REALS(ls))
         if (G_LIN(k).lt.0 .or. G_LIN(k).ge.G_CHARSET_SIZE) call mat_err(37) ! improper MACROS
         if (G_ERR .gt. 0) return
         if (k.lt.1024) k = k+1
         if (k.eq.1024) then
            call journal('sc',' input buffer limit is',k,'characters')
          endif
      enddo
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE-1
      G_LIN(K) = G_EOL
      G_LINE_POINTER(6) = k
      G_LINE_POINTER(4) = G_LINE_POINTER(1)
      G_LINE_POINTER(3) = 0
      G_LINE_POINTER(2) = 0
      G_LINECOUNT(1) = 0
      G_CHRA = blank
      G_PT = G_PT+1
      G_PSTK(G_PT) = G_LINE_POINTER(1)
      G_RSTK(G_PT) = 21
!     *call* parse
      goto 15
!.......................................................................
   49 continue
      G_PT = G_PT-1
      if (G_DEBUG_LEVEL .eq. 1)then
         call journal('sc','MACEND',G_PT,G_BOTTOM_OF_SCRATCH_IN_USE)
      endif
      k = G_LINE_POINTER(1) - 4
      G_LINE_POINTER(1) = G_LIN(K+1)
      G_LINE_POINTER(4) = G_LIN(K+2)
      G_LINE_POINTER(6) = G_LIN(K+3)
      G_CHRA = BLANK
      call mat_getsym()
      goto 80
!.......................................................................
!     lhs finished, start rhs
   50 continue
      if (G_SYM .eq. equal) call mat_getsym()
      G_PT = G_PT+1
      call mat_putid(G_IDS(1,G_PT),id)
      G_PSTK(G_PT) = excnt
      G_RSTK(G_PT) = 2
!     *call* expr
      goto 92
!.......................................................................
!     store results
   60 continue
      G_RHS = G_PSTK(G_PT)
      call MAT_STACK_PUT(G_IDS(1,G_PT))
      if (G_ERR .gt. 0) goto 01
      G_PT = G_PT-1
      G_LHS = G_LHS-1
      if (G_LHS .gt. 0) goto 60
      goto 70
!.......................................................................
!     update and possibly print operation counts
   70 continue
      k = G_FLOP_COUNTER(1)
      if (K .ne. 0) G_STACK_REALS(G_BIGMEM-3) = dble(k)
      G_STACK_REALS(G_BIGMEM-2) = G_STACK_REALS(G_BIGMEM-2) + dble(K)
      G_FLOP_COUNTER(1) = 0
      if (.not.(G_CHRA.eq.comma .or. (G_SYM.eq.comma .and. G_CHRA.eq.G_EOL)))goto 80
      call mat_getsym()
      i5 = 10**5

      if (k .eq. 0) then
         call journal('   no flops')
      elseif (k .EQ. 1) then
         call journal('    1 flop')
      else
         call journal('sc','',k,' flops')
      endif
      goto 80
!.......................................................................
!     finish statement
   80 continue
      G_FIN = 0
      p = 0
      r = 0
      if (G_PT .gt. 0) p = G_PSTK(G_PT)
      if (G_PT .gt. 0) r = G_RSTK(G_PT)
      if (G_DEBUG_LEVEL .eq. 1)then
         call journal('sc','FINISH',G_PT,G_PTZ,p,r,G_LINE_POINTER(1))
      endif
      if (G_SYM.eq.comma .or. G_SYM.eq.semi) goto 15
      if (r.eq.21 .and. p.eq.G_LINE_POINTER(1)) goto 49
      if (G_PT .gt. G_PTZ) goto 91
      goto 10
!.......................................................................
!     simulate recursion
!.......................................................................
   91 continue
      call mat_clause()
      if (G_ERR .gt. 0) goto 01
      if (G_PT .le. G_PTZ) goto 15
      r = G_RSTK(G_PT)
      select case(R)
      case(3:5);   goto 92
      case(13:14); goto 15
      case(21);    goto 49
      case default
         write(*,*)'INTERNAL ERROR 91'
         call mat_err(22) ! recursion difficulties
         goto 01
      end select
!.......................................................................
   92 CONTINUE
      call mat_expr()
      if (G_ERR .gt. 0) goto 01
      r = G_RSTK(G_PT)
      select case(r)
      case(1);     goto 35
      case(2)
         if (G_SYM.eq.semi .or. G_SYM.eq.comma .or. G_SYM.eq.G_EOL) goto 60
         if (G_SYM.eq.name .and. mat_eqid(G_SYN,else)) goto 60
         if (G_SYM.eq.name .and. mat_eqid(G_SYN,ennd)) goto 60
         call mat_err(40)
         if (G_ERR .gt. 0) goto 01
         goto 60
      case(3:5);   goto 91
      case(6:7);   goto 93
      case(10:11); goto 94
      case(18:19); goto 94
      case(20);    goto 46
      case default
         write(*,*)'Internal error 92'
         call mat_err(22) ! recursion difficulties
         goto 01
      end select
!.......................................................................
   93 continue
      call mat_term()
      if (G_ERR .gt. 0) goto 01
      r = G_RSTK(G_PT)
      select case(R)
      case(6:7);   goto 92
      case(8:9);   goto 94
      case(15);    goto 95
      case default
         write(*,*)'INTERNAL ERROR 93'
         call mat_err(22) ! recursion difficulties
         goto 01
      end select
!.......................................................................
   94 continue
      call mat_factor()
      if (G_ERR .gt. 0) goto 01
      r = G_RSTK(G_PT)
      select case(R)
      case(8:9);   goto 93
      case(10:11); goto 92
      case(12);    goto 94
      case(16:17); goto 95
      case(18:19); goto 92
      case default
         write(*,*)'INTERNAL ERROR 94'
         call mat_err(22) ! recursion difficulties
         goto 01
      end select
!.......................................................................
!     call mat_matfns by returning to MAT88
   95 continue
      if(G_BOTTOM_OF_SCRATCH_IN_USE.lt.1)then
         !call journal('sc','*mat_parse* stack emptied',G_BOTTOM_OF_SCRATCH_IN_USE)
      else
         if (G_FIN.gt.0 .and. G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE).lt.0) call mat_err(14)
      endif
      if (G_ERR .gt. 0) goto 01
      return
!.......................................................................
end subroutine mat_parse
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_comand(id)

character(len=*),intent(in)  :: id
integer                      :: chr
integer                      :: i, j, k, ii, jj
integer                      :: l
integer                      :: ierr

! a list of names this procedure matches to use for some preliminary tests
character(len=10),parameter :: cmd(*)=[ character(len=10) :: &
 & 'clear', 'else', 'end  ', 'exit', 'for', 'help', 'if   ', 'long    ', 'quit', 'semi', &
 & 'short', 'what', 'while', 'who ', 'sh',  'lala', 'shell', 'continue' &
 & ]

FINISHED: block
!
   if (G_DEBUG_LEVEL .eq. 1)call journal('MAT_COMAND')

   G_FUN = 0

   do k = size(cmd),0,-1
     if(k.eq.0)then                          ! did not match anything
        G_FIN = 0                            ! did not match anything
        return
     elseif (id.eq.cmd(k))then               ! found match to command

        select case(G_CHRA)                  ! check next character
        case(comma,semi,G_EOL)               ! next character is end of a command so good to go
           exit
        case(:z_low,a_up:z_up,score)         ! if alphanumeric or a HELP command so good to go
           exit
        end select

        if (id.eq.'help')then                ! special case where anything after the help could be a topic
           exit
        else
           call mat_err(16)                  ! improper command
           return
        endif

     endif
   enddo

   G_FIN = 1                                 ! found a match and next character passed tests
!===================================================================================================================================
   COMAND : select case(id)
!===================================================================================================================================
   case('clear')
   ! alphameric character
   if ( (G_CHRA.ge.a_low.and.G_CHRA.le.Z_low) .or. (G_CHRA.GE.a_up.and.G_CHRA.LE.z_up) .or. G_CHRA.eq.score) then
      call mat_getsym()
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE+1
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
      G_RHS = 0
      call mat_stack_put(G_SYN)
      if (G_ERR .gt. 0) return
      G_FIN = 1
   else
      G_TOP_OF_SAVED = GG_MAX_NUMBER_OF_NAMES-3
   endif
!===================================================================================================================================
   case('for')
      G_FIN = -11
      exit FINISHED
   case('while')
      G_FIN = -12
      exit FINISHED
   case('if')
      G_FIN = -13
      exit FINISHED
   case('else')
      G_FIN = -14
      exit FINISHED
   case('end')
      G_FIN = -15
      exit FINISHED
!===================================================================================================================================
   case('exit')
      IF (G_PT .GT. G_PTZ)then
         G_FIN = -16
         exit COMAND
      endif
      K = int(G_STACK_REALS(G_BIGMEM-2))
      call journal('sc',' total flops ',k)

      select case( int(mat_urand(G_CURRENT_RANDOM_SEED)*9) )    ! for serendipity's sake randomly pick a sign-off
      case(1); call journal(' adios')
      case(2); call journal(' adieu')
      case(3); call journal(' arrivederci')
      case(4); call journal(' au revior')
      case(5); call journal(' so long')
      case(6); call journal(' sayonara')
      case(7); call journal(' auf wiedersehen')
      case default
         call journal(' cheerio')
      end select

      G_FUN = 99
!===================================================================================================================================
   case('quit')
      K = G_LINE_POINTER(1) - 7
      IF (K .LE. 0)then
         G_FUN = 99
         exit COMAND
      endif
      call mat_files(-G_RIO,G_BUF)
      G_LINE_POINTER(1) = G_LIN(K+1)
      G_LINE_POINTER(4) = G_LIN(K+2)
      G_LINE_POINTER(6) = G_LIN(K+3)
      G_PTZ = G_LIN(K+4)
      G_RIO = G_LIN(K+5)
      G_LINECOUNT(4) = G_LIN(K+6)
      G_CHRA = BLANK
      G_SYM = COMMA
      exit FINISHED
!===================================================================================================================================
   case('continue')
      write(*,*)'GOT HERE A:CONTINUE'
      G_FUN = 99
      exit FINISHED
!===================================================================================================================================
   case('lala')
      call journal('QUIT SINGING AND GET BACK TO WORK.')
!===================================================================================================================================
   case('shell')
      call journal(' Your place or mine?')
!===================================================================================================================================
   case('short','long')
      if(k.eq.11)then
         G_FMT = 1
      else
         G_FMT = 2
      endif
      if (G_CHRA.eq.e_low .or. G_CHRA.eq.d_low .or. G_CHRA.eq.e_up .or. chr.eq.d_up ) G_FMT = G_FMT+2
      if (G_CHRA .eq. z_low) G_FMT = 5
      if (G_CHRA.eq.e_low .or. G_CHRA.eq.d_low .or. G_CHRA.eq.z_low) call mat_getsym()
      if (G_CHRA.eq.e_UP .or. G_CHRA.eq.d_up .or. G_CHRA.eq.z_up ) call mat_getsym()
!===================================================================================================================================
   case('semi')
      G_LINECOUNT(3) = 1 - G_LINECOUNT(3)  ! toggle "semi" mode
!===================================================================================================================================
   case('who')
      call journal(' Your current variables are...')
      call mat_print_id(G_STACK_IDS(1,G_TOP_OF_SAVED),GG_MAX_NUMBER_OF_NAMES-G_TOP_OF_SAVED+1)
      l = G_BIGMEM-G_STACK_ID_LOC(G_TOP_OF_SAVED)+1
      call journal('sc','using',l,'out of',G_BIGMEM,'elements')
!===================================================================================================================================
   case('what')
!===================================================================================================================================
   case('sh')
      call sh_command()
!===================================================================================================================================
   case('help')
      HELP_ : block
      character(len=GG_LINELEN) :: topic_name
         G_BUF=G_CHARSET(blank+1)
         if (G_CHRA .eq. G_EOL) then                                ! if no topic
            topic_name= ' '
         else
            call mat_getsym()
            if (G_SYM .ne. NAME) then
               if (G_SYM .eq. 0) G_SYM = dot
               G_BUF(1)  = G_CHARSET(G_SYM+1)
               G_BUF(2:) = G_CHARSET(blank+1)
            else
               do i = 1, GG_MAX_NAME_LENGTH                         ! assuming G_BUF can hold at least GG_MAX_NAME_LENGTH characters
                 G_BUF(i) = G_CHARSET(G_SYN(i)+1)
               enddo
            endif
            call mat_buf2str(topic_name,G_BUF,len(topic_name))      ! convert ADE array to string
         endif
         call help_command(G_HELP_TEXT,topic_name,merge(G_LINECOUNT(:2),[0,huge(0)],G_PROMPT))
      endblock HELP_
!===================================================================================================================================
   case default ! did not find a match
      G_FIN = 0
      return
!===================================================================================================================================
   end select COMAND
!===================================================================================================================================
   call mat_getsym()
endblock FINISHED
end subroutine mat_comand
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine sh_command()

! ident_29="@(#)M_matrix::sh_command(3f): start system shell interactively"

character(len=GG_LINELEN) :: line
integer                   :: istat

   call get_environment_variable('SHELL',line)               ! get command to execute
   IF (G_CHRA .eq. G_EOL )then                               ! if next character on stack is end-of-line call interactive shell
      call execute_command_line(line,cmdstat=istat)          ! call shell interactively
   else                                                      ! there were characters after SH on the line
      call execute_command_line(line,cmdstat=istat)          ! call shell interactively
   endif

end subroutine sh_command
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_plot(lplot,x,y,n,p,k)

! ident_30="@(#)M_matrix::mat_plot(3fp): Plot X vs. Y on LPLOT.  If K is nonzero, then P(1),...,P(K) are extra parameters"

integer           :: lplot
integer           :: n
doubleprecision   :: x(n)
doubleprecision   :: y(n)
doubleprecision   :: p(*)
integer           :: k

integer           :: lets(k)
character(len=k)  :: string
doubleprecision   :: xmin,ymin,xmax,ymax,dy,dx,y1,y0
character(len=79) :: pbuf                             ! work space for ascii plot
integer,parameter :: h=20,w=79                        ! h = height, w = width
integer           :: tlun
integer           :: ios
integer           :: ch
integer           :: i
integer           :: j
integer           :: jmax
integer           :: l

!!      if (k .gt. 0) write(lplot,01) (p(i), i=1,k)
!!   01 FORMAT('Extra parameters',1000(f5.1,/))

   xmin = x(1)
   xmax = x(1)
   ymin = y(1)
   ymax = y(1)

   do i = 1, n
      xmin = dmin1(xmin,x(i))
      xmax = dmax1(xmax,x(i))
      ymin = dmin1(ymin,y(i))
      ymax = dmax1(ymax,y(i))
   enddo

   dx = xmax - xmin
   if (dx .eq. 0.0d0) dx = 1.0d0
   dy = ymax - ymin
   write(lplot,'(80x)')
   do l = 1, h
      pbuf(:)=' '  ! blank out the line
      y1 = ymin + (h-l+1)*dy/h
      y0 = ymin + (h-l)*dy/h
      jmax = 1
      do i = 1, n
         if (y(i) .gt. y1) cycle
         if (l.ne.h .and. y(i).le.y0) cycle
         j = 1 + (w-1)*(x(i) - xmin)/dx
         pbuf(j:j) = '*'
         jmax = max(jmax,j)
      enddo
      write(lplot,'(1x,a)') pbuf(1:jmax)
   enddo

   ! set up the data file
   open(newunit=tlun,file='xy.dat')
   do i=1,n
      write(tlun,*)x(i),y(i)
   enddo
   flush(tlun)

   string=' '
   lets=0
   do i=1,k
      ch=p(i)
      if ((ch.ge.0) .and. (ch.lt.G_CHARSET_SIZE)) then
         lets(i) = G_CHARSET(ch+1)
      endif
   enddo
   call mat_buf2str(string,lets,k)

   ! call the external program xy(1) converting the parameters to a string of options
   call journal('sc','xy xy.dat ',trim(string))
   call execute_command_line('xy xy.dat '//trim(string))
   close(unit=tlun,status='delete',iostat=ios)

end subroutine mat_plot
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_matfn1()

! ident_31="@(#)M_matrix::mat_matfn1(3fp): evaluate functions involving gaussian elimination"

doubleprecision   :: dtr(2)
doubleprecision   :: dti(2)
doubleprecision   :: sr(1)
doubleprecision   :: si(1)
doubleprecision   :: rcond
doubleprecision   :: t
doubleprecision   :: t0
doubleprecision   :: t1
doubleprecision   :: eps
character(len=80) ::  mline
integer           :: i
integer           :: info
integer           :: j
integer           :: k
integer           :: ka
integer           :: kb
integer           :: l
integer           :: l2
integer           :: l3
integer           :: li
integer           :: lj
integer           :: lk
integer           :: ll
integer           :: ls
integer           :: lu
integer           :: m
integer           :: m2
integer           :: n
integer           :: n2
integer           :: nn
!
   IF (G_DEBUG_LEVEL .EQ. 1) call journal('sc','*MATFN1* ', G_FIN)
!
   L = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
   M = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
   N = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
!===================================================================================================================================
   select case(G_FIN)
!===================================================================================================================================
    case(-1) ! MATRIX RIGHT DIVISION, A/A2
      l2 = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      m2 = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      n2 = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      if (m2 .ne. n2) call mat_err(20)
      if (G_err .gt. 0) return
      if (m*n .ne. 1) then
         if (n .ne. n2) then
            call mat_err(11)
            return
         endif
         l3 = l2 + m2*n2

         if(too_much_memory( l3+n2 - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

         call ml_wgeco(G_STACK_REALS(l2),G_STACK_IMAGS(l2),m2,n2,G_BUF,rcond,G_STACK_REALS(l3),G_STACK_IMAGS(l3))
         if (rcond .eq. 0.0d0) call mat_err(19)
         if (G_err .gt. 0) return
         t = mat_flop(1.0d0 + rcond)
         if (t.eq.1.0d0 .and. G_FUN.ne.21)then
            call journal('WARNING:')
            call journal('MATRIX IS CLOSE TO SINGULAR OR BADLY SCALED.')
            WRITE(mline,'(''RESULTS MAY BE INACCURATE. RCOND='',1PD13.4)') RCOND
            call journal(mline)
         endif
         if (t.eq.1.0d0 .and. G_FUN.eq.21)then
            call journal('WARNING')
            call journal('EIGENVECTORS ARE BADLY CONDITIONED.')
            WRITE(mline,'(''RESULTS MAY BE INACCURATE. RCOND='',1PD13.4)') RCOND
            call journal(mline)
         endif
         do i = 1, m
            do j = 1, n
               ls = l+i-1+(j-1)*m
               ll = l3+j-1
               G_STACK_REALS(ll) = G_STACK_REALS(ls)
               G_STACK_IMAGS(ll) = -G_STACK_IMAGS(ls)
            enddo
            call ml_wgesl(G_STACK_REALS(l2),G_STACK_IMAGS(l2),m2,n2,G_BUF,G_STACK_REALS(l3),G_STACK_IMAGS(l3),1)
            do j = 1, n
               ll = l+i-1+(j-1)*m
               ls = l3+j-1
               G_STACK_REALS(ll) = G_STACK_REALS(ls)
               G_STACK_IMAGS(ll) = -G_STACK_IMAGS(ls)
            enddo
         enddo
         if (G_FUN .ne. 21) goto 99
   !
   !     CHECK FOR IMAGINARY ROUNDOFF IN MATRIX FUNCTIONS
         sr(1) = mat_wasum(n*n,G_STACK_REALS(l),G_STACK_REALS(l),1)
         si(1) = mat_wasum(n*n,G_STACK_IMAGS(l),G_STACK_IMAGS(l),1)
         eps = G_STACK_REALS(G_BIGMEM-4)
         t = eps*sr(1)
         if (G_DEBUG_LEVEL .eq. 18)then
            WRITE(G_OUTPUT_LUN,'('' SR,SI,EPS,T'',1P4D13.4)') SR(1),SI(1),EPS,T ! debug 18
         endif
         if (si(1) .le. eps*sr(1)) call mat_rset(n*n,0.0d0,G_STACK_IMAGS(l),1)
         goto 99
   !
      endif

      sr(1) = G_STACK_REALS(l)
      si(1) = G_STACK_IMAGS(l)
      n = n2
      m = n
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
      call mat_wcopy(n*n,G_STACK_REALS(l2),G_STACK_IMAGS(l2),1,G_STACK_REALS(l),G_STACK_IMAGS(l),1)
!===================================================================================================================================
    case(-2) ! MATRIX LEFT DIVISION A BACKSLASH A2
      l2 = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      m2 = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      n2 = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      if (m .ne. n) call mat_err(20)
      if (G_err .gt. 0) return
      if (m2*n2 .ne. 1) then
         l3 = l2 + m2*n2

         if(too_much_memory( l3+n - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

         call ml_wgeco(G_STACK_REALS(l),G_STACK_IMAGS(l),m,n,G_BUF,rcond,G_STACK_REALS(l3),G_STACK_IMAGS(l3))
         if (rcond .eq. 0.0d0) call mat_err(19)
         if (G_err .gt. 0) return
         t = mat_flop(1.0d0 + rcond)
         if (t .eq. 1.0d0) then
            call journal('WARNING:')
            call journal('MATRIX IS CLOSE TO SINGULAR OR BADLY SCALED.')
            WRITE(mline,'(''RESULTS MAY BE INACCURATE. RCOND='',1PD13.4)') RCOND
            call journal(mline)
         endif
         if (m2 .ne. n) call mat_err(12)
         if (G_err .gt. 0) return
         do j = 1, n2
            lj = l2+(j-1)*m2
            call ml_wgesl(G_STACK_REALS(l),G_STACK_IMAGS(l),m,n,G_BUF,G_STACK_REALS(lj),G_STACK_IMAGS(lj),0)
         enddo
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = n2
         call mat_wcopy(m2*n2,G_STACK_REALS(l2),G_STACK_IMAGS(l2),1,G_STACK_REALS(l),G_STACK_IMAGS(l),1)
         goto 99
      endif
      sr(1) = G_STACK_REALS(l2)
      si(1) = G_STACK_IMAGS(l2)
!===================================================================================================================================
   end select
!===================================================================================================================================
   select case(G_FIN)
!===================================================================================================================================
    case(1) ! COMMAND::INV
      if (m .ne. n) call mat_err(20)
      if (G_err .gt. 0) return
      if (G_DEBUG_LEVEL .ne. 17) then
         do j = 1, n
            do i = 1, n
               ls = l+i-1+(j-1)*n
               t0 = G_STACK_REALS(ls)
               t1 = mat_flop(1.0d0/(dble(i+j-1)))
               if (t0 .ne. t1) goto 32
            enddo
         enddo
         call mat_inverse_hilbert(G_STACK_REALS(l),n,n)
         call mat_rset(n*n,0.0d0,G_STACK_IMAGS(l),1)
         if (G_fin .lt. 0) call mat_wscal(n*n,sr(1),si(1),G_STACK_REALS(l),G_STACK_IMAGS(l),1)
         goto 99
      endif
32    continue
      l3 = l + n*n

      if(too_much_memory( l3+n - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

      call ml_wgeco(G_STACK_REALS(l),G_STACK_IMAGS(l),m,n,G_BUF,rcond,G_STACK_REALS(l3),G_STACK_IMAGS(l3))
      if (rcond .eq. 0.0d0) call mat_err(19)
      if (G_err .gt. 0) return
      t = mat_flop(1.0d0 + rcond)
      if (t .eq. 1.0d0) then
         call journal('warning:')
         call journal('matrix is close to singular or badly scaled.')
         write(mline,'(''results may be inaccurate. rcond='',1pd13.4)') rcond
         call journal(mline)
      endif
      call ml_wgedi(G_STACK_REALS(l),G_STACK_IMAGS(l),m,n,G_BUF,dtr,dti,G_STACK_REALS(l3),G_STACK_IMAGS(l3),1)
      if (G_fin .lt. 0) call mat_wscal(n*n,sr(1),si(1),G_STACK_REALS(l),G_STACK_IMAGS(l),1)
!===================================================================================================================================
    case (2) ! COMMAND::DET
      if (m .ne. n) call mat_err(20)
      if (G_err .gt. 0) return
      call ml_wgefa(G_STACK_REALS(l),G_STACK_IMAGS(l),m,n,G_BUF,info)
      !SUBROUTINE ML_WGEDI(ar,ai,LDA,N,ipvt,detr,deti,workr,worki,JOB)
      call ml_wgedi(G_STACK_REALS(l),G_STACK_IMAGS(l),m,n,G_BUF,dtr,dti,sr(1),si(1),10)
      k = int(dtr(2))
      ka = iabs(k)+2
      t = 1.0d0
      do i = 1, ka
         t = t/10.0d0
         if (t .ne. 0.0d0) goto 42
      enddo
      G_STACK_REALS(l) = dtr(1)*10.d0**k
      G_STACK_IMAGS(l) = dti(1)*10.d0**k
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      goto 99
42    continue
      if (dti(1) .eq. 0.0d0)then
         write(mline,43) dtr(1),k
         call journal(mline)
      else
         write(mline,44) dtr(1),dti(1),k
         call journal(mline)
      endif
      G_STACK_REALS(l) = dtr(1)
      G_STACK_IMAGS(l) = dti(1)
      G_STACK_REALS(l+1) = dtr(2)
      G_STACK_IMAGS(l+1) = 0.0d0
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 2
43    format(' det =  ',f7.4,7h * 10**,i4)
44    format(' det =  ',f7.4,' + ',f7.4,' i ',7h * 10**,i4)
!===================================================================================================================================
    case(3) ! COMMAND::RCOND
      if (m .ne. n) call mat_err(20)
      if (G_err .gt. 0) return
      l3 = l + n*n

      if(too_much_memory( l3+n - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

      call ml_wgeco(G_STACK_REALS(l),G_STACK_IMAGS(l),m,n,G_BUF,rcond,G_STACK_REALS(l3),G_STACK_IMAGS(l3))
      G_STACK_REALS(l) = rcond
      G_STACK_IMAGS(l) = 0.0d0
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      if (G_lhs .ne. 1)then
         l = l + 1
         call mat_wcopy(n,G_STACK_REALS(l3),G_STACK_IMAGS(l3),1,G_STACK_REALS(l),G_STACK_IMAGS(l),1)
         G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE + 1
         G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) = l
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      endif
!===================================================================================================================================
    case(4) ! COMMAND::LU
      if (m .ne. n) call mat_err(20)
      if (G_err .gt. 0) return
      call ml_wgefa(G_STACK_REALS(l),G_STACK_IMAGS(l),m,n,G_BUF,info)
      if (G_lhs .ne. 2) goto 99
      nn = n*n
      if (G_BOTTOM_OF_SCRATCH_IN_USE+1 .ge. G_TOP_OF_SAVED) call mat_err(18)
      if (G_err .gt. 0) return
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE+1
      G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) = l + nn
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = n

      if(too_much_memory( l+nn+nn - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

      do kb = 1, n
         k = n+1-kb
         do i = 1, n
            ll = l+i-1+(k-1)*n
            lu = ll + nn
            if (i .le. k) G_STACK_REALS(lu) = G_STACK_REALS(ll)
            if (i .le. k) G_STACK_IMAGS(lu) = G_STACK_IMAGS(ll)
            if (i .gt. k) G_STACK_REALS(lu) = 0.0d0
            if (i .gt. k) G_STACK_IMAGS(lu) = 0.0d0
            if (i .lt. k) G_STACK_REALS(ll) = 0.0d0
            if (i .lt. k) G_STACK_IMAGS(ll) = 0.0d0
            if (i .eq. k) G_STACK_REALS(ll) = 1.0d0
            if (i .eq. k) G_STACK_IMAGS(ll) = 0.0d0
            if (i .gt. k) G_STACK_REALS(ll) = -G_STACK_REALS(ll)
            if (i .gt. k) G_STACK_IMAGS(ll) = -G_STACK_IMAGS(ll)
         enddo
         i = G_BUF(k)
         if (i .eq. k) cycle
         li = l+i-1+(k-1)*n
         lk = l+k-1+(k-1)*n
         call mat_wswap(n-k+1,G_STACK_REALS(li),G_STACK_IMAGS(li),n,G_STACK_REALS(lk),G_STACK_IMAGS(lk),n)
      enddo
!===================================================================================================================================
    case(5) ! COMMAND::inverse_hilbert
      n = int(G_STACK_REALS(l))
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
      call mat_inverse_hilbert(G_STACK_REALS(l),n,n)
      call mat_rset(n*n,0.0d0,G_STACK_IMAGS(l),1)
      if (G_fin .lt. 0) call mat_wscal(n*n,sr(1),si(1),G_STACK_REALS(l),G_STACK_IMAGS(l),1)
!===================================================================================================================================
    case(6) ! COMMAND::CHOLESKY
      if (m .ne. n) call mat_err(20)
      if (G_err .gt. 0) return
      call mat_wpofa(G_STACK_REALS(l),G_STACK_IMAGS(l),m,n,G_err)
      if (G_err .ne. 0) call mat_err(29)
      if (G_err .gt. 0) return
      do j = 1, n
         ll = l+j+(j-1)*m
         call mat_wset(m-j,0.0d0,0.0d0,G_STACK_REALS(ll),G_STACK_IMAGS(ll),1)
      enddo
!===================================================================================================================================
    case(7) ! COMMAND::RREF
      if (G_rhs .ge. 2)then
         G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE-1
         l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
         if (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) .ne. m) call mat_err(5)
         if (G_err .gt. 0) return
         n = n + G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
      endif
      call mat_rref(G_STACK_REALS(l),G_STACK_IMAGS(l),m,m,n,G_STACK_REALS(G_BIGMEM-4))
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
!===================================================================================================================================
   end select
!
99 continue
end subroutine mat_matfn1
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_matfn2()
integer          :: i
integer          :: inc
integer          :: j
integer          :: job
integer          :: k
integer          :: l
integer          :: l1
integer          :: l2
integer          :: ld
integer          :: le
integer          :: lj
integer          :: ll
integer          :: ls
integer          :: lw
integer          :: m
integer          :: n
integer          :: nn
!
!     evaluate elementary functions and functions involving eigenvalues and eigenvectors
!
      doubleprecision tr(1),ti(1),sr,si,powr,powi
      logical herm,schur,vect,hess
!
      if (G_DEBUG_LEVEL .EQ. 1) call journal('sc','*MATFN2* ', G_FIN)
!
!     functions/G_FIN
!     **   SIN  COS ATAN  EXP  SQRT LOG
!      0    1    2    3    4    5    6
!    EIG  SCHU HESS POLY ROOT
!     11   12   13   14   15
!    ABS  ROUN REAL IMAG CONJ
!     21   22   23   24   25
      if (G_FIN .ne. 0) goto 05
         l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE+1)
         powr = G_STACK_REALS(l)
         powi = G_STACK_IMAGS(l)
   05 continue
      l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
      m = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
      n = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
      if (G_FIN .ge. 11 .and. G_FIN .le. 13) goto 10
      if (G_FIN .eq. 14 .and. (m.eq.1 .or. n.eq.1))then
         goto 50
      endif
      if (G_FIN .eq. 14) goto 10
      if (G_FIN .eq. 15) goto 60

      if (G_FIN .gt. 20) goto 40
      if (m .eq. 1 .or. n .eq. 1) goto 40
      ! what about fall-though?
!===================================================================================================================================
!     EIGENVALUES AND VECTORS
   10 continue
      IF (M .NE. N) call mat_err(20)
      IF (G_ERR .GT. 0) return
      SCHUR = G_FIN .EQ. 12
      HESS = G_FIN .EQ. 13
      VECT = G_LHS.EQ.2 .OR. G_FIN.LT.10
      NN = N*N
      L2 = L + NN
      LD = L2 + NN
      LE = LD + N
      LW = LE + N

      if(too_much_memory( LW+N - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

      call mat_wcopy(NN,G_STACK_REALS(L),G_STACK_IMAGS(L),1,G_STACK_REALS(L2),G_STACK_IMAGS(L2),1)
!
!     CHECK IF HERMITIAN
      HERM=.FALSE.
      DO J = 1, N
         DO I = 1, J
            LS = L+I-1+(J-1)*N
            LL = L+(I-1)*N+J-1
            HERM = G_STACK_REALS(LL).EQ.G_STACK_REALS(LS) .AND. G_STACK_IMAGS(LL).EQ.-G_STACK_IMAGS(LS)
            IF (.NOT. HERM) goto 30
         enddo
      enddo
!
!     HERMITIAN EIGENVALUE PROBLEM
      call mat_wset(NN,0.0D0,0.0D0,G_STACK_REALS(L),G_STACK_IMAGS(L),1)
      call mat_wset(N,1.0D0,0.0D0,G_STACK_REALS(L),G_STACK_IMAGS(L),N+1)
      call mat_wset(N,0.0D0,0.0D0,G_STACK_IMAGS(LD),G_STACK_IMAGS(LE),1)
      job = 0
      IF (VECT) JOB = 1
      call ML_HTRIDI(N,N, &
      G_STACK_REALS(L2),G_STACK_IMAGS(L2), &
      G_STACK_REALS(LD),G_STACK_REALS(LE), &
      G_STACK_REALS(LE),G_STACK_REALS(LW))
      IF(.NOT.HESS)call ML_IMTQL2(N,N,G_STACK_REALS(LD),G_STACK_REALS(LE),G_STACK_REALS(L),G_ERR,JOB)
      IF (G_ERR .GT. 0) call mat_err(24)
      IF (G_ERR .GT. 0) return
      IF (JOB .NE. 0) call ML_HTRIBK(N,N,G_STACK_REALS(L2),G_STACK_IMAGS(L2), &
                                         G_STACK_REALS(LW),N,G_STACK_REALS(L), &
                                         G_STACK_IMAGS(L))
      goto 31
!
!     NON-HERMITIAN EIGENVALUE PROBLEM
   30 continue
      call ML_CORTH(N,N,1,N,G_STACK_REALS(L2),G_STACK_IMAGS(L2), &
                            G_STACK_REALS(LW),G_STACK_IMAGS(LW))
      IF (.NOT.VECT .AND. HESS) goto 31
      JOB = 0
      IF (VECT) JOB = 2
      IF (VECT .AND. SCHUR) JOB = 1
      IF (HESS) JOB = 3
      call ML_COMQR3(N,N,1,N,G_STACK_REALS(LW),G_STACK_IMAGS(LW), &
                             G_STACK_REALS(L2),G_STACK_IMAGS(L2),  &
                             G_STACK_REALS(LD),G_STACK_IMAGS(LD), &
                             G_STACK_REALS(L),G_STACK_IMAGS(L), &
                             G_ERR,JOB)
      IF (G_ERR .GT. 0) call mat_err(24)
      IF (G_ERR .GT. 0) return
!
!     VECTORS
   31 continue
      IF (.NOT.VECT) goto 34
      IF (G_BOTTOM_OF_SCRATCH_IN_USE+1 .GE. G_TOP_OF_SAVED) call mat_err(18)
      IF (G_ERR .GT. 0) return
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE+1
      G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) = L2
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = N
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = N
!
!     DIAGONAL OF VALUES OR CANONICAL FORMS
   34 continue
      IF (.NOT.VECT .AND. .NOT.SCHUR .AND. .NOT.HESS) goto 37
      DO J = 1, N
         LJ = L2+(J-1)*N
         IF (SCHUR .AND. (.NOT.HERM)) LJ = LJ+J
         IF (HESS .AND. (.NOT.HERM)) LJ = LJ+J+1
         LL = L2+J*N-LJ
         call mat_wset(LL,0.0D0,0.0D0,G_STACK_REALS(LJ),G_STACK_IMAGS(LJ),1)
      enddo
      IF (.NOT.HESS .OR. HERM) call mat_wcopy(N,G_STACK_REALS(LD),G_STACK_IMAGS(LD),1,G_STACK_REALS(L2),G_STACK_IMAGS(L2),N+1)
      LL = L2+1
      IF (HESS .AND. HERM)call mat_wcopy(N-1,G_STACK_REALS(LE+1),G_STACK_IMAGS(LE+1),1,G_STACK_REALS(LL),G_STACK_IMAGS(LL),N+1)
      LL = L2+N
      IF (HESS .AND. HERM)call mat_wcopy(N-1,G_STACK_REALS(LE+1),G_STACK_IMAGS(LE+1),1,G_STACK_REALS(LL),G_STACK_IMAGS(LL),N+1)
      IF (G_FIN .LT. 10) goto 42
      IF (VECT .OR. .NOT.(SCHUR.OR.HESS)) goto 99
      call mat_wcopy(NN,G_STACK_REALS(L2),G_STACK_IMAGS(L2),1,G_STACK_REALS(L),G_STACK_IMAGS(L),1)
      goto 99
!
!     VECTOR OF EIGENVALUES
   37 continue
      IF (G_FIN .EQ. 14) goto 52
      call mat_wcopy(N,G_STACK_REALS(LD),G_STACK_IMAGS(LD),1,G_STACK_REALS(L),G_STACK_IMAGS(L),1)
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      goto 99
!===================================================================================================================================
!     ELEMENTARY FUNCTIONS
!     FOR MATRICES.. X,D = EIG(A), FUN(A) = X*FUN(D)/X
   40 continue
      INC = 1
      N = M*N
      L2 = L
      goto 44
   42 continue
      INC = N+1
   44 continue
      DO J = 1, N
        LS = L2+(J-1)*INC
        SR = G_STACK_REALS(LS)
        SI = G_STACK_IMAGS(LS)
        TI = 0.0D0
        IF (G_FIN .eq. 0) then
          call mat_wlog(SR,SI,SR,SI)
          call mat_wmul(SR,SI,POWR,POWI,SR,SI)
          TR(1) = DEXP(SR)*DCOS(SI)
          TI(1) = DEXP(SR)*DSIN(SI)
        endif

        select case(G_FIN)
        CASE( 1)
                 TR(1) = DSIN(SR)*DCOSH(SI)
                 TI(1) = DCOS(SR)*DSINH(SI)
        CASE( 2)
                 TR(1) = DCOS(SR)*DCOSH(SI)
                 TI(1) = (-DSIN(SR))*DSINH(SI)
        CASE( 3)
                 call mat_watan(SR,SI,TR(1),TI(1))
        CASE( 4)
                 TR(1) = DEXP(SR)*DCOS(SI)
                 TI(1) = DEXP(SR)*DSIN(SI)
        CASE( 5)
                 call mat_wsqrt(SR,SI,TR(1),TI(1))
        CASE( 6)
                 call mat_wlog(SR,SI,TR(1),TI(1))
        CASE( 21)
                 TR(1) = mat_pythag(SR,SI)
        CASE( 22)
                 TR(1) = mat_round(SR)
        CASE( 23)
                 TR(1) = SR
        CASE( 24)
                 TR(1) = SI
        CASE( 25)
                 TR(1) = SR
                 TI(1) = -SI
        end select

        IF (G_ERR .GT. 0) return
        G_STACK_REALS(LS) = mat_flop(TR(1))
        G_STACK_IMAGS(LS) = 0.0D0
        IF (TI(1) .NE. 0.0D0) G_STACK_IMAGS(LS) = mat_flop(TI(1))
      enddo
      IF (INC .EQ. 1) goto 99
      DO J = 1, N
        LS = L2+(J-1)*INC
        SR = G_STACK_REALS(LS)
        SI = G_STACK_IMAGS(LS)
        LS = L+(J-1)*N
        LL = L2+(J-1)*N
        call mat_wcopy(N,G_STACK_REALS(LS),G_STACK_IMAGS(LS),1,G_STACK_REALS(LL),G_STACK_IMAGS(LL),1)
        call mat_wscal(N,SR,SI,G_STACK_REALS(LS),G_STACK_IMAGS(LS),1)
      enddo
!     SIGNAL MATFN1 TO DIVIDE BY EIGENVECTORS
      G_FUN = 21
      G_FIN = -1
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE-1
      goto 99
!===================================================================================================================================
!     POLY
      ! form polynomial with given vector as roots
   50 continue
      N = MAX(M,N)
      LD = L+N+1
      call mat_wcopy(N,G_STACK_REALS(L),G_STACK_IMAGS(L),1,G_STACK_REALS(LD),G_STACK_IMAGS(LD),1)
      goto 52
!===================================================================================================================================
!     FORM CHARACTERISTIC POLYNOMIAL
   52 continue
      call mat_wset(N+1,0.0D0,0.0D0,G_STACK_REALS(L),G_STACK_IMAGS(L),1)
      G_STACK_REALS(L) = 1.0D0
      DO J = 1, N
         call matX_waxpy(J,-G_STACK_REALS(LD),-G_STACK_IMAGS(LD), &
                            G_STACK_REALS(L),G_STACK_IMAGS(L), &
                            -1,  &
                            G_STACK_REALS(L+1),G_STACK_IMAGS(L+1), &
                            -1)
         LD = LD+1
      enddo
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = N+1
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      goto 99
!===================================================================================================================================
!     ROOTS
   60 continue
      LL = L+M*N
      G_STACK_REALS(LL) = -1.0D0
      G_STACK_IMAGS(LL) = 0.0D0
      K = -1
   61 continue
      K = K+1
      L1 = L+K
      IF (DABS(G_STACK_REALS(L1))+DABS(G_STACK_IMAGS(L1)) .EQ. 0.0D0) goto 61
      N = MAX(M*N - K-1, 0)
      IF (N .LE. 0) goto 65
      L2 = L1+N+1
      LW = L2+N*N

      if(too_much_memory( LW+N - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

      call mat_wset(N*N+N,0.0D0,0.0D0,G_STACK_REALS(L2),G_STACK_IMAGS(L2),1)
      DO J = 1, N
         LL = L2+J+(J-1)*N
         G_STACK_REALS(LL) = 1.0D0
         LS = L1+J
         LL = L2+(J-1)*N
         call mat_wdiv(-G_STACK_REALS(LS),-G_STACK_IMAGS(LS), &
                        G_STACK_REALS(L1),G_STACK_IMAGS(L1),  &
                        G_STACK_REALS(LL),G_STACK_IMAGS(LL))
         IF (G_ERR .GT. 0) return
      enddo
      call ML_COMQR3(N,N,1,N,G_STACK_REALS(LW),G_STACK_IMAGS(LW), &
                             G_STACK_REALS(L2),G_STACK_IMAGS(L2), &
                             G_STACK_REALS(L),G_STACK_IMAGS(L), &
                             TR,TI,G_ERR,0)
      IF (G_ERR .GT. 0) call mat_err(24)
      IF (G_ERR .GT. 0) return
   65 continue
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = N
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      goto 99
!===================================================================================================================================
   99 continue
end subroutine mat_matfn2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_matfn3()

! ident_32="@(#)M_matrix::mat_matfn3(3fp): evaluate functions involving singular value decomposition"

integer         :: i
integer         :: j
integer         :: jb
integer         :: job
integer         :: k
integer         :: l
integer         :: l1
integer         :: l2
integer         :: ld
integer         :: li
integer         :: lj
integer         :: ll
integer         :: ls
integer         :: lu
integer         :: lv
integer         :: m
integer         :: mn
integer         :: n
logical         :: fro,inf
doubleprecision :: p,s,t(1,1),tol,eps
!
   if (G_DEBUG_LEVEL .eq. 1) call journal('sc','*MATFN3* ', G_FIN)
!
   if (G_fin.eq.1 .and. G_rhs.eq.2) G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE-1
   l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
   m = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
   n = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
   mn = m*n
   !      SVD PINV COND NORM RANK
   !        1    2    3    4    5
   FUN3: select case(G_fin)
!===================================================================================================================================
    case(3) ! COMMAND::COND
      ld = l + m*n
      l1 = ld + min(m+1,n)
      l2 = l1 + n

      if(too_much_memory( l2+min(m,n) - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

      call ml_wsvdc(G_STACK_REALS(l),G_STACK_IMAGS(l),   &
                  & m,m,n,                               &
                  & G_STACK_REALS(ld),G_STACK_IMAGS(ld), &
                  & G_STACK_REALS(l1),G_STACK_IMAGS(l1), &
                  & t,t,1,t,t,1,                         &
                  & G_STACK_REALS(l2),G_STACK_IMAGS(l2), &
                  & 0,G_err)
      if (G_err .ne. 0) call mat_err(24)
      if (G_err .gt. 0) return
      s = G_STACK_REALS(ld)
      ld = ld + min(m,n) - 1
      t(1,1) = G_STACK_REALS(ld)
      if (t(1,1) .ne. 0.0d0) then
         G_STACK_REALS(l) = mat_flop(s/t(1,1))
         G_STACK_IMAGS(l) = 0.0d0
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      else
         call journal(' CONDITION IS INFINITE')
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
      endif
!===================================================================================================================================
    case(4) ! command::norm

      p = 2.0d0
      inf = .false.

      if (G_rhs .eq. 2)then
         fro = int(G_STACK_REALS(l)).eq.15 .and. mn.gt.1
         inf = int(G_STACK_REALS(l)).eq.18 .and. mn.gt.1
         if (.not. fro) then
            p = G_STACK_REALS(l)
         endif
         G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE-1
         l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
         m = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
         n = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
         mn = m*n
         if (fro) then
            m = mn
            n = 1
         endif
      endif

      if (m .gt. 1 .and. n .gt. 1) then
         ! matrix norm

         if (INF)then
            s = 0.0d0
            do i = 1, m
               li = l+i-1
               t(1,1) = mat_wasum(n,G_STACK_REALS(LI),G_STACK_IMAGS(li),m)
               s = dmax1(s,t(1,1))
            enddo
         elseif (p .eq. 1.0d0) then
            s = 0.0d0
            do j = 1, n
               lj = l+(j-1)*m
               t(1,1) = mat_wasum(m,G_STACK_REALS(LJ),G_STACK_IMAGS(lj),1)
               s = dmax1(s,t(1,1))
            enddo
         elseif (p .ne. 2.0d0) then
            call mat_err(23)
            return
         else
            ld = l + m*n
            l1 = ld + min(m+1,n)
            l2 = l1 + n

            if(too_much_memory( l2+min(m,n) - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )then
               return
            endif

            call ml_wsvdc(G_STACK_REALS(l),G_STACK_IMAGS(l), &
                        & m,m,n, &
                        & G_STACK_REALS(ld),G_STACK_IMAGS(ld), &
                        & G_STACK_REALS(l1),G_STACK_IMAGS(l1), &
                        & t,t,1,t,t,1, &
                        & G_STACK_REALS(l2),G_STACK_IMAGS(l2), &
                        & 0,G_err)

            if (G_ERR .ne. 0)then
               call mat_err(24)
               return
            endif

            s = G_STACK_REALS(LD)
         endif

      elseif (p .eq. 1.0d0)then
         s = mat_wasum(MN,G_STACK_REALS(l),G_STACK_IMAGS(l),1)
      elseif (p .eq. 2.0d0) then
         s = mat_wnrm2(MN,G_STACK_REALS(l),G_STACK_IMAGS(l),1)
      else
         i = mat_iwamax(mn,G_STACK_REALS(l),G_STACK_IMAGS(l),1) + l - 1
         s = dabs(G_STACK_REALS(i)) + dabs(G_STACK_IMAGS(i))

         if (.not.(inf .or. s .eq. 0.0d0))then
            t(1,1) = 0.0d0
            do i = 1, mn
               ls = l+i-1
               t(1,1) = mat_flop(t(1,1) + (mat_pythag(G_STACK_REALS(ls),G_STACK_IMAGS(ls))/s)**p)
            enddo
            if (p .ne. 0.0d0) then
               p = 1.0d0/p
            endif
            s = mat_flop(s*t(1,1)**p)
         endif
      endif

      G_STACK_REALS(l) = s
      G_STACK_IMAGS(l) = 0.0d0
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
!===================================================================================================================================
    case(1) !     COMMAND::SVD
      IF (G_LHS .EQ. 3)then
         K = M
         IF (G_RHS .EQ. 2) K = MIN(M,N)
         LU = L + M*N
         LD = LU + M*K
         LV = LD + K*N
         L1 = LV + N*N
         L2 = L1 + N

         if(too_much_memory( L2+MIN(M,N) - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

         JOB = 11
         IF (G_RHS .EQ. 2) JOB = 21
         call ml_wsvdc(G_STACK_REALS(l),G_STACK_IMAGS(l), &
         & m,m,n, &
         & G_STACK_REALS(ld),G_STACK_IMAGS(ld), &
         & G_STACK_REALS(l1),G_STACK_IMAGS(l1), &
         & G_STACK_REALS(lu),G_STACK_IMAGS(lu), &
         & m, &
         & G_STACK_REALS(lv),G_STACK_IMAGS(lv), &
         & n, &
         & G_STACK_REALS(l2),G_STACK_IMAGS(l2), &
         & job,G_err)
         DO JB = 1, N
            DO I = 1, K
               J = N+1-JB
               LL = LD+I-1+(J-1)*K
               IF (I.NE.J) G_STACK_REALS(LL) = 0.0D0
               G_STACK_IMAGS(LL) = 0.0D0
               LS = LD+I-1
               IF (I.EQ.J) G_STACK_REALS(LL) = G_STACK_REALS(LS)
               LS = L1+I-1
               IF (G_ERR.NE.0 .AND. I.EQ.J-1) G_STACK_REALS(LL) = G_STACK_REALS(LS)
            enddo
         enddo
         IF (G_ERR .NE. 0) call mat_err(24)
         G_ERR = 0
         call mat_wcopy(M*K+K*N+N*N, &
                      & G_STACK_REALS(LU),G_STACK_IMAGS(LU), &
                      & 1, &
                      & G_STACK_REALS(L),G_STACK_IMAGS(L), &
                      & 1)
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = M
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = K
         IF (G_BOTTOM_OF_SCRATCH_IN_USE+1 .GE. G_TOP_OF_SAVED) call mat_err(18)
         IF (G_ERR .GT. 0) return
         G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE+1
         G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) = L + M*K
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = K
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = N
         IF (G_BOTTOM_OF_SCRATCH_IN_USE+1 .GE. G_TOP_OF_SAVED) call mat_err(18)
         IF (G_ERR .GT. 0) return
         G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE+1
         G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) = L + M*K + K*N
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = N
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = N
      else
         LD = L + M*N
         L1 = LD + MIN(M+1,N)
         L2 = L1 + N

         if(too_much_memory( L2+MIN(M,N) - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

         call ml_wsvdc(G_STACK_REALS(l),G_STACK_IMAGS(l),m,m,n, &
         & G_STACK_REALS(ld),G_STACK_IMAGS(ld),G_STACK_REALS(l1),G_STACK_IMAGS(l1), &
         & t,t,1,t,t,1,G_STACK_REALS(l2),G_STACK_IMAGS(l2),0,G_err)
         IF (G_ERR .NE. 0) call mat_err(24)
         IF (G_ERR .GT. 0) return
         K = MIN(M,N)
         call mat_wcopy(K,G_STACK_REALS(LD),G_STACK_IMAGS(LD),1,G_STACK_REALS(L),G_STACK_IMAGS(L),1)
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = K
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      endif
!===================================================================================================================================
    case(2,5) ! COMMAND::PINV AND RANK
      TOL = -1.0D0
      IF (G_RHS .EQ. 2) then
         TOL = G_STACK_REALS(L)
         G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE-1
         L = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
         M = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
         N = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
      endif
      LU = L + M*N
      LD = LU + M*M
      IF (G_FIN .EQ. 5) LD = L + M*N
      LV = LD + M*N
      L1 = LV + N*N
      IF (G_FIN .EQ. 5) L1 = LD + N
      L2 = L1 + N

      if(too_much_memory( L2+MIN(M,N) - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

      IF (G_FIN .EQ. 2) JOB = 11
      IF (G_FIN .EQ. 5) JOB = 0
      call ML_WSVDC(G_STACK_REALS(L),G_STACK_IMAGS(L),M,M,N, &
                  & G_STACK_REALS(LD),G_STACK_IMAGS(LD), &
                  & G_STACK_REALS(L1),G_STACK_IMAGS(L1), &
                  & G_STACK_REALS(LU),G_STACK_IMAGS(LU), &
                  & M, &
                  & G_STACK_REALS(LV),G_STACK_IMAGS(LV), &
                  & N, &
                  & G_STACK_REALS(L2),G_STACK_IMAGS(L2), &
                  & JOB,G_ERR)
      IF (G_ERR .NE. 0) call mat_err(24)
      IF (G_ERR .GT. 0) return
      EPS = G_STACK_REALS(G_BIGMEM-4)
      IF (TOL .LT. 0.0D0) TOL = mat_flop(dble(MAX(M,N))*EPS*G_STACK_REALS(LD))
      MN = MIN(M,N)
      K = 0
      DO J = 1, MN
         LS = LD+J-1
         S = G_STACK_REALS(LS)
         IF (S .LE. TOL) exit
         K = J
         LL = LV+(J-1)*N
         IF (G_FIN .EQ. 2) call mat_wrscal(N,1.0D0/S,G_STACK_REALS(LL),G_STACK_IMAGS(LL),1)
      enddo
      if (G_fin .ne. 5) then
         do j = 1, m
            do i = 1, n
               ll = l+i-1+(j-1)*n
               l1 = lv+i-1
               l2 = lu+j-1
               G_STACK_REALS(ll) = mat_wdotcr(k,G_STACK_REALS(l2),G_STACK_IMAGS(l2),m,G_STACK_REALS(l1),G_STACK_IMAGS(l1),n)
               G_STACK_IMAGS(ll) = mat_wdotci(k,G_STACK_REALS(l2),G_STACK_IMAGS(l2),m,G_STACK_REALS(l1),G_STACK_IMAGS(l1),n)
            enddo
         enddo
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = m
      else
         G_STACK_REALS(l) = dble(k)
         G_STACK_IMAGS(l) = 0.0d0
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      endif
!===================================================================================================================================
   end select FUN3
!
end subroutine mat_matfn3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE mat_matfn4()

! ident_33="@(#)M_matrix::mat_matfn4(3fp): evaluate functions involving qr decomposition (least squares)"

integer           :: info
integer           :: j
integer           :: jb
integer           :: job
integer           :: k
integer           :: l
integer           :: l2
integer           :: l3
integer           :: l4
integer           :: le
integer           :: ll
integer           :: ls
integer           :: m
integer           :: m2
integer           :: mm
integer           :: mn
integer           :: n
integer           :: n2
character(len=81) :: message
DOUBLEPRECISION   :: T(1),TOL,EPS
!
      IF (G_DEBUG_LEVEL .EQ. 1) call journal('sc','*MATFN4* ', G_FIN)
!
      L = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
      M = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
      N = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)

      IF (G_FIN .EQ. -1) then
         goto 10
      elseIF (G_FIN .EQ. -2) then
         goto 20
      else
         goto 40
      endif
!
!     RECTANGULAR MATRIX RIGHT DIVISION, A/A2
   10 continue
      L2 = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      M2 = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      N2 = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE + 1
      IF (N.GT.1 .AND. N.NE.N2) call mat_err(11)
      IF (G_ERR .GT. 0) return
      call mat_stack1(QUOTE)
      IF (G_ERR .GT. 0) return
      LL = L2+M2*N2
      call mat_wcopy(M*N,G_STACK_REALS(L),G_STACK_IMAGS(L),1,G_STACK_REALS(LL),G_STACK_IMAGS(LL),1)
      call mat_wcopy(M*N+M2*N2,G_STACK_REALS(L2),G_STACK_IMAGS(L2),1,G_STACK_REALS(L),G_STACK_IMAGS(L),1)
      G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) = L+M2*N2
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = M
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = N
      call mat_stack1(QUOTE)
      IF (G_ERR .GT. 0) return
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE - 1
      M = N2
      N = M2
      goto 20
!
!     RECTANGULAR MATRIX LEFT DIVISION A BACKSLASH A2
!
   20 continue
      L2 = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      M2 = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      N2 = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      IF (M2*N2 .GT. 1) goto 21
        M2 = M
        N2 = M

        if(too_much_memory( L2+M*M - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

        call mat_wset(M*M-1,0.0D0,0.0D0,G_STACK_REALS(L2+1),G_STACK_IMAGS(L2+1),1)
        call mat_wcopy(M,G_STACK_REALS(L2),G_STACK_IMAGS(L2),0,G_STACK_REALS(L2),G_STACK_IMAGS(L2),M+1)
   21 continue
      IF (M2 .NE. M) call mat_err(12)
      IF (G_ERR .GT. 0) return
      L3 = L2 + MAX(M,N)*N2
      L4 = L3 + N

      if(too_much_memory( L4 + N - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

      IF (M .GT. N) goto 23
      DO JB = 1, N2
        J = N+1-JB
        LS = L2 + (J-1)*M
        LL = L2 + (J-1)*N
        call mat_wcopy(M,G_STACK_REALS(LS),G_STACK_IMAGS(LS),-1,G_STACK_REALS(LL),G_STACK_IMAGS(LL),-1)
      enddo
   23 continue
      DO J = 1, N
        G_BUF(J) = 0
      enddo
      call ML_WQRDC(G_STACK_REALS(L),G_STACK_IMAGS(L), &
                  & M,M,N, &
                  & G_STACK_REALS(L4),G_STACK_IMAGS(L4), &
                  & G_BUF, &
                  & G_STACK_REALS(L3),G_STACK_IMAGS(L3), &
                  & 1)
      K = 0
      EPS = G_STACK_REALS(G_BIGMEM-4)
      T(1) = DABS(G_STACK_REALS(L))+DABS(G_STACK_IMAGS(L))
      TOL = mat_flop(dble(MAX(M,N))*EPS*T(1))
      MN = MIN(M,N)
      DO J = 1, MN
        LS = L+J-1+(J-1)*M
        T(1) = DABS(G_STACK_REALS(LS)) + DABS(G_STACK_IMAGS(LS))
        IF (T(1) .GT. TOL) K = J
      enddo
      IF (K .LT. MN) then
         WRITE(message,'(" RANK DEFICIENT,  RANK =",I4,",  TOL =",1PD13.4)') K,TOL
         call journal(message)
      endif
      MN = MAX(M,N)
      DO J = 1, N2
        LS = L2+(J-1)*MN
        call ML_WQRSL(G_STACK_REALS(L),G_STACK_IMAGS(L), &
                        & M,M,K, &
                        & G_STACK_REALS(L4),G_STACK_IMAGS(L4), &
                        & G_STACK_REALS(LS),G_STACK_IMAGS(LS), &
                        & T,T, &
                        & G_STACK_REALS(LS),G_STACK_IMAGS(LS), &
                        & G_STACK_REALS(LS),G_STACK_IMAGS(LS), &
                        & T,T,T,T,100,INFO)
        LL = LS+K
        call mat_wset(N-K,0.0D0,0.0D0,G_STACK_REALS(LL),G_STACK_IMAGS(LL),1)
      enddo
      DO J = 1, N
        G_BUF(J) = -G_BUF(J)
      enddo
      DO J = 1, N
        IF (G_BUF(J) .GT. 0) cycle
        K = -G_BUF(J)
        G_BUF(J) = K
   33   CONTINUE
          IF (K .EQ. J) cycle
          LS = L2+J-1
          LL = L2+K-1
          call mat_wswap(N2,G_STACK_REALS(LS),G_STACK_IMAGS(LS),MN,G_STACK_REALS(LL),G_STACK_IMAGS(LL),MN)
          G_BUF(K) = -G_BUF(K)
          K = G_BUF(K)
          goto 33
      enddo
      DO J = 1, N2
        LS = L2+(J-1)*MN
        LL = L+(J-1)*N
        call mat_wcopy(N,G_STACK_REALS(LS),G_STACK_IMAGS(LS),1,G_STACK_REALS(LL),G_STACK_IMAGS(LL),1)
      enddo
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = N
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = N2
      IF (G_FIN .EQ. -1) call mat_stack1(QUOTE)
      IF (G_ERR .GT. 0) return
      goto 99
!===================================================================================================================================
!     QR
!
   40 continue
      MM = MAX(M,N)
      LS = L + MM*MM
      IF (G_LHS.EQ.1 .AND. G_FIN.EQ.1) LS = L
      LE = LS + M*N
      L4 = LE + MM

      if(too_much_memory( L4+MM - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

      IF (LS.NE.L) call mat_wcopy(M*N,G_STACK_REALS(L),G_STACK_IMAGS(L),1,G_STACK_REALS(LS),G_STACK_IMAGS(LS),1)
      JOB = 1
      IF (G_LHS.LT.3) JOB = 0
      DO J = 1, N
        G_BUF(J) = 0
      enddo
      call ML_WQRDC(G_STACK_REALS(LS),G_STACK_IMAGS(LS), &
       & M,M,N, &
       & G_STACK_REALS(L4),G_STACK_IMAGS(L4), &
       & G_BUF, &
       & G_STACK_REALS(LE),G_STACK_IMAGS(LE), &
       & JOB)
      IF (G_LHS.EQ.1 .AND. G_FIN.EQ.1) goto 99
      call mat_wset(M*M,0.0D0,0.0D0,G_STACK_REALS(L),G_STACK_IMAGS(L),1)
      call mat_wset(M,1.0D0,0.0D0,G_STACK_REALS(L),G_STACK_IMAGS(L),M+1)
      DO J = 1, M
        LL = L+(J-1)*M
        call ML_WQRSL(G_STACK_REALS(LS),G_STACK_IMAGS(LS),M,M,N,G_STACK_REALS(L4),G_STACK_IMAGS(L4),   &
     &             G_STACK_REALS(LL),G_STACK_IMAGS(LL),G_STACK_REALS(LL),G_STACK_IMAGS(LL),T,T,        &
     &             T,T,T,T,T,T,10000,INFO)
      enddo
      IF (G_FIN .EQ. 2) goto 99
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = M
      DO J = 1, N
        LL = LS+J+(J-1)*M
        call mat_wset(M-J,0.0D0,0.0D0,G_STACK_REALS(LL),G_STACK_IMAGS(LL),1)
      enddo
      IF (G_BOTTOM_OF_SCRATCH_IN_USE+1 .GE. G_TOP_OF_SAVED) call mat_err(18)
      IF (G_ERR .GT. 0) return
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE+1
      G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) = LS
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = M
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = N
      IF (G_LHS .EQ. 2) goto 99
      call mat_wset(N*N,0.0D0,0.0D0,G_STACK_REALS(LE),G_STACK_IMAGS(LE),1)
      DO J = 1, N
        LL = LE+G_BUF(J)-1+(J-1)*N
        G_STACK_REALS(LL) = 1.0D0
      enddo
      IF (G_BOTTOM_OF_SCRATCH_IN_USE+1 .GE. G_TOP_OF_SAVED) call mat_err(18)
      IF (G_ERR .GT. 0) return
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE+1
      G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) = LE
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = N
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = N
      goto 99
!===================================================================================================================================
!
   99 continue
END SUBROUTINE mat_matfn4
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_matfn5()

! ident_34="@(#)M_matrix::mat_matfn5(3fp):file handling and other I/O"

character(len=GG_LINELEN)  :: mline
character(len=256)  :: errmsg
character(len=1024) :: name
character(len=1)    :: ch_char
integer             :: temp_lun
integer             :: ios

integer,save        :: flag=0  ! should be saved or set at each call?
integer,save        :: lrat=5
integer,save        :: mrat=100
integer             :: ch,top2,ch1(1)
integer             :: id(GG_MAX_NAME_LENGTH)
doubleprecision     :: eps,b,s,t,tdum(2)
logical             :: text
integer             :: i, j, k, l, m, n
integer             :: img
integer             :: job
integer             :: l2
integer             :: ll
integer             :: ls
integer             :: lun
integer             :: lunit
integer             :: lw
integer             :: lx
integer             :: ly
integer             :: mn
logical             :: isfound
!
   if (G_DEBUG_LEVEL .eq. 1) call journal('sc','*MATFN5* ',G_FIN)
   l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
   m = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
   n = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)

   !  functions/G_FIN
   !  exec save load prin diar disp base line char plot rat  debu doc
   !    1    2    3    4    5    6    7    8    9   10   11   12   13

   select case(G_fin)
      case(:5,13)
         ! convert file name
         mn = m*n
         if (G_SYM .eq. semi)then
            flag = 0
         else
            flag = 3
         endif

         if (G_RHS .ge. 2) then            ! if more than one parameter on exec('filename',flag) get value of FLAG
            flag = int(G_STACK_REALS(l))
            top2 = G_BOTTOM_OF_SCRATCH_IN_USE
            G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE-1
            l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
            mn = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
         endif
         lun = -1
         if (mn.eq.1 .and. G_STACK_REALS(l).LT.10.0d0) lun = int(G_STACK_REALS(l))

         if (lun .lt. 0) then
             do j = 1, GG_LINELEN
                ls = l+j-1
                if (j .le. mn) ch = int(G_STACK_REALS(ls))
                if (j .gt. mn) ch = blank
                if (ch.lt.0 .or. ch.ge.g_charset_size) call mat_err(38)
                if (G_ERR .gt. 0) return
                G_BUF(j) = G_CHARSET(ch+1)
             enddo
         endif
      case(6:12)
      case default
      end select
!===================================================================================================================================
      FUN5 : select case(G_fin)
!===================================================================================================================================
      case(1)                                               ! command::exec
      if (lun .eq. 0) then                                  ! exec(0)
         G_RIO = G_INPUT_LUN
         G_ERR = 99
      else
         k = G_LINE_POINTER(6)
         G_LIN(k+1) = G_LINE_POINTER(1)
         G_LIN(k+2) = G_LINE_POINTER(3)
         G_LIN(k+3) = G_LINE_POINTER(6)
         G_LIN(k+4) = G_PTZ
         G_LIN(k+5) = G_RIO
         G_LIN(k+6) = G_LINECOUNT(4)
         G_LINE_POINTER(1) = k + 7
         G_LINECOUNT(4) = flag
         G_PTZ = G_PT - 4
         if (G_RIO .eq. G_INPUT_LUN) G_RIO = 12
         G_RIO = G_RIO + 1
         if (LUN .gt. 0) G_RIO = lun
         !!call find_exec_file(G_BUF,isfound)
         if (lun .lt. 0) call mat_files(G_RIO,G_BUF,status='old')
         if (flag .ge. 4)call journal(' PAUSE MODE. Enter blank lines.')
         G_SYM = G_EOL
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
      endif
!===================================================================================================================================
      case(2) ! COMMAND::SAVE
      if (lun .lt. 0) lunit = 1
      if (lun .lt. 0) call mat_files(lunit,G_BUF)
      if (lun .gt. 0) lunit = lun
      k = GG_MAX_NUMBER_OF_NAMES-4
      if (k .lt. G_TOP_OF_SAVED) k = GG_MAX_NUMBER_OF_NAMES
      if (G_RHS .eq. 2) k = top2
      if (G_RHS .eq. 2) call mat_putid(G_STACK_IDS(1,k),G_SYN)
      do
         l = G_STACK_ID_LOC(k)
         m = G_STACK_ROWS(k)
         n = G_STACK_COLS(k)
         do i = 1, GG_MAX_NAME_LENGTH
            j = G_STACK_IDS(i,k)+1
            G_BUF(i) = G_CHARSET(j)
         enddo
         img = 0
         if (mat_wasum(m*n,G_STACK_IMAGS(l),G_STACK_IMAGS(l),1) .ne. 0.0d0) img = 1
         if(.not.G_FILE_OPEN_ERROR)call mat_savlod(lunit,G_BUF,m,n,img,0,G_STACK_REALS(l),G_STACK_IMAGS(l))
         k = k-1
         if (k .lt. G_TOP_OF_SAVED) exit
      enddo
      call mat_files(-LUNIT,G_BUF)
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
!===================================================================================================================================
      case(3) ! COMMAND::LOAD
      IF (LUN .LT. 0) LUNIT = 2
      IF (LUN .LT. 0) call mat_files(LUNIT,G_BUF) ! open the unit
      IF (LUN .GT. 0) LUNIT = LUN
      do
         JOB = G_STACK_ID_LOC(G_TOP_OF_SAVED) - L
         IF(.not.G_FILE_OPEN_ERROR)call mat_savlod(LUNIT,ID, &
                                                 & G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE), &
                                                 & G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE), &
                                                 & IMG,JOB, &
                                                 & G_STACK_REALS(L),G_STACK_IMAGS(L))
         MN = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
         IF (MN .NE. 0)then
            IF (IMG .EQ. 0) call mat_rset(MN,0.0D0,G_STACK_IMAGS(L),1)
            DO I = 1, GG_MAX_NAME_LENGTH
               J = 0
               do
                  J = J+1
                  IF (ID(I).NE.G_CHARSET(J) .AND. J.LE.BLANK) cycle
                  exit
               enddo
               ID(I) = J-1
            enddo
            G_SYM = SEMI
            G_RHS = 0
            call MAT_STACK_PUT(ID)
            G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE + 1
         else
            exit
         endif
      enddo
      call mat_files(-LUNIT,G_BUF) ! close unit
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
!===================================================================================================================================
      case(4) ! command::print
      k = G_OUTPUT_LUN                                ! hold
      G_OUTPUT_LUN = lun
      if (lun .lt. 0) G_OUTPUT_LUN = 7
      if (lun .lt. 0) call mat_files(G_OUTPUT_LUN,G_BUF)

      l = G_LINECOUNT(2)                              ! hold
      G_LINECOUNT(2) = 999999                         ! turn off paging of output
      if (G_RHS .gt. 1) call mat_print(G_SYN,top2)

      G_LINECOUNT(2) = l                              ! restore
      G_OUTPUT_LUN = k                                ! restore

      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
!===================================================================================================================================
      case(5) ! command::diary
      if (lun < 0) call mat_files(8,G_BUF)
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
!===================================================================================================================================
      case(6,7) !     COMMAND::DISPLAY
60    continue
      if (G_FIN.eq.7)goto 65
      if (G_RHS .eq. 2) goto 65
      mn = m*n
      text = .true.
      do i = 1, mn
        ls = l+i-1
        ch = int(G_STACK_REALS(LS))
        text = text .and. (ch.ge.0) .and. (ch.lt.G_CHARSET_SIZE)
        text = text .and. (dble(ch).eq.G_STACK_REALS(ls) )
      enddo

      do i = 1, m
         do j = 1, n
           ls = l+i-1+(j-1)*m
           if (G_STACK_REALS(ls) .eq. 0.0d0) ch = blank
           if (G_STACK_REALS(ls) .gt. 0.0d0) ch = plus
           if (G_STACK_REALS(ls) .lt. 0.0d0) ch = minus
           if (text) ch = int(G_STACK_REALS(ls))
           G_BUF(j) = G_CHARSET(ch+1)
         enddo
         call mat_buf2str(mline,G_BUF,n)
         call journal(mline)
      enddo
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
      exit FUN5
!. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!     COMMAND::BASE
   65 CONTINUE
      IF (G_RHS .NE. 2) call mat_err(39) ! Incorrect number of arguments
      IF (G_STACK_REALS(L) .LE. 1.0D0) call mat_err(36)
      IF (G_ERR .GT. 0) exit FUN5
      B = G_STACK_REALS(L)
      L2 = L
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE-1
      G_RHS = 1
      L = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
      M = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
      EPS = G_STACK_REALS(G_BIGMEM-4)
      DO I = 1, M
         LS = L2+(I-1)*N
         LL = L+I-1
         call mat_base(G_STACK_REALS(LL),B,EPS,G_STACK_REALS(LS),N)
      enddo
      call mat_rset(M*N,0.0D0,G_STACK_IMAGS(L2),1)
      call mat_wcopy(M*N,G_STACK_REALS(L2),G_STACK_IMAGS(L2),1,G_STACK_REALS(L),G_STACK_IMAGS(L),1)
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = N
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = M
      call mat_stack1(QUOTE)
      IF (G_FIN .EQ. 6) goto 60
!===================================================================================================================================
      case(8)
!     COMMAND::LINES
      G_LINECOUNT(2) = int(G_STACK_REALS(L))
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
!===================================================================================================================================
      case(9) !     COMMAND::CHAR
      K = IABS(int(G_STACK_REALS(L)))
      IF (M*N.NE.1 .OR. K.GE.G_CHARSET_SIZE) call mat_err(36)
      IF (G_ERR .GT. 0) exit FUN5
      CH = G_CHARSET(K+1)
      IF (G_STACK_REALS(L) .LT. 0.0D0) CH = G_ALT_CHARSET(K+1)
      WRITE(mline,'('' REPLACE CHARACTER '',A1)') CHAR(CH)
      call journal(mline)
      READ(G_INPUT_LUN,'(A1)') CH_CHAR
      call mat_str2buf(ch_char,ch1,1); ch=ch1(1)
      IF (G_STACK_REALS(L) .GE. 0.0D0) G_CHARSET(K+1) = CH
      IF (G_STACK_REALS(L) .LT. 0.0D0) G_ALT_CHARSET(K+1) = CH
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
!===================================================================================================================================
      case(10) !     COMMAND::PLOT
      IF (G_RHS .GE. 2) goto 82
      N = M*N
      DO I = 1, N
         LL = L+I-1
         G_STACK_IMAGS(LL) = dble(I)
      enddo
      call mat_plot(G_OUTPUT_LUN,G_STACK_IMAGS(L),G_STACK_REALS(L),N,TDUM,0)
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
      exit FUN5
!. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

   82 continue
      IF (G_RHS .EQ. 2) K = 0
      IF (G_RHS .EQ. 3) K = M*N
      IF (G_RHS .GT. 3) K = G_RHS - 2
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE - (G_RHS - 1)
      N = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
      IF (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE+1)*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE+1) .NE. N) call mat_err(5)
      IF (G_ERR .GT. 0) exit FUN5
      LX = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
      LY = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      IF (G_RHS .GT. 3) L = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE+2)
      call mat_plot(G_OUTPUT_LUN,G_STACK_REALS(LX),G_STACK_REALS(LY),N,G_STACK_REALS(L),K)
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
!===================================================================================================================================
      case(11) ! COMMAND::RAT
      if (G_rhs .ne. 2) then
         mn = m*n
         l2 = l
         if (G_lhs .eq. 2) l2 = l + mn
         lw = l2 + mn

         if(too_much_memory( lw + lrat - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

         if (G_lhs .eq. 2) G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE + 1
         G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) = l2
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = m
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
         call mat_rset(G_lhs*mn,0.0d0,G_STACK_IMAGS(l),1)
         do i = 1, mn
            call mat_rat(G_STACK_REALS(l),lrat,mrat,s,t,G_STACK_REALS(lw))
            G_STACK_REALS(l) = s
            G_STACK_REALS(l2) = t
            if (G_lhs .eq. 1) G_STACK_REALS(l) = mat_flop(s/t)
            l = l + 1
            l2 = l2 + 1
         enddo
      else
         mrat = int(G_STACK_REALS(l))
         lrat = int(G_STACK_REALS(l-1))
         G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE - 1
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
      endif
!===================================================================================================================================
      case(12) !     COMMAND::DEBUG
      G_DEBUG_LEVEL = int(G_STACK_REALS(L))
      call journal('sc',' DEBUG ',G_DEBUG_LEVEL)
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
!===================================================================================================================================
      case(13) !     COMMAND::DOC
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
!===================================================================================================================================
      end select FUN5
!===================================================================================================================================
end subroutine mat_matfn5
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_stack_get(id)

! ident_35="@(#)M_matrix::mat_stack_get(3fp): get variables from storage"

integer,intent(in)  :: id(GG_MAX_NAME_LENGTH)
integer             :: i
integer             :: j
integer             :: k
integer             :: l
integer             :: l2
integer             :: l3
integer             :: li
integer             :: lj
integer             :: current_location
integer             :: ll
integer             :: ls
integer             :: m
integer             :: mk
integer             :: mn
integer             :: mnk
integer             :: n
character(len=GG_MAX_NAME_LENGTH)    :: id_name

   if (G_DEBUG_LEVEL .eq. 1)then
      call journal('sc','*MAT_STACK_GET* ID(1)=',id(1), id(2), id(3), id(4))
   endif

   call mat_putid(G_STACK_IDS(1,G_TOP_OF_SAVED-1), ID)    ! copy ID to next blank entry in G_STACK_IDS in case it is not there(?)

   do k=GG_MAX_NUMBER_OF_NAMES,1,-1                       ! start at bottom and search up through names till find the name
      if (mat_eqid(G_STACK_IDS(1,k), id))exit             ! if found name exit loop
   enddo
   ! if (?)
   ! or if matched the name inserted above did not find it.
   if ( (k .ge. GG_MAX_NUMBER_OF_NAMES-1 .and. G_RHS .gt. 0) .or. (k .eq. G_TOP_OF_SAVED-1) ) then
      g_fin = 0
      return
   endif

   current_location = G_STACK_ID_LOC(K)                               ! found it, so this is the location where the data begins
   IF (G_RHS .EQ. 1) then                                             ! VECT(ARG)
      IF (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) .EQ. 0) goto 99
      L = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
      MN = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
      MNK = G_STACK_ROWS(K)*G_STACK_COLS(K)                            ! number of values in this variable
      IF (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) .LT. 0) MN = MNK
      DO I = 1, MN
        LL = L+I-1
        LS = current_location+I-1
        IF (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) .GT. 0) LS = current_location + int(G_STACK_REALS(LL)) - 1
        IF (LS .LT. current_location .OR. LS .GE. current_location+MNK) call mat_err(21)          ! Subscript out of range
        IF (G_ERR .GT. 0) return
        G_STACK_REALS(LL) = G_STACK_REALS(LS)
        G_STACK_IMAGS(LL) = G_STACK_IMAGS(LS)
      enddo
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      IF (G_STACK_ROWS(K) .GT. 1) G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = MN
      IF (G_STACK_ROWS(K) .EQ. 1) G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = MN
      goto 99
   elseif (G_RHS .EQ. 2) then                                              ! MATRIX(ARG,ARG)
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE-1
      L = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
      IF (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE+1) .EQ. 0) G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
      IF (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) .EQ. 0) goto 99
      L2 = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      M = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
      IF (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) .LT. 0) M = G_STACK_ROWS(K)
      N = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE+1)*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      IF (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE+1) .LT. 0) N = G_STACK_COLS(K)
      L3 = L2 + N
      MK = G_STACK_ROWS(K)
      MNK = G_STACK_ROWS(K)*G_STACK_COLS(K)
      DO J = 1, N
         DO I = 1, M
           LI = L+I-1
           IF (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) .GT. 0) LI = L + int(G_STACK_REALS(LI)) - 1
           LJ = L2+J-1
           IF (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE+1) .GT. 0) LJ = L2 + int(G_STACK_REALS(LJ)) - 1
           LS = current_location + LI-L + (LJ-L2)*MK
           IF (LS.LT.current_location .OR. LS.GE.current_location+MNK) call mat_err(21)
           IF (G_ERR .GT. 0) return
           LL = L3 + I-1 + (J-1)*M
           G_STACK_REALS(LL) = G_STACK_REALS(LS)
           G_STACK_IMAGS(LL) = G_STACK_IMAGS(LS)
         enddo
      enddo
      MN = M*N
      call mat_wcopy(MN,G_STACK_REALS(L3),G_STACK_IMAGS(L3),1,G_STACK_REALS(L),G_STACK_IMAGS(L),1)
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = M
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = N
      goto 99
   elseif (G_RHS .GT. 2) then
      call mat_err(21)                                                     ! Subscript out of range
      IF (G_ERR .GT. 0) return
   else                                                                    ! SCALAR
      L = 1
      IF (G_BOTTOM_OF_SCRATCH_IN_USE .GT. 0) &
        & L = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) + &
        & G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
      IF (G_BOTTOM_OF_SCRATCH_IN_USE+1 .GE. G_TOP_OF_SAVED) call mat_err(18)  ! Too many names
      IF (G_ERR .GT. 0) return

      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE+1

      !  LOAD VARIABLE TO TOP OF STACK
      G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) = L
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = G_STACK_ROWS(K)
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = G_STACK_COLS(K)
      MN = G_STACK_ROWS(K)*G_STACK_COLS(K)

      if(too_much_memory( L+MN - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

      !  IF RAND, MATFN6 GENERATES RANDOM NUMBER
      IF (K .EQ. GG_MAX_NUMBER_OF_NAMES) then
         G_FIN = 7
         G_FUN = 6
         return
      endif
      call mat_wcopy(MN,G_STACK_REALS(current_location),G_STACK_IMAGS(current_location),1,G_STACK_REALS(L),G_STACK_IMAGS(L),1)
   endif

99 continue
   G_FIN = -1
   G_FUN = 0

END SUBROUTINE MAT_STACK_GET
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_stack2(op)

! ident_36="@(#)M_matrix::ml_stackp(3fp): binary and ternary operations"

integer           :: op
doubleprecision   :: sr,si,e1,st,e2

integer           ::  i
integer           ::  j
integer           ::  k
integer           ::  k1
integer           ::  k2
integer           ::  kexp
integer           ::  l
integer           ::  l1
integer           ::  l2
integer           ::  l3
integer           ::  ll
integer           ::  ls
integer           ::  m
integer           ::  m2
integer           ::  mn
integer           ::  n
integer           ::  n2
integer           ::  nexp
integer           :: op_select

   if (G_DEBUG_LEVEL .eq. 1) then
      call journal('sc',',STACK2 ',op)
   endif
   l2 = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
   m2 = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
   n2 = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
   G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE-1
   l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
   m = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
   n = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
   G_FUN = 0

   if(op.eq.DSTAR)then
      op_select=-op
   else
      op_select=op
   endif
   DO_OP: select case(op_select)
!-----------------------------------------------------------------------------------------------------------------------------------
   case (PLUS) ! ADDITION
      if (m .lt. 0) then
         if (m2 .ne. n2) then
            call mat_err(8)
            exit DO_OP
         endif
         m = m2
         n = n2
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = m
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
         sr = G_STACK_REALS(l)
         si = G_STACK_IMAGS(l)
         call mat_wcopy(m*n,G_STACK_REALS(l+1),G_STACK_IMAGS(l+1),1,G_STACK_REALS(l),G_STACK_IMAGS(l),1)
         call finish()
         exit DO_OP
      endif
      if (m2 .lt. 0)then
         if (m .ne. n) then
            call mat_err(8)
            exit DO_OP
         endif
         sr = G_STACK_REALS(l2)
         si = G_STACK_IMAGS(l2)
         call finish()
         exit DO_OP
      endif
      if (m .ne. m2) then
         call mat_err(8)
         exit DO_OP
      endif
      if (n .ne. n2) then
         call mat_err(8)
         exit DO_OP
      endif
      call matX_waxpy(m*n,1.0d0,0.0d0,G_STACK_REALS(l2),G_STACK_IMAGS(l2),1,G_STACK_REALS(l),G_STACK_IMAGS(l),1)
!-----------------------------------------------------------------------------------------------------------------------------------
   case (MINUS) ! SUBTRACTION
      if (m .lt. 0) then
         if (m2 .ne. n2)then
            call mat_err(9)
            exit do_op
         endif
         m = m2
         n = n2
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = m
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
         sr = G_STACK_REALS(l)
         si = G_STACK_IMAGS(l)
         call mat_wcopy(m*n,G_STACK_REALS(l+1),G_STACK_IMAGS(l+1),1,G_STACK_REALS(l),G_STACK_IMAGS(l),1)
         call mat_wrscal(m*n,-1.0d0,G_STACK_REALS(l),G_STACK_IMAGS(l),1)
         call finish()
         exit DO_OP
      endif
      if (m2 .lt. 0) then
         ! add or subtract scalar
         if (m .ne. n) then
            call mat_err(9)
            exit DO_OP
         endif
         sr = -G_STACK_REALS(l2)
         si = -G_STACK_IMAGS(l2)
         call finish()
         exit DO_OP
      endif
      if (m .ne. m2)then
         call mat_err(9)
         exit DO_OP
      endif
      if (n .ne. n2) then
         call mat_err(9)
         exit DO_OP
      endif
      call matX_waxpy(M*N,-1.0D0,0.0D0,G_STACK_REALS(L2),G_STACK_IMAGS(L2),1,G_STACK_REALS(L),G_STACK_IMAGS(L),1)
!-----------------------------------------------------------------------------------------------------------------------------------
   case (STAR) ! MULTIPLICATION
      if (m2*m2*n2 .eq. 1) goto 10
      if (m*n .eq. 1) goto 11
      if (m2*n2 .eq. 1) goto 10
      if (n .ne. m2) call mat_err(10)
      if (g_err .gt. 0) then
         exit do_op
      endif
      mn = m*n2
      ll = l + mn

      if(too_much_memory( ll+m*n+m2*n2 - G_STACK_ID_LOC(G_TOP_OF_SAVED)) ) exit do_op

      call mat_wcopy(m*n+m2*n2,G_STACK_REALS(l),G_STACK_IMAGS(l),-1,G_STACK_REALS(ll),G_STACK_IMAGS(ll),-1)
      do j = 1, n2
         do i = 1, m
            k1 = l + mn + (i-1)
            k2 = l2 + mn + (j-1)*m2
            k = l + (i-1) + (j-1)*m
            G_STACK_REALS(k) = mat_wdotur(N,G_STACK_REALS(k1),G_STACK_IMAGS(k1),m,G_STACK_REALS(k2),G_STACK_IMAGS(k2),1)
            G_STACK_IMAGS(k) = mat_wdotui(N,G_STACK_REALS(k1),G_STACK_IMAGS(k1),m,G_STACK_REALS(k2),G_STACK_IMAGS(k2),1)
         enddo
      enddo
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = n2
      exit do_op
!-----------------------------------------------------------------------------------------------------------------------------------
   ! multiplication by scalar
   10 continue
      sr = G_STACK_REALS(l2)
      si = G_STACK_IMAGS(l2)
      l1 = l
      goto 13
   11 continue
      sr = G_STACK_REALS(l)
      si = G_STACK_IMAGS(l)
      l1 = l+1
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = m2
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = n2
   13 continue
      mn = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
      call mat_wscal(mn,sr,si,G_STACK_REALS(l1),G_STACK_IMAGS(l1),1)
      if (l1.ne.l) call mat_wcopy(mn,G_STACK_REALS(l1),G_STACK_IMAGS(l1),1,G_STACK_REALS(l),G_STACK_IMAGS(l),1)
!-----------------------------------------------------------------------------------------------------------------------------------
   case (-DSTAR) ! POWER
      IF (M2*N2 .NE. 1) call mat_err(30)
      IF (G_ERR .GT. 0) then
         exit do_op
      endif
      IF (M .NE. N) call mat_err(20)
      IF (G_ERR .GT. 0) then
         exit do_op
      endif
      NEXP = int(G_STACK_REALS(L2))

      IF ( (G_STACK_REALS(L2) .NE. dble(NEXP)) .or. (G_STACK_IMAGS(L2) .NE. 0.0D0) .or. (NEXP .LT. 2) )then
         ! NONINTEGER OR NONPOSITIVE POWER, USE EIGENVECTORS
         G_FUN = 2
         G_FIN = 0
         exit DO_OP
      endif

      MN = M*N

      if(too_much_memory( L2+MN+N - G_STACK_ID_LOC(G_TOP_OF_SAVED)) ) exit do_op

      call mat_wcopy(MN,G_STACK_REALS(L),G_STACK_IMAGS(L),1,G_STACK_REALS(L2),G_STACK_IMAGS(L2),1)
      L3 = L2+MN
      DO KEXP = 2, NEXP
         DO J = 1, N
            LS = L+(J-1)*N
            call mat_wcopy(N,G_STACK_REALS(LS),G_STACK_IMAGS(LS),1,G_STACK_REALS(L3),G_STACK_IMAGS(L3),1)
            DO I = 1, N
               LS = L2+I-1
               LL = L+I-1+(J-1)*N
               G_STACK_REALS(LL)=mat_wdotur(N,G_STACK_REALS(LS),G_STACK_IMAGS(LS),N,G_STACK_REALS(L3),G_STACK_IMAGS(L3),1)
               G_STACK_IMAGS(LL)=mat_wdotui(N,G_STACK_REALS(LS),G_STACK_IMAGS(LS),N,G_STACK_REALS(L3),G_STACK_IMAGS(L3),1)
            enddo
         enddo
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   case (SLASH) ! right division
      if (m2*n2 .ne. 1) then
         if (m2 .eq. n2) G_FUN = 1
         if (m2 .ne. n2) G_FUN = 4
         G_FIN = -1
         G_RHS = 2
         exit DO_OP
      endif
      sr = G_STACK_REALS(l2)
      si = G_STACK_IMAGS(l2)
      mn = m*n
      do i = 1, mn
         ll = l+i-1
         call mat_wdiv(G_STACK_REALS(ll),G_STACK_IMAGS(ll),sr,si,G_STACK_REALS(ll),G_STACK_IMAGS(ll))
         if (G_ERR .gt. 0) exit
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   case (BSLASH) ! LEFT DIVISION
      if (m*n .ne. 1) then
         if (m .eq. n) G_FUN = 1
         if (m .ne. n) G_FUN = 4
         G_FIN = -2
         G_RHS = 2
         exit DO_OP
      endif
      SR = G_STACK_REALS(L)
      SI = G_STACK_IMAGS(L)
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = M2
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = N2
      MN = M2*N2
      DO I = 1, MN
         LL = L+I-1
         call mat_wdiv(G_STACK_REALS(LL+1),G_STACK_IMAGS(LL+1),SR,SI,G_STACK_REALS(LL),G_STACK_IMAGS(LL))
         IF (G_ERR .GT. 0) exit
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   case (COLON) ! COLON
      E2 = G_STACK_REALS(L2)
      ST = 1.0D0
      N = 0
      IF (G_RHS .GE. 3) then
         ST = G_STACK_REALS(L)
         G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE-1
         L = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
         IF (ST .EQ. 0.0D0) goto 63
      endif

      E1 = G_STACK_REALS(L)
      ! CHECK FOR CLAUSE
      IF (G_RSTK(G_PT) .EQ. 3) then
   !     FOR CLAUSE
         G_STACK_REALS(L) = E1
         G_STACK_REALS(L+1) = ST
         G_STACK_REALS(L+2) = E2
         G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = -3
         G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = -1
         exit DO_OP
      endif

      if(too_much_memory( L + MAX(3,int((E2-E1)/ST)) - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) ) exit do_op

      do
         IF (ST .GT. 0.0D0 .AND. G_STACK_REALS(L) .GT. E2) exit
         IF (ST .LT. 0.0D0 .AND. G_STACK_REALS(L) .LT. E2) exit
         N = N+1
         L = L+1
         G_STACK_REALS(L) = E1 + dble(N)*ST
         G_STACK_IMAGS(L) = 0.0D0
      enddo

   63 continue
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = N
      G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
      IF (N .EQ. 0) G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
!-----------------------------------------------------------------------------------------------------------------------------------
   case (dot+1:2*dot) ! element-wise operations
      op = op - dot
      if (m.ne.m2 .or. n.ne.n2) call mat_err(10)
      if (g_err .gt. 0) then
         exit do_op
      endif
      mn = m*n
      do i = 1, mn
         j = l+i-1
         k = l2+i-1
         select case(op)
         case(STAR)
         call mat_wmul(G_STACK_REALS(J),G_STACK_IMAGS(J), &
                                        G_STACK_REALS(K),G_STACK_IMAGS(K), &
                                        G_STACK_REALS(J),G_STACK_IMAGS(J))
         case(SLASH)
         call mat_wdiv(G_STACK_REALS(J),G_STACK_IMAGS(J), &
                                        G_STACK_REALS(K),G_STACK_IMAGS(K), &
                                        G_STACK_REALS(J),G_STACK_IMAGS(J))
         case(BSLASH)
         call mat_wdiv(G_STACK_REALS(K),G_STACK_IMAGS(K), &
                                        G_STACK_REALS(J),G_STACK_IMAGS(J), &
                                        G_STACK_REALS(J),G_STACK_IMAGS(J))
         end select
         IF (G_ERR .GT. 0) exit
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   case (2*dot+1:) ! kronecker
      G_FIN = op - 2*dot - star + 11
      G_FUN = 6
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE + 1
      G_RHS = 2
!-----------------------------------------------------------------------------------------------------------------------------------
   case default
      write(*,*)'<ERROR> unknown operator ',op
      stop
   end select DO_OP
!-----------------------------------------------------------------------------------------------------------------------------------
contains
subroutine finish()
   do i = 1, n
      ll = l + (i-1)*(n+1)
      G_STACK_REALS(ll) = mat_flop(G_STACK_REALS(LL)+sr)
      G_STACK_IMAGS(ll) = mat_flop(G_STACK_IMAGS(LL)+si)
   enddo
end subroutine finish
end subroutine mat_stack2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_getlin() ! get a new input line

character(len=GG_LINELEN) :: mline
character(len=GG_LINELEN) :: shift_mline

integer                   :: istat
integer,parameter         :: retu(GG_MAX_NAME_LENGTH)=[26,30,18,29,GG_PAD(:)] ! quit
integer                   :: i, j, k
integer                   :: l
integer                   :: n
integer                   :: ios
!.......................................................................
   l = G_LINE_POINTER(1)
!.......................................................................
   11 continue

      G_BUF(:GG_LINELEN)= G_CHARSET(blank+1)      ! blank out buffer before reading into it
      n = GG_LINELEN+1

      ! get line of input and place it in line buffer
      if(size(G_PSEUDO_FILE).ne.0)then
         mline=get_pseudo_line()
      else
         mline(:)=' '
         read(G_RIO,'(a)',iostat=ios) mline       ! read input line from file
         if(ios.ne.0)then
             if(is_iostat_end(ios))then           ! hit end of file
                call mat_putid(G_LIN(l),retu) ! store RETU onto G_LIN(L) to simulate RETURN command
                l = l + 4
                goto 45
             else
                goto 15
             endif
         endif
      endif
      if(G_ECHO)write(*,'(*(g0))')'',trim(mline)
      shift_mline=adjustl(mline)
      if(shift_mline(1:2).eq.'!!')then            ! edit command line history
         mline='. '//mline(3:)
      endif

      if(G_RIO.eq.stdin)then
         call journal('t',mline)   ! reading from standard input, so copy to trail file
      else
         call journal('c',mline)   ! reading from an exec() command, so write as a comment
      endif
      call redo(mline,'.')         ! pass line to REDO(3f). This is a no-op except for storing the line into the input history
                                   ! (unless the input line is the "r" command)

      ! look for other lines to immediately process and then ignore
      shift_mline=adjustl(mline)
      if(shift_mline(1:1).eq.'#')then
         mline=''                                                      ! ignore lines with a # as first non-blank character
      elseif(shift_mline(1:1).eq.'!')then
         if(shift_mline.eq.'!')then
            call get_environment_variable('SHELL',shift_mline)         ! get command to execute
            call execute_command_line(shift_mline,cmdstat=istat)       ! call system shell
         else
            call execute_command_line(shift_mline(2:),cmdstat=istat)   ! call system shell
         endif
         mline=''
      endif

      call mat_str2buf(mline,G_BUF,GG_LINELEN)    ! convert input line to "Hollerith" buffer
!.......................................................................
   15 continue
      n = n-1
      if(n.lt.1)then
         n=1
      elseif (G_BUF(n) .eq. G_CHARSET(blank+1))then
         goto 15 ! trim off trailing spaces
      endif

      if (mod(G_LINECOUNT(4),2) .eq. 1) then
              call mat_buf2str(mline,G_BUF,n) ! convert ADE buffer to character
              call journal('s',mline) ! just to standard output
      endif
!.......................................................................
      do j = 1, n
         do k = 1, G_CHARSET_SIZE  ! make sure this letter is in set of MAT88 characters and get its MAT88 number
           if (G_BUF(j).eq.G_CHARSET(k) .or. G_BUF(j).eq.G_ALT_CHARSET(k)) goto 30
         enddo
         call journal('sc','Unknown character at column ',j) ! this is not a known character
         k = G_EOL+1
         if (k .gt. G_EOL) then
            l = G_LINE_POINTER(1)
            goto 11   ! Unknown character , K not changed. get new line
         endif
         if (k .eq. G_EOL) exit
         if (k .eq. -1) l = l-1
         if (k .le. 0) cycle
!
   30    continue
         k = k-1   ! K is index into ALF*, should be in range 0 to 51
         if (k.eq.slash .and. G_BUF(j+1).eq.G_BUF(j)) exit     ! if // rest is comment
         if (k.eq.dot .and. G_BUF(j+1).eq.G_BUF(j)) goto 11    ! if .. line continuation
         if (k.eq.bslash .and. n.eq.1) then                    ! if \ in column 1
            n = G_LINE_POINTER(6) - G_LINE_POINTER(1)
            do i = 1, n
               k = G_LIN(l+i-1)
               G_BUF(i) = G_CHARSET(k+1)
            enddo
            goto 15
         endif
         G_LIN(l) = k
         if (l.lt.1024) l = l+1
         if (l.eq.1024) call journal('sc','input buffer limit exceeded=',l)
      enddo
!.......................................................................
   45 CONTINUE      ! line is ready, reset line pointers
      G_LIN(l) = G_EOL
      G_LINE_POINTER(6) = l
      G_LINE_POINTER(4) = G_LINE_POINTER(1)
      G_LINE_POINTER(3) = 0
      G_LINE_POINTER(2) = 0
      G_LINECOUNT(1) = 0
      call mat_getch() ! load first character onto G_CHRA

contains

function get_pseudo_line() result(line)
character(len=GG_LINELEN) :: line
! reallocating all the time is inefficient
   line=G_PSEUDO_FILE(1)
   if(size(G_PSEUDO_FILE).gt.1)then
      G_PSEUDO_FILE=G_PSEUDO_FILE(2:)
   else
      G_PSEUDO_FILE=[character(len=GG_LINELEN) :: ]
   endif
end function get_pseudo_line

end subroutine mat_getlin
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_clause()
character(len=GG_LINELEN) :: mline
doubleprecision    :: e1,e2
integer            :: op
integer            :: r
integer,parameter  :: for(GG_MAX_NAME_LENGTH)  =[15,24,27,36,36,GG_PAD(2:)]
integer,parameter  :: while(GG_MAX_NAME_LENGTH)=[32,17,18,21,14,GG_PAD(2:)]
integer,parameter  :: iff(GG_MAX_NAME_LENGTH)  =[18,15,36,36,36,GG_PAD(2:)]
integer,parameter  :: else(GG_MAX_NAME_LENGTH) =[14,21,28,14,36,GG_PAD(2:)]
integer,parameter  :: ennd(GG_MAX_NAME_LENGTH) =[14,23,13,36,36,GG_PAD(2:)]
integer,parameter  :: do(GG_MAX_NAME_LENGTH)   =[13,24,36,36,36,GG_PAD(2:)]
integer,parameter  :: thenn(GG_MAX_NAME_LENGTH)=[29,17,14,23,36,GG_PAD(2:)]
integer            :: i
integer            :: j
integer            :: kount
integer            :: l
integer            :: l2
integer            :: lj
integer            :: m
integer            :: n

   r = -G_FIN-10
   G_FIN = 0
   if (G_DEBUG_LEVEL .eq. 1)then
      write(mline,'('' CLAUSE '',3I4)') G_PT,G_RSTK(G_PT),r
      call journal(mline)
   endif
   if (r.lt.1 .or. r.gt.6) goto 01
   goto (02,30,30,80,99,90),R
01 continue
   r = G_RSTK(G_PT)
   goto (99,99,05,40,45,99,99,99,99,99,99,99,15,55,99,99,99),R
   call journal('*mat_clause* -- internal error')
   goto 99
!.......................................................................
!     FOR
02 continue
   call mat_getsym()
   if (G_SYM .ne. name) call mat_err(34) ! improper for clause
   if (G_ERR .gt. 0) return
   G_PT = G_PT+2
   call mat_putid(G_IDS(1,G_PT),G_SYN)
   call mat_getsym()
   if (G_SYM .ne. equal) call mat_err(34) ! improper for clause
   if (G_ERR .gt. 0) return
   call mat_getsym()
   G_RSTK(G_PT) = 3
   ! *call* expr
   return
05 continue
   G_PSTK(G_PT-1) = 0
   G_PSTK(G_PT) = G_LINE_POINTER(4) - 1
   if (mat_eqid(G_SYN,DO)) G_SYM = semi
   if (G_SYM .eq. comma) G_SYM = semi
   if (G_SYM .ne. semi) call mat_err(34) ! improper for clause
   if (G_ERR .gt. 0) return
10 continue
   j = G_PSTK(G_PT-1)
   G_LINE_POINTER(4) = G_PSTK(G_PT)
   G_SYM = semi
   G_CHRA = blank
   j = j+1
   l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
   m = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
   n = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
   lj = l+(j-1)*m
   l2 = l + m*n
   if (m .ne. -3) goto 12
   lj = l+3
   l2 = lj
   G_STACK_REALS(lj) = G_STACK_REALS(l) + dble(j-1)*G_STACK_REALS(l+1)
   G_STACK_IMAGS(lj) = 0.0d0
   if (G_STACK_REALS(l+1).gt.0.0d0 .and. G_STACK_REALS(lj).gt.G_STACK_REALS(l+2)) goto 20
   if (G_STACK_REALS(l+1).lt.0.0d0 .and. G_STACK_REALS(lj).lt.G_STACK_REALS(l+2)) goto 20
   m = 1
   n = j
12 continue
   if (j .gt. n) goto 20
   if (G_BOTTOM_OF_SCRATCH_IN_USE+1 .ge. G_TOP_OF_SAVED) call mat_err(18) ! too many names
   if (G_ERR .gt. 0) return
   G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE+1
   G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) = l2
   G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = m
   G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1

   if(too_much_memory( l2+m - G_STACK_ID_LOC(G_TOP_OF_SAVED) ) )return

   call mat_wcopy(m,G_STACK_REALS(lj),G_STACK_IMAGS(lj),1,G_STACK_REALS(l2),G_STACK_IMAGS(l2),1)
   G_RHS = 0
   call mat_stack_put(G_IDS(1,G_PT))
   if (G_ERR .gt. 0) return
   G_PSTK(G_PT-1) = j
   G_PSTK(G_PT) = G_LINE_POINTER(4)
   G_RSTK(G_PT) = 13
!     *call* PARSE
   return
15 continue
   goto 10
20 continue
   G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
   G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
   G_RHS = 0
   call mat_stack_put(G_IDS(1,G_PT))
   if (G_ERR .gt. 0) return
   G_PT = G_PT-2
   goto 80
!.......................................................................
!
!     WHILE OR IF
!
30 continue
   G_PT = G_PT+1
   call mat_putid(G_IDS(1,G_PT),G_SYN)
   G_PSTK(G_PT) = G_LINE_POINTER(4)-1
35 continue
   G_LINE_POINTER(4) = G_PSTK(G_PT)
   G_CHRA = blank
   call mat_getsym()
   G_RSTK(G_PT) = 4
!     *call* EXPR
   return
40 continue
   if (G_SYM.ne.equal .and. G_SYM.NE.LESS .and. G_SYM.NE.GREAT)call mat_err(35)    ! improper WHILE or IF clause
   if (G_ERR .gt. 0) return
   op = G_SYM
   call mat_getsym()
   if (G_SYM.EQ.equal .or. G_SYM.EQ.great) op = op + G_SYM
   if (op .gt. great) call mat_getsym()
   G_PSTK(G_PT) = 256*G_PSTK(G_PT) + op
   G_RSTK(G_PT) = 5
!     *call* EXPR
   return
45 continue
   op = mod(G_PSTK(G_PT),256)
   G_PSTK(G_PT) = G_PSTK(G_PT)/256
   l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE-1)
   e1 = G_STACK_REALS(l)
   l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
   e2 = G_STACK_REALS(l)
   G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE - 2
   if (mat_eqid(G_SYN,do) .or. mat_eqid(G_SYN,thenn)) G_SYM = semi
   if (G_SYM .EQ. COMMA) G_SYM = SEMI
   if (G_SYM .NE. SEMI) call mat_err(35) ! improper WHILE or IF clause
   if (G_ERR .GT. 0) return
   if (op.eq.equal         .and. e1.eq.e2) goto 50
   if (op.eq.less          .and. e1.lt.e2) goto 50
   if (op.eq.great         .and. e1.gt.e2) goto 50
   if (op.eq.(less+equal)  .and. e1.le.e2) goto 50
   if (op.eq.(great+equal) .and. e1.ge.e2) goto 50
   if (op.eq.(less+great)  .and. e1.ne.e2) goto 50
   G_PT = G_PT-1
   goto 80
50 continue
   G_RSTK(G_PT) = 14
!     *call* PARSE
   return
55 continue
   IF (mat_eqid(G_IDS(1,G_PT),while)) goto 35
   G_PT = G_PT-1
   if (mat_eqid(G_SYN,else)) goto 80
   return
!.......................................................................
!     SEARCH FOR MATCHING END OR ELSE
80 continue
   kount = 0
   call mat_getsym()
82 continue
   if (G_SYM .eq. G_EOL) return
   if (G_SYM .ne. NAME) goto 83
   if (mat_eqid(G_SYN,ennd) .and. kount.eq.0) return
   if (mat_eqid(G_SYN,else) .and. kount.eq.0) return
   if (mat_eqid(G_SYN,ennd) .or. mat_eqid(G_SYN,else))kount = kount-1
   if (mat_eqid(G_SYN,for) .or. mat_eqid(G_SYN,while).or.mat_eqid(G_SYN,iff)) kount = kount+1
83 continue
   call mat_getsym()
   goto 82
!.......................................................................
!     EXIT FROM LOOP
90 continue
   if (G_DEBUG_LEVEL .eq. 1)then
      write(mline,'('' EXIT '',10I4)') (G_RSTK(i),i=1,G_PT)
      call journal(mline)
   endif

   if (G_RSTK(G_PT) .eq. 14) G_PT = G_PT-1
   if (G_PT .le. G_PTZ) return

   if (G_RSTK(G_PT) .eq. 14) G_PT = G_PT-1
   if (G_PT-1 .le. G_PTZ) return

   if (G_RSTK(G_PT) .eq. 13) G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE-1
   if (G_RSTK(G_PT) .eq. 13) G_PT = G_PT-2
   goto 80
!.......................................................................
!
99 continue
   call mat_err(22)    ! recursion difficulties
end subroutine mat_clause
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rat(x,len,maxd,a,b,d)

! ident_37="@(#)M_matrix::mat_rat(3fp): A/B = continued fraction approximation to X using  len  terms each less than MAXD"

integer         :: len,maxd
doubleprecision :: x,a,b,d(len)
doubleprecision :: s,t,z
integer         :: i
integer         :: ib
integer         :: k
   z = x
   k=0  ! preset to illegal value
   if(len.lt.1)then
      call journal('*mat_rat* internal error -- len<1')
      return
   endif
   do i = 1, len
      k = i
      d(k) = mat_round(z)
      z = z - d(k)
      if (dabs(z)*dble(maxd) .le. 1.0d0) exit
      z = 1.0d0/z
   enddo
   t = d(k)
   s = 1.0d0
   if (k .ge. 2) then
      do ib = 2, k
         i = k+1-ib
         z = t
         t = d(i)*t + s
         s = z
      enddo
   endif
   if (s .lt. 0.0d0) t = -t
   if (s .lt. 0.0d0) s = -s
   if (G_DEBUG_LEVEL .eq. 27)then
      write(G_OUTPUT_LUN,50) x,t,s,(d(i),i=1,k) ! debug 27
50    format(/1x,1pd23.15,0pf8.0,' /',f8.0,4x,6f5.0/(1x,45x,6f5.0))
   endif
   a = t
   b = s
end subroutine mat_rat
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_expr()
character(len=80) :: mline
integer           :: r
integer           :: sign
integer,parameter :: EYE(GG_MAX_NAME_LENGTH)=[14,34,14,36,GG_PAD(:)]
integer           :: kount
integer           :: ls
integer           :: op

   if (G_DEBUG_LEVEL .eq. 1) then
      write(mline,'('' EXPR '',2I4)') G_pt,G_RSTK(G_pt)
      call journal(mline)
   endif
   r = G_RSTK(G_pt)
!===================================================================================================================================
!        1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 16 18 19 20
   goto (01,01,01,01,01,05,25,99,99,01,01,99,99,99,99,99,99,01,01,01),R
!  what about drop-though???
!===================================================================================================================================
01 continue
   if (G_sym .eq. colon) call mat_putid(G_syn,eye)
   if (G_sym .eq. colon) G_sym = name
   kount = 1
02 continue
   sign = plus
   if (G_sym .eq. minus) sign = minus
   if (G_sym.eq.plus .or. G_sym.eq.minus) call mat_getsym()
   G_pt = G_pt+1
   if (G_pt .gt. G_PSIZE-1) call mat_err(26) ! too complicated (stack overflow)
   if (G_err .gt. 0) return
   G_PSTK(G_pt) = sign + 256*kount
   G_RSTK(G_pt) = 6
   ! *call* term
   return
!===================================================================================================================================
05 continue
   sign = mod(G_PSTK(G_pt),256)
   kount = G_PSTK(G_pt)/256
   G_pt = G_pt-1
   if (sign .eq. minus) call mat_stack1(minus)
   if (G_err .gt. 0) return
10 continue
   if (G_sym.eq.plus .or. G_sym.eq.minus) goto 20
   goto 50
20 continue
   if (G_RSTK(G_pt) .ne. 10) goto 21
!     blank is delimiter inside angle brackets
   ls = G_LINE_POINTER(3) - 2
   if (G_LIN(ls) .eq. blank) goto 50
21 continue
   op = G_sym
   call mat_getsym()
   G_pt = G_pt+1
   G_PSTK(G_pt) = op + 256*kount
   G_RSTK(G_pt) = 7
!     *call* term
   return
!===================================================================================================================================
25 continue
   op = mod(G_PSTK(G_pt),256)
   kount = G_PSTK(G_pt)/256
   G_pt = G_pt-1
   call mat_stack2(op)
   if (G_err .gt. 0) return
   goto 10
50 continue
   if (G_sym .ne. colon) goto 60
   call mat_getsym()
   kount = kount+1
   goto 02
60 continue
   if (kount .gt. 3) call mat_err(33)  ! too many colons
   if (G_err .gt. 0) return
   G_rhs = kount
   if (kount .gt. 1) call mat_stack2(colon)
   if (G_err .gt. 0) return
   return
99 continue
   call mat_err(22)     ! recursion difficulties
   if (G_err .gt. 0) return
end subroutine mat_expr
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_factor()
character(len=80) :: mline
integer           :: r
integer           :: id(gg_max_name_length)
integer           :: excnt
integer           :: i, j, k
integer           :: l
integer           :: ln
integer           :: ls
integer           :: n

   if (G_DEBUG_LEVEL .eq. 1) then
      write(mline,'('' factor '',3I4)') G_PT,G_RSTK(G_PT),G_SYM
      call journal(mline)
   endif

   r = G_RSTK(G_PT)
   !      1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19
   goto (99,99,99,99,99,99,99,01,01,25,45,65,99,99,99,55,75,32,37),r
01 continue
   if (G_SYM.eq.num .or. G_SYM.eq.quote .or.  G_SYM.EQ.less) goto 10

   if (G_SYM .eq. great)then
      !  MACROS STRING
         call mat_getsym()
         if (G_SYM .eq. less .and. G_CHRA.EQ.G_EOL) call mat_err(28)
         if (G_ERR .gt. 0) return
         G_PT = G_PT+1
         G_RSTK(G_PT) = 18
         ! *call* EXPR
         return
   endif

   excnt = 0
   if (G_SYM .eq. name)then
      ! FUNCTION OR MATRIX ELEMENT
      call mat_putid(id,G_SYN)
      call mat_getsym()
      if (G_SYM .eq. lparen) goto 42
      G_RHS = 0
      call mat_funs(ID)
      if (G_FIN .ne. 0) then
         call mat_err(25)
         return
      endif
      call mat_stack_get(id)
      if (G_ERR .gt. 0) return
      if (G_FIN .eq. 7) goto 50
      if (G_FIN .eq. 0) call mat_putid(G_IDS(1,G_PT+1),id)

      if (G_FIN .eq. 0) then
         call mat_err(4) ! undefined variable
         return
      endif
      goto 60
   endif
   id(1) = BLANK
   if (G_SYM .eq. lparen) goto 42
   call mat_err(2)
   if (G_ERR .gt. 0) return
!
!     PUT SOMETHING ON THE STACK
10 continue
   l = 1
   IF (G_BOTTOM_OF_SCRATCH_IN_USE .gt. 0) &
      & l = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) &
      & + G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) &
      & * G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
   if (G_BOTTOM_OF_SCRATCH_IN_USE+1 .ge. G_TOP_OF_SAVED) call mat_err(18)
   if (G_ERR .gt. 0) return

   G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE+1
   G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) = l
   if (G_SYM .eq. QUOTE) goto 15
   if (G_SYM .eq. LESS) goto 20
!
!  SINGLE NUMBER, GETSYM STORED IT IN G_STACK_IMAGS
   G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
   G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
   G_STACK_REALS(L) = G_STACK_IMAGS(G_BIGMEM)
   G_STACK_IMAGS(L) = 0.0D0
   call mat_getsym()
   goto 60
!
!  STRING
15 continue
   n = 0
   G_LINE_POINTER(4) = G_LINE_POINTER(3)
   call mat_getch()  ! get next character

16 continue
   if (G_CHRA .eq. QUOTE) goto 18
17 continue
   ln = l+n
   if (G_CHRA .eq. G_EOL) call mat_err(31)
   if (G_ERR .gt. 0) return
   G_STACK_REALS(LN) = dble(G_CHRA)
   G_STACK_IMAGS(LN) = 0.0d0
   n = n+1
   call mat_getch()  ! get next character
   goto 16

18 continue
   call mat_getch()  ! get next character
   if (G_CHRA .eq. QUOTE) goto 17
   if (n .le. 0) call mat_err(31)
   if (G_ERR .gt. 0) return
   G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 1
   G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = n
   call mat_getsym()
   goto 60
!
!  EXPLICIT MATRIX

20 continue
   G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
   G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0

21 continue
   G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE + 1
   G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) = &
      &   G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE-1) &
      & + G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE-1)&
      & * G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE-1)
   G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
   G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0
   call mat_getsym()

22 continue
   if (G_SYM.eq.semi .or. G_SYM.eq.great .or. G_SYM.eq.G_EOL) then
      if (G_SYM.eq.semi .and. G_CHRA.eq.g_eol) call mat_getsym()
      call mat_stack1(quote)
      if (G_ERR .gt. 0) return
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE - 1
      if (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) .eq. 0)  &
         & G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      if (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) .ne. G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE+1) &
         & .and. G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE+1) .gt. 0)call mat_err(6)
      if (G_ERR .gt. 0) return
      G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) &
         & + G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE+1)
      if (G_SYM .eq. g_eol) call mat_getlin()
      if (G_SYM .ne. great) goto 21
      call mat_stack1(quote)
      if (G_ERR .gt. 0) return
      call mat_getsym()
      goto 60
   endif
   if (G_SYM .eq. comma) call mat_getsym()
   G_PT = G_PT+1
   G_RSTK(G_PT) = 10
   ! *call* EXPR
   return
!==================================================================================================================================!
25 CONTINUE
   G_PT = G_PT-1
   G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE - 1
   if (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) .eq. 0)  &
      & G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE+1)

   if (G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) .ne. G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE+1))then
      call mat_err(5)
      return
   endif
   G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) =  &
      & G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE) + G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE+1)
   goto 22
!==================================================================================================================================!
32 CONTINUE
   G_PT = G_PT-1
   IF (G_SYM.NE.LESS .AND. G_SYM.NE.G_EOL) call mat_err(37) ! Improper MACROS
   IF (G_ERR .GT. 0) return
   IF (G_SYM .EQ. LESS) call mat_getsym()
   K = G_LINE_POINTER(6)
   G_LIN(K+1) = G_LINE_POINTER(1)
   G_LIN(K+2) = G_LINE_POINTER(2)
   G_LIN(K+3) = G_LINE_POINTER(6)
   G_LINE_POINTER(1) = K + 4
!     TRANSFER STACK TO INPUT LINE
   K = G_LINE_POINTER(1)
   L = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
   N = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
   DO J = 1, N
      LS = L + J-1
      G_LIN(K) = int(G_STACK_REALS(LS))
      if (G_LIN(k).lt.0 .or. G_LIN(k).ge.G_CHARSET_SIZE) call mat_err(37) ! Improper MACROS
      if (G_ERR .gt. 0) return
      if (k.lt.1024) k = k+1
      if (k.eq.1024)call journal('sc','Input buffer char limit exceeded=',K)
   enddo
   G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE-1
   G_LIN(k) = G_EOL
   G_LINE_POINTER(6) = k
   G_LINE_POINTER(4) = G_LINE_POINTER(1)
   G_LINE_POINTER(3) = 0
   G_LINE_POINTER(2) = 0
   G_LINECOUNT(1) = 0
   G_CHRA = blank
   call mat_getsym()
   G_PT = G_PT+1
   G_RSTK(G_PT) = 19
!     *call* EXPR
   return
!==================================================================================================================================!
37 CONTINUE
   G_PT = G_PT-1
   K = G_LINE_POINTER(1) - 4
   G_LINE_POINTER(1) = G_LIN(K+1)
   G_LINE_POINTER(4) = G_LIN(K+2)
   G_LINE_POINTER(6) = G_LIN(K+3)
   G_CHRA = BLANK
   call mat_getsym()
   goto 60
!==================================================================================================================================!
42 CONTINUE
   call mat_getsym()
   EXCNT = EXCNT+1
   G_PT = G_PT+1
   G_PSTK(G_PT) = EXCNT
   call mat_putid(G_IDS(1,G_PT),ID)
   G_RSTK(G_PT) = 11
!     *call* EXPR
   return
!==================================================================================================================================!
45 CONTINUE
   call mat_putid(ID,G_IDS(1,G_PT))
   EXCNT = G_PSTK(G_PT)
   G_PT = G_PT-1
   IF (G_SYM .EQ. COMMA) goto 42
   IF (G_SYM .NE. RPAREN) call mat_err(3)
   IF (G_ERR .GT. 0) return
   IF (G_SYM .EQ. RPAREN) call mat_getsym()
   IF (ID(1) .EQ. BLANK) goto 60
   G_RHS = EXCNT
   call MAT_STACK_GET(ID)
   IF (G_ERR .GT. 0) return
   IF (G_FIN .EQ. 0) call mat_funs(ID)
   IF (G_FIN .EQ. 0) call mat_err(4) ! undefined variable
   IF (G_ERR .GT. 0) return
   ! EVALUATE MATRIX FUNCTION
50 CONTINUE
   G_PT = G_PT+1
   G_RSTK(G_PT) = 16
   ! *call* MATFN
   return
!==================================================================================================================================!
55 CONTINUE
   G_PT = G_PT-1
   goto 60
!==================================================================================================================================!
!  CHECK FOR QUOTE (TRANSPOSE) AND ** (POWER)
60 CONTINUE
   IF (G_SYM .eq. QUOTE) then
      I = G_LINE_POINTER(3) - 2
      IF (G_LIN(I) .EQ. BLANK) goto 90
      call mat_stack1(QUOTE)
      IF (G_ERR .GT. 0) return
   call mat_getsym()
   endif
   IF (G_SYM.NE.STAR .OR. G_CHRA.NE.STAR) goto 90
   call mat_getsym()
   call mat_getsym()
   G_PT = G_PT+1
   G_RSTK(G_PT) = 12
!     *call* FACTOR
   goto 01
!==================================================================================================================================!
65 CONTINUE
   G_PT = G_PT-1
   call mat_stack2(DSTAR)
   IF (G_ERR .GT. 0) return
   IF (G_FUN .NE. 2) goto 90
   !  MATRIX POWER, USE EIGENVECTORS
   G_PT = G_PT+1
   G_RSTK(G_PT) = 17
   ! *call* MATFN
   return
!==================================================================================================================================!
75 CONTINUE
   G_PT = G_PT-1
90 CONTINUE
   return
!==================================================================================================================================!
99 CONTINUE
   call mat_err(22) ! recursion difficulties
   IF (G_ERR .GT. 0) return
END SUBROUTINE mat_factor
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_term()

character(len=GG_LINELEN) :: mline
integer            :: op
integer            :: ierr
integer            :: ilen

   if (G_DEBUG_LEVEL .eq. 1) then
      mline='TERM '
      call mat_appnum(real(G_pt),mline,ilen,ierr)
      call mat_appnum(real(G_RSTK(G_pt)),mline,ilen,ierr)
      call journal(mline)
   endif

   select case( G_RSTK(G_PT) )
   case(6,7)
      G_PT = G_PT+1
      G_RSTK(G_PT) = 8
      ! *call* factor
      return
   case(8)
      G_PT = G_PT-1
   case(9)
      op = G_PSTK(G_PT)
      G_PT = G_PT-1
      call mat_stack2(op)
      if (G_ERR .gt. 0)then
         return
      endif
      ! some binary ops done in matfns
      if (G_FUN .ne. 0) then
         G_PT = G_PT+1
         G_RSTK(G_PT) = 15
         ! *call* matfn
         return
      endif
   case(15)
      G_PT = G_PT-1
   case default
      call mat_err(22)
      return
   end select

   op = 0
   if (G_SYM .eq. dot) then
      op = dot
      call mat_getsym
   endif
   if (.not.(G_SYM.eq.star .or. G_SYM.eq.slash .or. G_SYM.eq.bslash)) then
      return
   endif

   op = op + G_SYM
   call mat_getsym()

   if (G_SYM .eq. dot)then
      op = op + G_SYM
      call mat_getsym()
   endif

   G_PT = G_PT+1
   G_PSTK(G_PT) = op
   G_RSTK(G_PT) = 9
   ! *call* factor

end subroutine mat_term
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_savlod(lsave,id,m,n,img,job,xreal,ximag)

! ident_38="@(#)M_matrix::mat_savlod(3fp): read next variable from a save file or write next variable to it"

integer,intent(in)                :: lsave                                    ! logical unit number
integer                           :: id(GG_MAX_NAME_LENGTH)                   ! name, format 4a1
integer                           :: m, n                                     ! dimensions
integer                           :: img                                      ! nonzero if ximag is nonzero.  returned on a load
integer                           :: job                                      ! 0 for save, = space available for load
doubleprecision                   :: xreal(*), ximag(*)                       ! real and optional imaginary parts
character(len=GG_MAX_NAME_LENGTH) :: cid
integer                           :: i,j,k,l
integer                           :: ios
character(len=256)                :: message
                                                                              ! system dependent formats
character(len=*),parameter        :: f101 ='(A32,3I4)'                        ! ID, MxN dimensions of ID, imaginary or real flag
character(len=*),parameter        :: f102 ='(4Z18)'                           ! format for data

      if (job .le. 0) then                                                    ! save
         call mat_buf2str(cid,id,GG_MAX_NAME_LENGTH)                          ! convert ID to a character string
         write(lsave,f101) cid,m,n,img
         do j = 1, n
            k = (j-1)*m+1
            l = j*m
            write(lsave,f102) (xreal(i),i=k,l)                                ! real
            if (img .ne. 0) write(lsave,f102) (ximag(i),i=k,l)                ! imaginary
         enddo

      else                                                                    ! load
         read(lsave,f101,iostat=ios,iomsg=message) cid,m,n,img
         if(ios.ne.0)then
            call journal(message)
            M=0
            N=0
         else
            call mat_str2buf(cid,id,GG_MAX_NAME_LENGTH)                       ! convert character string to an ID
            if (m*n .gt. job) then
               m=0
               n=0
            else
               do j = 1, n
                  k = (j-1)*m+1
                  l = j*m
                  read(lsave,f102,iostat=ios,iomsg=message) (xreal(i),i=k,l)  ! real
                  if(ios.ne.0)then
                     call journal(message)
                     M=0
                     N=0
                     exit
                  else if (img .ne. 0) then
                  read(lsave,f102,iostat=ios,iomsg=message) (ximag(i),i=k,l)  ! imaginary
                  if(ios.ne.0)then
                     M=0
                     N=0
                     exit
                  endif
                  endif
               enddo
            endif
         endif
      endif
end subroutine mat_savlod
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_urand(iy)
!>
!!##NAME
!!    mat_urand(3f) - [] uniform random number generator
!!
!!##SYNOPSIS
!!
!!   doubleprecision function mat_urand(iy)
!!
!!    integer,intent(inout) :: iy
!!
!!##DESCRIPTION
!!    mat_urand(3f) is a uniform random number generator based on theory and
!!    suggestions given in D.E. Knuth (1969), Vol 2. The integer IY should
!!    be initialized to an arbitrary integer prior to the first call to
!!    mat_urand(3f). The calling program should not alter the value of IY
!!    between subsequent calls to mat_urand(3f). Values of mat_urand(3f) will
!!    be returned in the interval (0,1).
!!
!!##OPTIONS
!!    IY seed for generating a sequence.
!!
!!##EXAMPLE
!!
integer              :: iy
integer,save         :: ia
integer,save         :: ic
integer,save         :: itwo=2
integer,save         :: m2=0
integer              :: m
integer,save         :: mic
doubleprecision      :: halfm
doubleprecision,save :: s
doubleprecision      :: datan
doubleprecision      :: dsqrt
!-----------------------------------------------------------------------
   if (m2 .eq. 0) then                                ! if first entry, compute machine integer word length
      m = 1
      INFINITE : do
         m2 = m
         m = itwo*m2
         if (m .le. m2) exit INFINITE
      enddo INFINITE
      halfm = m2
      ia = 8*int(halfm*datan(1.d0)/8.d0) + 5          ! compute multiplier and increment for linear congruential method
      ic = 2*int(halfm*(0.5d0-dsqrt(3.d0)/6.d0)) + 1
      mic = (m2 - ic) + m2
      s = 0.5d0/halfm                                 ! s is the scale factor for converting to floating point
   endif
   ! compute next random number
   iy = iy*ia

   if (iy .gt. mic) iy = (iy - m2) - m2     ! this statement is for computers which do not allow integer overflow on addition

   iy = iy + ic

   if (iy/2 .gt. m2) iy = (iy - m2) - m2    ! this statement is for computers where the word length for addition is greater than
                                            ! for multiplication
   if (iy .lt. 0) iy = (iy + m2) + m2       ! this statement is for computers where integer overflow affects the sign bit

   mat_urand = dble(iy)*s
end function mat_urand
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wnrm2(n,xr,xi,incx)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: s
integer         :: ix
integer         :: i
   !     norm2(x)
   s = 0.0d0
   if (n .gt. 0) then
      ix = 1
      do i = 1, n
         s = mat_pythag(s,xr(ix))
         s = mat_pythag(s,xi(ix))
         ix = ix + incx
      enddo
   endif
   mat_wnrm2 = s
   end function mat_wnrm2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wasum(n,xr,xi,incx)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: s
integer         :: ix
integer         :: i

   !     norm1(x)
   s = 0.0d0
   if (n .gt. 0) then
      ix = 1
      do i = 1, n
         s = mat_flop(s + dabs(xr(ix)) + dabs(xi(ix)))
         ix = ix + incx
      enddo
   endif
   mat_wasum = s
end function mat_wasum
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wdotui(n,xr,xi,incx,yr,yi,incy)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: yr(*)
doubleprecision :: yi(*)
integer         :: incy
doubleprecision :: s
integer         :: ix
integer         :: iy
integer         :: i
   s = 0.0d0
   if (n .gt. 0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do i = 1, n
         s = s + xr(ix)*yi(iy) + xi(ix)*yr(iy)
         if (s .ne. 0.0d0) s = mat_flop(s)
         ix = ix + incx
         iy = iy + incy
      enddo
   endif
   mat_wdotui = s
end function mat_wdotui
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wdotcr(n,xr,xi,incx,yr,yi,incy)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: yr(*)
doubleprecision :: yi(*)
integer         :: incy

doubleprecision :: s
integer         :: ix
integer         :: iy
integer         :: i

   s = 0.0d0
   if (n .gt. 0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do i = 1, n
         s = mat_flop(s + xr(ix)*yr(iy) + xi(ix)*yi(iy))
         ix = ix + incx
         iy = iy + incy
      enddo
   endif
   mat_wdotcr = s
end function mat_wdotcr
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wdotci(n,xr,xi,incx,yr,yi,incy)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: yr(*)
doubleprecision :: yi(*)
integer         :: incy

integer         :: ix
integer         :: iy
integer         :: i
doubleprecision :: s

   s = 0.0d0

   if (n .gt. 0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1

      do i = 1, n
         s = s + xr(ix)*yi(iy) - xi(ix)*yr(iy)
         if (s .ne. 0.0d0) s = mat_flop(s)
         ix = ix + incx
         iy = iy + incy
      enddo

   endif

   mat_wdotci = s
end function mat_wdotci
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
integer function mat_iwamax(n,xr,xi,incx)

! ident_39="@(#)M_matrix::mat_iwamax(3fp):index of norminf(x)"

integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: s
doubleprecision :: p
integer         :: i, k
integer         :: ix

   k = 0
   if (n .gt. 0) then
      k = 1
      s = 0.0d0
      ix = 1
      do i = 1, n
         p = dabs(xr(ix)) + dabs(xi(ix))
         if (p .gt. s) k = i
         if (p .gt. s) s = p
         ix = ix + incx
      enddo
   endif
   mat_iwamax = k
end function mat_iwamax
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_flop(x)
!>
!!##NAME
!!    mat_flop(3fp) - [M_matrix] count and possibly chop each floating point operation
!!
!!##SYNOPSIS
!!
!!
!!##DESCRIPTION
!!    Count and possibly chop each floating point operation.
!!
!!    this is a system-dependent function
!!##OPTIONS
!!
!!##NOTES
!!    FLP(1)  is flop counter
!!    FLP(2)  is number of places to be chopped
doubleprecision,intent(in) :: x
doubleprecision            :: mask(14),xx,mm
integer                    :: k
logical                    :: lx(2),lm(2)
equivalence (lx(1),xx),(lm(1),mm)
equivalence (mask(1),mas(1,1))
!>>>>>>>>>>>>>>>>>>
!*!GFORTRAN BUG in 8.3
!*!real,save                  :: mas(2,14)=reshape([ &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'fff0ffff',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'ff00ffff',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'f000ffff',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'0000ffff',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'0000fff0',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'0000ff00',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'0000f000',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'fff0ffff',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'ff00ffff',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'f000ffff',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'0000ffff',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'0000fff0',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'0000ff80',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0))],shape(mas))
integer :: i,j
logical,save :: setup=.false.
real,save                  :: mas(2,14)
character(len=8),save      :: setmas(2,14)=reshape([ &
   & 'ffffffff','fff0ffff', &
   & 'ffffffff','ff00ffff', &
   & 'ffffffff','f000ffff', &
   & 'ffffffff','0000ffff', &
   & 'ffffffff','0000fff0', &
   & 'ffffffff','0000ff00', &
   & 'ffffffff','0000f000', &
   & 'ffffffff','00000000', &
   & 'fff0ffff','00000000', &
   & 'ff00ffff','00000000', &
   & 'f000ffff','00000000', &
   & '0000ffff','00000000', &
   & '0000fff0','00000000', &
   & '0000ff80','00000000'],shape(mas))
   if(.not.setup)then
      do i=1,2
         do j=1,14
            read(setmas(i,j),'(z8)')mas(i,j)
         enddo
      enddo
      setup=.true.
   endif
!<<<<<<<<<<<<<<<<<<

   G_FLOP_COUNTER(1) = G_FLOP_COUNTER(1) + 1
   k = G_FLOP_COUNTER(2)

   select case(k)
   case(:0)
      mat_flop = x
   case(1:15)
      mat_flop = 0.0d0
   case default
      xx = x
      mm = mask(k)
      lx(1) = lx(1) .and. lm(1)
      lx(2) = lx(2) .and. lm(2)
      mat_flop = xx
   end select

end function mat_flop
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
logical function mat_eqid(x,y)

!     check for equality of two integer arrays

integer,intent(in) :: x(GG_MAX_NAME_LENGTH)
integer,intent(in) :: y(GG_MAX_NAME_LENGTH)

integer            :: i

   mat_eqid = .true.

   do i = 1, GG_MAX_NAME_LENGTH
      mat_eqid = mat_eqid .and. (x(i).eq.y(i))
   enddo

end function mat_eqid
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_round(x)
doubleprecision           :: x,y,z,e
doubleprecision,parameter :: h=1.0d9
   z = dabs(x)
   y = z + 1.0d0
   if (y .ne. z)then
      y = 0.0d0
      e = h
      do
         if (e .ge. z) exit
         e = 2.0d0*e
      enddo
      do
         if (e .le. h) exit
         if (e .le. z) y = y + e
         if (e .le. z) z = z - e
         e = e/2.0d0
      enddo
      z = int(z + 0.5d0)
      y = y + z
      if (x .lt. 0.0d0) y = -y
      mat_round = y
   else
      mat_round = x
   endif
end function mat_round
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
function getd2(varname) result(answer)
character(len=*),intent(in)  :: varname
doubleprecision, allocatable :: answer(:,:)
integer                      :: ierr
   call mat88_get(answer,varname,type=0,ierr=ierr)
end function getd2
!===================================================================================================================================
!x!function getc2(varname) result(answer)
!x!character(len=*),intent(in)          :: varname
!x!complex(kind=dp),allocatable         :: answer(:,:)
!x!doubleprecision(kind=dp),allocatable :: AR(:,:), AI(:,:)
!x!integer                              :: ierr
!x!   call mat88_get(AR,varname,type=0,ierr=ierr)
!x!   call mat88_get(AI,varname,type=1,ierr=ierr)
!x!   answer=cmplx(AR,AI)
!x!end function getc2
!===================================================================================================================================
function geti2(varname) result(answer)
character(len=*),intent(in) :: varname
integer, allocatable        :: answer(:,:)
   answer=int(getd2('varname'))
end function geti2
!===================================================================================================================================
function getr2(varname) result(answer)
character(len=*),intent(in) :: varname
real, allocatable           :: answer(:,:)
   answer=int(getd2('varname'))
end function getr2
!===================================================================================================================================
function gets2(varname) result(answer)
character(len=*),intent(in)  :: varname
integer, allocatable         :: A(:,:)
character(len=:),allocatable :: answer(:)
integer                      :: i
   A=int(geti2('varname'))
   allocate( character(len=size(A,dim=1)) :: answer(size(A,dim=2)) )
   do i=1,size(A,dim=2)
      call mat_buf2str(answer(i),A(i,:),len(answer))
   enddo
end function gets2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine mat88_get(A,varname,type,IERR)

! ident_40="@(#)M_matrix::mat88_get(3f) :: access MAT88 variable stack and get a variable by name and its data from the stack"

character(len=*),intent(in)              :: varname    ! the name of A.
integer,intent(in)                       :: type       ! type =  0  get REAL A from MAT88, type  = 1  get IMAGINARY A into MAT88,
integer,INTENT(OUT)                      :: ierr       ! return with nonzero IERR after MAT88 error message.
doubleprecision,allocatable,intent(out)  :: a(:,:)     ! A is an M by N matrix

integer                      :: id(GG_MAX_NAME_LENGTH)
integer                      :: i,j,k,l,m,n
   IERR=0
   G_ERR=0
   ! convert character name to mat88 character set
   call mat_str2buf(varname,id,len(varname))
   do j=1,GG_MAX_NAME_LENGTH
      do k = 1, G_CHARSET_SIZE                       ! make sure this letter is in set of MAT88 characters and get its MAT88 number
         if (id(j).eq.G_CHARSET(k) .or. id(j).eq.G_ALT_CHARSET(k)) then
            id(j)=k-1
         endif
      enddo
   enddo
   call mat_putid(G_STACK_IDS(1,G_TOP_OF_SAVED-1), ID)    ! copy ID to next blank entry in G_STACK_IDS for messages(?)

   do k=GG_MAX_NUMBER_OF_NAMES,1,-1                       ! start at bottom and search up through names till find the name
      if (mat_eqid(G_STACK_IDS(1,k), id))exit             ! if found name exit loop
   enddo

   ! if matched the name inserted above did not find it.
   if ( (k .ge. GG_MAX_NUMBER_OF_NAMES-1 .and. G_RHS .gt. 0) .or. (k .eq. G_TOP_OF_SAVED-1) ) then
      call journal('sc','<ERROR>MAT88_GET: unknown variable name',varname)
      IERR=4
      if(allocated(a))deallocate(a)
      allocate(a(0,0))
   else
      call mat_stack_get(id)

      if(G_ERR.ge.0)then
         if(G_ERR.eq.0)G_ERR=4
         call mat_err(G_ERR)
         IERR=G_ERR
         if(allocated(a))deallocate(a)
         allocate(a(0,0))
      else
         M=G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
         N=G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
         if(allocated(a))deallocate(a)
         allocate(a(m,n))
         l=G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
         do i=1,m
            do j=1,n
               if(type.eq.0)then
                  a(i,j)=G_STACK_REALS(l)       ! type =  0  GET REAL A FROM MAT88,
               else
                  a(i,j)=G_STACK_IMAGS(l)       ! type =  1  GET IMAGINARY A FROM MAT88,
               endif
               l=l+1
            enddo
         enddo
      endif

      if(G_DEBUG_LEVEL.ne.0)then
         call printit()
      endif

   endif

contains

subroutine printit()
character(len=GG_MAX_NAME_LENGTH) :: name
integer          :: l
   write(*,*)repeat('=',80)
   write(*,*)'*MAT88_GET'
   write(*,*)'   varname=',varname
   write(*,*)'   ID=',ID(:)
   write(*,*)'   G_BOTTOM_OF_SCRATCH_IN_USE=',G_BOTTOM_OF_SCRATCH_IN_USE
   write(*,*)'   G_ERR=',G_ERR
   if(G_ERR.lt.0.and.G_BOTTOM_OF_SCRATCH_IN_USE.gt.0)then
      write(*,*)'   G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)=',G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
      write(*,*)'   G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)=',G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE),M
      write(*,*)'   G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)=',G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE),N
      l=G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
      write(*,'(3x,*(g0.4,1x))')'REAL VALUES=     ',G_STACK_REALS(L:L+(M*N-1))
      write(*,'(3x,*(g0.4,1x))')'IMAGINARY VALUES=',G_STACK_IMAGS(L:L+(M*N-1))
   endif
   write(*,*)repeat('=',80)
end subroutine printit

end subroutine mat88_get
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE mat88_put(A,CID,JOB,IERR) !JSU

! ident_41="@(#)M_matrix:: mat88_put(3f): put a variable name and its data onto MAT88 stack"

character(len=*),intent(in) :: cid                    ! the name of A.
doubleprecision,intent(in)  :: a(:,:)                 ! A is an M by N matrix, stored in an array with leading dimension size_of_a.
integer,intent(in)          :: job                    !     JOB =  1  put real A from MAT88,
                                                      !         = 11  put imag part of A into MAT88.
integer,intent(out)         :: ierr                   ! return with nonzero ierr after MAT88 error message.
integer                     :: id(GG_MAX_NAME_LENGTH) ! ID = name, in numeric format
integer                     :: i,j,k,location
integer                     :: m,n,mn                 ! m, n = dimensions
integer                     :: size_of_a
integer                     :: space_left             ! JOB = SPACE AVAILABLE FOR LOAD
integer                     :: img
   ierr=0
   ! convert character name to mat88 character set
   call mat_str2buf(cid,id,len(cid))
   do j=1,GG_MAX_NAME_LENGTH
      do k = 1, G_CHARSET_SIZE  ! make sure this letter is in set of MAT88 characters
         if (id(j).eq.G_CHARSET(k) .or. id(j).eq.G_ALT_CHARSET(k)) then
            id(j)=k-1
         endif
      enddo
   enddo
      !L = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
      !M = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
      !N = G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
      !   JOB = G_STACK_ID_LOC(G_TOP_OF_SAVED) - L

   write(*,*)'GOT HERE A:G_BOTTOM_OF_SCRATCH_IN_USE:',G_BOTTOM_OF_SCRATCH_IN_USE
   if(G_BOTTOM_OF_SCRATCH_IN_USE.ne.0)then
      location = G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE) ! location of bottom of used scratch space
   else
     write(*,*)'GOT HERE B:',G_STACK_ID_LOC
     location=1
   endif

   m=size(a,dim=1)
   n=size(a,dim=2)
   space_left = G_STACK_ID_LOC(G_TOP_OF_SAVED) - location
   if (m*n .GT. space_left ) then
      m = 0
      n = 0
      call journal('sc','<ERROR>*mat88_put* insufficient space to save data to MAT88')
      return
   elseif(m*n.eq.0)then
      return
   else
      size_of_a=size(a)
   endif

   !if(all(imag(a).eq.0)then
   !      img=0  ! all imaginary values are 0
   !else
   !      img=1
   !endif

   G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)=m ! copy to global values
   G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)=n

   location = G_STACK_ID_LOC(G_TOP_OF_SAVED) +1
   write(*,*)'GOT HERE A:LOCATION:',location

   do i = 1, m
      do j = 1, n
         G_STACK_REALS(location)=a(i,j)              ! real
         if (img .ne. 0) then
            G_STACK_IMAGS(location)=a(i,j)           ! imaginary
         endif
         location=location+1
      enddo
   enddo

   mn = G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)*G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
   if (mn .ne. 0)then
      !!------
      !!if (img .eq. 0) call mat_rset(mn,0.0d0,G_STACK_IMAGS(location),1)
      if (img .eq. 0) G_STACK_IMAGS(location:location+mn-1:1)=0.0d0 ! reset array to zero
      !!------
      do i = 1, GG_MAX_NAME_LENGTH
         j = 0
         do
            j = j+1
            if (id(i).ne.G_CHARSET(J) .and. j.le.blank) cycle
            exit
         enddo
         id(i) = j-1
      enddo
      G_SYM = semi
      G_RHS = 0
      call mat_stack_put(ID)
      G_BOTTOM_OF_SCRATCH_IN_USE = G_BOTTOM_OF_SCRATCH_IN_USE + 1
   endif

   G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE) = 0

contains

subroutine printit()
character(len=GG_MAX_NAME_LENGTH) :: name
integer          :: location
   write(*,*)repeat('=',80)
   write(*,*)'CID=',CID
   write(*,*)'ID=',ID(:)
   write(*,*)'G_BOTTOM_OF_SCRATCH_IN_USE=',G_BOTTOM_OF_SCRATCH_IN_USE
   write(*,*)'G_ERR=',G_ERR
   if(G_ERR.lt.1.and.G_BOTTOM_OF_SCRATCH_IN_USE.gt.0)then
      write(*,*)'G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)=',G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
      write(*,*)'G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)=',G_STACK_ROWS(G_BOTTOM_OF_SCRATCH_IN_USE)
      write(*,*)'G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)=',G_STACK_COLS(G_BOTTOM_OF_SCRATCH_IN_USE)
      location=G_STACK_ID_LOC(G_BOTTOM_OF_SCRATCH_IN_USE)
      write(*,'(*(g0.4,1x))')'REAL VALUES=     ',G_STACK_REALS(location:location+(M*N-1))
      write(*,'(*(g0.4,1x))')'IMAGINARY VALUES=',G_STACK_IMAGS(location:location+(M*N-1))
   endif
end subroutine printit

end subroutine mat88_put
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function too_much_memory(expression)
integer,intent(in) :: expression
logical            :: too_much_memory

! ident_42="@(#)too much memory required"

   G_ERR=expression
   if(G_ERR.gt.0)then
      call mat_err(17)
      too_much_memory=.true.
   else
      too_much_memory=.false.
   endif

end function too_much_memory
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function system_getenv(name,default) result(value)

! ident_43="@(#)M_system::system_getenv(3f): call get_environment_variable as a function with a default value(3f)"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: default
integer                              :: howbig
integer                              :: stat
character(len=:),allocatable         :: value

   if(NAME.ne.'')then
      call get_environment_variable(name, length=howbig, status=stat, trim_name=.true.)  ! get length required to hold value
      if(howbig.ne.0)then
         select case (stat)
         case (1)     ! print *, NAME, " is not defined in the environment. Strange..."
            value=''
         case (2)     ! print *, "This processor doesn't support environment variables. Boooh!"
            value=''
         case default ! make string to hold value of sufficient size and get value
            if(allocated(value))deallocate(value)
            allocate(character(len=max(howbig,1)) :: VALUE)
            call get_environment_variable(name,value,status=stat,trim_name=.true.)
            if(stat.ne.0)VALUE=''
         end select
      else
         value=''
      endif
   else
      value=''
   endif
   if(value.eq.''.and.present(default))value=default

end function system_getenv
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wpofa(ar,ai,lda,n,info)
integer          :: lda
doubleprecision  :: ar(lda,*)
doubleprecision  :: ai(lda,*)
integer          :: n
integer          :: info

doubleprecision  :: s
doubleprecision  :: TR
doubleprecision  :: TI
integer          :: j
integer          :: jm1
integer          :: k

   do j = 1, n
      info = j
      s = 0.0d0
      jm1 = j-1
      if (jm1 .ge. 1) then
         do k = 1, jm1
           tr=ar(k,j)-mat_wdotcr(k-1,ar(1,k),ai(1,k),1,ar(1,j),ai(1,j),1)
           ti=ai(k,j)-mat_wdotci(k-1,ar(1,k),ai(1,k),1,ar(1,j),ai(1,j),1)
           call mat_wdiv(tr,ti,ar(k,k),ai(k,k),tr,ti)
           ar(k,j) = tr
           ai(k,j) = ti
           s = s + tr*tr + ti*ti
         enddo
      endif
      s = ar(j,j) - s
      if (s.le.0.0d0 .or. ai(j,j).ne.0.0d0) goto 40
      ar(j,j) = dsqrt(s)
   enddo
   info = 0
40 continue
end subroutine mat_wpofa
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_watan(xr,xi,yr,yi)

! ident_44="@(#)M_matrix::mat_watan(3fp): y = atan(x) = (i/2)*log((i+x)/(i-x))"

doubleprecision :: xr
doubleprecision :: xi
doubleprecision :: yr
doubleprecision :: yi
doubleprecision :: tr
doubleprecision :: ti

   if (xi .eq. 0.0d0) then
      yr = datan2(xr,1.0d0)
      yi = 0.0d0
   elseif (xr.ne.0.0d0 .or. dabs(xi).ne.1.0d0) then
      call mat_wdiv(xr,1.0d0+xi,-xr,1.0d0-xi,tr,ti)
      call mat_wlog(tr,ti,tr,ti)
      yr = -(ti/2.0d0)
      yi = tr/2.0d0
   else
      call mat_err(32) ! Singularity of LOG or ATAN
   endif

end subroutine mat_watan
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rrotg(da,db,c,s)

! ident_45="@(#)M_matrix::mat_rrotg(3fp): construct Givens plane rotation."

doubleprecision :: da
doubleprecision :: db
doubleprecision :: c
doubleprecision :: s

doubleprecision :: rho
doubleprecision :: r
doubleprecision :: z

   rho = db
   if ( dabs(da) .gt. dabs(db) ) rho = da
   c = 1.0d0
   s = 0.0d0
   z = 1.0d0
   r = mat_flop(dsign(mat_pythag(da,db),rho))
   if (r .ne. 0.0d0) c = mat_flop(da/r)
   if (r .ne. 0.0d0) s = mat_flop(db/r)
   if ( dabs(da) .gt. dabs(db) ) z = s
   if (dabs(db) .ge. dabs(da) .and. c .ne. 0.0d0)z = mat_flop(1.0d0/c)
   da = r
   db = z
end subroutine mat_rrotg
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wsign(xr,xi,yr,yi,zr,zi)

! ident_46="@(#)M_matrix::mat_wsign(3fp): if y .ne. 0, z = x*y/abs(y)"

doubleprecision :: xr
doubleprecision :: xi
doubleprecision :: yr
doubleprecision :: yi
doubleprecision :: zr
doubleprecision :: zi
doubleprecision :: t
   t = mat_pythag(yr,yi)
   zr = xr
   zi = xi
   if (t .ne. 0.0d0) call mat_wmul(yr/t,yi/t,zr,zi,zr,zi)
end subroutine mat_wsign
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_help_text()
G_HELP_TEXT=[ CHARACTER(LEN=128) :: &
'================================================================================',&
'MAT88 USERS'' GUIDE                                                             ',&
'                                                                                ',&
'                            May, 1981                                           ',&
'                            Dec, 2018                                           ',&
'                            Apr, 2018                                           ',&
'                                                                                ',&
'                 Based on the MATLAB package by                                 ',&
'                                                                                ',&
'                           Cleve Moler                                          ',&
'                 Department of Computer Science                                 ',&
'                    University of New Mexico                                    ',&
'                                                                                ',&
'   <ABSTRACT> MAT88 is an interactive computer procedure that serves            ',&
'   as a convenient "laboratory" for computations involving matrices. It         ',&
'   provides easy access to matrix software developed by the LINPACK and         ',&
'   EISPACK projects.                                                            ',&
'                                                                                ',&
'                            CONTENTS                                            ',&
'                                                                                ',&
'          -  Elementary operations                                              ',&
'          -  MAT88 functions                                                    ',&
'          -  Rows, columns and submatrices                                      ',&
'          -  "for", "while" and "if"                                            ',&
'          -  Commands, text, files and macros                                   ',&
'          -  The numerical algorithms                                           ',&
'          -  "flop" and "chop"                                                  ',&
'          -  Census example                                                     ',&
'          -  Partial differential equation example                              ',&
'          -  Eigenvalue sensitivity example                                     ',&
'          -  Communicating with other programs                                  ',&
'          -  Appendix  (The HELP file)                                          ',&
'                                                                                ',&
'   The capabilities range from standard tasks such as solving simultaneous      ',&
'   linear equations and inverting matrices, through symmetric and               ',&
'   nonsymmetric eigenvalue problems, to fairly sophisticated matrix             ',&
'   tools such as the singular value decomposition.                              ',&
'                                                                                ',&
'   MAT88 is well suited for classroom use.  It should be useful in              ',&
'   introductory courses in applied linear algebra, as well as more              ',&
'   advanced courses in numerical analysis, matrix theory, statistics and        ',&
'   applications of matrices to other disciplines. In nonacademic settings,      ',&
'   MAT88 can serve as a "desk calculator" for the quick solution of             ',&
'   small problems involving matrices.                                           ',&
'                                                                                ',&
'   The program is written in Fortran and is designed to be readily              ',&
'   installed under any operating system which permits interactive               ',&
'   execution of Fortran programs. The resources required are fairly             ',&
'   modest. There are less than 7000 lines of Fortran source code,               ',&
'   including the LINPACK and EISPACK subroutines used.                          ',&
'                                                                                ',&
'   The size of the matrices that can be handled in MAT88 depends upon           ',&
'   the amount of storage that is set aside when the system is compiled          ',&
'   on a particular machine.  Since most of the algorithms used access           ',&
'   memory in a sequential fashion, a large amount of allocated storage          ',&
'   causes little or no difficulties even when virtual memory is used.           ',&
'                                                                                ',&
'   In some ways, MAT88 resembles SPEAKEASY [4] and, to a lesser extent,         ',&
'   APL. All are interactive terminal languages that ordinarily accept           ',&
'   single-line commands or statements, process them immediately, and print      ',&
'   the results. All have arrays or matrices as principal data types. But        ',&
'   for MAT88, the matrix is the only data type (although scalars, vectors       ',&
'   and text are special cases), the underlying system is portable and           ',&
'   requires fewer resources, and the supporting subroutines are more            ',&
'   powerful and in some cases, have better numerical properties.                ',&
'                                                                                ',&
'   Together, LINPACK and EISPACK provide for powerful matrix                    ',&
'   computation. EISPACK is a package of over 70 Fortran subroutines for         ',&
'   various matrix eigenvalue computations that are based for the most           ',&
'   part on Algol procedures published by Wilkinson, Reinsch and their           ',&
'   colleagues [5]. LINPACK is a package of 40 Fortran subroutines (in           ',&
'   each of four data types) for solving and analyzing simultaneous linear       ',&
'   equations and related matrix problems. Since MAT88 is not primarily          ',&
'   concerned with either execution time efficiency or storage savings,          ',&
'   it ignores most of the special matrix properties that LINPACK and            ',&
'   EISPACK subroutines use to advantage. Consequently, only 8 subroutines       ',&
'   from LINPACK and 5 from EISPACK are actually involved.                       ',&
'                                                                                ',&
'   In more advanced applications, MAT88 can be used in conjunction with         ',&
'   other programs in several ways. It is possible to define new MAT88           ',&
'   functions and add them to the system. With most operating systems,           ',&
'   it is possible to use the local file system to pass matrices between         ',&
'   MAT88 and other programs. MAT88 command and statement input can be           ',&
'   obtained from a local file instead of from the terminal. The most            ',&
'   power and flexibility is obtained by using MAT88 as a subroutine             ',&
'   which is called by other programs.                                           ',&
'                                                                                ',&
'   This document first gives an overview of MAT88 from the user''s              ',&
'   point of view. Several extended examples involving data fitting,             ',&
'   partial differential equations, eigenvalue sensitivity and other             ',&
'   topics are included.  The system was designed and programmed using           ',&
'   techniques described by Wirth [6], implemented in nonrecursive,              ',&
'   portable Fortran. There is a brief discussion of some of the matrix          ',&
'   algorithms and of their numerical properties. A final section describes      ',&
'   how MAT88 can be used with other programs. The appendix includes the         ',&
'   HELP documentation available on-line.                                        ',&
'                                                                                ',&
'================================================================================',&
'ELEMENTARY OPERATIONS                                                           ',&
'                                                                                ',&
'   MAT88 works with essentially only one kind of object, a rectangular          ',&
'   matrix with complex elements. If the imaginary parts of the elements         ',&
'   are all zero, they are not printed, but they still occupy storage. In        ',&
'   some situations, special meaning is attached to 1 by 1 matrices,             ',&
'   that is scalars, and to 1 by n and m by 1 matrices, that is row and          ',&
'   column vectors.                                                              ',&
'                                                                                ',&
'   Matrices can be introduced into MAT88 in four different                      ',&
'   ways:                                                                        ',&
'                                                                                ',&
'           --  Explicit list of elements,                                       ',&
'           --  Use of "for" and "while" statements,                             ',&
'           --  Read from an external file,                                      ',&
'           --  Execute an external Fortran program.                             ',&
'                                                                                ',&
'   The explicit list is surrounded by angle brackets, ''<'' and ''>'' or        ',&
'   braces ''['' and '']'', and uses the semicolon '';'' to indicate the ends    ',&
'   of the rows. For example, the input line                                     ',&
'                                                                                ',&
'      A = <1 2 3; 4 5 6; 7 8 9>                                                 ',&
'                                                                                ',&
'   will result in the output                                                    ',&
'                                                                                ',&
'      A     =                                                                   ',&
'                                                                                ',&
'          1.    2.   3.                                                         ',&
'          4.    5.   6.                                                         ',&
'          7.    8.   9.                                                         ',&
'                                                                                ',&
'   The matrix A will be saved for later use. The individual elements            ',&
'   are separated by commas or blanks and can be any MAT88 expressions,          ',&
'   for example                                                                  ',&
'                                                                                ',&
'      x = < -1.3, 4/5, 4*atan(1) >                                              ',&
'                                                                                ',&
'   results in                                                                   ',&
'                                                                                ',&
'      x     =                                                                   ',&
'                                                                                ',&
'        -1.3000   0.8000   3.1416                                               ',&
'                                                                                ',&
'   The elementary functions available include sqrt, log, exp, sin, cos,         ',&
'   atan, abs, round, real, imag, and conjg.                                     ',&
'                                                                                ',&
'   Large matrices can be spread across several input lines, with the            ',&
'   carriage returns replacing the semicolons. The above matrix could            ',&
'   also have been produced by                                                   ',&
'                                                                                ',&
'      A = < 1 2 3                                                               ',&
'            4 5 6                                                               ',&
'            7 8 9 >                                                             ',&
'                                                                                ',&
'   Matrices can be input from the local file system. Say a file named           ',&
'   ''xyz'' contains five lines of text,                                         ',&
'                                                                                ',&
'      A = <                                                                     ',&
'      1 2 3                                                                     ',&
'      4 5 6                                                                     ',&
'      7 8 9                                                                     ',&
'      >;                                                                        ',&
'                                                                                ',&
'   then the MAT88 statement exec(''xyz'') reads the matrix and assigns it       ',&
'   to A .                                                                       ',&
'                                                                                ',&
'   The "for" statement allows the generation of matrices whose elements         ',&
'   are given by simple formulas. Our example matrix A could also have           ',&
'   been produced by                                                             ',&
'                                                                                ',&
'      for i = 1:3, for j = 1:3, A(i,j) = 3*(i-1)+j;                             ',&
'                                                                                ',&
'   The semicolon at the end of the line suppresses the printing, which          ',&
'   in this case would have been nine versions of A with changing elements.      ',&
'                                                                                ',&
'   Several statements may be given on a line, separated by semicolons           ',&
'   or commas.                                                                   ',&
'                                                                                ',&
'   Two consecutive periods anywhere on a line indicate continuation. The        ',&
'   periods and any following characters are deleted, then another line          ',&
'   is input and concatenated onto the previous line.                            ',&
'                                                                                ',&
'   Two consecutive slashes anywhere on a line cause the remainder of            ',&
'   the line to be ignored. This is useful for inserting comments.               ',&
'                                                                                ',&
'   Names of variables are formed by a letter, followed by any number of         ',&
'   letters and digits, but only the first 32 characters are remembered.         ',&
'                                                                                ',&
'   The special character prime ('') is used to denote the transpose of          ',&
'   a matrix, so                                                                 ',&
'                                                                                ',&
'      X = X''                                                                   ',&
'                                                                                ',&
'   changes the row vector above into the column vector                          ',&
'                                                                                ',&
'      X     =                                                                   ',&
'                                                                                ',&
'        -1.3000                                                                 ',&
'         0.8000                                                                 ',&
'         3.1416                                                                 ',&
'                                                                                ',&
'   Individual matrix elements may be referenced by enclosing their              ',&
'   subscripts in parentheses. When any element is changed, the entire           ',&
'   matrix is reprinted. For example, using the above matrix,                    ',&
'                                                                                ',&
'      B(3,3) = B(1,3) + B(3,1)                                                  ',&
'                                                                                ',&
'   results in                                                                   ',&
'                                                                                ',&
'      B     =                                                                   ',&
'                                                                                ',&
'          1.    2.    3.                                                        ',&
'          4.    5.    6.                                                        ',&
'          7.    8.   10.                                                        ',&
'                                                                                ',&
'   Addition, subtraction and multiplication of matrices are denoted by          ',&
'   +, -, and * . The operations are performed whenever the matrices             ',&
'   have the proper dimensions. For example, with the above A and x,             ',&
'   the expressions A + X and X*A are incorrect because A is 3 by 3 and          ',&
'   X is now 3 by 1. However,                                                    ',&
'                                                                                ',&
'      B = A*B                                                                   ',&
'                                                                                ',&
'   is correct and results in the output                                         ',&
'                                                                                ',&
'      B     =                                                                   ',&
'                                                                                ',&
'         9.7248                                                                 ',&
'        17.6496                                                                 ',&
'        28.7159                                                                 ',&
'                                                                                ',&
'   Note that both upper and lower case letters are allowed for input            ',&
'   (on those systems which have both).                                          ',&
'                                                                                ',&
'   There are two "matrix division" symbols in MAT88, \ and / .                  ',&
'   (If your terminal does not have a backslash, use $ instead, or               ',&
'   see "char".) If A and B are matrices, then A\B and B/A correspond            ',&
'   formally to left and right multiplication of B by the inverse of             ',&
'   A, that is inv(A)*B and B*inv(A), but the result is obtained                 ',&
'   directly without the computation of the inverse. In the scalar               ',&
'   case, 3\1 and 1/3 have the same value, namely one-third. In                  ',&
'   general, A\B denotes the solution X to the equation A*X = B and              ',&
'   B/A denotes the solution to X*A = B.                                         ',&
'                                                                                ',&
'   Left division, A\B, is defined whenever B has as many rows as A. If A        ',&
'   is square, it is factored using Gaussian elimination. The factors are        ',&
'   used to solve the equations A*X(:,j) = B(:,j) where B(:,j) denotes the       ',&
'   j-th column of B. The result is a matrix X with the same dimensions          ',&
'   as B. If A is nearly singular (according to the LINPACK condition            ',&
'   estimator, RCOND(3f)), a warning message is printed. If A is not             ',&
'   square, it is factored using Householder orthogonalization with column       ',&
'   pivoting. The factors are used to solve the under- or overdetermined         ',&
'   equations in a least squares sense. The result is an M by N matrix X         ',&
'   where M is the number of columns of A and N is the number of columns         ',&
'   of B . Each column of X has at most K nonzero components, where K is         ',&
'   the effective rank of A .                                                    ',&
'                                                                                ',&
'   Right division, B/A, can be defined in terms of left division by B/A =       ',&
'   (A''\B'')''.                                                                 ',&
'                                                                                ',&
'   For example, since our vector b was computed as A*X, the statement           ',&
'                                                                                ',&
'      Y = A\B                                                                   ',&
'                                                                                ',&
'   results in                                                                   ',&
'                                                                                ',&
'      Y     =                                                                   ',&
'                                                                                ',&
'        -1.3000                                                                 ',&
'         0.8000                                                                 ',&
'         3.1416                                                                 ',&
'                                                                                ',&
'   Of course, Y is not exactly equal to X because of the roundoff errors        ',&
'   involved in both A*X and A\B , but we are not printing enough digits         ',&
'   to see the difference. The result of the statement                           ',&
'                                                                                ',&
'      E = X - Y                                                                 ',&
'                                                                                ',&
'   depends upon the particular computer being used. In one case it              ',&
'   produces                                                                     ',&
'                                                                                ',&
'      E     =                                                                   ',&
'                                                                                ',&
'         1.0e-15 *                                                              ',&
'                                                                                ',&
'           .3053                                                                ',&
'          -.2498                                                                ',&
'           .0000                                                                ',&
'                                                                                ',&
'   The quantity 1.0e-15 is a scale factor which multiplies all the              ',&
'   components which follow. Thus our vectors X and Y actually                   ',&
'   agree to about 15 decimal places on this computer.                           ',&
'                                                                                ',&
'   It is also possible to obtain element-by-element                             ',&
'   multiplicative operations. If A and B have the same dimensions,              ',&
'   then A .* B denotes the matrix whose elements are simply the                 ',&
'   products of the individual elements of A and B . The expressions             ',&
'   A ./ B and A .\ B give the quotients of the individual elements.             ',&
'                                                                                ',&
'   There are several possible output formats. The statement                     ',&
'                                                                                ',&
'      long, X                                                                   ',&
'                                                                                ',&
'   results in                                                                   ',&
'                                                                                ',&
'      X     =                                                                   ',&
'                                                                                ',&
'         -1.300000000000000                                                     ',&
'           .800000000000000                                                     ',&
'          3.141592653589793                                                     ',&
'                                                                                ',&
'   The statement                                                                ',&
'                                                                                ',&
'      short                                                                     ',&
'                                                                                ',&
'   restores the original format.                                                ',&
'                                                                                ',&
'   The expression A**p means A to the p-th power. It is                         ',&
'   defined if A is a square matrix and p is a scalar. If p is an                ',&
'   integer greater than one, the power is computed by repeated                  ',&
'   multiplication. For other values of p the calculation involves               ',&
'   the eigenvalues and eigenvectors of A.                                       ',&
'                                                                                ',&
'   Previously defined matrices and matrix expressions can be                    ',&
'   used inside brackets to generate larger matrices, for example                ',&
'                                                                                ',&
'      C = <A, B; <4 2 0>*X, X''>                                                ',&
'                                                                                ',&
'   results in                                                                   ',&
'                                                                                ',&
'      C     =                                                                   ',&
'                                                                                ',&
'         1.0000   2.0000   3.0000   9.7248                                      ',&
'         4.0000   5.0000   6.0000  17.6496                                      ',&
'         7.0000   8.0000  10.0000  28.7159                                      ',&
'        -3.6000  -1.3000   0.8000   3.1416                                      ',&
'                                                                                ',&
'   There are four predefined variables, "eps", "flop", "rand" and               ',&
'   "eye". The variable "eps" is used as a tolerance is determining such         ',&
'   things as near singularity and rank. Its initial value is the distance       ',&
'   from 1.0 to the next largest floating point number on the particular         ',&
'   computer being used. The user may reset this to any other value,             ',&
'   including zero. "eps" is changed by "chop", which is described later         ',&
'   in this manual.                                                              ',&
'                                                                                ',&
'   The value of "rand" is a random variable, with a choice of a uniform         ',&
'   or a normal distribution.                                                    ',&
'                                                                                ',&
'   The name "eye" is used in place of "i" to denote identity matrices           ',&
'   because "i" is often used as a subscript or as sqrt(-1).  The dimensions     ',&
'   of "eye" are determined by context. For example,                             ',&
'                                                                                ',&
'      B = A + 3*eye                                                             ',&
'                                                                                ',&
'   adds 3 to the diagonal elements of A and                                     ',&
'                                                                                ',&
'      X = eye/A                                                                 ',&
'                                                                                ',&
'   is one of several ways in MAT88 to invert a matrix.                          ',&
'                                                                                ',&
'   "flop" provides a measure of the number of floating point operations,        ',&
'   or "flops", required for each calculation by reporting the CPU time          ',&
'   consumed.                                                                    ',&
'                                                                                ',&
'   A statement may consist of an expression alone, in which case a              ',&
'   variable named "ans" is created and the result stored in "ans" for           ',&
'   possible future use. Thus                                                    ',&
'                                                                                ',&
'      A\A - eye                                                                 ',&
'                                                                                ',&
'   is the same as                                                               ',&
'                                                                                ',&
'      ans = A\A - eye                                                           ',&
'                                                                                ',&
'   (Roundoff error usually causes this result to be a matrix of "small"         ',&
'   numbers, rather than all zeros.)                                             ',&
'                                                                                ',&
'   All computations are done using either single or double precision real       ',&
'   arithmetic, whichever is appropriate for the particular computer. There      ',&
'   is no mixed-precision arithmetic.  The Fortran COMPLEX data type             ',&
'   is not used because many systems create unnecessary underflows and           ',&
'   overflows with complex operations and because some systems do not            ',&
'   allow double precision complex arithmetic.                                   ',&
'                                                                                ',&
'================================================================================',&
'FUNCTIONS                                                                       ',&
'                                                                                ',&
'   Much of MAT88''s computational power comes from the various                  ',&
'   matrix functions available. The current list includes:                       ',&
'                                                                                ',&
'      inv(A)          - Inverse.                                                ',&
'      det(A)          - Determinant.                                            ',&
'      cond(A)         - Condition number.                                       ',&
'      rcond(A)        - A measure of nearness to singularity.                   ',&
'      eig(A)          - Eigenvalues and eigenvectors.                           ',&
'      schur(A)        - Schur triangular form.                                  ',&
'      hess(A)         - Hessenberg or tridiagonal form.                         ',&
'      poly(A)         - Characteristic polynomial.                              ',&
'      svd(A)          - Singular value decomposition.                           ',&
'      pinv(A,eps)     - Pseudo-inverse with optional tolerance.                 ',&
'      rank(A,eps)     - Matrix rank with optional tolerance.                    ',&
'      lu(A)           - Factors from Gaussian elimination.                      ',&
'      chol(A)         - Factor from Cholesky factorization.                     ',&
'      qr(A)           - Factors from Householder orthogonalization.             ',&
'      rref(A)         - Reduced row echelon form.                               ',&
'      orth(A)         - Orthogonal vectors spanning range of A.                 ',&
'      exp(A)          - e to the A.                                             ',&
'      log(A)          - Natural logarithm.                                      ',&
'      sqrt(A)         - Square root.                                            ',&
'      sin(A)          - Trigonometric sine.                                     ',&
'      cos(A)          - Cosine.                                                 ',&
'      atan(A)         - Arctangent.                                             ',&
'      round(A)        - Round the elements to nearest integers.                 ',&
'      abs(A)          - Absolute value of the elements.                         ',&
'      real(A)         - Real parts of the elements.                             ',&
'      imag(A)         - Imaginary parts of the elements.                        ',&
'      conjg(A)        - Complex conjugate.                                      ',&
'      sum(A)          - Sum of the elements.                                    ',&
'      prod(A)         - Product of the elements.                                ',&
'      diag(A)         - Extract or create diagonal matrices.                    ',&
'      tril(A)         - Lower triangular part of A.                             ',&
'      triu(A)         - Upper triangular part of A.                             ',&
'      norm(A,p)       - Norm with p = 1, 2 or ''Infinity''.                     ',&
'      eye(m,n)        - Portion of identity matrix.                             ',&
'      rand(m,n)       - Matrix with random elements.                            ',&
'      ones(m,n)       - Matrix of all ones.                                     ',&
'      magic(n)        - Interesting test matrices.                              ',&
'      invh(n)         - Inverse Hilbert matrices.                               ',&
'      roots(C)        - Roots of polynomial with coefficients C.                ',&
'      display(A,p)    - Print base p representation of A.                       ',&
'      kron(A,B)       - Kronecker tensor product of A and B.                    ',&
'      plot(X,Y)       - Plot Y as a function of X .                             ',&
'      rat(A)          - Find "simple" rational approximation to A.              ',&
'      user(A)         - Function defined by external program.                   ',&
'                                                                                ',&
'   Some of these functions have different interpretations when the              ',&
'   argument is a matrix or a vector and some of them have additional            ',&
'   optional arguments. Details are given in the HELP document in the            ',&
'   appendix.                                                                    ',&
'                                                                                ',&
'   Several of these functions can be used in a generalized assignment           ',&
'   statement with two or three variables on the left hand side. For             ',&
'   example                                                                      ',&
'                                                                                ',&
'      <X,D> = eig(A)                                                            ',&
'                                                                                ',&
'   stores the eigenvectors of A in the matrix X and a diagonal matrix           ',&
'   containing the eigenvalues in the matrix D. The statement                    ',&
'                                                                                ',&
'      eig(A)                                                                    ',&
'                                                                                ',&
'   simply computes the eigenvalues and stores them in "ans".                    ',&
'                                                                                ',&
'   Future versions of MAT88 will probably include additional functions,         ',&
'   since they can easily be added to the system.                                ',&
'                                                                                ',&
'================================================================================',&
'ROWS COLUMNS AND SUBMATRICES                                                    ',&
'                                                                                ',&
'   Individual elements of a matrix can be accessed by giving their              ',&
'   subscripts in parentheses, eg. A(1,2), x(i), TAB(ind(k)+1).                  ',&
'   An expression used as a subscript is rounded to the nearest integer.         ',&
'                                                                                ',&
'   Individual rows and columns can be accessed using a colon '':'' (or a        ',&
'   ''|'') for the free subscript. For example, A(1,:) is the first row of       ',&
'   A and A(:,j) is the j-th column. Thus                                        ',&
'                                                                                ',&
'      A(i,:) = A(i,:) + c*A(k,:)                                                ',&
'                                                                                ',&
'   adds c times the k-th row of A to the i-th row.                              ',&
'                                                                                ',&
'   The colon is used in several other ways in MAT88, but all of the uses        ',&
'   are based on the following definition.                                       ',&
'                                                                                ',&
'      j:k    is the same as  <j, j+1, ..., k>                                   ',&
'      j:k    is empty if  j > k .                                               ',&
'      j:i:k  is the same as  <j, j+i, j+2i, ..., k>                             ',&
'      j:i:k  is empty if  i > 0 and j > k or if i < 0 and j < k .               ',&
'                                                                                ',&
'   The colon is usually used with integers, but it is possible to               ',&
'   use arbitrary real scalars as well. Thus                                     ',&
'                                                                                ',&
'      1:4  is the same as  <1, 2, 3, 4>                                         ',&
'      0: 0.1: 0.5 is the same as <0.0, 0.1, 0.2, 0.3, 0.4, 0.5>                 ',&
'                                                                                ',&
'   In general, a subscript can be a vector. If X and V are vectors,             ',&
'   then X(V) is <X(V(1)), X(V(2)), ..., X(V(n))> . This can also be             ',&
'   used with matrices. If V has m components and W has n components,            ',&
'   then A(V,W) is the m by n matrix formed from the elements of A whose         ',&
'   subscripts are the elements of V and W.  Combinations of the colon           ',&
'   notation and the indirect subscripting allow manipulation of various         ',&
'   submatrices. For example,                                                    ',&
'                                                                                ',&
'      A(<1,5>,:) = A(<5,1>,:)  interchanges rows 1 and 5 of A.                  ',&
'      A(2:k,1:n)  is the submatrix formed from rows 2 through k                 ',&
'         and columns 1 through n of A .                                         ',&
'      A(:,<3 1 2>)  is a permutation of the first three columns.                ',&
'                                                                                ',&
'   The notation A(:) has a special meaning. On the right hand side of an        ',&
'   assignment statement, it denotes all the elements of A, regarded as          ',&
'   a single column. When an expression is assigned to A(:), the current         ',&
'   dimensions of A, rather than of the expression, are used.                    ',&
'                                                                                ',&
'================================================================================',&
'FOR WHILE AND IF                                                                ',&
'                                                                                ',&
'   The "for" clause allows statements to be repeated a specific                 ',&
'   number of times. The general form is                                         ',&
'                                                                                ',&
'      for variable = expr, statement, ..., statement, end                       ',&
'                                                                                ',&
'   The "end" and the comma before it may be omitted. In general, the            ',&
'   expression may be a matrix, in which case the columns are stored one         ',&
'   at a time in the variable and the following statements, up to the            ',&
'   "end" or the end of the line, are executed. The expression is often          ',&
'   of the form j:k, and its "columns" are simply the scalars from j to          ',&
'   k. Some examples (assume n has already been assigned a value):               ',&
'                                                                                ',&
'      for i = 1:n, for j = 1:n, A(i,j) = 1/(i+j-1);                             ',&
'                                                                                ',&
'   generates the Hilbert matrix.                                                ',&
'                                                                                ',&
'      for j = 2:n-1, for i = j:n-1, ...                                         ',&
'         A(i,j) = 0; end; A(j,j) = j; end; A                                    ',&
'                                                                                ',&
'   changes all but the "outer edge" of the lower triangle and then              ',&
'   prints the final matrix.                                                     ',&
'                                                                                ',&
'      for h = 1.0: -0.1: -1.0, (<h, cos(pi*h)>)                                 ',&
'                                                                                ',&
'   prints a table of cosines.                                                   ',&
'                                                                                ',&
'      <X,D> = eig(A); for v = X, v, A*v                                         ',&
'                                                                                ',&
'   displays eigenvectors, one at a time.                                        ',&
'                                                                                ',&
'        The "while" clause allows statements to be repeated an                  ',&
'   indefinite number of times. The general form is                              ',&
'                                                                                ',&
'      while expr relop expr,   statement,..., statement, end                    ',&
'                                                                                ',&
'   where relop is =, <, >, <=, >=, or <> (not equal). The statements are        ',&
'   repeatedly executed as long as the indicated comparison between the          ',&
'   real parts of the first components of the two expressions is true. Here      ',&
'   are two examples. (Exercise for the reader: What do these segments do?)      ',&
'                                                                                ',&
'      eps = 1;                                                                  ',&
'      while 1 + eps > 1, eps = eps/2;                                           ',&
'      eps = 2*eps                                                               ',&
'                                                                                ',&
'      E = 0*A;  F = E + eye; n = 1;                                             ',&
'      while norm(E+F-E,1) > 0, E = E + F; F = A*F/n; n = n + 1;                 ',&
'      E                                                                         ',&
'                                                                                ',&
'   The IF clause allows conditional execution of statements.  The general       ',&
'   form is                                                                      ',&
'                                                                                ',&
'      if expr relop expr,  statement, ..., statement,                           ',&
'         else statement, ..., statement                                         ',&
'                                                                                ',&
'   The first group of statements are executed if the relation is true and       ',&
'   the second group are executed if the relation is false.  The "else"          ',&
'   and the statements following it may be omitted. For example,                 ',&
'                                                                                ',&
'      if abs(i-j) = 2, A(i,j) = 0;                                              ',&
'                                                                                ',&
'================================================================================',&
'COMMANDS TEXTFILES AND MACROS                                                   ',&
'                                                                                ',&
'   MAT88 has several commands which control the output format and the           ',&
'   overall execution of the system.                                             ',&
'                                                                                ',&
'   The "help" command allows on-line access to short portions of text           ',&
'   describing various operations, functions and special characters. The         ',&
'   entire "help" document is reproduced in an appendix.                         ',&
'                                                                                ',&
'   Results are usually printed in a scaled fixed point format that shows        ',&
'   4 or 5 significant figures. The commands "short", "long", "short e",         ',&
'   "long e" and "long z" alter the output format, but do not alter the          ',&
'   precision of the computations or the internal storage.                       ',&
'                                                                                ',&
'   The "who" command provides information about the functions and               ',&
'   variables that are currently defined.                                        ',&
'                                                                                ',&
'   The "clear" command erases all variables, except "eps", "flop",              ',&
'   "rand" and "eye". The statement A = <> indicates that a "0 by 0"             ',&
'   matrix is to be stored in A. This causes A to be erased so that its          ',&
'   storage can be used for other variables.                                     ',&
'                                                                                ',&
'   The "quit" and "exit" commands cause return to the underlying operating      ',&
'   system through the Fortran RETURN statement.                                 ',&
'                                                                                ',&
'   MAT88 has a limited facility for handling text. Any string of                ',&
'   characters delineated by quotes (with two quotes used to allow one           ',&
'   quote within the string) is saved as a vector of integer values with         ',&
'   ''1'' = 1, ''A'' = 10, '' '' = 36, etc. (The complete list is in the appendix',&
'   under "char".) For example                                                   ',&
'                                                                                ',&
'      ''2*A + 3''  is the same as  <2 43 10 36 41 36 3>                         ',&
'                                                                                ',&
'   It is possible, though seldom very meaningful, to use such                   ',&
'   strings in matrix operations. More frequently, the text is used              ',&
'   as a special argument to various functions.                                  ',&
'                                                                                ',&
'      norm(A,''inf'')    computes the infinity norm of A .                      ',&
'      display(T)       prints the text stored in T .                            ',&
'      exec(''file'')     obtains MAT88 input from an external file.             ',&
'      save(''file'')     stores all the current variables in a file.            ',&
'      load(''file'')     retrieves all the variables from a file.               ',&
'      print(''file'',X)  prints X on a file.                                    ',&
'      diary(''file'')    makes a copy of the complete MAT88 session.            ',&
'                                                                                ',&
'   The text can also be used in a limited string substitution                   ',&
'   macro facility. If a variable, say T, contains the source text               ',&
'   for a MAT88 statement or expression, then the construction                   ',&
'                                                                                ',&
'      > T <                                                                     ',&
'                                                                                ',&
'   causes T to be executed or evaluated. For example                            ',&
'                                                                                ',&
'      T = ''2*A + 3'';                                                          ',&
'      S = ''B = >T< + 5''                                                       ',&
'      A = 4;                                                                    ',&
'      > S <                                                                     ',&
'                                                                                ',&
'   produces                                                                     ',&
'                                                                                ',&
'      B     =                                                                   ',&
'                                                                                ',&
'         16.                                                                    ',&
'                                                                                ',&
'   Some other examples are given under MACROS in the appendix. This             ',&
'   facility is useful for fairly short statements and expressions.              ',&
'   More complicated MAT88 "programs" should use the "exec" facility.            ',&
'                                                                                ',&
'================================================================================',&
'NUMERICAL ALGORITHMS                                                            ',&
'                                                                                ',&
'   The algorithms underlying the basic MAT88 functions are described in         ',&
'   the LINPACK and EISPACK guides [1-3]. The following list gives the           ',&
'   subroutines used by these functions.                                         ',&
'                                                                                ',&
'      inv(A)          - CGECO,CGEDI                                             ',&
'      det(A)          - CGECO,CGEDI                                             ',&
'      lu(A)           - CGEFA                                                   ',&
'      rcond(A)        - CGECO                                                   ',&
'      chol(A)         - CPOFA                                                   ',&
'      svd(A)          - CSVDC                                                   ',&
'      cond(A)         - CSVDC                                                   ',&
'      norm(A,2)       - CSVDC                                                   ',&
'      pinv(A,eps)     - CSVDC                                                   ',&
'      rank(A,eps)     - CSVDC                                                   ',&
'      qr(A)           - CQRDC,CQRSL                                             ',&
'      orth(A)         - CQRDC,CSQSL                                             ',&
'      A\B and B/A     - CGECO,CGESL if A is square.                             ',&
'                      - CQRDC,CQRSL if A is not square.                         ',&
'      eig(A)          - HTRIDI,IMTQL2,HTRIBK if A is Hermitian.                 ',&
'                      - CORTH,COMQR2         if A is not Hermitian.             ',&
'      schur(A)        - same as EIG.                                            ',&
'      hess(A)         - same as EIG.                                            ',&
'                                                                                ',&
'   Minor modifications were made to all these subroutines. The LINPACK          ',&
'   routines were changed to replace the Fortran complex arithmetic              ',&
'   with explicit references to real and imaginary parts.  Since most            ',&
'   of the floating point arithmetic is concentrated in a few low-level          ',&
'   subroutines which perform vector operations (the Basic Linear Algebra        ',&
'   Subprograms), this was not an extensive change. It also facilitated          ',&
'   implementation of the "flop" and "chop" features which count and             ',&
'   optionally truncate each floating point operation.                           ',&
'                                                                                ',&
'   The EISPACK subroutine COMQR2 was modified to allow access to the            ',&
'   Schur triangular form, ordinarily just an intermediate result. IMTQL2        ',&
'   was modified to make computation of the eigenvectors optional. Both          ',&
'   subroutines were modified to eliminate the machine-dependent accuracy        ',&
'   parameter and all the EISPACK subroutines were changed to include            ',&
'   "flop" and "chop".                                                           ',&
'                                                                                ',&
'   The algorithms employed for the "poly" and "roots" functions                 ',&
'   illustrate an interesting aspect of the modern approach to eigenvalue        ',&
'   computation. "poly(A)" generates the characteristic polynomial of            ',&
'   A and "roots(poly(A))" finds the roots of that polynomial, which             ',&
'   are, of course, the eigenvalues of A . But both "poly" and "roots"           ',&
'   use EISPACK eigenvalues subroutines, which are based on similarity           ',&
'   transformations. So the classical approach which characterizes               ',&
'   eigenvalues as roots of the characteristic polynomial is actually            ',&
'   reversed.                                                                    ',&
'                                                                                ',&
'   If A is an n by n matrix, "poly(A)" produces the coefficients C(1)           ',&
'   through C(n+1), with C(1) = 1, in                                            ',&
'                                                                                ',&
'         det(z*eye-A) = C(1)*z**n + ... + C(n)*z + C(n+1) .                     ',&
'                                                                                ',&
'   The algorithm can be expressed compactly using MAT88:                        ',&
'                                                                                ',&
'         Z = eig(A);                                                            ',&
'         C = 0*ones(n+1,1);  C(1) = 1;                                          ',&
'         for j = 1:n, C(2:j+1) = C(2:j+1) - Z(j)*C(1:j);                        ',&
'         C                                                                      ',&
'                                                                                ',&
'   This recursion is easily derived by expanding the product                    ',&
'                                                                                ',&
'         (z - z(1))*(z - z(2))* ... * (z-z(n)) .                                ',&
'                                                                                ',&
'   It is possible to prove that "poly(A)" produces the coefficients in          ',&
'   the characteristic polynomial of a matrix within roundoff error of           ',&
'   A. This is true even if the eigenvalues of A are badly conditioned. The      ',&
'   traditional algorithms for obtaining the characteristic polynomial           ',&
'   which do not use the eigenvalues do not have such satisfactory               ',&
'   numerical properties.                                                        ',&
'                                                                                ',&
'   If C is a vector with n+1 components, "roots(C)" finds the roots of          ',&
'   the polynomial of degree n ,                                                 ',&
'                                                                                ',&
'          p(z) = C(1)*z**n + ... + C(n)*z + C(n+1) .                            ',&
'                                                                                ',&
'   The algorithm simply involves computing the eigenvalues of the               ',&
'   companion matrix:                                                            ',&
'                                                                                ',&
'         A = 0*ones(n,n)                                                        ',&
'         for j = 1:n, A(1,j) = -C(j+1)/C(1);                                    ',&
'         for i = 2:n, A(i,i-1) = 1;                                             ',&
'         eig(A)                                                                 ',&
'                                                                                ',&
'   It is possible to prove that the results produced are the exact              ',&
'   eigenvalues of a matrix within roundoff error of the companion matrix        ',&
'   A, but this does not mean that they are the exact roots of a polynomial      ',&
'   with coefficients within roundoff error of those in C . There are            ',&
'   more accurate, more efficient methods for finding polynomial roots,          ',&
'   but this approach has the crucial advantage that it does not require         ',&
'   very much additional code.                                                   ',&
'                                                                                ',&
'   The elementary functions "exp", "log", "sqrt", "sin", "cos" and "atan"       ',&
'   are applied to square matrices by diagonalizing the matrix, applying         ',&
'   the functions to the individual eigenvalues and then transforming            ',&
'   back. For example, "exp(A)" is computed by                                   ',&
'                                                                                ',&
'         <X,D> = eig(A);                                                        ',&
'         for j = 1:n, D(j,j) = exp(D(j,j));                                     ',&
'         X*D/X                                                                  ',&
'                                                                                ',&
'   This is essentially method number 14 out of the 19 ''dubious''               ',&
'   possibilities described in [8]. It is dubious because it doesn''t always     ',&
'   work. The matrix of eigenvectors X can be arbitrarily badly conditioned      ',&
'   and all accuracy lost in the computation of X*D/X. A warning message         ',&
'   is printed if "rcond(X)" is very small, but this only catches the            ',&
'   extreme cases. An example of a case not detected is when A has a double      ',&
'   eigenvalue, but theoretically only one linearly independent eigenvector      ',&
'   associated with it.  The computed eigenvalues will be separated by           ',&
'   something on the order of the square root of the roundoff level. This        ',&
'   separation will be reflected in "rcond(X)" which will probably not           ',&
'   be small enough to trigger the error message. The computed "exp(A)"          ',&
'   will be accurate to only half precision. Better methods are known for        ',&
'   computing "exp(A)", but they do not easily extend to the other five          ',&
'   functions and would require a considerable amount of additional code.        ',&
'                                                                                ',&
'   The expression A**p is evaluated by repeated multiplication if p is          ',&
'   an integer greater than 1. Otherwise it is evaluated by                      ',&
'                                                                                ',&
'         <X,D> = eig(A);                                                        ',&
'         for j = 1:n, D(j,j) = exp(p*log(D(j,j)))                               ',&
'         X*D/X                                                                  ',&
'                                                                                ',&
'   This suffers from the same potential loss of accuracy if X is                ',&
'   badly conditioned. It was partly for this reason that the case p =           ',&
'   1 is included in the general case. Comparison of A**1 with A gives           ',&
'   some idea of the loss of accuracy for other values of p and for the          ',&
'   elementary functions.                                                        ',&
'                                                                                ',&
'   "rref", the reduced row echelon form, is of some interest in                 ',&
'   theoretical linear algebra, although it has little computational             ',&
'   value. It is included in MAT88 for pedagogical reasons. The algorithm        ',&
'   is essentially Gauss-Jordan elimination with detection of negligible         ',&
'   columns applied to rectangular matrices.                                     ',&
'                                                                                ',&
'   There are three separate places in MAT88 where the rank of a matrix          ',&
'   is implicitly computed: in rref(A), in A\B for non-square A, and             ',&
'   in the pseudoinverse pinv(A). Three different algorithms with three          ',&
'   different criteria for negligibility are used and so it is possible          ',&
'   that three different values could be produced for the same matrix. With      ',&
'   rref(A), the rank of A is the number of nonzero rows. The elimination        ',&
'   algorithm used for "rref" is the fastest of the three rank-determining       ',&
'   algorithms, but it is the least sophisticated numerically and the            ',&
'   least reliable.  With A\B, the algorithm is essentially that used            ',&
'   by example subroutine SQRST in chapter 9 of the LINPACK guide. With          ',&
'   pinv(A), the algorithm is based on the singular value decomposition          ',&
'   and is described in chapter 11 of the LINPACK guide. The SVD algorithm       ',&
'   is the most time-consuming, but the most reliable and is therefore           ',&
'   also used for rank(A).                                                       ',&
'                                                                                ',&
'   The uniformly distributed random numbers in "rand" are obtained from         ',&
'   the machine-independent random number generator URAND described in           ',&
'   [9]. It is possible to switch to normally distributed random numbers,        ',&
'   which are obtained using a transformation also described in [9].             ',&
'                                                                                ',&
'        The computation of                                                      ',&
'                                                                                ',&
'                   2    2                                                       ',&
'             sqrt(a  + b )                                                      ',&
'                                                                                ',&
'   is required in many matrix algorithms, particularly those involving          ',&
'   complex arithmetic. A new approach to carrying out this operation is         ',&
'   described by Moler and Morrison [10]. It is a cubically convergent           ',&
'   algorithm which starts with a and b , rather than with their squares,        ',&
'   and thereby avoids destructive arithmetic underflows and overflows. In       ',&
'   MAT88, the algorithm is used for complex modulus, Euclidean vector           ',&
'   norm, plane rotations, and the shift calculation in the eigenvalue           ',&
'   and singular value iterations.                                               ',&
'                                                                                ',&
'================================================================================',&
'FLOP AND CHOP                                                                   ',&
'                                                                                ',&
'   Detailed information about the amount of work involved in matrix             ',&
'   calculations and the resulting accuracy is provided by "flop" and            ',&
'   "chop". The basic unit of work is the "flop", or floating point              ',&
'   operation. Roughly, one flop is one execution of a Fortran statement         ',&
'   like                                                                         ',&
'                                                                                ',&
'         S = S + X(I)*Y(I)                                                      ',&
'                                                                                ',&
'   or                                                                           ',&
'                                                                                ',&
'         Y(I) = Y(I) + T*X(I)                                                   ',&
'                                                                                ',&
'   In other words, it consists of one floating point multiplication,            ',&
'   together with one floating point addition and the associated                 ',&
'   indexing and storage reference operations.                                   ',&
'                                                                                ',&
'   MAT88 will print the CPU time required for a particular                      ',&
'   statement when the statement is terminated by an extra comma. For            ',&
'   example, the line                                                            ',&
'                                                                                ',&
'         n = 20;  rand(n)*rand(n);,                                             ',&
'                                                                                ',&
'   ends with an extra comma. Two 20 by 20 random matrices are generated         ',&
'   and multiplied together. The result is assigned to "ans", but the            ',&
'   semicolon suppresses its printing. The only output is                        ',&
'                                                                                ',&
'           8800 flops                                                           ',&
'                                                                                ',&
'   This is n**3 + 2*n**2 flops, n**2 for each random matrix and n**3            ',&
'   for the product.                                                             ',&
'                                                                                ',&
'   "flop" is a predefined vector with two components. "flop(1)" is              ',&
'   the number of flops used by the most recently executed statement,            ',&
'   except that statements with zero flops are ignored. For example,             ',&
'   after executing the previous statement,                                      ',&
'                                                                                ',&
'         flop(1)/n**3                                                           ',&
'                                                                                ',&
'   results in                                                                   ',&
'                                                                                ',&
'         ans   =                                                                ',&
'                                                                                ',&
'             1.1000                                                             ',&
'                                                                                ',&
'   "flop(2)" is the cumulative total of all the flops used since                ',&
'   the beginning of the MAT88 session. The statement                            ',&
'                                                                                ',&
'         flop = <0 0>                                                           ',&
'                                                                                ',&
'   resets the total.                                                            ',&
'                                                                                ',&
'   There are several difficulties associated with keeping a                     ',&
'   precise count of floating point operations.                                  ',&
'                                                                                ',&
'   As the program generally uses complex values but only performs               ',&
'   operations on the real matrices in many cases where all the imaginary        ',&
'   values are zero it may not provide an accurate measure of the relative       ',&
'   costs of real and complex arithmetic.                                        ',&
'                                                                                ',&
'   The result of each floating point operation may also be "chopped"            ',&
'   to simulate a computer with a shorter word length. The details               ',&
'   of this chopping operation depend upon the format of the floating            ',&
'   point word. Usually, the fraction in the floating point word can be          ',&
'   regarded as consisting of several octal or hexadecimal digits. The           ',&
'   least significant of these digits can be set to zero by a logical            ',&
'   masking operation. Thus the statement                                        ',&
'                                                                                ',&
'         chop(p)                                                                ',&
'                                                                                ',&
'   causes the p least significant octal or hexadecimal digits in                ',&
'   the result of each floating point operation to be set to zero.               ',&
'   For example, if the computer being used has an IBM 360 long floating         ',&
'   point word with 14 hexadecimal digits in the fraction, then "chop(8)"        ',&
'   results in simulation of a computer with only 6 hexadecimal digits           ',&
'   in the fraction, i.e. a short floating point word. On a computer such        ',&
'   as the CDC 6600 with 16 octal digits, "chop(8)" results in about the         ',&
'   same accuracy because the remaining 8 octal digits represent the same        ',&
'   number of bits as 6 hexadecimal digits.                                      ',&
'                                                                                ',&
'   Some idea of the effect of "chop" on any particular system can               ',&
'   be obtained by executing the following statements.                           ',&
'                                                                                ',&
'         long,   t = 1/10                                                       ',&
'         long z, t = 1/10                                                       ',&
'         chop(8)                                                                ',&
'         long,   t = 1/10                                                       ',&
'         long z, t = 1/10                                                       ',&
'                                                                                ',&
'   The following Fortran subprograms illustrate more details of                 ',&
'   "flop" and "chop". The first subprogram is a simplified example of a         ',&
'   system-dependent function used within MAT88 itself. The common variable      ',&
'   G_FLOP_COUNTER is essentially the first component of the variable            ',&
'   FLOP. The common variable CHP is initially zero, but it is set to p          ',&
'   by the statement "chop(p)". To shorten the DATA statement, we assume         ',&
'   there are only 6 hexadecimal digits. We also assume an extension of          ',&
'   Fortran that allows .AND. to be used as a binary operation between           ',&
'   two real variables.                                                          ',&
'                                                                                ',&
'         REAL FUNCTION FLOP(X)                                                  ',&
'         REAL X                                                                 ',&
'         INTEGER G_FLOP_COUNTER,CHP                                             ',&
'         COMMON G_FLOP_COUNTER,CHP                                              ',&
'         REAL MASK(5)                                                           ',&
'         DATA MASK/ZFFFFFFF0,ZFFFFFF00,ZFFFFF000,ZFFFF0000,ZFFF00000/           ',&
'         G_FLOP_COUNTER = G_FLOP_COUNTER + 1                                    ',&
'         IF (CHP .EQ. 0) FLOP = X                                               ',&
'         IF (CHP .GE. 1 .AND. CHP .LE. 5) FLOP = X .AND. MASK(CHP)              ',&
'         IF (CHP .GE. 6) FLOP = 0.0                                             ',&
'         END REAL FUNCTION FLOP                                                 ',&
'                                                                                ',&
'   The following subroutine illustrates a typical use of the                    ',&
'   previous function within MAT88. It is a simplified version of                ',&
'   the Basic Linear Algebra Subprogram that adds a scalar multiple              ',&
'   of one vector to another. We assume here that the vectors are                ',&
'   stored with a memory increment of one.                                       ',&
'                                                                                ',&
'         SUBROUTINE SAXPY(N,TR,TI,XR,XI,YR,YI)                                  ',&
'         REAL TR,TI,XR(N),XI(N),YR(N),YI(N),FLOP                                ',&
'         IF (N .LE. 0) return                                                   ',&
'         IF (TR .EQ. 0.0 .AND. TI .EQ. 0.0) return                              ',&
'         DO I = 1, N                                                            ',&
'            YR(I) = FLOP(YR(I) + TR*XR(I) - TI*XI(I))                           ',&
'            YI(I) = YI(I) + TR*XI(I) + TI*XR(I)                                 ',&
'            IF (YI(I) .NE. 0.0D0) YI(I) = FLOP(YI(I))                           ',&
'         enddo                                                                  ',&
'         END SUBROUTINE SAXPY                                                   ',&
'                                                                                ',&
'   The saxpy operation is perhaps the most fundamental                          ',&
'   operation within LINPACK. It is used in the computation of the               ',&
'   LU, the QR and the SVD factorizations, and in several other                  ',&
'   places. We see that adding a multiple of one vector with n                   ',&
'   components to another uses n flops if the vectors are real and               ',&
'   between n and 2*n flops if the vectors have nonzero imaginary                ',&
'   components.                                                                  ',&
'                                                                                ',&
'   The permanent MAT88 variable "eps" is reset by the statement                 ',&
'   CHOP(p). Its new value is usually the smallest inverse power of              ',&
'   two that satisfies the Fortran logical test                                  ',&
'                                                                                ',&
'               FLOP(1.0+eps) .GT. 1.0                                           ',&
'                                                                                ',&
'   However, if "eps" had been directly reset to a larger value, the             ',&
'   old value is retained.                                                       ',&
'                                                                                ',&
'================================================================================',&
'CENSUS EXAMPLE                                                                  ',&
'                                                                                ',&
'   Our first extended example involves predicting the population of the         ',&
'   United States in 1980 using extrapolation of various fits to the             ',&
'   census data from 1900 through 1970. There are eight observations,            ',&
'   so we begin with the MAT88 statement                                         ',&
'                                                                                ',&
'      n = 8                                                                     ',&
'                                                                                ',&
'   The values of the dependent variable, the population in millions,            ',&
'   can be entered with                                                          ',&
'                                                                                ',&
'      y = < 75.995   91.972  105.711  123.203   ...                             ',&
'           131.669  150.697  179.323  203.212>''                                ',&
'                                                                                ',&
'   In order to produce a reasonably scaled matrix, the independent              ',&
'   variable, time, is transformed from the interval [1900,1970] to              ',&
'   [-1.00,0.75]. This can be accomplished directly with                         ',&
'                                                                                ',&
'      t = -1.0:0.25:0.75                                                        ',&
'                                                                                ',&
'   or in a fancier, but perhaps clearer, way with                               ',&
'                                                                                ',&
'      t = 1900:10:1970;   t = (t - 1940*ones(t))/40                             ',&
'                                                                                ',&
'   Either of these is equivalent to                                             ',&
'                                                                                ',&
'      t = <-1 -.75 -.50 -.25 0 .25 .50 .75>                                     ',&
'                                                                                ',&
'   The interpolating polynomial of degree n-1 involves an Vandermonde           ',&
'   matrix of order n with elements that might be generated by                   ',&
'                                                                                ',&
'      for i = 1:n, for j = 1:n, a(i,j) = t(i)**(j-1);                           ',&
'                                                                                ',&
'   However, this results in an error caused by 0**0 when i = 5 and              ',&
'   j = 1 . The preferable approach is                                           ',&
'                                                                                ',&
'      A = ones(n,n);                                                            ',&
'      for i = 1:n, for j = 2:n, a(i,j) = t(i)*a(i,j-1);                         ',&
'                                                                                ',&
'   Now the statement                                                            ',&
'                                                                                ',&
'      cond(A)                                                                   ',&
'                                                                                ',&
'   produces the output                                                          ',&
'                                                                                ',&
'      ans  =                                                                    ',&
'                                                                                ',&
'         1.1819E+03                                                             ',&
'                                                                                ',&
'   which indicates that transformation of the time variable has resulted        ',&
'   in a reasonably well conditioned matrix.                                     ',&
'                                                                                ',&
'        The statement                                                           ',&
'                                                                                ',&
'      c = A\y                                                                   ',&
'                                                                                ',&
'   results in                                                                   ',&
'                                                                                ',&
'      C     =                                                                   ',&
'                                                                                ',&
'        131.6690                                                                ',&
'         41.0406                                                                ',&
'        103.5396                                                                ',&
'        262.4535                                                                ',&
'       -326.0658                                                                ',&
'       -662.0814                                                                ',&
'        341.9022                                                                ',&
'        533.6373                                                                ',&
'                                                                                ',&
'   These are the coefficients in the interpolating polynomial                   ',&
'                                                                                ',&
'         n-1                                                                    ',&
'                                                                                ',&
'         c  + c t + ... + c t                                                   ',&
'          1    2           n                                                    ',&
'                                                                                ',&
'   Our transformation of the time variable has resulted in t = 1                ',&
'   corresponding to the year 1980. Consequently, the extrapolated               ',&
'   population is simply the sum of the coefficients. This can be                ',&
'   computed by                                                                  ',&
'                                                                                ',&
'      p = sum(c)                                                                ',&
'                                                                                ',&
'   The result is                                                                ',&
'                                                                                ',&
'      P     =                                                                   ',&
'                                                                                ',&
'        426.0950                                                                ',&
'                                                                                ',&
'   which indicates a 1980 population of over 426 million. Clearly, using        ',&
'   the seventh degree interpolating polynomial to extrapolate even a            ',&
'   fairly short distance beyond the end of the data interval is not a           ',&
'   good idea.                                                                   ',&
'                                                                                ',&
'   The coefficients in least squares fits by polynomials of lower degree        ',&
'   can be computed using fewer than n columns of the matrix.                    ',&
'                                                                                ',&
'      for k = 1:n, c = A(:,1:k)\y,  p = sum(c)                                  ',&
'                                                                                ',&
'   would produce the coefficients of these fits, as well as the                 ',&
'   resulting extrapolated population. If we do not want to print all the        ',&
'   coefficients, we can simply generate a small table of populations            ',&
'   predicted by polynomials of degrees zero through seven. We also              ',&
'   compute the maximum deviation between the fitted and observed values.        ',&
'                                                                                ',&
'      for k = 1:n, X = A(:,1:k);  c = X\y;  ...                                 ',&
'         d(k) = k-1;  p(k) = sum(c);  e(k) = norm(X*c-y,''inf'');               ',&
'      <d, p, e>                                                                 ',&
'                                                                                ',&
'   The resulting output is                                                      ',&
'                                                                                ',&
'         0   132.7227  70.4892                                                  ',&
'         1   211.5101   9.8079                                                  ',&
'         2   227.7744   5.0354                                                  ',&
'         3   241.9574   3.8941                                                  ',&
'         4   234.2814   4.0643                                                  ',&
'         5   189.7310   2.5066                                                  ',&
'         6   118.3025   1.6741                                                  ',&
'         7   426.0950   0.0000                                                  ',&
'                                                                                ',&
'   The zeroth degree fit, 132.7 million, is the result of fitting a             ',&
'   constant to the data and is simply the average. The results obtained         ',&
'   with polynomials of degree one through four all appear reasonable. The       ',&
'   maximum deviation of the degree four fit is slightly greater than the        ',&
'   degree three, even though the sum of the squares of the deviations           ',&
'   is less. The coefficients of the highest powers in the fits of degree        ',&
'   five and six turn out to be negative and the predicted populations of        ',&
'   less than 200 million are probably unrealistic. The hopefully absurd         ',&
'   prediction of the interpolating polynomial concludes the table.              ',&
'                                                                                ',&
'   We wish to emphasize that roundoff errors are not significant                ',&
'   here. Nearly identical results would be obtained on other computers,         ',&
'   or with other algorithms. The results simply indicate the difficulties       ',&
'   associated with extrapolation of polynomial fits of even modest degree.      ',&
'                                                                                ',&
'   A stabilized fit by a seventh degree polynomial can be obtained using        ',&
'   the pseudoinverse, but it requires a fairly delicate choice of a             ',&
'   tolerance. The statement                                                     ',&
'                                                                                ',&
'      s = svd(A)                                                                ',&
'                                                                                ',&
'   produces the singular values                                                 ',&
'                                                                                ',&
'      S     =                                                                   ',&
'                                                                                ',&
'         3.4594                                                                 ',&
'         2.2121                                                                 ',&
'         1.0915                                                                 ',&
'         0.4879                                                                 ',&
'         0.1759                                                                 ',&
'         0.0617                                                                 ',&
'         0.0134                                                                 ',&
'         0.0029                                                                 ',&
'                                                                                ',&
'   We see that the last three singular values are less than 0.1 ,               ',&
'   consequently, A can be approximately by a matrix of rank five with an        ',&
'   error less than 0.1 . The Moore-Penrose pseudoinverse of this rank           ',&
'   five matrix is obtained from the singular value decomposition with           ',&
'   the following statements                                                     ',&
'                                                                                ',&
'      c = pinv(A,0.1)*y, p = sum(c), e = norm(a*c-y,''inf'')                    ',&
'                                                                                ',&
'   The output is                                                                ',&
'                                                                                ',&
'      C     =                                                                   ',&
'                                                                                ',&
'       134.7972                                                                 ',&
'        67.5055                                                                 ',&
'        23.5523                                                                 ',&
'         9.2834                                                                 ',&
'         3.0174                                                                 ',&
'         2.6503                                                                 ',&
'        -2.8808                                                                 ',&
'         3.2467                                                                 ',&
'                                                                                ',&
'      P     =                                                                   ',&
'                                                                                ',&
'       241.1720                                                                 ',&
'                                                                                ',&
'      E     =                                                                   ',&
'                                                                                ',&
'         3.9469                                                                 ',&
'                                                                                ',&
'   The resulting seventh degree polynomial has coefficients which are much      ',&
'   smaller than those of the interpolating polynomial given earlier. The        ',&
'   predicted population and the maximum deviation are reasonable. Any           ',&
'   choice of the tolerance between the fifth and sixth singular values          ',&
'   would produce the same results, but choices outside this range result        ',&
'   in pseudoinverses of different rank and do not work as well.                 ',&
'                                                                                ',&
'   The one term exponential approximation                                       ',&
'                                                                                ',&
'        y(t) = k exp(pt)                                                        ',&
'                                                                                ',&
'   can be transformed into a linear approximation by taking logarithms.         ',&
'                                                                                ',&
'        log(y(t)) = log k + pt                                                  ',&
'                                                                                ',&
'                  = c  + c t                                                    ',&
'                     1    2                                                     ',&
'                                                                                ',&
'   The following segment makes use of the fact that a function of a             ',&
'   vector is the function applied to the individual components.                 ',&
'                                                                                ',&
'      X = A(:,1:2);                                                             ',&
'      c = X\log(y)                                                              ',&
'      p = exp(sum(c))                                                           ',&
'      e = norm(exp(X*c)-y,''inf'')                                              ',&
'                                                                                ',&
'   The resulting output is                                                      ',&
'                                                                                ',&
'      C     =                                                                   ',&
'                                                                                ',&
'         4.9083                                                                 ',&
'         0.5407                                                                 ',&
'                                                                                ',&
'      P     =                                                                   ',&
'                                                                                ',&
'       232.5134                                                                 ',&
'                                                                                ',&
'      E     =                                                                   ',&
'                                                                                ',&
'         4.9141                                                                 ',&
'                                                                                ',&
'   The predicted population and maximum deviation appear satisfactory and       ',&
'   indicate that the exponential model is a reasonable one to consider.         ',&
'                                                                                ',&
'   As a curiosity, we return to the degree six polynomial.  Since the           ',&
'   coefficient of the high order term is negative and the value of the          ',&
'   polynomial at t = 1 is positive, it must have a root at some value           ',&
'   of t greater than one. The statements                                        ',&
'                                                                                ',&
'      X = A(:,1:7);                                                             ',&
'      c = X\y;                                                                  ',&
'      c = c(7:-1:1);  //reverse the order of the coefficients                   ',&
'      z = roots(c)                                                              ',&
'                                                                                ',&
'   produce                                                                      ',&
'                                                                                ',&
'      Z     =                                                                   ',&
'                                                                                ',&
'         1.1023-  0.0000*i                                                      ',&
'         0.3021+  0.7293*i                                                      ',&
'        -0.8790+  0.6536*i                                                      ',&
'        -1.2939-  0.0000*i                                                      ',&
'        -0.8790-  0.6536*i                                                      ',&
'         0.3021-  0.7293*i                                                      ',&
'                                                                                ',&
'   There is only one real, positive root. The corresponding time on the         ',&
'   original scale is                                                            ',&
'                                                                                ',&
'      1940 + 40*real(z(1))                                                      ',&
'                                                                                ',&
'        =  1984.091                                                             ',&
'                                                                                ',&
'   We conclude that the United States population should become zero             ',&
'   early in February of 1984.                                                   ',&
'                                                                                ',&
'================================================================================',&
'PARTIAL DIFFERENTIAL EQUATION EXAMPLE                                           ',&
'                                                                                ',&
'   Our second extended example is a boundary value problem for Laplace''s       ',&
'   equation. The underlying physical problem involves the conductivity          ',&
'   of a medium with cylindrical inclusions and is considered by Keller          ',&
'   and Sachs [7].                                                               ',&
'                                                                                ',&
'        Find a function  u(x,y)  satisfying Laplace''s equation                 ',&
'                                                                                ',&
'                  u   + u   = 0                                                 ',&
'                   xx    yy                                                     ',&
'                                                                                ',&
'   The domain is a unit square with a quarter circle of radius rho removed      ',&
'   from one corner. There are Neumann conditions on the top and bottom          ',&
'   edges and Dirichlet conditions on the remainder of the boundary.             ',&
'                                                                                ',&
'                            u  = 0                                              ',&
'                             n                                                  ',&
'                                                                                ',&
'                        -------------                                           ',&
'                       |             .                                          ',&
'                       |             .                                          ',&
'                       |              .                                         ',&
'                       |               .  u = 1                                 ',&
'                       |                 .                                      ',&
'                       |                    .                                   ',&
'                       |                       .                                ',&
'                u = 0  |                        |                               ',&
'                       |                        |                               ',&
'                       |                        |                               ',&
'                       |                        |  u = 1                        ',&
'                       |                        |                               ',&
'                       |                        |                               ',&
'                       |                        |                               ',&
'                        ------------------------                                ',&
'                                                                                ',&
'                                 u  = 0                                         ',&
'                                  n                                             ',&
'                                                                                ',&
'   The effective conductivity of an medium is then given by the integral        ',&
'   along the left edge,                                                         ',&
'                                                                                ',&
'                               1                                                ',&
'                    sigma = integral  u (0,y) dy                                ',&
'                              0        n                                        ',&
'                                                                                ',&
'   It is of interest to study the relation between the radius rho and           ',&
'   the conductivity sigma. In particular, as rho approaches one, sigma          ',&
'   becomes infinite.                                                            ',&
'                                                                                ',&
'   Keller and Sachs use a finite difference approximation. The following        ',&
'   technique makes use of the fact that the equation is actually Laplace''s     ',&
'   equation and leads to a much smaller matrix problem to solve.                ',&
'                                                                                ',&
'        Consider an approximate solution of the form                            ',&
'                                                                                ',&
'                    n      2j-1                                                 ',&
'              u =  sum  c r    cos(2j-1)t                                       ',&
'                   j=1   j                                                      ',&
'                                                                                ',&
'   where r,t are polar coordinates (t is theta). The coefficients are           ',&
'   to be determined. For any set of coefficients, this function already         ',&
'   satisfies the differential equation because the basis functions are          ',&
'   harmonic; it satisfies the normal derivative boundary condition on           ',&
'   the bottom edge of the domain because we used cos t in preference to         ',&
'   sin t ; and it satisfies the boundary condition on the left edge of          ',&
'   the domain because we use only odd multiples of t .                          ',&
'                                                                                ',&
'   The computational task is to find coefficients so that the boundary          ',&
'   conditions on the remaining edges are satisfied as well as possible. To      ',&
'   accomplish this, pick m points (r,t) on the remaining edges. It is           ',&
'   desirable to have m > n and in practice we usually choose m to be two        ',&
'   or three times as large as n .  Typical values of n are 10 or 20 and         ',&
'   of m are 20 to 60. An m by n matrix A is generated. The i,j element          ',&
'   is the j-th basis function, or its normal derivative, evaluated at           ',&
'   the i-th boundary point. A right hand side with m components is also         ',&
'   generated. In this example, the elements of the right hand side are          ',&
'   either zero or one. The coefficients are then found by solving the           ',&
'   overdetermined set of equations                                              ',&
'                                                                                ',&
'               Ac = b                                                           ',&
'                                                                                ',&
'   in a least squares sense.                                                    ',&
'                                                                                ',&
'   Once the coefficients have been determined, the approximate solution         ',&
'   is defined everywhere on the domain. It is then possible to compute the      ',&
'   effective conductivity sigma . In fact, a very simple formula results,       ',&
'                                                                                ',&
'                        n       j-1                                             ',&
'              sigma =  sum  (-1)   c                                            ',&
'                       j=1          j                                           ',&
'                                                                                ',&
'   To use MAT88 for this problem, the following "program" is first stored       ',&
'   in the local computer file system, say under the name "PDE".                 ',&
'                                                                                ',&
'      //Conductivity example.                                                   ',&
'      //Parameters ---                                                          ',&
'         rho       //radius of cylindrical inclusion                            ',&
'         n         //number of terms in solution                                ',&
'         m         //number of boundary points                                  ',&
'      //initialize operation counter                                            ',&
'         flop = <0 0>;                                                          ',&
'      //initialize variables                                                    ',&
'         m1 = round(m/3);   //number of points on each straight edge            ',&
'         m2 = m - m1;       //number of points with Dirichlet conditions        ',&
'         pi = 4*atan(1);                                                        ',&
'      //generate points in Cartesian coordinates                                ',&
'         //right hand edge                                                      ',&
'         for i = 1:m1, x(i) = 1; y(i) = (1-rho)*(i-1)/(m1-1);                   ',&
'         //top edge                                                             ',&
'         for i = m2+1:m, x(i) = (1-rho)*(m-i)/(m-m2-1); y(i) = 1;               ',&
'         //circular edge                                                        ',&
'         for i = m1+1:m2, t = pi/2*(i-m1)/(m2-m1+1); ...                        ',&
'            x(i) = 1-rho*sin(t);  y(i) = 1-rho*cos(t);                          ',&
'      //convert to polar coordinates                                            ',&
'         for i = 1:m-1, th(i) = atan(y(i)/x(i));  ...                           ',&
'            r(i) = sqrt(x(i)**2+y(i)**2);                                       ',&
'         th(m) = pi/2;  r(m) = 1;                                               ',&
'      //generate matrix                                                         ',&
'         //Dirichlet conditions                                                 ',&
'         for i = 1:m2, for j = 1:n, k = 2*j-1; ...                              ',&
'            a(i,j) = r(i)**k*cos(k*th(i));                                      ',&
'         //Neumann conditions                                                   ',&
'         for i = m2+1:m, for j = 1:n, k = 2*j-1; ...                            ',&
'            a(i,j) = k*r(i)**(k-1)*sin((k-1)*th(i));                            ',&
'      //generate right hand side                                                ',&
'         for i = 1:m2, b(i) = 1;                                                ',&
'         for i = m2+1:m, b(i) = 0;                                              ',&
'      //solve for coefficients                                                  ',&
'         c = A\b                                                                ',&
'      //compute effective conductivity                                          ',&
'         c(2:2:n) = -c(2:2:n);                                                  ',&
'         sigma = sum(c)                                                         ',&
'      //output total operation count                                            ',&
'         ops = flop(2)                                                          ',&
'                                                                                ',&
'   The program can be used within MAT88 by setting the three parameters         ',&
'   and then accessing the file. For example,                                    ',&
'                                                                                ',&
'      rho = .9;                                                                 ',&
'      n = 15;                                                                   ',&
'      m = 30;                                                                   ',&
'      exec(''PDE'')                                                             ',&
'                                                                                ',&
'   The resulting output is                                                      ',&
'                                                                                ',&
'      rho   =                                                                   ',&
'                                                                                ',&
'         .9000                                                                  ',&
'                                                                                ',&
'      n     =                                                                   ',&
'                                                                                ',&
'       15.                                                                      ',&
'                                                                                ',&
'      m     =                                                                   ',&
'                                                                                ',&
'       30.                                                                      ',&
'                                                                                ',&
'      c     =                                                                   ',&
'                                                                                ',&
'         2.2275                                                                 ',&
'        -2.2724                                                                 ',&
'         1.1448                                                                 ',&
'         0.1455                                                                 ',&
'        -0.1678                                                                 ',&
'        -0.0005                                                                 ',&
'        -0.3785                                                                 ',&
'         0.2299                                                                 ',&
'         0.3228                                                                 ',&
'        -0.2242                                                                 ',&
'        -0.1311                                                                 ',&
'         0.0924                                                                 ',&
'         0.0310                                                                 ',&
'        -0.0154                                                                 ',&
'        -0.0038                                                                 ',&
'                                                                                ',&
'      sigm  =                                                                   ',&
'                                                                                ',&
'         5.0895                                                                 ',&
'                                                                                ',&
'      ops   =                                                                   ',&
'                                                                                ',&
'         16204.                                                                 ',&
'                                                                                ',&
'   A total of 16204 floating point operations were necessary to set up the      ',&
'   matrix, solve for the coefficients and compute the conductivity. The         ',&
'   operation count is roughly proportional to m*n**2. The results obtained      ',&
'   for sigma as a function of rho by this approach are essentially the          ',&
'   same as those obtained by the finite difference technique of Keller          ',&
'   and Sachs, but the computational effort involved is much less.               ',&
'                                                                                ',&
'================================================================================',&
'EIGENVALUE SENSITIVITY EXAMPLE                                                  ',&
'                                                                                ',&
'   In this example, we construct a matrix whose eigenvalues are moderately      ',&
'   sensitive to perturbations and then analyze that sensitivity. We             ',&
'   begin with the statement                                                     ',&
'                                                                                ',&
'      B = <3 0 7; 0 2 0; 0 0 1>                                                 ',&
'                                                                                ',&
'   which produces                                                               ',&
'                                                                                ',&
'      B     =                                                                   ',&
'                                                                                ',&
'          3.    0.    7.                                                        ',&
'          0.    2.    0.                                                        ',&
'          0.    0.    1.                                                        ',&
'                                                                                ',&
'   Obviously, the eigenvalues of B are 1, 2 and 3 . Moreover, since             ',&
'   B is not symmetric, these eigenvalues are slightly sensitive to              ',&
'   perturbation. (The value b(1,3) = 7 was chosen so that the elements          ',&
'   of the matrix A below are less than 1000.)                                   ',&
'                                                                                ',&
'   We now generate a similarity transformation to disguise the eigenvalues      ',&
'   and make them more sensitive.                                                ',&
'                                                                                ',&
'      L = <1 0 0; 2 1 0; -3 4 1>, M = L\L''                                     ',&
'                                                                                ',&
'      L     =                                                                   ',&
'                                                                                ',&
'          1.    0.    0.                                                        ',&
'          2.    1.    0.                                                        ',&
'         -3.    4.    1.                                                        ',&
'                                                                                ',&
'      M     =                                                                   ',&
'                                                                                ',&
'          1.0000    2.0000   -3.0000                                            ',&
'         -2.0000   -3.0000   10.0000                                            ',&
'         11.0000   18.0000  -48.0000                                            ',&
'                                                                                ',&
'   The matrix M has determinant equal to 1 and is moderately badly              ',&
'   conditioned. The similarity transformation is                                ',&
'                                                                                ',&
'      A = M*B/M                                                                 ',&
'                                                                                ',&
'      A     =                                                                   ',&
'                                                                                ',&
'        -64.0000   82.0000   21.0000                                            ',&
'        144.0000 -178.0000  -46.0000                                            ',&
'       -771.0000  962.0000  248.0000                                            ',&
'                                                                                ',&
'   Because det(M) = 1 , the elements of A would be exact integers               ',&
'   if there were no roundoff. So,                                               ',&
'                                                                                ',&
'      A = round(A)                                                              ',&
'                                                                                ',&
'      A     =                                                                   ',&
'                                                                                ',&
'        -64.   82.   21.                                                        ',&
'        144. -178.  -46.                                                        ',&
'       -771.  962.  248.                                                        ',&
'                                                                                ',&
'   This, then, is our test matrix. We can now forget how it                     ',&
'   was generated and analyze its eigenvalues.                                   ',&
'                                                                                ',&
'      <X,D> = eig(A)                                                            ',&
'                                                                                ',&
'      D     =                                                                   ',&
'                                                                                ',&
'          3.0000    0.0000    0.0000                                            ',&
'          0.0000    1.0000    0.0000                                            ',&
'          0.0000    0.0000    2.0000                                            ',&
'                                                                                ',&
'      X     =                                                                   ',&
'                                                                                ',&
'          -.0891    3.4903   41.8091                                            ',&
'           .1782   -9.1284  -62.7136                                            ',&
'          -.9800   46.4473  376.2818                                            ',&
'                                                                                ',&
'   Since A is similar to B, its eigenvalues are also 1, 2 and 3.  They          ',&
'   happen to be computed in another order by the EISPACK subroutines. The       ',&
'   fact that the columns of X, which are the eigenvectors, are so far           ',&
'   from being orthonormal is our first indication that the eigenvalues          ',&
'   are sensitive. To see this sensitivity, we display more figures of           ',&
'   the computed eigenvalues.                                                    ',&
'                                                                                ',&
'      long, diag(D)                                                             ',&
'                                                                                ',&
'      ans   =                                                                   ',&
'                                                                                ',&
'         2.999999999973599                                                      ',&
'         1.000000000015625                                                      ',&
'         2.000000000011505                                                      ',&
'                                                                                ',&
'   We see that, on this computer, the last five significant figures are         ',&
'   contaminated by roundoff error. A somewhat superficial explanation           ',&
'   of this is provided by                                                       ',&
'                                                                                ',&
'      short,  cond(X)                                                           ',&
'                                                                                ',&
'      ans   =                                                                   ',&
'                                                                                ',&
'         3.2216e+05                                                             ',&
'                                                                                ',&
'   The condition number of X gives an upper bound for the relative              ',&
'   error in the computed eigenvalues. However, this condition                   ',&
'   number is affected by scaling.                                               ',&
'                                                                                ',&
'      X = X/diag(X(3,:)),  cond(X)                                              ',&
'                                                                                ',&
'      X     =                                                                   ',&
'                                                                                ',&
'           .0909     .0751     .1111                                            ',&
'          -.1818    -.1965    -.1667                                            ',&
'          1.0000    1.0000    1.0000                                            ',&
'                                                                                ',&
'      ans   =                                                                   ',&
'                                                                                ',&
'         1.7692e+03                                                             ',&
'                                                                                ',&
'   Rescaling the eigenvectors so that their last components are all             ',&
'   equal to one has two consequences. The condition of X is decreased           ',&
'   by over two orders of magnitude. (This is about the minimum condition        ',&
'   that can be obtained by such diagonal scaling.)  Moreover, it is now         ',&
'   apparent that the three eigenvectors are nearly parallel.                    ',&
'                                                                                ',&
'   More detailed information on the sensitivity of the individual               ',&
'   eigenvalues involves the left eigenvectors.                                  ',&
'                                                                                ',&
'      Y = inv(X''),  Y''*A*X                                                    ',&
'                                                                                ',&
'      Y     =                                                                   ',&
'                                                                                ',&
'       -511.5000  259.5000  252.0000                                            ',&
'        616.0000 -346.0000 -270.0000                                            ',&
'        159.5000  -86.5000  -72.0000                                            ',&
'                                                                                ',&
'      ans   =                                                                   ',&
'                                                                                ',&
'          3.0000     .0000     .0000                                            ',&
'           .0000    1.0000     .0000                                            ',&
'           .0000     .0000    2.0000                                            ',&
'                                                                                ',&
'   We are now in a position to compute the sensitivities of the individual      ',&
'   eigenvalues.                                                                 ',&
'                                                                                ',&
'      for j = 1:3, c(j) = norm(Y(:,j))*norm(X(:,j)); end,  C                    ',&
'                                                                                ',&
'      C     =                                                                   ',&
'                                                                                ',&
'        833.1092                                                                ',&
'        450.7228                                                                ',&
'        383.7564                                                                ',&
'                                                                                ',&
'   These three numbers are the reciprocals of the cosines of the                ',&
'   angles between the left and right eigenvectors. It can be shown that         ',&
'   perturbation of the elements of A can result in a perturbation of            ',&
'   the j-th eigenvalue which is c(j) times as large.  In this example,          ',&
'   the first eigenvalue has the largest sensitivity.                            ',&
'                                                                                ',&
'   We now proceed to show that A is close to a matrix with a double             ',&
'   eigenvalue. The direction of the required perturbation is given by           ',&
'                                                                                ',&
'      E = -1.e-6*Y(:,1)*X(:,1)''                                                ',&
'                                                                                ',&
'      E     =                                                                   ',&
'                                                                                ',&
'         1.0e-03 *                                                              ',&
'                                                                                ',&
'           .0465    -.0930     .5115                                            ',&
'          -.0560     .1120    -.6160                                            ',&
'          -.0145     .0290    -.1595                                            ',&
'                                                                                ',&
'   With some trial and error which we do not show, we bracket the               ',&
'   point where two eigenvalues of a perturbed A coalesce and then               ',&
'   become complex.                                                              ',&
'                                                                                ',&
'      eig(A + .4*E),  eig(A + .5*E)                                             ',&
'                                                                                ',&
'      ans   =                                                                   ',&
'                                                                                ',&
'          1.1500                                                                ',&
'          2.5996                                                                ',&
'          2.2504                                                                ',&
'                                                                                ',&
'      ans   =                                                                   ',&
'                                                                                ',&
'         2.4067 +  .1753*i                                                      ',&
'         2.4067 -  .1753*i                                                      ',&
'         1.1866 + 0.0000*i                                                      ',&
'                                                                                ',&
'   Now, a bisecting search, driven by the imaginary part of one of              ',&
'   the eigenvalues, finds the point where two eigenvalues are nearly            ',&
'   equal.                                                                       ',&
'                                                                                ',&
'      r = .4;  s = .5;                                                          ',&
'                                                                                ',&
'      while s-r > 1.e-14, t = (r+s)/2; d = eig(A+t*E); ...                      ',&
'        if imag(d(1))=0, r = t; else, s = t;                                    ',&
'                                                                                ',&
'      long,  T                                                                  ',&
'                                                                                ',&
'      T     =                                                                   ',&
'                                                                                ',&
'           .450380734134507                                                     ',&
'                                                                                ',&
'   Finally, we display the perturbed matrix, which is obviously close           ',&
'   to the original, and its pair of nearly equal eigenvalues.  (We have         ',&
'   dropped a few digits from the long output.)                                  ',&
'                                                                                ',&
'      A+t*E,  eig(A+t*E)                                                        ',&
'                                                                                ',&
'      A                                                                         ',&
'                                                                                ',&
'       -63.999979057   81.999958114   21.000230369                              ',&
'       143.999974778 -177.999949557  -46.000277434                              ',&
'      -771.000006530  962.000013061  247.999928164                              ',&
'                                                                                ',&
'      ans   =                                                                   ',&
'                                                                                ',&
'         2.415741150                                                            ',&
'         2.415740621                                                            ',&
'         1.168517777                                                            ',&
'                                                                                ',&
'   The first two eigenvectors of A + t*E are almost indistinguishable           ',&
'   indicating that the perturbed matrix is almost defective.                    ',&
'                                                                                ',&
'      <X,D> = eig(A+t*E);  X = X/diag(X(3,:))                                   ',&
'                                                                                ',&
'      X     =                                                                   ',&
'                                                                                ',&
'          .096019578     .096019586    .071608466                               ',&
'         -.178329614    -.178329608   -.199190520                               ',&
'         1.000000000    1.000000000   1.000000000                               ',&
'                                                                                ',&
'      short,  cond(X)                                                           ',&
'                                                                                ',&
'      ans   =                                                                   ',&
'                                                                                ',&
'         3.3997e+09                                                             ',&
'                                                                                ',&
'================================================================================',&
'COMMUNICATING WITH OTHER PROGRAMS                                               ',&
'                                                                                ',&
'   There are four different ways MAT88 can be used in                           ',&
'   conjunction with other programs:                                             ',&
'                                                                                ',&
'         -- user,                                                               ',&
'         -- exec,                                                               ',&
'         -- save and load,                                                      ',&
'         -- MATZ, call and quit .                                               ',&
'                                                                                ',&
'   Let us illustrate each of these by the following simple                      ',&
'   example.                                                                     ',&
'                                                                                ',&
'         n = 6                                                                  ',&
'         for i = 1:n, for j = 1:n, a(i,j) = abs(i-j);                           ',&
'         A                                                                      ',&
'         X = inv(A)                                                             ',&
'                                                                                ',&
'   The example A could be introduced into MAT88 by writing                      ',&
'   the following Fortran subroutine.                                            ',&
'                                                                                ',&
'            SUBROUTINE mat88_user(A,M,N,S,T)                                    ',&
'            DOUBLEPRECISION A(*),S,T                                            ',&
'            N = int(A(1))                                                       ',&
'            M = N                                                               ',&
'            DO J = 1, N                                                         ',&
'               DO I = 1, N                                                      ',&
'                  K = I + (J-1)*M                                               ',&
'                  A(K) = IABS(I-J)                                              ',&
'               enddo                                                            ',&
'            enddo                                                               ',&
'            END SUBROUTINE USER                                                 ',&
'                                                                                ',&
'   This subroutine should be compiled and linked into MAT88 in                  ',&
'   place of the original version of USER. Then the MAT88                        ',&
'   statements                                                                   ',&
'                                                                                ',&
'         n = 6                                                                  ',&
'         A = user(n)                                                            ',&
'         X = inv(A)                                                             ',&
'                                                                                ',&
'   do the job.                                                                  ',&
'                                                                                ',&
'   The example A could be generated by storing the following                    ',&
'   text in a file named, say, EXAMPLE .                                         ',&
'                                                                                ',&
'         for i = 1:n, for j = 1:n, a(i,j) = abs(i-j);                           ',&
'                                                                                ',&
'   Then the MAT88 statements                                                    ',&
'                                                                                ',&
'         n = 6                                                                  ',&
'         exec(''EXAMPLE'',0)                                                    ',&
'         X = inv(A)                                                             ',&
'                                                                                ',&
'   have the desired effect. The 0 as the optional second parameter              ',&
'   of exec indicates that the text in the file should not be printed            ',&
'   on the terminal.                                                             ',&
'                                                                                ',&
'   The matrices A and X could also be stored in files. Two                      ',&
'   separate main programs would be involved. The first is:                      ',&
'                                                                                ',&
'            PROGRAM MAINA                                                       ',&
'            DOUBLEPRECISION A(10,10)                                            ',&
'            N = 6                                                               ',&
'            DO J = 1, N                                                         ',&
'               DO I = 1, N                                                      ',&
'                  A(I,J) = IABS(I-J)                                            ',&
'               enddo                                                            ',&
'            enddo                                                               ',&
'            OPEN(UNIT=1,FILE=''A'')                                             ',&
'            WRITE(1,101) N,N                                                    ',&
'        101 FORMAT(''A   '',2I4)                                                ',&
'            DO J = 1, N                                                         ',&
'               WRITE(1,102) (A(I,J),I=1,N)                                      ',&
'            enddo                                                               ',&
'        102 FORMAT(4Z18)                                                        ',&
'            END PROGRAM MAINA                                                   ',&
'                                                                                ',&
'   The OPEN statement may take different forms on different systems.            ',&
'   It attaches Fortran logical unit number 1 to the file named A.               ',&
'   The FORMAT number 102 may also be system dependent. This                     ',&
'   particular one is appropriate for hexadecimal computers with an 8            ',&
'   byte double precision floating point word. Check, or modify,                 ',&
'   MAT88 subroutine SAVLOD.                                                     ',&
'                                                                                ',&
'   After this program is executed, enter MAT88 and give the                     ',&
'   following statements:                                                        ',&
'                                                                                ',&
'         load(''A'')                                                            ',&
'         X = inv(A)                                                             ',&
'         save(''X'',X)                                                          ',&
'                                                                                ',&
'   If all goes according to plan, this will read the matrix A from              ',&
'   the file A, invert it, store the inverse in X and then write the             ',&
'   matrix X on the file X. The following program can then access X.             ',&
'                                                                                ',&
'            PROGRAM MAINX                                                       ',&
'            DOUBLEPRECISION X(10,10)                                            ',&
'            OPEN(UNIT=1,FILE=''X'')                                             ',&
'            REWIND 1                                                            ',&
'            READ (1,101) ID,M,N                                                 ',&
'        101 FORMAT(A32,2I4)                                                     ',&
'            DO J = 1, N                                                         ',&
'               READ(1,102) (X(I,J),I=1,M)                                       ',&
'            ENDDO                                                               ',&
'        102 FORMAT(4Z18)                                                        ',&
'            ...                                                                 ',&
'            ...                                                                 ',&
'                                                                                ',&
'   The most elaborate mechanism involves using MAT88 as a subroutine            ',&
'   within another program. Communication with the MAT88 stack is                ',&
'   accomplished using subroutine MATZ which is distributed with MAT88,          ',&
'   but which is not used by MAT88 itself. The preamble of MATZ is:              ',&
'                                                                                ',&
'         SUBROUTINE MATZ(A,LDA,M,N,ID,JOB,IERR)                                 ',&
'         INTEGER LDA,M,N,JOB,IERR                                               ',&
'         character(len=*) :: id                                                 ',&
'         DOUBLEPRECISION A(LDA,N)                                               ',&
'                                                                                ',&
'         ! ACCESS MAT88 VARIABLE STACK                                          ',&
'         ! A IS AN M BY N MATRIX, STORED IN AN ARRAY WITH                       ',&
'         !     LEADING DIMENSION LDA.                                           ',&
'         ! ID IS THE NAME OF A. ID IS UP TO FOUR CHARACTERS.                    ',&
'         ! JOB =  0  GET REAL A FROM MAT88,                                     ',&
'         !     =  1  PUT REAL A INTO MAT88,                                     ',&
'         !     = 10  GET IMAG PART OF A FROM MAT88,                             ',&
'         !     = 11  PUT IMAG PART OF A INTO MAT88.                             ',&
'         ! RETURN WITH NONZERO IERR AFTER MAT88 ERROR MESSAGE.                  ',&
'         !                                                                      ',&
'         ! USES MAT88 ROUTINES STACKG, STACKP AND ERROR                         ',&
'                                                                                ',&
'        The preamble of subroutine MAT88 is:                                    ',&
'                                                                                ',&
'         SUBROUTINE MAT88(INIT)                                                 ',&
'         ! INIT = 0 FOR FIRST ENTRY, NONZERO FOR SUBSEQUENT ENTRIES             ',&
'                                                                                ',&
'        To do our example, write the following program:                         ',&
'                                                                                ',&
'            DOUBLEPRECISION A(10,10),X(10,10)                                   ',&
'            DATA LDA/10/                                                        ',&
'            call M_88(0,'''')                                                   ',&
'            N = 6                                                               ',&
'            DO J = 1, N                                                         ',&
'               DO I = 1, N                                                      ',&
'                  A(I,J) = IABS(I-J)                                            ',&
'               enddo                                                            ',&
'            enddo                                                               ',&
'            call MATZ(A,LDA,N,N,''A'',1,IERR)                                   ',&
'            IF (IERR .NE. 0) GO TO ...                                          ',&
'            call MAT88(1,'''')                                                  ',&
'            call MATZ(X,LDA,N,N,''X'',0,IERR)                                   ',&
'            IF (IERR .NE. 0) GO TO ...                                          ',&
'            ...                                                                 ',&
'            ...                                                                 ',&
'                                                                                ',&
'   When this program is executed, the call to MAT88(0) produces the             ',&
'   MAT88 greeting, then waits for input. The command                            ',&
'                                                                                ',&
'            quit                                                                ',&
'                                                                                ',&
'   sends control back to our example program. The matrix A is                   ',&
'   generated by the program and sent to the stack by the first call             ',&
'   to MATZ. The call to MAT88(1) produces the MAT88(1) prompt. Then             ',&
'   the statements                                                               ',&
'                                                                                ',&
'            X = inv(A)                                                          ',&
'            quit                                                                ',&
'                                                                                ',&
'   will invert our matrix, put the result on the stack and go back              ',&
'   to our program. The second call to MATZ will retrieve X .                    ',&
'                                                                                ',&
'   By the way, this matrix X is interesting. Take a look at                     ',&
'   round(2*(n-1)*X).                                                            ',&
'                                                                                ',&
'================================================================================',&
'ACKNOWLEDGEMENT                                                                 ',&
'                                                                                ',&
'   Most of the work on MAT88 has been carried out at the University             ',&
'   of New Mexico, where it is being supported by the National Science           ',&
'   Foundation. Additional work has been done during visits to Stanford          ',&
'   Linear Accelerator Center, Argonne National Laboratory and Los Alamos        ',&
'   Scientific Laboratory, where support has been provided by NSF and            ',&
'   the Department of Energy.                                                    ',&
'                                                                                ',&
'================================================================================',&
'REFERENCES                                                                      ',&
'                                                                                ',&
' [1]  J. J. Dongarra, J. R. Bunch, C. B. Moler and G. W. Stewart,               ',&
'      LINPACK Users'' Guide, Society for Industrial and Applied                 ',&
'      Mathematics, Philadelphia, 1979.                                          ',&
'                                                                                ',&
' [2]  B. T. Smith, J. M. Boyle, J. J. Dongarra, B. S. Garbow, Y.                ',&
'      Ikebe, V. C. Klema, C. B. Moler, Matrix Eigensystem Routines              ',&
'      -- EISPACK Guide, Lecture Notes in Computer Science, volume               ',&
'      6, second edition, Springer-Verlag, 1976.                                 ',&
'                                                                                ',&
' [3]  B. S. Garbow, J. M. Boyle, J. J. Dongarra, C. B. Moler,                   ',&
'      Matrix Eigensystem Routines -- EISPACK Guide Extension,                   ',&
'      Lecture Notes in Computer Science, volume 51, Springer-                   ',&
'      Verlag, 1977.                                                             ',&
'                                                                                ',&
' [4]  S. Cohen and S. Piper, SPEAKEASY III Reference Manual,                    ',&
'      Speakeasy Computing Corp., Chicago, Ill., 1979.                           ',&
'                                                                                ',&
' [5]  J. H. Wilkinson and C. Reinsch, Handbook for Automatic                    ',&
'      Computation, volume II, Linear Algebra, Springer-Verlag,                  ',&
'     1971.                                                                      ',&
'                                                                                ',&
' [6]  Niklaus Wirth, Algorithms + Data Structures = Programs,                   ',&
'      Prentice-Hall, 1976.                                                      ',&
'                                                                                ',&
' [7]  H. B. Keller and D. Sachs, "Calculations of the Conductivity              ',&
'      of a Medium Containing Cylindrical Inclusions", J. Applied                ',&
'      Physics 35, 537-538, 1964.                                                ',&
'                                                                                ',&
' [8]  C. B. Moler and C. F. Van Loan, Nineteen Dubious Ways to                  ',&
'      Compute the Exponential of a Matrix, SIAM Review 20, 801-                 ',&
'      836, 1979.                                                                ',&
'                                                                                ',&
' [9]  G. E. Forsythe, M. A. Malcolm and C. B. Moler, Computer                   ',&
'      Methods for Mathematical Computations, Prentice-Hall, 1977.               ',&
'                                                                                ',&
' [10] C. B. Moler and D. R. Morrison, "Replacing square roots by                ',&
'      Pythagorean sums", University of New Mexico, Computer                     ',&
'      Science Department, technical report, submitted for                       ',&
'     publication, 1980.                                                         ',&
'                                                                                ',&
'================================================================================',&
'SUMMARY    A list of basic (case-sensitive) section and topic names             ',&
'   .______________._________________________________________________________.   ',&
'   |SYNTAX        | [ ] < > ( ) = .  , !  ; \ / '''' + - * : semi             | ',&
'   |______________._________________________________________________________|   ',&
'   |VARIABLES     | ans    clear who                                        |   ',&
'   |______________._________________________________________________________|   ',&
'   |BASIC         | atan   cos   exp    log    sin      sqrt                |   ',&
'   |______________._________________________________________________________|   ',&
'   |HIGH          | abs    base  chol   chop   cond     conjg  det    diag  |   ',&
'   |              | eig    eye   hess   invh   imag     inv    kron   lu    |   ',&
'   |              | magic  norm  ones   orth   pinv     poly   prod   qr    |   ',&
'   |              | rand   rank  rcond  rat    real     rref   roots  round |   ',&
'   |              | schur  size  sum    svd    tril     triu   user   zeros |   ',&
'   |______________._________________________________________________________|   ',&
'   |FLOW control  | else   end   if     for    while    exit   quit         |   ',&
'   |______________._________________________________________________________|   ',&
'   |FILE access   | exec   load  print  save                                |   ',&
'   |______________._________________________________________________________|   ',&
'   |OUTPUT options| lines  long  short  diary  display  plot                |   ',&
'   |______________._________________________________________________________|   ',&
'   |DOCUMENTATION | help   manual topics NEWS                               |   ',&
'   |______________._________________________________________________________|   ',&
'   |MISCELLANEOUS | char   eps   debug  what   sh       MACROS  EDIT  flops |   ',&
'   |______________._________________________________________________________|   ',&
'================================================================================',&
'SAMPLE                                                                          ',&
'      Here are a few sample statements:                                         ',&
'                                                                                ',&
'       A = <1 2; 3 4>                                                           ',&
'       b = <5 6>''                                                              ',&
'       x = A\b                                                                  ',&
'       <V,D> = eig(A),  norm(A-V*D/V)                                           ',&
'       help \ , help eig                                                        ',&
'       exec(''demo'',7)                                                         ',&
'                                                                                ',&
'      For more information, generate the MAT88 Users'' Guide                    ',&
'      using                                                                     ',&
'                                                                                ',&
'        help manual                                                             ',&
'        w help.txt                                                              ',&
'        q                                                                       ',&
'================================================================================',&
'DOCUMENTATION                                                                   ',&
'                                                                                ',&
'help  topic|SECTION_NAME                                                        ',&
'                                                                                ',&
'      "help" gives assistance. It is equivalent to "help SUMMARY"               ',&
'      by default.                                                               ',&
'                                                                                ',&
'      o  "help" with no options lists common topic and section names.           ',&
'      o  The special topic "topics" shows all topic lines.                      ',&
'      o  The special topic "manual" displays all the help text.                 ',&
'         Enter "h" at the "continue ..." prompt for additional options.         ',&
'                                                                                ',&
'      For example:                                                              ',&
'                                                                                ',&
'         help        // a list of common topics and section names               ',&
'         help topics // a list of topics including the first line of            ',&
'                     // the topic.                                              ',&
'         help abs    // produces help on the function "abs".                    ',&
'         help FLOW   // the entire section on flow control is displayed.        ',&
'         help manual // show all the help text                                  ',&
'         help help   // obviously prints this message.                          ',&
'                                                                                ',&
'      Alternatively, To place all the documenation in a file, use               ',&
'      "help manual" and enter "w help.txt" at the "continue .." prompt.         ',&
'NEWS                                                                            ',&
'      MAT88 is intended to be used primarily by families of FORTRAN             ',&
'      programs that wish to add a consistent interactive "calculator"           ',&
'      mode for interactively inspecting and modifying data.                     ',&
'                                                                                ',&
'      May, 1981.                                                                ',&
'                                                                                ',&
'      This is a port of the Argonne National Lab. FORTRAN 77 MATLAB             ',&
'      routine circa 1981.                                                       ',&
'                                                                                ',&
'      Mar, 1990.                                                                ',&
'                                                                                ',&
'      Input lines can now be recalled and edited.  A "!!" on a line by          ',&
'      itself calls the command history mode. Enter "?" after entering           ',&
'      the mode for details.                                                     ',&
'                                                                                ',&
'      Apr, 2021.                                                                ',&
'                                                                                ',&
'      Rewritten but largely true to the original documentation.                 ',&
'                                                                                ',&
'what  does nothing for now                                                      ',&
'                                                                                ',&
'sh    Starts the command shell interactively, using the command defined by      ',&
'      the environment variable SHELL. Note that in addition any line            ',&
'      starting with an exclamation (!) is passed to the system for              ',&
'      execution.                                                                ',&
'================================================================================',&
'SYNTAX                                                                          ',&
'[     See "<"                                                                   ',&
']     See "<"                                                                   ',&
'>     See "<" . Also see MACROS.                                                ',&
'<     < > or [ ] are brackets used in forming vectors and matrices.             ',&
'      "<6.9 9.64 sqrt(-1)>" is a vector with three elements separated by        ',&
'      blanks. "[1+I 2-I 3]" and "[1 +I 2 -I 3]" are not the same. The           ',&
'      first has three elements, the second has five.  <11 12 13; 21 22          ',&
'      23> is a 2 by 3 matrix. The semicolon ends the first row.                 ',&
'                                                                                ',&
'      Vectors and matrices can be used inside < > brackets.  <A B; C>           ',&
'      is allowed if the number of rows of A equals the number of rows           ',&
'      of B and the number of columns of A plus the number of columns of         ',&
'      B equals the number of columns of C. This rule generalizes in a           ',&
'      hopefully obvious way to allow fairly complicated constructions.          ',&
'                                                                                ',&
'      A = < > stores an empty matrix in A, thereby removing it from the         ',&
'      list of current variables.                                                ',&
'                                                                                ',&
'      For the use of < and > on the left of the = in multiple assignment        ',&
'      statements, See "lu", "eig", "svd" and so on.                             ',&
'                                                                                ',&
'      In "while" and "if" clauses, "<>" means less than or greater than,        ',&
'      i.e. not equal, "<" means less than, ">" means greater than,              ',&
'      "<=" means less than or equal, ">=" means greater than or equal.          ',&
'                                                                                ',&
'      For the use of ">" and "<" to delineate macros, see MACROS.               ',&
'                                                                                ',&
'{     see "(".                                                                  ',&
'}     see "(".                                                                  ',&
')     See "(" .                                                                 ',&
'(     ( ) or { } are used to indicate precedence in arithmetic expressions      ',&
'      and to enclose arguments of functions in the usual way. They are          ',&
'      also used to enclose subscripts of vectors and matrices in a manner       ',&
'      somewhat more general than the usual way. If X and V are vectors,         ',&
'      then X(V) is <X(V(1)), X(V(2)), ..., X(V(N))>. The components of V        ',&
'      are rounded to nearest integers and used as subscripts. An error          ',&
'      occurs if any such subscript is less than 1 or greater than the           ',&
'      dimension of X. Some examples:                                            ',&
'                                                                                ',&
'         X(3) is the third element of X .                                       ',&
'         X([1 2 3]) is the first three elements of X. So is                     ',&
'         X([sqrt(2), sqrt(3), 4*atan(1)]) .                                     ',&
'         If X has N components, X(N:-1:1) reverses them.                        ',&
'                                                                                ',&
'      The same indirect subscripting is used in matrices. If V has              ',&
'      M components and W has N components, then A(V,W) is the M by N            ',&
'      matrix formed from the elements of A whose subscripts are the             ',&
'      elements of V and W. For example...  A(<1,5>,:) = A(<5,1>,:)              ',&
'      interchanges rows 1 and 5 of A.                                           ',&
'                                                                                ',&
'=     Used in assignment statements and to mean equality in "while"             ',&
'      and "if" clauses.                                                         ',&
'                                                                                ',&
'.     Decimal point. 314/100, 3.14 and .314E1 are all the                       ',&
'      same.                                                                     ',&
'                                                                                ',&
'      Element-by-element multiplicative operations are obtained                 ',&
'      using .* , ./ , or .\ . For example, C = A ./ B is the                    ',&
'      matrix with elements c(i,j) = a(i,j)/b(i,j) .                             ',&
'                                                                                ',&
'      Kronecker tensor products and quotients are obtained with                 ',&
'      .*. , ./. and .\. . See "kron".                                           ',&
'                                                                                ',&
'      Two or more points at the end of the line indicate                        ',&
'      continuation. The total line length limit is 1024                         ',&
'      characters.                                                               ',&
'                                                                                ',&
',     Used to separate matrix subscripts and function arguments.                ',&
'      Used at the end of "for", "while" and "if" clauses. Used to               ',&
'      separate statements in multi-statement lines. In this                     ',&
'      situation, it may be replaced by a semicolon to suppress                  ',&
'      printing.                                                                 ',&
'                                                                                ',&
'!     If the line begins with two exclamations command history                  ',&
'      mode is entered. The rest of the line is treated as an optional           ',&
'      initial history edit command. Enter "!!?" to enter history mode           ',&
'      and then display additional instructions.                                 ',&
'                                                                                ',&
'      Otherwise if the exclamation is the first character of a line the         ',&
'      rest of the line is passed to the system to be executed.                  ',&
'                                                                                ',&
'      Examples:                                                                 ',&
'                                                                                ',&
'         // enter command history mode and change all occurrences of            ',&
'         // "abc" to "123" on the last command entered.                         ',&
'         !!c/abc/123                                                            ',&
'                                                                                ',&
'         // pass command to system                                              ',&
'         !ls -ltrasd                                                            ',&
'                                                                                ',&
'      see "EDIT"                                                                ',&
'                                                                                ',&
';     Used inside brackets to end rows.                                         ',&
'                                                                                ',&
'      Used after an expression or statement to suppress printing.               ',&
'      See "semi".                                                               ',&
'                                                                                ',&
'\     Backslash or matrix left division. A\B is roughly the                     ',&
'      same as "inv(A)*B", except it is computed in a different                  ',&
'      way. If A is an N by N matrix and B is a column vector                    ',&
'      with N components, or a matrix with several such columns,                 ',&
'      then X = A\B is the solution to the equation A*X = B                      ',&
'      computed by Gaussian elimination. A warning message is                    ',&
'      printed if A is badly scaled or nearly singular.                          ',&
'      A\eye produces the inverse of A .                                         ',&
'                                                                                ',&
'      If A is an M by N matrix with M < or > N and B is a                       ',&
'      column vector with M components, or a matrix with several                 ',&
'      such columns, then X = A\B is the solution in the least                   ',&
'      squares sense to the under- or overdetermined system of                   ',&
'      equations A*X = B. The effective rank, K, of A is                         ',&
'      determined from the QR decomposition with pivoting. A                     ',&
'      solution X is computed which has at most K nonzero                        ',&
'      components per column. If K < N this will usually not be                  ',&
'      the same solution as pinv(A)*B .                                          ',&
'      A\eye produces a generalized inverse of A.                                ',&
'                                                                                ',&
'      If A and B have the same dimensions, then A .\ B has                      ',&
'      elements a(i,j)\b(i,j) .                                                  ',&
'                                                                                ',&
'      Also, see "edit".                                                         ',&
'                                                                                ',&
'/     Slash or matrix right division. B/A is roughly the same                   ',&
'      as B*inv(A) . More precisely, B/A = (A''\B'')'' . See \ .                 ',&
'                                                                                ',&
'      IF A and B have the same dimensions, then A ./ B has                      ',&
'      elements a(i,j)/b(i,j) .                                                  ',&
'                                                                                ',&
'      Two or more slashes together on a line indicate a logical                 ',&
'      end of line. Any following text is ignored.                               ',&
'                                                                                ',&
'''     Transpose. X'' is the complex conjugate transpose of X .                 ',&
'      Quote. ''ANY TEXT'' is a vector whose components are the MAT88            ',&
'      internal codes for the characters. A quote within the text is             ',&
'      indicated by two quotes. See "display" and "FILE" .                       ',&
'                                                                                ',&
'+     Addition. X + Y . X and Y must have the same dimensions.                  ',&
'                                                                                ',&
'-     Subtraction. X - Y . X and Y must have the same                           ',&
'      dimensions.                                                               ',&
'                                                                                ',&
'*     Matrix multiplication, X*Y . Any scalar (1 by 1 matrix)                   ',&
'      may multiply anything. Otherwise, the number of columns of                ',&
'      X must equal the number of rows of Y .                                    ',&
'                                                                                ',&
'      Element-by-element multiplication is obtained with X .* Y .               ',&
'                                                                                ',&
'      The Kronecker tensor product is denoted by X .*. Y .                      ',&
'                                                                                ',&
'      Powers. X**p is X to the p power. p must be a                             ',&
'      scalar. If X is a matrix, see "HIGH" .                                    ',&
'                                                                                ',&
':     Colon. Used in subscripts, "for" iterations and possibly                  ',&
'      elsewhere.                                                                ',&
'                                                                                ',&
'         J:K  is the same as  <J, J+1, ..., K>                                  ',&
'         J:K  is empty if  J > K .                                              ',&
'         J:I:K  is the same as  <J, J+I, J+2I, ..., K>                          ',&
'         J:I:K  is empty if  I > 0 and J > K or if I < 0 and J < K .            ',&
'                                                                                ',&
'      The colon notation can be used to pick out selected rows,                 ',&
'      columns and elements of vectors and matrices.                             ',&
'                                                                                ',&
'         A(:) is all the elements of A, regarded as a single column.            ',&
'         A(:,J)  is the J-th column of A                                        ',&
'         A(J:K)  is A(J),A(J+1),...,A(K)                                        ',&
'         A(:,J:K)  is A(:,J),A(:,J+1),...,A(:,K) and so on.                     ',&
'                                                                                ',&
'      For the use of the colon in the "for" statement, See "for" .              ',&
'                                                                                ',&
'semi  "semi" toggles the action of semicolons at the end of lines.              ',&
'      It will make semicolons cause rather than suppress printing.              ',&
'      A second "semi" restores the initial interpretation.                      ',&
'================================================================================',&
'VARIABLES                                                                       ',&
'                                                                                ',&
'ans   Variable created automatically when expressions are not                   ',&
'      assigned to anything else.                                                ',&
'                                                                                ',&
'clear  Erases all variables, except "eps", "flop", "eye" and "rand".            ',&
'       X = <> erases only variable X . So does "clear X".                       ',&
'                                                                                ',&
'who   Lists current variables.                                                  ',&
'================================================================================',&
'MACROS                                                                          ',&
'                                                                                ',&
'       The macro facility involves text and inward pointing angle               ',&
'       brackets. If "STRING" is the source text for any MAT88                   ',&
'       expression or statement, then                                            ',&
'                                                                                ',&
'             t = ''STRING'';                                                    ',&
'       encodes the text as a vector of integers and stores that                 ',&
'       vector in t. "display(t)" will print the text and                        ',&
'                                                                                ',&
'             >t<                                                                ',&
'       causes the text to be interpreted, either as a statement or              ',&
'       as a factor in an expression. For example                                ',&
'                                                                                ',&
'             t = ''1/(i+j-1)'';                                                 ',&
'             display(t)                                                         ',&
'             for i = 1:n, for j = 1:n, a(i,j) = >t<;                            ',&
'                                                                                ',&
'       generates the Hilbert matrix of order n.                                 ',&
'       Another example showing indexed text,                                    ',&
'                                                                                ',&
'             S = <''x = 3            ''                                         ',&
'                  ''y = 4            ''                                         ',&
'                  ''z = sqrt(x*x+y*y)''>                                        ',&
'             for k = 1:3, >S(k,:)<                                              ',&
'                                                                                ',&
'       It is necessary that the strings making up the "rows" of                 ',&
'       the "matrix" S have the same lengths.                                    ',&
'                                                                                ',&
'================================================================================',&
'BASIC FUNCTIONS                                                                 ',&
'                                                                                ',&
'      For matrix arguments X , the functions "sin", "cos", "atan",              ',&
'      "sqrt", "log", "exp" and X**p are computed using eigenvalues D            ',&
'      and eigenvectors V . If <V,D> = eig(X) then f(X) = V*f(D)/V . This        ',&
'      method may give inaccurate results if V is badly conditioned. Some        ',&
'      idea of the accuracy can be obtained by comparing X**1 with X .           ',&
'      For vector arguments, the function is applied to each component.          ',&
'                                                                                ',&
'atan  atan(X) is the arctangent of X . See HIGH .                               ',&
'                                                                                ',&
'cos   cos(X) is the cosine of X . See HIGH .                                    ',&
'                                                                                ',&
'exp   exp(X) is the exponential of X , e to the X . See HIGH.                   ',&
'                                                                                ',&
'log   log(X) is the natural logarithm of X. See HIGH.                           ',&
'      Complex results are produced if X is not positive, or has                 ',&
'      nonpositive eigenvalues.                                                  ',&
'                                                                                ',&
'sin   sin(X) is the sine of X. See HIGH.                                        ',&
'                                                                                ',&
'sqrt  sqrt(X) is the square root of X. See HIGH. Complex                        ',&
'      results are produced if X is not positive, or has                         ',&
'      nonpositive eigenvalues.                                                  ',&
'================================================================================',&
'HIGH LEVEL FUNCTIONS                                                            ',&
'                                                                                ',&
'abs   abs(X) is the absolute value, or complex modulus, of the                  ',&
'      elements of X .                                                           ',&
'                                                                                ',&
'base  base(X,B) is a vector containing the base B representation                ',&
'      of X. This is often used in conjunction with "display".                   ',&
'      "display(X,B)" is the same as "display(base(X,B))". For example,          ',&
'      "display(4*atan(1),16)" prints the hexadecimal representation of pi.      ',&
'                                                                                ',&
'chol  Cholesky factorization. "chol(X)" uses only the diagonal                  ',&
'      and upper triangle of X. The lower triangular is assumed to be            ',&
'      the (complex conjugate) transpose of the upper. If X is positive          ',&
'      definite, then "R = chol(X)" produces an upper triangular R so            ',&
'      that R''*R = X . If X is not positive definite, an error message          ',&
'      is printed.                                                               ',&
'                                                                                ',&
'chop  Truncate arithmetic. "chop(P)" causes P places to be chopped              ',&
'      off after each arithmetic operation in subsequent computations. This      ',&
'      means P hexadecimal digits on some computers and P octal digits           ',&
'      on others. "chop(0)" restores full precision.                             ',&
'                                                                                ',&
'cond  Condition number in 2-norm. "cond(X)" is the ratio of the                 ',&
'      largest singular value of X to the smallest.                              ',&
'                                                                                ',&
'conjg  "conjg(X)" is the complex conjugate of X .                               ',&
'                                                                                ',&
'det   "det(X)" is the determinant of the square matrix X .                      ',&
'                                                                                ',&
'diag  If V is a row or column vector with N components,                         ',&
'      "diag(V,K)" is a square matrix of order "N+abs(K)" with the               ',&
'      elements of V on the K-th diagonal. K = 0 is the main diagonal,           ',&
'      K > 0 is above the main diagonal and K < 0 is below the main              ',&
'      diagonal. "diag(V)" simply puts V on the main diagonal. eg.               ',&
'                                                                                ',&
'         diag(-M:M) + diag(ones(2*M,1),1) + diag(ones(2*M,1),-1)                ',&
'                                                                                ',&
'      produces a tridiagonal matrix of order 2*M+1 .                            ',&
'                                                                                ',&
'      If X is a matrix, "diag(X,K)" is a column vector formed from the          ',&
'      elements of the K-th diagonal of X.  "diag(X)" is the main diagonal       ',&
'      of X.  "diag(diag(X))" is a diagonal matrix .                             ',&
'                                                                                ',&
'eig   Eigenvalues and eigenvectors.                                             ',&
'      "eig(X)" is a vector containing the eigenvalues of a square               ',&
'      matrix X.                                                                 ',&
'      "<V,D> = eig(X)" produces a diagonal matrix D of                          ',&
'      eigenvalues and a full matrix V whose columns are the                     ',&
'      corresponding eigenvectors so that X*V = V*D .                            ',&
'                                                                                ',&
'eye   Identity matrix. "eye(N)" is the N by N identity matrix.                  ',&
'      "eye(M,N)" is an M by N matrix with 1''s on the diagonal and              ',&
'      zeros elsewhere. "eye(A)" is the same size as A. "eye"                    ',&
'      with no arguments is an identity matrix of whatever order                 ',&
'      is appropriate in the context. For example "A + 3*eye"                    ',&
'      adds 3 to each diagonal element of A.                                     ',&
'                                                                                ',&
'hess  Hessenberg form. The Hessenberg form of a matrix is zero                  ',&
'      below the first subdiagonal. If the matrix is symmetric or                ',&
'      Hermitian, the form is tridiagonal. <P,H> = "hess(A)" produces a          ',&
'      unitary matrix P and a Hessenberg matrix H so that A = P*H*P''. By        ',&
'      itself, "hess(A)" returns H.                                              ',&
'                                                                                ',&
'invh  Inverse Hilbert matrix. "invh(N)" is the inverse of a N_by_N              ',&
'      Hilbert matrix (which is a famous example of a badly conditioned          ',&
'      matrix). The result is exact for N less than about 15, depending          ',&
'      upon the computer.                                                        ',&
'                                                                                ',&
'         for i = 1:N, for j = 1:N, A(i,j) = 1/(i+j-1);                          ',&
'                                                                                ',&
'      generates the NxN Hilbert matrix.                                         ',&
'                                                                                ',&
'      "invh" has an alias of "inverse_hilbert" and "invhilb".                   ',&
'                                                                                ',&
'imag  "imag(X)" is the imaginary part of X .                                    ',&
'                                                                                ',&
'inv   "inv(X)" is the inverse of the square matrix X . A warning                ',&
'      message is printed if X is badly scaled or nearly                         ',&
'      singular.                                                                 ',&
'                                                                                ',&
'kron  "kron(X,Y)" is the Kronecker tensor product of X and Y. It                ',&
'      is also denoted by X .*. Y . The result is a large matrix                 ',&
'      formed by taking all possible products between the elements               ',&
'      of X and those of Y . For example, if X is 2 by 3, then                   ',&
'      X .*. Y is                                                                ',&
'                                                                                ',&
'            < x(1,1)*Y  x(1,2)*Y  x(1,3)*Y                                      ',&
'              x(2,1)*Y  x(2,2)*Y  x(2,3)*Y >                                    ',&
'                                                                                ',&
'      The five-point discrete Laplacian for an n-by-n grid can be               ',&
'      generated by                                                              ',&
'                                                                                ',&
'            T = diag(ones(n-1,1),1);  T = T + T'';  I = eye(T);                 ',&
'            A = T.*.I + I.*.T - 4*eye;                                          ',&
'                                                                                ',&
'      Just in case they might be useful, MAT88 includes                         ',&
'      constructions called Kronecker tensor quotients, denoted by               ',&
'      X ./. Y and X .\. Y . They are obtained by replacing the                  ',&
'      element-wise multiplications in X .*. Y with divisions.                   ',&
'                                                                                ',&
'lu    Factors from Gaussian elimination. <L,U> = LU(X) stores a                 ',&
'      upper triangular matrix in U and a ''psychologically lower                ',&
'      triangular matrix'', i.e. a product of lower triangular and               ',&
'      permutation matrices, in L , so that X = L*U . By itself,                 ',&
'      "lu(X)" returns the output from CGEFA .                                   ',&
'                                                                                ',&
'magic  Magic square. "magic(N)" is an N by N matrix constructed                 ',&
'       from the integers 1 through N**2 with equal row, column and              ',&
'       diagonal sums. N must be a positive whole number not equal to two.       ',&
'                                                                                ',&
'norm  computes the norm or P-norm of X                                          ',&
'                                                                                ',&
'      norm(X,P) computes the P-norm of X. P=2 is the default, which defines     ',&
'      the standard norm.                                                        ',&
'                                                                                ',&
'      For matrices..                                                            ',&
'          norm(X,1)      is the 1-norm of X; ie. the largest column sum         ',&
'                         of X.                                                  ',&
'                                                                                ',&
'          norm(X,2)      the largest singular value of X.                       ',&
'          or norm(X)                                                            ',&
'                                                                                ',&
'          norm(X,''inf'')  is the infinity norm of X; ie. the largest row       ',&
'                         sum of X.                                              ',&
'                                                                                ',&
'          norm(X,''fro'')  is the F-norm, i.e. "sqrt(sum(diag(X''*X)))" .       ',&
'                                                                                ',&
'      For vectors..                                                             ',&
'          norm(V,P)      the same as sum(V(I)**P)**(1/P) .                      ',&
'                         ??? what about negative values of (I) and odd P? abs() or not',&
'                                                                                ',&
'          norm(V,2)      the square root of the sum of the squares of           ',&
'          or norm(V)     the entries of V.                                      ',&
'                                                                                ',&
'          norm(V,''inf'')  the value is max(abs(V)) .                           ',&
'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<',&
'!!          If X is a vector, then                                              ',&
'!!                                                                              ',&
'!!            norm(x,p) = sum(abs(x) .^ p) ^ (1/p)                              ',&
'!!            norm(x,1) is the sum of the absolute values of X.                 ',&
'!!            norm(x)/sqrt(n) is the root-mean-square value.                    ',&
'!!            norm(x,-inf)=min(abs(x))                                          ',&
'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<',&
'                                                                                ',&
'ones  All ones. "ones(N)" is an N by N matrix of ones. "ones(M,N)"              ',&
'      is an M by N matrix of ones . "ones(A)" is the same size as A and         ',&
'      all ones .                                                                ',&
'                                                                                ',&
'orth  Orthogonalization. "Q = orth(X)" is a matrix with                         ',&
'      orthonormal columns, i.e. Q''*Q = eye, which span the same                ',&
'      space as the columns of X .                                               ',&
'                                                                                ',&
'pinv  Pseudoinverse.                                                            ',&
'                                                                                ',&
'      "X = pinv(A)" produces a matrix X of the same dimensions as A''           ',&
'      so that A*X*A = A , X*A*X = X and AX and XA are Hermitian . The           ',&
'      computation is based on "svd(A)" and any singular values less             ',&
'      than a tolerance are treated as zero. The default tolerance is            ',&
'      "norm(size(A),''inf'')*norM(A)*eps". This tolerance may be overridden     ',&
'      with "X = pinv(A,tol)". See "rank".                                       ',&
'                                                                                ',&
'poly  Characteristic polynomial.                                                ',&
'                                                                                ',&
'      If A is an N by N matrix, "poly(A)" is a column vector with               ',&
'      N+1 elements which are the coefficients of the characteristic             ',&
'      polynomial, "det(lambda*eye - A)" .                                       ',&
'                                                                                ',&
'      If V is a vector, "poly(V)" is a vector whose elements are the            ',&
'      coefficients of the polynomial whose roots are the elements of V          ',&
'      . For vectors, "roots" and "poly" are inverse functions of each           ',&
'      other, up to ordering, scaling, and roundoff error.                       ',&
'                                                                                ',&
'      "roots(poly(1:20))" generates Wilkinson''s famous example.                ',&
'                                                                                ',&
'prod  "prod(X)" is the product of all the elements of X .                       ',&
'                                                                                ',&
'qr    Orthogonal-triangular decomposition.  "<Q,R> = qr(X)" produces an         ',&
'      upper triangular matrix R of the same                                     ',&
'      dimension as X and a unitary matrix Q so that X = Q*R .                   ',&
'                                                                                ',&
'      "<Q,R,E> = qr(X)" produces a permutation matrix E, an upper               ',&
'      triangular R with decreasing diagonal elements and a unitary Q            ',&
'      so that X*E = Q*R .  By itself, "qr(X)" returns the output of             ',&
'      "cqrdc". "triu(qr(X))" is R .                                             ',&
'                                                                                ',&
'rand  Random numbers and matrices. "rand(N)" is an N by N matrix                ',&
'      with random entries. "rand(M,N)" is an M by N matrix with                 ',&
'      random entries. "rand(A)" is the same size as A. "rand"                   ',&
'      with no arguments is a scalar whose value changes each time               ',&
'      it is referenced.                                                         ',&
'                                                                                ',&
'      Ordinarily, random numbers are uniformly distributed in                   ',&
'      the interval "(0.0,1.0). rand(''normal'')" switches to a                  ',&
'      normal distribution with mean 0.0 and variance 1.0.                       ',&
'      "rand(''uniform'')" switches back to the uniform distribution.            ',&
'      "rand(''seed'')" returns the current value of the seed for the            ',&
'      generator. "rand(''seed'',n)" sets the seed to n.                         ',&
'      "rand(''seed'',0)" resets the seed to 0, its value when MAT88             ',&
'      is first entered.                                                         ',&
'                                                                                ',&
'rank  Rank. "K = rank(X)" is the number of singular values of X                 ',&
'      that are larger than "norm(size(X),''inf'')*norm(X)*eps".                 ',&
'      "K = rank(X,tol)" is the number of singular values of X that              ',&
'      are larger than tol.                                                      ',&
'                                                                                ',&
'rcond  "rcond(X)" is an estimate for the reciprocal of the                      ',&
'       condition of X in the 1-norm obtained by the LINPACK                     ',&
'       condition estimator. If X is well conditioned, rcond(X)                  ',&
'       is near 1.0. If X is badly conditioned, rcond(X) is                      ',&
'       near 0.0.                                                                ',&
'       <R, Z> = rcond(A) sets R to rcond(A) and also produces a                 ',&
'       vector Z so that                                                         ',&
'                                                                                ',&
'                 norm(A*Z,1) = R*norm(A,1)*norm(Z,1)                            ',&
'                                                                                ',&
'       So, if rcond(A) is small, then Z is an approximate null                  ',&
'       vector.                                                                  ',&
'                                                                                ',&
'rat   An experimental function which attempts to remove the                     ',&
'      roundoff error from results that should be "simple"                       ',&
'      rational numbers.                                                         ',&
'      "rat(X)" approximates each element of X by a continued                    ',&
'      fraction of the form                                                      ',&
'                                                                                ',&
'                a/b = d1 + 1/(d2 + 1/(d3 + ... + 1/dk))                         ',&
'                                                                                ',&
'      with k <= len, integer di and abs(di) <= max . The default                ',&
'      values of the parameters are len = 5 and max = 100.                       ',&
'      "rat(len,max)" changes the default values. Increasing either              ',&
'      len or max increases the number of possible fractions.                    ',&
'      "<A,B> = rat(X)" produces integer matrices A and B so that                ',&
'                                                                                ',&
'                A ./ B  =  rat(X)                                               ',&
'                                                                                ',&
'      Some examples:                                                            ',&
'                                                                                ',&
'            long                                                                ',&
'            T = invh(6), X = inv(T)                                             ',&
'            <A,B> = rat(X)                                                      ',&
'            H = A ./ B, S = inv(H)                                              ',&
'                                                                                ',&
'            short e                                                             ',&
'            d = 1:8,  e = ones(d),  A = abs(d''*e - e''*d)                      ',&
'            X = inv(A)                                                          ',&
'            rat(X)                                                              ',&
'            display(ans)                                                        ',&
'                                                                                ',&
'real  "real(X)" is the real part of X.                                          ',&
'                                                                                ',&
'rref  "rref(A)" is the reduced row echelon form of the rectangular              ',&
'      matrix. rref(A,B) is the same as rref(<A,B>) .                            ',&
'                                                                                ',&
'roots  Find polynomial roots. "roots(C)" computes the roots of the              ',&
'       polynomial whose coefficients are the elements of the vector C.          ',&
'       If C has N+1 components, the polynomial is                               ',&
'                                                                                ',&
'          C(1)*X**N + ... + C(N)*X + C(N+1)                                     ',&
'                                                                                ',&
'       See "poly".                                                              ',&
'                                                                                ',&
'round  "round(X)" rounds the elements of X to the nearest integers.             ',&
'                                                                                ',&
'schur  Schur decomposition. "<U,T> = schur(X)" produces an upper                ',&
'       triangular matrix T , with the eigenvalues of X on the                   ',&
'       diagonal, and a unitary matrix U so that X = U*T*U'' and                 ',&
'       U''*U = eye . By itself, "schur(X)" returns T .                          ',&
'                                                                                ',&
'size  If X is an M by N matrix, then size(X) is <M, N> .                        ',&
'      Can also be used with a multiple assignment,                              ',&
'            <M, N> = size(X) .                                                  ',&
'                                                                                ',&
'sum   "sum(X)" is the sum of all the elements of X.                             ',&
'      "sum(diag(X))" is the trace of X.                                         ',&
'                                                                                ',&
'svd   Singular value decomposition. "<U,S,V> = svd(X)" produces a               ',&
'      diagonal matrix S , of the same dimension as X and with                   ',&
'      nonnegative diagonal elements in decreasing order, and                    ',&
'      unitary matrices U and V so that X = U*S*V'' .                            ',&
'                                                                                ',&
'      By itself, "svd(X)" returns a vector containing the singular              ',&
'      values.                                                                   ',&
'                                                                                ',&
'      "<U,S,V> = svd(X,0)" produces the "economy size"                          ',&
'      decomposition. If X is m by n with m > n, then only the                   ',&
'      first n columns of U are computed and S is n by n .                       ',&
'                                                                                ',&
'tril  Lower triangle. "tril(X)" is the lower triangular part of X.              ',&
'      "tril(X,K)" is the elements on and below the K-th diagonal of             ',&
'      X. K = 0 is the main diagonal, K > 0 is above the main                    ',&
'      diagonal and K < 0 is below the main diagonal.                            ',&
'                                                                                ',&
'triu  Upper triangle. "triu(X)" is the upper triangular part of X.              ',&
'      "triu(X,K)" is the elements on and above the K-th diagonal of X. K        ',&
'      = 0 is the main diagonal, K > 0 is above the main diagonal and K <        ',&
'      0 is below the main diagonal.                                             ',&
'                                                                                ',&
'user  Allows personal Fortran subroutines to be linked into                     ',&
'      MAT88. The subroutine should have the heading                             ',&
'                                                                                ',&
'         SUBROUTINE USER(A,M,N,S,T)                                             ',&
'         REAL or DOUBLEPRECISION A(M,N),S,T                                     ',&
'                                                                                ',&
'      The MAT88 statement "Y = user(X,s,t)" results in a call to the            ',&
'      subroutine with a copy of the matrix X stored in the argument A,          ',&
'      its column and row dimensions in M and N, and the scalar parameters       ',&
'      s and t stored in S and T. If s and t are omitted, they are set           ',&
'      to 0.0. After the return, A is stored in Y. The dimensions M and          ',&
'      N may be reset within the subroutine. The statement Y = "user(K)"         ',&
'      results in a call with M = 1, N = 1 and A(1,1) = "float(K)". After        ',&
'      the subroutine has been written, it must be compiled and linked           ',&
'      to the MAT88 object code within the local operating system.               ',&
'                                                                                ',&
'zeros                                                                           ',&
'      Returns a matrix of all zeros.                                            ',&
'                                                                                ',&
'         zeros(N)    returns an N by N matrix of zeroes.                        ',&
'         zeros(M,N)  returns an M by N matrix of zeroes.                        ',&
'         zeros(X)    returns a matrix of zeroes of the same order as X.         ',&
'================================================================================',&
'FLOW CONTROL                                                                    ',&
'                                                                                ',&
'else  Used with "if".                                                           ',&
'                                                                                ',&
'end   Terminates the scope of "for", "while" and "if" statements.               ',&
'      Without "end"s, "for" and "while" repeat all statements up to             ',&
'      the end of the line. Each "end" is paired with the closest                ',&
'      previous unpaired "for" or "while" and serves to terminate its            ',&
'      scope. The line                                                           ',&
'                                                                                ',&
'         for I=1:N, for J=1:N, A(I,J)=1/(I+J-1); A                              ',&
'                                                                                ',&
'      would cause A to be printed N**2 times, once for each new                 ',&
'      element. On the other hand, the line                                      ',&
'                                                                                ',&
'         for I=1:N, for J=1:N, A(I,J)=1/(I+J-1); end, end, A                    ',&
'                                                                                ',&
'      will lead to only the final printing of A.                                ',&
'      Similar considerations apply to "while".                                  ',&
'                                                                                ',&
'      See "exit" (terminates execution of loops or of MAT88 itself).            ',&
'                                                                                ',&
'if    Conditionally execute statements                                          ',&
'                                                                                ',&
'      SIMPLE FORM                                                               ',&
'       Enter                                                                    ',&
'                                                                                ',&
'         if expression rop expression, statements                               ',&
'                                                                                ',&
'      where rop is =, <, >, <=, >=, or <> (not equal). The                      ',&
'      statements are executed once if the indicated comparison                  ',&
'      between the real parts of the first components of the two                 ',&
'      expressions is true, otherwise the statements are skipped.                ',&
'                                                                                ',&
'      EXAMPLE                                                                   ',&
'        Enter                                                                   ',&
'                                                                                ',&
'         if abs(i-j) = 1, a(i,j) = -1;                                          ',&
'                                                                                ',&
'      More complicated forms use "end" in the same way it is used with          ',&
'      "for" and "while" and use "else" as an abbreviation for "end",            ',&
'                                                                                ',&
'         if expression not rop expression                                       ',&
'                                                                                ',&
'      EXAMPLE                                                                   ',&
'        Enter                                                                   ',&
'                                                                                ',&
'         for i = 1:n, for j = 1:n, ...                                          ',&
'            if i = j, a(i,j) = 2; else if abs(i-j) = 1, a(i,j) = -1; ...        ',&
'            else a(i,j) = 0;                                                    ',&
'                                                                                ',&
'      An easier way to accomplish the same thing is                             ',&
'                                                                                ',&
'         a = 2*eye(n);                                                          ',&
'         for i = 1:n-1, a(i,i+1) = -1; a(i+1,i) = -1;                           ',&
'                                                                                ',&
'for   Repeat statements a specific number of times.                             ',&
'                                                                                ',&
'         for variable = expr, statement, ..., statement, end                    ',&
'                                                                                ',&
'      The "end" at the end of a line may be omitted. The comma before the       ',&
'      "end" may also be omitted. The columns of the expression are stored       ',&
'      one at a time in the variable and then the following statements,          ',&
'      up to the "end", are executed.  The expression is often of the form       ',&
'      X:Y, in which case its columns are simply scalars. Some examples          ',&
'      (assume N has already been assigned a value).                             ',&
'                                                                                ',&
'       for I = 1:N, for J = 1:N, A(I,J) = 1/(I+J-1);                            ',&
'       for J = 2:N-1, A(J,J) = J; end; A                                        ',&
'       for S = 1.0: -0.1: 0.0, ... steps S with increments of -0.1 .            ',&
'       for E = eye(N), ... sets E to the unit N-vectors.                        ',&
'       for V = A, ... has the same effect as                                    ',&
'       for J = 1:N, V = A(:,J); ... except J is also set here.                  ',&
'                                                                                ',&
'while  Repeat statements an indefinite number of times.                         ',&
'                                                                                ',&
'          while expr rop expr, statement, ..., statement, end                   ',&
'                                                                                ',&
'       where rop is =, <, >, <=, >=, or <> (not equal). The "end"               ',&
'       at the end of a line may be omitted. The comma before the                ',&
'       "end" may also be omitted. The commas may be replaced by                 ',&
'       semicolons to avoid printing. The statements are                         ',&
'       repeatedly executed as long as the indicated comparison                  ',&
'       between the real parts of the first components of the two                ',&
'       expressions is true.                                                     ',&
'                                                                                ',&
'       EXAMPLE                                                                  ',&
'       (assume a matrix A is already defined).                                  ',&
'                                                                                ',&
'        E = 0*A; F = E + eye; N = 1;                                            ',&
'        while norm(E+F-E,1) > 0, E = E + F; F = A*F/N; N = N + 1;               ',&
'        E                                                                       ',&
'                                                                                ',&
'exit  Causes termination of a "for" or "while" loop.                            ',&
'      If not in a loop, terminates execution of MAT88.                          ',&
'      Also see "quit".                                                          ',&
'                                                                                ',&
'quit  From the terminal, causes return to the operating system                  ',&
'      or other program which invoked MAT88. From inside an                      ',&
'      "exec", causes return to the invoking "exec", or to the                   ',&
'      terminal.                                                                 ',&
'                                                                                ',&
'================================================================================',&
'FILE ACCESS                                                                     ',&
'                                                                                ',&
'      The "exec", "save", "load", "diary", and "print" functions access         ',&
'      files.  The ''file'' parameter takes different forms for different        ',&
'      operating systems. On most systems, ''file'' may be a string of           ',&
'      up to 1024 characters in quotes. For example, "save(''A'')" or            ',&
'      "exec(''MAT88/demo.exec'')" . The string will be used as the name         ',&
'      of a file in the local operating system.  On all systems, ''file''        ',&
'      may be a positive integer k less than 10 which will be used as            ',&
'      a Fortran logical unit number. Some systems then automatically            ',&
'      access a file with a name like FORT.k or FORk.DAT. Other systems          ',&
'      require a file with a name like FT0kF001 to be assigned to unit k         ',&
'      before MAT88 is executed. Check your local installation for details.      ',&
'                                                                                ',&
'      The filename must be composed of recognized characters. See "char".       ',&
'                                                                                ',&
'      Also see "quit" and "exit".                                               ',&
'                                                                                ',&
'exec  "exec(''file'',k)" obtains subsequent MAT88 input from an                 ',&
'      external file. The printing of input is controlled by the                 ',&
'      optional parameter k .                                                    ',&
'                                                                                ',&
'         If k = 0 , there is no echo, prompt or pause. This is the              ',&
'                    default if the exec command is followed by a semicolon.     ',&
'         If k = 1 , the input is echoed.                                        ',&
'         If k = 2 , the MAT88 prompt <> is printed.                             ',&
'         If k = 3 , there will be echos and prompts, but no pauses.             ',&
'                    This is the the default if the exec command is not          ',&
'                    followed by a semicolon.                                    ',&
'         If k = 4 , MAT88 pauses before each prompt and waits for a             ',&
'                    null line to continue.                                      ',&
'         If k = 7 , there will be echos, prompts and pauses. This is            ',&
'                    useful for demonstrations on video terminals.               ',&
'                                                                                ',&
'      "exec(0)" causes subsequent input to be obtained from the                 ',&
'      terminal. An end-of-file has the same effect.                             ',&
'                                                                                ',&
'      "exec"''s may be nested, i.e. the text in the file may contain            ',&
'      "exec" of another file.                                                   ',&
'                                                                                ',&
'      "exec"''s may also be driven by "for" and                                 ',&
'      "while" loops.                                                            ',&
'                                                                                ',&
'load  "load(''file'')" retrieves all the variables from the file .              ',&
'      See FILE and "save" for more details. To prepare your own                 ',&
'      file for "load"ing, change the "read" to "write" in the code              ',&
'      given under "save".                                                       ',&
'                                                                                ',&
'print  "print(''file'',X)" prints X on the file using the current               ',&
'       format determined by "short", "long z", etc. See FILE.                   ',&
'                                                                                ',&
'doc   does nothing at the moment                                                ',&
'                                                                                ',&
'save  "save(''file'')" stores all the current variables in a file.              ',&
'      "save(''file'',X)" saves only X . See FILE .                              ',&
'                                                                                ',&
'      The variables may be retrieved later by "load(''file'')" or by your       ',&
'      own program using the following code for each matrix.  The lines          ',&
'      involving "ximag" may be eliminated if everything is known to             ',&
'      be real.                                                                  ',&
'                                                                                ',&
'            ! attach lunit to ''file'', then ...                                ',&
'            REAL or DOUBLEPRECISION XREAL(MMAX,NMAX)                            ',&
'            REAL or DOUBLEPRECISION XIMAG(MMAX,NMAX)                            ',&
'            READ(lunit,101) ID,M,N,IMG                                          ',&
'            DO J = 1, N                                                         ',&
'               READ(lunit,102) (XREAL(I,J), I=1,M)                              ',&
'               IF (IMG .NE. 0) READ(lunit,102) (XIMAG(I,J),I=1,M)               ',&
'            enddo                                                               ',&
'                                                                                ',&
'      The formats used are system dependent. The following are                  ',&
'      typical. See SUBROUTINE mat_savlod(3f) in your local implementation       ',&
'      of MAT88.                                                                 ',&
'                                                                                ',&
'        101 FORMAT(4A1,3I4)                                                     ',&
'        102 FORMAT(4Z18)                                                        ',&
'        102 FORMAT(4O20)                                                        ',&
'        102 FORMAT(4D25.18)                                                     ',&
'                                                                                ',&
'================================================================================',&
'OUTPUT OPTIONS                                                                  ',&
'      ( Also see "FILE" ("exec", "load", "print", "save" ))                     ',&
'                                                                                ',&
'lines  An internal count is kept of the number of lines of output               ',&
'       since the last input. Whenever this count approaches a                   ',&
'       limit, the user is asked whether or not to suppress                      ',&
'       printing until the next input. Initially the limit is 21.                ',&
'       "lines(N)" resets the limit to N .                                       ',&
'                                                                                ',&
'long   See "short" also.                                                        ',&
'                                                                                ',&
'       Determine output format. All computations are done in                    ',&
'       complex arithmetic and double precision if it is available.              ',&
'       "short" and "long" merely switch between different output                ',&
'       formats.                                                                 ',&
'                                                                                ',&
'        long     // Scaled fixed point format with about 15 digits.             ',&
'        long e   // Floating point format with about 15 digits.                 ',&
'        long z   // System dependent format, often hexadecimal.                 ',&
'                                                                                ',&
'short  See "long" also.                                                         ',&
'       Determine output format. All computations are done in                    ',&
'       complex arithmetic and double precision if it is available.              ',&
'       "short" and "long" merely switch between different output                ',&
'       formats.                                                                 ',&
'                                                                                ',&
'        short    // Scaled fixed point format with about 5 digits.              ',&
'        short e  // Floating point format with about 5 digits.                  ',&
'                                                                                ',&
'diary  "diary(''file'')" causes a copy of all subsequent terminal input and     ',&
'       most of the resulting output to be written on the file. "diary(0)"       ',&
'       turns it off. See "FILE".                                                ',&
'                                                                                ',&
'display  "display(X)" prints X in a compact format.                             ',&
'                                                                                ',&
'      If a base is specified the values are printed as numeric                  ',&
'      values in the specified base.                                             ',&
'                                                                                ',&
'           display(0:10,10 ) // display values in base 10                       ',&
'           display(0:10,16 ) // display values as hexadecimal values            ',&
'           display(0:10,2 )  // display values as binary numbers                ',&
'                                                                                ',&
'      If no base is specified and all the                                       ',&
'      elements of X are integers between 0 and 86, then X is                    ',&
'      interpreted as MAT88 text and printed accordingly.                        ',&
'                                                                                ',&
'         <>display(''the analysis is complete'')                                ',&
'           the analysis is complete                                             ',&
'         display(0:86) // print the default MAT88 character set                 ',&
'                                                                                ',&
'      Otherwise, + , - and blank are printed for positive,                      ',&
'      negative and zero elements.                                               ',&
'                                                                                ',&
'         display(rand(24,80)-rand(24,80))                                       ',&
'                                                                                ',&
'      Imaginary parts are ignored.                                              ',&
'                                                                                ',&
'      Note that "display(X,B)" is the same as "display(base(X,B))" except       ',&
'      it forces "display" to display numeric values.                            ',&
'                                                                                ',&
'      "display" has an alias of "disp".                                         ',&
'                                                                                ',&
'plot  "plot(X,Y)" produces a plot of the elements of Y against                  ',&
'      those of X. plot(Y) is the same as plot(1:n,Y) where n is the number      ',&
'      of elements in Y. plot(X,Y,P) or "plot(X,Y,p1,...,pk)" passes the         ',&
'      optional parameter vector P or scalars p1 through pk to the plot          ',&
'      routine. The default plot routine is a crude printer-plot. This           ',&
'      version writes the data as a simple X Y table into a scratch file         ',&
'      called "scratch.dat" and then calls the command                           ',&
'                                                                                ',&
'        xy scratch.dat [options]                                                ',&
'                                                                                ',&
'      Hopefully, you have the command xy(1) in your search path.                ',&
'      If not, you can make one by creating a script that calls                  ',&
'      a plotting utility.                                                       ',&
'                                                                                ',&
'         t = 0:50;                                                              ',&
'         plot( t.*cos(t), t.*sin(t) )                                           ',&
'         opts='' -m -1 -title test plot -d pdf''                                ',&
'         plot( t.*cos(t), t.*sin(t),opts)                                       ',&
'================================================================================',&
'PERFORMANCE INFORMATION                                                         ',&
'                                                                                ',&
'flops  Count of floating point operations.                                      ',&
'                                                                                ',&
'       "flops" is a permanently defined row vector with two elements.           ',&
'       "flops(1)" is the cpu time consumed by the the previous                  ',&
'       statement. "flops(2)" is a cumulative total. "flops" can be used         ',&
'       in the same way as any other vector. "flops(2) = 0" resets the           ',&
'       cumulative total. In addition, "flops(1)" will be printed whenever       ',&
'       a statement is terminated by an extra comma. For example,                ',&
'                                                                                ',&
'         X = inv(A);,                                                           ',&
'                                                                                ',&
'       or                                                                       ',&
'                                                                                ',&
'         cond(A), (as the last statement on the line).                          ',&
'================================================================================',&
'MISCELLANEOUS                                                                   ',&
'                                                                                ',&
'char  "char(K)" requests an input line containing a single                      ',&
'      character to replace MAT88 character number K in the                      ',&
'      following table. For example, "char(45)" replaces backslash.              ',&
'      "char(-K)" replaces the alternate character number K. By default          ',&
'      the character set is:                                                     ',&
'                                                                                ',&
'                K  character alternate name                                     ',&
'              0 - 9   0 - 9    0 - 9   digits                                   ',&
'             10 - 35  A - Z    A - Z   letters                                  ',&
'               36                      blank                                    ',&
'               37       (        {     lparen                                   ',&
'               38       )        }     rparen                                   ',&
'               39       ;        ;     semi                                     ',&
'               40       :        |     colon                                    ',&
'               41       +        +     plus                                     ',&
'               42       -        -     minus                                    ',&
'               43       *        *     star                                     ',&
'               44       /        /     slash                                    ',&
'               45       \        $     backslash                                ',&
'               46       =        =     equal                                    ',&
'               47       .        @     dot                                      ',&
'               48       ,        ,     comma                                    ',&
'               49       ''        "     quote                                   ',&
'               50       <        [     less                                     ',&
'               51       >        ]     great                                    ',&
'             52 - 77  a - z    a - z   letters                                  ',&
'                                                                                ',&
'      unused: `~!#%^&_?                                                         ',&
'                                                                                ',&
'EDIT                                                                            ',&
'      A command line consisting of two exclamations("!!") will cause a          ',&
'      small line-based editor to be called (very similar to the CDC NOS         ',&
'      editor "xedit") with a copy of the previous input lines. When the         ',&
'      editor returns control to MAT88, it will execute the edited command       ',&
'      (by default).                                                             ',&
'                                                                                ',&
'      In editor mode the command to be edited is shifted over one and the       ',&
'      first character of input determines the edit mode. The letter "c"         ',&
'      begins a string change (ie. "c/oldstring/newstring/").  The "l"           ',&
'      command lists the lines. A number goes to that command line as            ',&
'      listed by the "l" command. If the change command begins with a            ',&
'      space a letter replaces the one above it with the exception of            ',&
'      the special characters # (delete) & (blank out) and ^ (insert the         ',&
'      following string here).                                                   ',&
'                                                                                ',&
'      An editing loop is entered until a carriage return on an empty            ',&
'      line is entered to accept the new line or a period is entered to          ',&
'      cancel the editing.                                                       ',&
'                                                                                ',&
'      For example, if you had entered a line such as:                           ',&
'                                                                                ',&
'         <M,N>=size(A);for I = 1:M, for J = 1:N, A(I,J) = A(I,J)+3.6;           ',&
'                                                                                ',&
'      Then to repeat the command changing "3.6" to "5.1" enter                  ',&
'                                                                                ',&
'         !!                                                                     ',&
'      the previous command is then displayed. Now enter                         ',&
'                                                                                ',&
'         c/3.6/5.1                                                              ',&
'                                                                                ',&
'      and then enter a carriage return and the edited line will be              ',&
'      executed.                                                                 ',&
'                                                                                ',&
'      The first command can appear on the same line if the line starts          ',&
'      with ". " (a period followed by a space). For example                     ',&
'                                                                                ',&
'         !! /rand                                                               ',&
'                                                                                ',&
'      would take you into edit mode on the last command containing the          ',&
'      string "rand"                                                             ',&
'                                                                                ',&
'      Enter "?" in edit mode to display further help on editor mode.            ',&
'                                                                                ',&
'eps   Floating point relative accuracy. A permanent variable                    ',&
'      whose value is initially the distance from 1.0 to the next largest        ',&
'      floating point number. The value is changed by "chop", and other          ',&
'      values may be assigned. "eps" is used as a default tolerance by "pinv"    ',&
'      and "rank".                                                               ',&
'                                                                                ',&
'lala  A placeholder for a new command.                                          ',&
'                                                                                ',&
'debug  "debu(1)" turns on verbose low-level debugging for the developer,        ',&
'       "debu(0)" turns it back off.                                             ',&
'                                                                                ',&
'================================================================================',&
'']
end subroutine mat_help_text
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_inverse_hilbert(a,lda,n)

! ident_47="@(#)M_matrix::ml_hilbr(3fp): generate doubleprecision inverse hilbert matrix"
!
! References:
! Forsythe, G. E. and C. B. Moler. Computer Solution of Linear Algebraic Systems. Englewood Cliffs, NJ: Prentice-Hall, 1967.

integer,intent(in)          :: lda
integer,intent(in)          :: n
doubleprecision,intent(out) :: a(lda,n)

doubleprecision :: p
doubleprecision :: r
integer         :: i
integer         :: j
integer         :: ip1

   p = dble(n)

   do i = 1, n
      if (i.ne.1) p = (dble(n-i+1) * p * dble(n+i-1)) / dble(i-1)**2
      r = p * p
      a(i,i) = r / dble(2*i-1)
      if (i.eq.n) cycle
      ip1 = i + 1
      do j = ip1, n
         r = (-1) * (dble(n-j+1) * r * (n+j-1)) / dble(j-1)**2
         a(i,j) = r/ dble(i+j-1)
         a(j,i) = a(i,j)
      enddo
   enddo

end subroutine mat_inverse_hilbert
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_magic(a,lda,n)
!
! ident_48="@(#)M_matrix::mat_magic(3fp): Algorithms for magic squares"

!        Algorithms taken from
!        Mathematical Recreations and Essays, 12th Ed.,
!        by W. W. Rouse Ball and H. S. M. Coxeter
!
integer         :: lda
integer         :: n
doubleprecision :: a(lda,n)

doubleprecision :: t
integer         :: i
integer         :: j
integer         :: k
integer         :: m
integer         :: mm
integer         :: i1
integer         :: im
integer         :: j1
integer         :: jm
integer         :: m1
integer         :: m2
!
   if (mod(n,4) .eq. 0) goto 100
   if (mod(n,2) .eq. 0) m = n/2
   if (mod(n,2) .ne. 0) m = n
!
!     odd order or upper corner of even order
!
   do j = 1,m
      do i = 1,m
         a(i,j) = 0
      enddo
   enddo
   i = 1
   j = (m+1)/2
   mm = m*m
   do k = 1, mm
      a(i,j) = k
      i1 = i-1
      j1 = j+1
      if(i1.lt.1) i1 = m
      if(j1.gt.m) j1 = 1
      if(int(a(i1,j1)).eq.0) goto 30
      i1 = i+1
      j1 = j
30    continue
      i = i1
      j = j1
   enddo
   if (mod(n,2) .ne. 0) return
!
!     rest of even order
!
   t = dble(m*m)
   do i = 1, m
      do j = 1, m
         im = i+m
         jm = j+m
         a(i,jm) = a(i,j) + 2*t
         a(im,j) = a(i,j) + 3*t
         a(im,jm) = a(i,j) + t
      enddo
   enddo
   m1 = (m-1)/2
   if (m1.eq.0) return
   do j = 1, m1
      call mat_rswap(m,a(1,j),1,a(m+1,j),1)
   enddo
   m1 = (m+1)/2
   m2 = m1 + m
   call mat_rswap(1,a(m1,1),1,a(m2,1),1)
   call mat_rswap(1,a(m1,m1),1,a(m2,m1),1)
   m1 = n+1-(m-3)/2
   if(m1.gt.n) return
   do j = m1, n
      call mat_rswap(m,a(1,j),1,a(m+1,j),1)
   enddo
   return
!
!     double even order
!
100 continue
   k = 1
   do i = 1, n
      do j = 1, n
         a(i,j) = k
         if (mod(i,4)/2 .eq. mod(j,4)/2) a(i,j) = n*n+1 - k
         k = k+1
      enddo
   enddo
end subroutine mat_magic
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rref(ar,ai,lda,m,n,eps)
integer,intent(in) :: lda
doubleprecision    :: ar(lda,*)
doubleprecision    :: ai(lda,*)
integer            :: m
integer            :: n
doubleprecision    :: eps

doubleprecision    :: tol
doubleprecision    :: tr
doubleprecision    :: ti
integer            :: i, j, k, l

   tol = 0.0d0
   do j = 1, n
      tol = dmax1(tol,mat_wasum(m,ar(1,j),ai(1,j),1))
   enddo
   tol = eps*dble(2*max0(m,n))*tol
   k = 1
   l = 1
   INFINITE: do
      IF (K.GT.M .OR. L.GT.N) RETURN

      i = mat_iwamax(m-k+1,ar(k,l),ai(k,l),1) + k-1
      if (dabs(ar(i,l))+dabs(ai(i,l)) .le. tol)then
         call mat_wset(m-k+1,0.0d0,0.0d0,ar(k,l),ai(k,l),1)
         l = l+1
         cycle INFINITE
      endif

      call mat_wswap(n-l+1,ar(i,l),ai(i,l),lda,ar(k,l),ai(k,l),lda)
      call mat_wdiv(1.0d0,0.0d0,ar(k,l),ai(k,l),tr,ti)
      call mat_wscal(n-l+1,tr,ti,ar(k,l),ai(k,l),lda)
      ar(k,l) = 1.0d0
      ai(k,l) = 0.0d0
      do i = 1, m
         tr = -ar(i,l)
         ti = -ai(i,l)
         if (i .ne. k) call matX_waxpy(n-l+1,tr,ti,ar(k,l),ai(k,l),lda,ar(i,l),ai(i,l),lda)
      enddo
      K = K+1
      L = L+1
   enddo INFINITE
end subroutine mat_rref
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_pythag(a,b)
doubleprecision :: a
doubleprecision :: b

doubleprecision :: p
doubleprecision :: q
doubleprecision :: r
doubleprecision :: s
doubleprecision :: t

   p = dmax1(dabs(a),dabs(b))
   q = dmin1(dabs(a),dabs(b))

   if (q .ne. 0.0d0) then

      INFINITE : do
         r = (q/p)**2
         t = 4.0d0 + r
         if (t .eq. 4.0d0) exit INFINITE
         s = r/t
         p = p + 2.0d0*p*s
         q = q*s
      enddo INFINITE

   endif

   mat_pythag = p
end function mat_pythag
end module M_matrix
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine matX_waxpy(N,SR,SI,xr,xi,INCX,yr,yi,INCY)
use M_matrix
implicit none
integer,intent(in)         :: n
doubleprecision,intent(in) :: sr
doubleprecision,intent(in) :: si
doubleprecision,intent(in) :: xr(*)
doubleprecision,intent(in) :: xi(*)
integer,intent(in)         :: incx
integer,intent(in)         :: incy

doubleprecision            :: yr(*)
doubleprecision            :: yi(*)
integer                    :: ix, iy

integer                    :: i

   if (n .le. 0) return
   if (sr .eq. 0.0d0 .and. si .eq. 0.0d0) return
   ix = 1
   iy = 1
   if (incx.lt.0) ix = (-n+1)*incx + 1
   if (incy.lt.0) iy = (-n+1)*incy + 1
   do i = 1, n
      yr(iy) = mat_flop(yr(iy) + sr*xr(ix) - si*xi(ix))
      yi(iy) = yi(iy) + sr*xi(ix) + si*xr(ix)
      if (yi(iy) .ne. 0.0d0) yi(iy) = mat_flop(yi(iy))
      ix = ix + incx
      iy = iy + incy
   enddo
end subroutine matX_waxpy
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
      SUBROUTINE ML_WGECO(AR,AI,LDA,N,IPVT,RCOND,ZR,ZI)
      use M_matrix
      INTEGER LDA,N,IPVT(*)
      DOUBLEPRECISION AR(LDA,*),AI(LDA,*),ZR(*),ZI(*)
      DOUBLEPRECISION RCOND
!
!     WGECO FACTORS A DOUBLE-COMPLEX MATRIX BY GAUSSIAN ELIMINATION
!     AND ESTIMATES THE CONDITION OF THE MATRIX.
!
!     IF  RCOND  IS NOT NEEDED, WGEFA IS SLIGHTLY FASTER.
!     TO SOLVE  A*X = B , FOLLOW WGECO BY WGESL.
!     TO COMPUTE  INVERSE(A)*C , FOLLOW WGECO BY WGESL.
!     TO COMPUTE  DETERMINANT(A) , FOLLOW WGECO BY WGEDI.
!     TO COMPUTE  INVERSE(A) , FOLLOW WGECO BY WGEDI.
!
!     ON ENTRY
!
!        A       DOUBLE-COMPLEX(LDA, N)
!                THE MATRIX TO BE FACTORED.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!     ON RETURN
!
!        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
!                WHICH WERE USED TO OBTAIN IT.
!                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
!                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
!                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
!
!        IPVT    INTEGER(N)
!                AN INTEGER VECTOR OF PIVOT INDICES.
!
!        RCOND   DOUBLEPRECISION
!                AN ESTIMATE OF THE RECIPROCAL CONDITION OF  A .
!                FOR THE SYSTEM  A*X = B , RELATIVE PERTURBATIONS
!                IN  A  AND  B  OF SIZE  EPSILON  MAY CAUSE
!                RELATIVE PERTURBATIONS IN  X  OF SIZE  EPSILON/RCOND .
!                IF  RCOND  IS SO SMALL THAT THE LOGICAL EXPRESSION
!        1.0 + RCOND .EQ. 1.0
!                IS TRUE, THEN  A  MAY BE SINGULAR TO WORKING
!                PRECISION.  IN PARTICULAR,  RCOND  IS ZERO  IF
!                EXACT SINGULARITY IS DETECTED OR THE ESTIMATE
!                UNDERFLOWS.
!
!        Z       DOUBLE-COMPLEX(N)
!                A WORK VECTOR WHOSE CONTENTS ARE USUALLY UNIMPORTANT.
!                IF  A  IS CLOSE TO A SINGULAR MATRIX, THEN  Z  IS
!                AN APPROXIMATE NULL VECTOR IN THE SENSE THAT
!                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) .
!
!     LINPACK. THIS VERSION DATED 07/01/79 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     LINPACK WGEFA
!     BLAS WAXPY,WDOTC,mat_wasum
!     FORTRAN DABS,DMAX1
!
!     INTERNAL VARIABLES
!
      DOUBLEPRECISION EKR,EKI,TR,TI,WKR,WKI,WKMR,WKMI
      DOUBLEPRECISION ANORM,S,SM,YNORM
      INTEGER INFO,J,K,KB,KP1,L
!
      DOUBLEPRECISION ZDUMR,ZDUMI
      DOUBLEPRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     COMPUTE 1-NORM OF A
!
      ANORM = 0.0D0
      DO J = 1, N
         ANORM = DMAX1(ANORM,mat_wasum(N,AR(1,J),AI(1,J),1))
      enddo
!
!     FACTOR
!
      CALL ML_WGEFA(AR,AI,LDA,N,IPVT,INFO)
!
!     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
!     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  CTRANS(A)*Y = E .
!     CTRANS(A)  IS THE CONJUGATE TRANSPOSE OF A .
!     THE COMPONENTS OF  E  ARE CHOSEN TO CAUSE MAXIMUM LOCAL
!     GROWTH IN THE ELEMENTS OF W  WHERE  CTRANS(U)*W = E .
!     THE VECTORS ARE FREQUENTLY RESCALED TO AVOID OVERFLOW.
!
!     SOLVE CTRANS(U)*W = E
!
      EKR = 1.0D0
      EKI = 0.0D0
      DO J = 1, N
         ZR(J) = 0.0D0
         ZI(J) = 0.0D0
      enddo
      DO 110 K = 1, N
         CALL mat_wsign(EKR,EKI,-ZR(K),-ZI(K),EKR,EKI)
         IF (CABS1(EKR-ZR(K),EKI-ZI(K)) .LE. CABS1(AR(K,K),AI(K,K))) GOTO 40
            S = CABS1(AR(K,K),AI(K,K)) / CABS1(EKR-ZR(K),EKI-ZI(K))
            CALL mat_wrscal(N,S,ZR,ZI,1)
            EKR = S*EKR
            EKI = S*EKI
   40    CONTINUE
         WKR = EKR - ZR(K)
         WKI = EKI - ZI(K)
         WKMR = -EKR - ZR(K)
         WKMI = -EKI - ZI(K)
         S = CABS1(WKR,WKI)
         SM = CABS1(WKMR,WKMI)
         IF (CABS1(AR(K,K),AI(K,K)) .EQ. 0.0D0) GOTO 50
            CALL mat_wdiv(WKR,WKI,AR(K,K),-AI(K,K),WKR,WKI)
            CALL mat_wdiv(WKMR,WKMI,AR(K,K),-AI(K,K),WKMR,WKMI)
         GOTO 60
   50    CONTINUE
            WKR = 1.0D0
            WKI = 0.0D0
            WKMR = 1.0D0
            WKMI = 0.0D0
   60    CONTINUE
         KP1 = K + 1
         IF (KP1 .GT. N) GOTO 100
            DO J = KP1, N
               CALL mat_wmul(WKMR,WKMI,AR(K,J),-AI(K,J),TR,TI)
               SM = mat_flop(SM + CABS1(ZR(J)+TR,ZI(J)+TI))
               CALL matX_waxpy(1,WKR,WKI,[AR(K,J)],[-AI(K,J)],1,ZR(J),ZI(J),1)
               S = mat_flop(S + CABS1(ZR(J),ZI(J)))
            enddo
            IF (S .GE. SM) GOTO 90
               TR = WKMR - WKR
               TI = WKMI - WKI
               WKR = WKMR
               WKI = WKMI
               DO J = KP1, N
                  CALL matX_waxpy(1,TR,TI,[AR(K,J)],[-AI(K,J)],1,ZR(J),ZI(J),1)
               enddo
   90       CONTINUE
  100    CONTINUE
         ZR(K) = WKR
         ZI(K) = WKI
  110 CONTINUE
      S = 1.0D0/mat_wasum(N,ZR,ZI,1)
      CALL mat_wrscal(N,S,ZR,ZI,1)
!
!     SOLVE CTRANS(L)*Y = W
!
      DO KB = 1, N
         K = N + 1 - KB
         IF (K .GE. N) GOTO 120
            ZR(K) = ZR(K) + mat_wdotcr(N-K,AR(K+1,K),AI(K+1,K),1,ZR(K+1),ZI(K+1),1)
            ZI(K) = ZI(K) + mat_wdotci(N-K,AR(K+1,K),AI(K+1,K),1,ZR(K+1),ZI(K+1),1)
  120    CONTINUE
         IF (CABS1(ZR(K),ZI(K)) .LE. 1.0D0) GOTO 130
            S = 1.0D0/CABS1(ZR(K),ZI(K))
            CALL mat_wrscal(N,S,ZR,ZI,1)
  130    CONTINUE
         L = IPVT(K)
         TR = ZR(L)
         TI = ZI(L)
         ZR(L) = ZR(K)
         ZI(L) = ZI(K)
         ZR(K) = TR
         ZI(K) = TI
      enddo
      S = 1.0D0/mat_wasum(N,ZR,ZI,1)
      CALL mat_wrscal(N,S,ZR,ZI,1)
!
      YNORM = 1.0D0
!
!     SOLVE L*V = Y
!
      DO K = 1, N
         L = IPVT(K)
         TR = ZR(L)
         TI = ZI(L)
         ZR(L) = ZR(K)
         ZI(L) = ZI(K)
         ZR(K) = TR
         ZI(K) = TI
         IF (K .LT. N) CALL matX_waxpy(N-K,TR,TI,AR(K+1,K),AI(K+1,K),1,ZR(K+1),ZI(K+1),1)
         IF (CABS1(ZR(K),ZI(K)) .LE. 1.0D0) cycle
            S = 1.0D0/CABS1(ZR(K),ZI(K))
            CALL mat_wrscal(N,S,ZR,ZI,1)
            YNORM = S*YNORM
      enddo
      S = 1.0D0/mat_wasum(N,ZR,ZI,1)
      CALL mat_wrscal(N,S,ZR,ZI,1)
      YNORM = S*YNORM
!
!     SOLVE  U*Z = V
!
      DO KB = 1, N
         K = N + 1 - KB
         IF (CABS1(ZR(K),ZI(K)) .LE. CABS1(AR(K,K),AI(K,K))) GOTO 170
            S = CABS1(AR(K,K),AI(K,K)) / CABS1(ZR(K),ZI(K))
            CALL mat_wrscal(N,S,ZR,ZI,1)
            YNORM = S*YNORM
  170    CONTINUE
         IF (CABS1(AR(K,K),AI(K,K)) .EQ. 0.0D0) GOTO 180
            CALL mat_wdiv(ZR(K),ZI(K),AR(K,K),AI(K,K),ZR(K),ZI(K))
  180    CONTINUE
         IF (CABS1(AR(K,K),AI(K,K)) .NE. 0.0D0) GOTO 190
            ZR(K) = 1.0D0
            ZI(K) = 0.0D0
  190    CONTINUE
         TR = -ZR(K)
         TI = -ZI(K)
         CALL matX_waxpy(K-1,TR,TI,AR(1,K),AI(1,K),1,ZR(1),ZI(1),1)
      enddo
!     MAKE ZNORM = 1.0
      S = 1.0D0/mat_wasum(N,ZR,ZI,1)
      CALL mat_wrscal(N,S,ZR,ZI,1)
      YNORM = S*YNORM
!
      IF (ANORM .NE. 0.0D0) RCOND = YNORM/ANORM
      IF (ANORM .EQ. 0.0D0) RCOND = 0.0D0
      END SUBROUTINE ML_WGECO
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WGEFA(AR,AI,LDA,N,IPVT,INFO)
      use M_matrix
      INTEGER LDA,N,IPVT(*),INFO
      DOUBLEPRECISION AR(LDA,*),AI(LDA,*)
!
!     WGEFA FACTORS A DOUBLE-COMPLEX MATRIX BY GAUSSIAN ELIMINATION.
!
!     WGEFA IS USUALLY CALLED BY WGECO, BUT IT CAN BE CALLED
!     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.
!     (TIME FOR WGECO) = (1 + 9/N)*(TIME FOR WGEFA) .
!
!     ON ENTRY
!
!        A       DOUBLE-COMPLEX(LDA, N)
!                THE MATRIX TO BE FACTORED.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!     ON RETURN
!
!        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
!                WHICH WERE USED TO OBTAIN IT.
!                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
!                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
!                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
!
!        IPVT    INTEGER(N)
!                AN INTEGER VECTOR OF PIVOT INDICES.
!
!        INFO    INTEGER
!                = 0  NORMAL VALUE.
!                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
!  CONDITION FOR THIS SUBROUTINE, BUT IT DOES
!  INDICATE THAT WGESL OR WGEDI WILL DIVIDE BY ZERO
!  IF CALLED.  USE  RCOND  IN WGECO FOR A RELIABLE
!  INDICATION OF SINGULARITY.
!
!     LINPACK. THIS VERSION DATED 07/01/79 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     BLAS WAXPY,WSCAL,mat_iwamax
!     FORTRAN DABS
!
!     INTERNAL VARIABLES
!
      DOUBLEPRECISION TR,TI
      INTEGER J,K,KP1,L,NM1
!
      DOUBLEPRECISION ZDUMR,ZDUMI
      DOUBLEPRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
!
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GOTO 70
      DO 60 K = 1, NM1
         KP1 = K + 1
!
!        FIND L = PIVOT INDEX
!
         L = mat_iwamax(N-K+1,AR(K,K),AI(K,K),1) + K - 1
         IPVT(K) = L
!
!        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
!
         IF (CABS1(AR(L,K),AI(L,K)) .EQ. 0.0D0) GOTO 40
!
!           INTERCHANGE IF NECESSARY
!
            IF (L .EQ. K) GOTO 10
               TR = AR(L,K)
               TI = AI(L,K)
               AR(L,K) = AR(K,K)
               AI(L,K) = AI(K,K)
               AR(K,K) = TR
               AI(K,K) = TI
   10       CONTINUE
!
!           COMPUTE MULTIPLIERS
!
            CALL mat_wdiv(-1.0D0,0.0D0,AR(K,K),AI(K,K),TR,TI)
            CALL mat_wscal(N-K,TR,TI,AR(K+1,K),AI(K+1,K),1)
!
!           ROW ELIMINATION WITH COLUMN INDEXING
!
            DO J = KP1, N
               TR = AR(L,J)
               TI = AI(L,J)
               IF (L .EQ. K) GOTO 20
                  AR(L,J) = AR(K,J)
                  AI(L,J) = AI(K,J)
                  AR(K,J) = TR
                  AI(K,J) = TI
   20          CONTINUE
               CALL matX_waxpy(N-K,TR,TI,AR(K+1,K),AI(K+1,K),1,AR(K+1,J),AI(K+1,J),1)
            enddo
         GOTO 50
   40    CONTINUE
            INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (CABS1(AR(N,N),AI(N,N)) .EQ. 0.0D0) INFO = N
      END SUBROUTINE ML_WGEFA
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WGESL(AR,AI,LDA,N,IPVT,BR,BI,JOB)
use M_matrix
INTEGER LDA,N,IPVT(*),JOB
DOUBLEPRECISION AR(LDA,*),AI(LDA,*),BR(*),BI(*)
!
!     WGESL SOLVES THE DOUBLE-COMPLEX SYSTEM
!     A * X = B  OR  CTRANS(A) * X = B
!     USING THE FACTORS COMPUTED BY WGECO OR WGEFA.
!
!     ON ENTRY
!
!        A       DOUBLE-COMPLEX(LDA, N)
!                THE OUTPUT FROM WGECO OR WGEFA.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!        IPVT    INTEGER(N)
!                THE PIVOT VECTOR FROM WGECO OR WGEFA.
!
!        B       DOUBLE-COMPLEX(N)
!                THE RIGHT HAND SIDE VECTOR.
!
!        JOB     INTEGER
!                = 0         TO SOLVE  A*X = B ,
!                = NONZERO   TO SOLVE  CTRANS(A)*X = B  WHERE
!         CTRANS(A)  IS THE CONJUGATE TRANSPOSE.
!
!     ON RETURN
!
!        B       THE SOLUTION VECTOR  X .
!
!     ERROR CONDITION
!
!        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
!        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY
!        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
!        SETTING OF LDA .  IT WILL NOT OCCUR IF THE SUBROUTINES ARE
!        CALLED CORRECTLY AND IF WGECO HAS SET RCOND .GT. 0.0
!        OR WGEFA HAS SET INFO .EQ. 0 .
!
!     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX
!     WITH  P  COLUMNS
!           CALL ML_WGECO(A,LDA,N,IPVT,RCOND,Z)
!           IF (RCOND IS TOO SMALL) GOTO ...
!           DO J = 1, P
!              CALL ML_WGESL(A,LDA,N,IPVT,C(1,J),0)
!           enddo
!
!     LINPACK. THIS VERSION DATED 07/01/79 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     BLAS WAXPY,WDOTC
!
!     INTERNAL VARIABLES
!
DOUBLEPRECISION TR,TI
INTEGER K,KB,L,NM1
!
   NM1 = N - 1
   IF (JOB .NE. 0) GOTO 50
!
!        JOB = 0 , SOLVE  A * X = B
!        FIRST SOLVE  L*Y = B
!
   IF (NM1 .GT. 1) then
      DO K = 1, NM1
         L = IPVT(K)
         TR = BR(L)
         TI = BI(L)
         IF (L .NE. K) then
            BR(L) = BR(K)
            BI(L) = BI(K)
            BR(K) = TR
            BI(K) = TI
         endif
         CALL matX_waxpy(N-K,TR,TI,AR(K+1,K),AI(K+1,K),1,BR(K+1),BI(K+1),1)
      enddo
   endif
!
!        NOW SOLVE  U*X = Y
!
   DO KB = 1, N
      K = N + 1 - KB
      CALL mat_wdiv(BR(K),BI(K),AR(K,K),AI(K,K),BR(K),BI(K))
      TR = -BR(K)
      TI = -BI(K)
      CALL matX_waxpy(K-1,TR,TI,AR(1,K),AI(1,K),1,BR(1),BI(1),1)
   enddo
   GOTO 100
50 CONTINUE
!
!  JOB = NONZERO, SOLVE  CTRANS(A) * X = B
!  FIRST SOLVE  CTRANS(U)*Y = B
!
   DO K = 1, N
      TR = BR(K) - mat_wdotcr(K-1,AR(1,K),AI(1,K),1,BR(1),BI(1),1)
      TI = BI(K) - mat_wdotci(K-1,AR(1,K),AI(1,K),1,BR(1),BI(1),1)
      CALL mat_wdiv(TR,TI,AR(K,K),-AI(K,K),BR(K),BI(K))
   enddo
!
!        NOW SOLVE CTRANS(L)*X = Y
!
   IF (NM1 .GE. 1) then
      DO KB = 1, NM1
         K = N - KB
         BR(K) = BR(K) + mat_wdotcr(N-K,AR(K+1,K),AI(K+1,K),1,BR(K+1),BI(K+1),1)
         BI(K) = BI(K) + mat_wdotci(N-K,AR(K+1,K),AI(K+1,K),1,BR(K+1),BI(K+1),1)
         L = IPVT(K)
         IF (L .EQ. K) cycle
         TR = BR(L)
         TI = BI(L)
         BR(L) = BR(K)
         BI(L) = BI(K)
         BR(K) = TR
         BI(K) = TI
      enddo
   endif
100 continue
end subroutine ml_wgesl
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WGEDI(ar,ai,LDA,N,ipvt,detr,deti,workr,worki,JOB)
      use M_matrix
      INTEGER LDA,N,IPVT(*),JOB
      DOUBLEPRECISION AR(LDA,*),AI(LDA,*),DETR(2),DETI(2),WORKR(*),WORKI(*)
!
!     WGEDI COMPUTES THE DETERMINANT AND INVERSE OF A MATRIX
!     USING THE FACTORS COMPUTED BY WGECO OR WGEFA.
!
!     ON ENTRY
!
!        A       DOUBLE-COMPLEX(LDA, N)
!                THE OUTPUT FROM WGECO OR WGEFA.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!        IPVT    INTEGER(N)
!                THE PIVOT VECTOR FROM WGECO OR WGEFA.
!
!        WORK    DOUBLE-COMPLEX(N)
!                WORK VECTOR.  CONTENTS DESTROYED.
!
!        JOB     INTEGER
!                = 11   BOTH DETERMINANT AND INVERSE.
!                = 01   INVERSE ONLY.
!                = 10   DETERMINANT ONLY.
!
!     ON RETURN
!
!        A       INVERSE OF ORIGINAL MATRIX IF REQUESTED.
!                OTHERWISE UNCHANGED.
!
!        DET     DOUBLE-COMPLEX(2)
!                DETERMINANT OF ORIGINAL MATRIX IF REQUESTED.
!                OTHERWISE NOT REFERENCED.
!                DETERMINANT = DET(1) * 10.0**DET(2)
!                WITH  1.0 .LE. CABS1(DET(1) .LT. 10.0
!                OR  DET(1) .EQ. 0.0 .
!
!     ERROR CONDITION
!
!        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS
!        A ZERO ON THE DIAGONAL AND THE INVERSE IS REQUESTED.
!        IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED CORRECTLY
!        AND IF WGECO HAS SET RCOND .GT. 0.0 OR WGEFA HAS SET
!        INFO .EQ. 0 .
!
!     LINPACK. THIS VERSION DATED 07/01/79 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     BLAS WAXPY,WSCAL,WSWAP
!     FORTRAN DABS,MOD
!
!     INTERNAL VARIABLES
!
      DOUBLEPRECISION TR,TI
      DOUBLEPRECISION TEN
      INTEGER I,J,K,KB,KP1,L,NM1
!
      DOUBLEPRECISION ZDUMR,ZDUMI
      DOUBLEPRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     COMPUTE DETERMINANT
!
      IF (JOB/10 .EQ. 0) GOTO 80
         DETR(1) = 1.0D0
         DETI(1) = 0.0D0
         DETR(2) = 0.0D0
         DETI(2) = 0.0D0
         TEN = 10.0D0
         DO 60 I = 1, N
           IF (IPVT(I) .EQ. I) GOTO 10
              DETR(1) = -DETR(1)
              DETI(1) = -DETI(1)
   10      CONTINUE
           CALL mat_wmul(AR(I,I),AI(I,I),DETR(1),DETI(1),DETR(1),DETI(1))
!          ...EXIT
!       ...EXIT
           IF (CABS1(DETR(1),DETI(1)) .EQ. 0.0D0) GOTO 70
   20      IF (CABS1(DETR(1),DETI(1)) .GE. 1.0D0) GOTO 30
              DETR(1) = TEN*DETR(1)
              DETI(1) = TEN*DETI(1)
              DETR(2) = DETR(2) - 1.0D0
              DETI(2) = DETI(2) - 0.0D0
           GOTO 20
   30      CONTINUE
   40      IF (CABS1(DETR(1),DETI(1)) .LT. TEN) GOTO 50
              DETR(1) = DETR(1)/TEN
              DETI(1) = DETI(1)/TEN
              DETR(2) = DETR(2) + 1.0D0
              DETI(2) = DETI(2) + 0.0D0
           GOTO 40
   50      CONTINUE
   60    CONTINUE
   70    CONTINUE
   80 CONTINUE
!
!     COMPUTE INVERSE(U)
!
      IF (MOD(JOB,10) .EQ. 0) GOTO 160
         DO K = 1, N
            CALL mat_wdiv(1.0D0,0.0D0,AR(K,K),AI(K,K),AR(K,K),AI(K,K))
            TR = -AR(K,K)
            TI = -AI(K,K)
            CALL mat_wscal(K-1,TR,TI,AR(1,K),AI(1,K),1)
            KP1 = K + 1
            IF (N .LT. KP1) cycle
            DO J = KP1, N
              TR = AR(K,J)
              TI = AI(K,J)
              AR(K,J) = 0.0D0
              AI(K,J) = 0.0D0
              CALL matX_waxpy(K,TR,TI,AR(1,K),AI(1,K),1,AR(1,J),AI(1,J),1)
            enddo
         enddo
!
!        FORM INVERSE(U)*INVERSE(L)
!
         NM1 = N - 1
         IF (NM1 .LT. 1) GOTO 150
         DO KB = 1, NM1
            K = N - KB
            KP1 = K + 1
            DO I = KP1, N
               WORKR(I) = AR(I,K)
               WORKI(I) = AI(I,K)
               AR(I,K) = 0.0D0
               AI(I,K) = 0.0D0
            enddo
            DO J = KP1, N
              TR = WORKR(J)
              TI = WORKI(J)
              CALL matX_waxpy(N,TR,TI,AR(1,J),AI(1,J),1,AR(1,K),AI(1,K),1)
            enddo
            L = IPVT(K)
            IF (L .NE. K)CALL mat_wswap(N,AR(1,K),AI(1,K),1,AR(1,L),AI(1,L),1)
         enddo
  150    CONTINUE
  160 CONTINUE
      END SUBROUTINE ML_WGEDI
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_HTRIDI(NM,N,AR,AI,D,E,E2,TAU)
      use M_matrix
!
      INTEGER I,J,K,L,N,II,NM,JP1
      DOUBLEPRECISION AR(NM,N),AI(NM,N),D(N),E(N),E2(N),TAU(2,N)
      DOUBLEPRECISION F,G,H,FI,GI,HH,SI,SCALE
!
!     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF
!     THE ALGOL PROCEDURE TRED1, NUM. MATH. 11, 181-195(1968)
!     BY MARTIN, REINSCH, AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
!
!     THIS SUBROUTINE REDUCES A COMPLEX HERMITIAN MATRIX
!     TO A REAL SYMMETRIC TRIDIAGONAL MATRIX USING
!     UNITARY SIMILARITY TRANSFORMATIONS.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE COMPLEX HERMITIAN INPUT MATRIX.
!          ONLY THE LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED.
!
!     ON OUTPUT.
!
!        AR AND AI CONTAIN INFORMATION ABOUT THE UNITARY TRANS-
!          FORMATIONS USED IN THE REDUCTION IN THEIR FULL LOWER
!          TRIANGLES.  THEIR STRICT UPPER TRIANGLES AND THE
!          DIAGONAL OF AR ARE UNALTERED.
!
!        D CONTAINS THE DIAGONAL ELEMENTS OF THE THE TRIDIAGONAL MATRIX.
!
!        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL
!          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO.
!
!        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.
!          E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.
!
!        TAU CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS.
!
!     MODIFIED TO GET RID OF ALL COMPLEX ARITHMETIC, C. MOLER, 6/27/79.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
      TAU(1,N) = 1.0D0
      TAU(2,N) = 0.0D0
!
      DO I = 1, N
         D(I) = AR(I,I)
      enddo
!     .......... FOR I=N STEP -1 UNTIL 1 DO -- ..........
      DO 300 II = 1, N
         I = N + 1 - II
         L = I - 1
         H = 0.0D0
         SCALE = 0.0D0
         IF (L .LT. 1) GOTO 130
!     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) ..........
         DO K = 1, L
            SCALE = mat_flop(SCALE + DABS(AR(I,K)) + DABS(AI(I,K)))
         enddo
!
         IF (SCALE .NE. 0.0D0) GOTO 140
         TAU(1,L) = 1.0D0
         TAU(2,L) = 0.0D0
  130    E(I) = 0.0D0
         E2(I) = 0.0D0
         GOTO 290
!
  140    continue
         DO K = 1, L
            AR(I,K) = mat_flop(AR(I,K)/SCALE)
            AI(I,K) = mat_flop(AI(I,K)/SCALE)
            H = mat_flop(H + AR(I,K)*AR(I,K) + AI(I,K)*AI(I,K))
         enddo
!
         E2(I) = mat_flop(SCALE*SCALE*H)
         G = mat_flop(DSQRT(H))
         E(I) = mat_flop(SCALE*G)
         F = mat_pythag(AR(I,L),AI(I,L))
!     .......... FORM NEXT DIAGONAL ELEMENT OF MATRIX T ..........
         IF (F .EQ. 0.0D0) GOTO 160
         TAU(1,L) = mat_flop((AI(I,L)*TAU(2,I) - AR(I,L)*TAU(1,I))/F)
         SI = mat_flop((AR(I,L)*TAU(2,I) + AI(I,L)*TAU(1,I))/F)
         H = mat_flop(H + F*G)
         G = mat_flop(1.0D0 + G/F)
         AR(I,L) = mat_flop(G*AR(I,L))
         AI(I,L) = mat_flop(G*AI(I,L))
         IF (L .EQ. 1) GOTO 270
         GOTO 170
  160    TAU(1,L) = -TAU(1,I)
         SI = TAU(2,I)
         AR(I,L) = G
  170    F = 0.0D0
!
         DO J = 1, L
            G = 0.0D0
            GI = 0.0D0
!     .......... FORM ELEMENT OF A*U ..........
            DO K = 1, J
               G = mat_flop(G + AR(J,K)*AR(I,K) + AI(J,K)*AI(I,K))
               GI = mat_flop(GI - AR(J,K)*AI(I,K) + AI(J,K)*AR(I,K))
            enddo
!
            JP1 = J + 1
            IF (L .LT. JP1) GOTO 220
!
            DO K = JP1, L
               G = mat_flop(G + AR(K,J)*AR(I,K) - AI(K,J)*AI(I,K))
               GI = mat_flop(GI - AR(K,J)*AI(I,K) - AI(K,J)*AR(I,K))
            enddo
!     .......... FORM ELEMENT OF P ..........
  220       continue
            E(J) = mat_flop(G/H)
            TAU(2,J) = mat_flop(GI/H)
            F = mat_flop(F + E(J)*AR(I,J) - TAU(2,J)*AI(I,J))
         enddo
!
         HH = mat_flop(F/(H + H))
!     .......... FORM REDUCED A ..........
         DO J = 1, L
            F = AR(I,J)
            G = mat_flop(E(J) - HH*F)
            E(J) = G
            FI = -AI(I,J)
            GI = mat_flop(TAU(2,J) - HH*FI)
            TAU(2,J) = -GI
!
            DO K = 1, J
               AR(J,K) = mat_flop(AR(J,K) - F*E(K) - G*AR(I,K) + FI*TAU(2,K) + GI*AI(I,K))
               AI(J,K) = mat_flop(AI(J,K) - F*TAU(2,K) - G*AI(I,K) - FI*E(K) - GI*AR(I,K))
            enddo
         enddo
!
  270    continue
         DO K = 1, L
            AR(I,K) = mat_flop(SCALE*AR(I,K))
            AI(I,K) = mat_flop(SCALE*AI(I,K))
         enddo
!
         TAU(2,L) = -SI
  290    HH = D(I)
         D(I) = AR(I,I)
         AR(I,I) = HH
         AI(I,I) = mat_flop(SCALE*DSQRT(H))
  300 CONTINUE
!
END SUBROUTINE ML_HTRIDI
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_HTRIBK(NM,N,AR,AI,TAU,M,ZR,ZI)
use M_matrix
!
INTEGER I,J,K,L,M,N,NM
DOUBLEPRECISION AR(NM,N),AI(NM,N),TAU(2,N),ZR(NM,M),ZI(NM,M)
DOUBLEPRECISION H,S,SI
!
!     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF
!     THE ALGOL PROCEDURE TRBAK1, NUM. MATH. 11, 181-195(1968)
!     BY MARTIN, REINSCH, AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
!
!     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A COMPLEX HERMITIAN
!     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING
!     REAL SYMMETRIC TRIDIAGONAL MATRIX DETERMINED BY  HTRIDI.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        AR AND AI CONTAIN INFORMATION ABOUT THE UNITARY TRANS-
!          FORMATIONS USED IN THE REDUCTION BY  HTRIDI  IN THEIR
!          FULL LOWER TRIANGLES EXCEPT FOR THE DIAGONAL OF AR.
!
!        TAU CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS.
!
!        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED.
!
!        ZR CONTAINS THE EIGENVECTORS TO BE BACK TRANSFORMED
!          IN ITS FIRST M COLUMNS.
!
!     ON OUTPUT.
!
!        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE TRANSFORMED EIGENVECTORS
!          IN THEIR FIRST M COLUMNS.
!
!     NOTE THAT THE LAST COMPONENT OF EACH RETURNED VECTOR
!     IS REAL AND THAT VECTOR EUCLIDEAN NORMS ARE PRESERVED.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
   IF (M .EQ. 0) GOTO 200
!     .......... TRANSFORM THE EIGENVECTORS OF THE REAL SYMMETRIC
!                TRIDIAGONAL MATRIX TO THOSE OF THE HERMITIAN
!                TRIDIAGONAL MATRIX. ..........
   DO K = 1, N
      DO J = 1, M
         ZI(K,J) = mat_flop(-(ZR(K,J)*TAU(2,K)))
         ZR(K,J) = mat_flop(ZR(K,J)*TAU(1,K))
      enddo
   enddo
!
   IF (N .EQ. 1) GOTO 200
!     .......... RECOVER AND APPLY THE HOUSEHOLDER MATRICES ..........
   DO I = 2, N
      L = I - 1
      H = AI(I,I)
      IF (H .EQ. 0.0D0) exit
      DO J = 1, M
         S = 0.0D0
         SI = 0.0D0
         DO K = 1, L
            S = mat_flop(S + AR(I,K)*ZR(K,J) - AI(I,K)*ZI(K,J))
            SI = mat_flop(SI + AR(I,K)*ZI(K,J) + AI(I,K)*ZR(K,J))
         enddo
!     .......... DOUBLE DIVISIONS AVOID POSSIBLE UNDERFLOW ..........
         S = mat_flop((S/H)/H)
         SI = mat_flop((SI/H)/H)
         DO K = 1, L
            ZR(K,J) = mat_flop(ZR(K,J) - S*AR(I,K) - SI*AI(I,K))
            ZI(K,J) = mat_flop(ZI(K,J) - SI*AR(I,K) + S*AI(I,K))
         enddo
      enddo
   enddo
!
200 continue
END SUBROUTINE ML_HTRIBK
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_IMTQL2(NM,N,D,E,Z,IERR,JOB)
      use M_matrix
      IMPLICIT NONE
      INTEGER I,J,K,L,M,N,II,NM,MML,IERR
      integer :: job
      DOUBLEPRECISION D(N),E(N),Z(NM,N)
      DOUBLEPRECISION B,C,F,G,P,R,S
!
!     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE IMTQL2,
!     NUM. MATH. 12, 377-383(1968) BY MARTIN AND WILKINSON,
!     AS MODIFIED IN NUM. MATH. 15, 450(1970) BY DUBRULLE.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971).
!
!     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
!     OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE IMPLICIT QL METHOD.
!     THE EIGENVECTORS OF A FULL SYMMETRIC MATRIX CAN ALSO
!     BE FOUND IF  TRED2  HAS BEEN USED TO REDUCE THIS
!     FULL MATRIX TO TRIDIAGONAL FORM.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.
!
!        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
!          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.
!
!        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE
!          REDUCTION BY  TRED2, IF PERFORMED.  IF THE EIGENVECTORS
!          OF THE TRIDIAGONAL MATRIX ARE DESIRED, Z MUST CONTAIN
!          THE IDENTITY MATRIX.
!
!      ON OUTPUT.
!
!        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
!          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT
!          UNORDERED FOR INDICES 1,2,...,IERR-1.
!
!        E HAS BEEN DESTROYED.
!
!        Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC
!          TRIDIAGONAL (OR FULL) MATRIX.  IF AN ERROR EXIT IS MADE,
!          Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED
!          EIGENVALUES.
!
!        IERR IS SET TO
!          ZERO       FOR NORMAL RETURN,
!          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
!  DETERMINED AFTER 30 ITERATIONS.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
!
!*****
!     MODIFIED BY C. MOLER TO ELIMINATE MACHEP 11/22/78
!     MODIFIED TO ADD JOB PARAMETER 08/27/79
!*****
      IERR = 0
      IF (N .EQ. 1) GOTO 1001
!
      DO I = 2, N
         E(I-1) = E(I)
      enddo
!
      E(N) = 0.0D0
!
      DO 240 L = 1, N
         J = 0
!     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT ..........
  105    continue
         DO M = L, N
            IF (M .EQ. N) GOTO 120
!*****
            P = mat_flop(DABS(D(M)) + DABS(D(M+1)))
            S = mat_flop(P + DABS(E(M)))
            IF (P .EQ. S) GOTO 120
!*****
         enddo
!
  120    continue
         P = D(L)
         IF (M .EQ. L) GOTO 240
         IF (J .EQ. 30) GOTO 1000
         J = J + 1
!     .......... FORM SHIFT ..........
         G = mat_flop((D(L+1) - P)/(2.0D0*E(L)))
         R = mat_flop(DSQRT(G*G+1.0D0))
         G = mat_flop(D(M) - P + E(L)/(G + DSIGN(R,G)))
         S = 1.0D0
         C = 1.0D0
         P = 0.0D0
         MML = M - L
!     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
         DO 200 II = 1, MML
            I = M - II
            F = mat_flop(S*E(I))
            B = mat_flop(C*E(I))
            IF (DABS(F) .LT. DABS(G)) GOTO 150
            C = mat_flop(G/F)
            R = mat_flop(DSQRT(C*C+1.0D0))
            E(I+1) = mat_flop(F*R)
            S = mat_flop(1.0D0/R)
            C = mat_flop(C*S)
            GOTO 160
  150       S = mat_flop(F/G)
            R = mat_flop(DSQRT(S*S+1.0D0))
            E(I+1) = mat_flop(G*R)
            C = mat_flop(1.0D0/R)
            S = mat_flop(S*C)
  160       G = mat_flop(D(I+1) - P)
            R = mat_flop((D(I) - G)*S + 2.0D0*C*B)
            P = mat_flop(S*R)
            D(I+1) = G + P
            G = mat_flop(C*R - B)
            IF (JOB .EQ. 0) GOTO 185
!     .......... FORM VECTOR ..........
            DO K = 1, N
               F = Z(K,I+1)
               Z(K,I+1) = mat_flop(S*Z(K,I) + C*F)
               Z(K,I) = mat_flop(C*Z(K,I) - S*F)
            enddo
  185       CONTINUE
!
  200    CONTINUE
!
         D(L) = mat_flop(D(L) - P)
         E(L) = G
         E(M) = 0.0D0
         GOTO 105
  240 CONTINUE
!     .......... ORDER EIGENVALUES AND EIGENVECTORS ..........
      DO II = 2, N
         I = II - 1
         K = I
         P = D(I)
!
         DO J = II, N
            IF (D(J) .GE. P) exit
            K = J
            P = D(J)
         enddo
!
         IF (K .EQ. I) exit
         D(K) = D(I)
         D(I) = P
!
         IF (JOB .EQ. 0) cycle
         DO J = 1, N
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
         enddo
      enddo
!
      GOTO 1001
!     .......... SET ERROR -- NO CONVERGENCE TO AN
!                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 CONTINUE
      IERR = L
 1001 CONTINUE
      RETURN
      END SUBROUTINE ML_IMTQL2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_CORTH(NM,N,LOW,IGH,AR,AI,ORTR,ORTI)
use M_matrix
!
INTEGER I,J,M,N,II,JJ,LA,MP,NM,IGH,KP1,LOW
DOUBLEPRECISION AR(NM,N),AI(NM,N),ORTR(IGH),ORTI(IGH)
DOUBLEPRECISION F,G,H,FI,FR,SCALE
!
!     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF
!     THE ALGOL PROCEDURE ORTHES, NUM. MATH. 12, 349-368(1968)
!     BY MARTIN AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).
!
!     GIVEN A COMPLEX GENERAL MATRIX, THIS SUBROUTINE
!     REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS
!     LOW THROUGH IGH TO UPPER HESSENBERG FORM BY
!     UNITARY SIMILARITY TRANSFORMATIONS.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
!          SUBROUTINE ML_CBAL.  IF  CBAL  HAS NOT BEEN USED,
!          SET LOW=1, IGH=N.
!
!        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE COMPLEX INPUT MATRIX.
!
!     ON OUTPUT.
!
!        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE HESSENBERG MATRIX.  INFORMATION
!          ABOUT THE UNITARY TRANSFORMATIONS USED IN THE REDUCTION
!          IS STORED IN THE REMAINING TRIANGLES UNDER THE
!          HESSENBERG MATRIX.
!
!        ORTR AND ORTI CONTAIN FURTHER INFORMATION ABOUT THE
!          TRANSFORMATIONS.  ONLY ELEMENTS LOW THROUGH IGH ARE USED.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
   LA = IGH - 1
   KP1 = LOW + 1
   IF (LA .LT. KP1) GOTO 200
!
   DO M = KP1, LA
      H = 0.0D0
      ORTR(M) = 0.0D0
      ORTI(M) = 0.0D0
      SCALE = 0.0D0
!     .......... SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) ..........
      DO I = M, IGH
         SCALE = mat_flop(SCALE + DABS(AR(I,M-1)) + DABS(AI(I,M-1)))
      enddo
!
      IF (SCALE .EQ. 0.0D0) cycle
      MP = M + IGH
!     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........
      DO II = M, IGH
         I = MP - II
         ORTR(I) = mat_flop(AR(I,M-1)/SCALE)
         ORTI(I) = mat_flop(AI(I,M-1)/SCALE)
         H = mat_flop(H + ORTR(I)*ORTR(I) + ORTI(I)*ORTI(I))
      enddo
!
      G = mat_flop(DSQRT(H))
      F = mat_pythag(ORTR(M),ORTI(M))
      IF (F .EQ. 0.0D0) GOTO 103
      H = mat_flop(H + F*G)
      G = mat_flop(G/F)
      ORTR(M) = mat_flop((1.0D0 + G)*ORTR(M))
      ORTI(M) = mat_flop((1.0D0 + G)*ORTI(M))
      GOTO 105
!
103   continue
      ORTR(M) = G
      AR(M,M-1) = SCALE
!     .......... FORM (I-(U*UT)/H)*A ..........
105   continue
      DO J = M, N
         FR = 0.0D0
         FI = 0.0D0
!     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........
         DO II = M, IGH
            I = MP - II
            FR = mat_flop(FR + ORTR(I)*AR(I,J) + ORTI(I)*AI(I,J))
            FI = mat_flop(FI + ORTR(I)*AI(I,J) - ORTI(I)*AR(I,J))
         enddo
!
         FR = mat_flop(FR/H)
         FI = mat_flop(FI/H)
!
         DO I = M, IGH
            AR(I,J) = mat_flop(AR(I,J) - FR*ORTR(I) + FI*ORTI(I))
            AI(I,J) = mat_flop(AI(I,J) - FR*ORTI(I) - FI*ORTR(I))
         enddo
!
      enddo
!     .......... FORM (I-(U*UT)/H)*A*(I-(U*UT)/H) ..........
      DO I = 1, IGH
         FR = 0.0D0
         FI = 0.0D0
!     .......... FOR J=IGH STEP -1 UNTIL M DO -- ..........
         DO JJ = M, IGH
            J = MP - JJ
            FR = mat_flop(FR + ORTR(J)*AR(I,J) - ORTI(J)*AI(I,J))
            FI = mat_flop(FI + ORTR(J)*AI(I,J) + ORTI(J)*AR(I,J))
         enddo
!
         FR = mat_flop(FR/H)
         FI = mat_flop(FI/H)
!
         DO J = M, IGH
            AR(I,J) = mat_flop(AR(I,J) - FR*ORTR(J) - FI*ORTI(J))
            AI(I,J) = mat_flop(AI(I,J) + FR*ORTI(J) - FI*ORTR(J))
         enddo
!
      enddo
!
      ORTR(M) = mat_flop(SCALE*ORTR(M))
      ORTI(M) = mat_flop(SCALE*ORTI(M))
      AR(M,M-1) = mat_flop(-(G*AR(M,M-1)))
      AI(M,M-1) = mat_flop(-(G*AI(M,M-1)))
   enddo
!
200 continue
END SUBROUTINE ML_CORTH
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine ml_comqr3(nm,n,low,igh,ortr,orti,hr,hi,wr,wi,zr,zi,ierr ,job)
!*****
!     MODIFICATION OF EISPACK COMQR2 TO ADD JOB PARAMETER
!     JOB = 0  OUTPUT H = SCHUR TRIANGULAR FORM, Z NOT USED
!         = 1  OUTPUT H = SCHUR FORM, Z = UNITARY SIMILARITY
!         = 2  SAME AS COMQR2
!         = 3  OUTPUT H = HESSENBERG FORM, Z = UNITARY SIMILARITY
!     ALSO ELIMINATE MACHEP
!     C. MOLER, 11/22/78 AND 09/14/80
!     OVERFLOW CONTROL IN EIGENVECTOR BACKSUBSTITUTION, 3/16/82
!*****
!
!
!     THIS SUBROUTINE IS A TRANSLATION OF A UNITARY ANALOGUE OF THE
!     ALGOL PROCEDURE  COMLR2, NUM. MATH. 16, 181-204(1970) BY PETERS
!     AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).
!     THE UNITARY ANALOGUE SUBSTITUTES THE QR ALGORITHM OF FRANCIS
!     (COMP. JOUR. 4, 332-345(1962)) FOR THE LR ALGORITHM.
!
!     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
!     OF A COMPLEX UPPER HESSENBERG MATRIX BY THE QR
!     METHOD.  THE EIGENVECTORS OF A COMPLEX GENERAL MATRIX
!     CAN ALSO BE FOUND IF  CORTH  HAS BEEN USED TO REDUCE
!     THIS GENERAL MATRIX TO HESSENBERG FORM.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
!          SUBROUTINE ML_CBAL.  IF  CBAL  HAS NOT BEEN USED,
!          SET LOW=1, IGH=N.
!
!        ORTR AND ORTI CONTAIN INFORMATION ABOUT THE UNITARY TRANS-
!          FORMATIONS USED IN THE REDUCTION BY  CORTH, IF PERFORMED.
!          ONLY ELEMENTS LOW THROUGH IGH ARE USED.  IF THE EIGENVECTORS
!          OF THE HESSENBERG MATRIX ARE DESIRED, SET ORTR(J) AND
!          ORTI(J) TO 0.0D0 FOR THESE ELEMENTS.
!
!        HR AND HI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE COMPLEX UPPER HESSENBERG MATRIX.
!          THEIR LOWER TRIANGLES BELOW THE SUBDIAGONAL CONTAIN FURTHER
!          INFORMATION ABOUT THE TRANSFORMATIONS WHICH WERE USED IN THE
!          REDUCTION BY  CORTH, IF PERFORMED.  IF THE EIGENVECTORS OF
!          THE HESSENBERG MATRIX ARE DESIRED, THESE ELEMENTS MAY BE
!          ARBITRARY.
!
!     ON OUTPUT.
!
!        ORTR, ORTI, AND THE UPPER HESSENBERG PORTIONS OF HR AND HI
!          HAVE BEEN DESTROYED.
!
!        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE EIGENVALUES.  IF AN ERROR
!          EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT
!          FOR INDICES IERR+1,...,N.
!
!        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE EIGENVECTORS.  THE EIGENVECTORS
!          ARE UNNORMALIZED.  IF AN ERROR EXIT IS MADE, NONE OF
!          THE EIGENVECTORS HAS BEEN FOUND.
!
!        IERR IS SET TO
!          ZERO       FOR NORMAL RETURN,
!          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
!  DETERMINED AFTER A TOTAL OF 30*N ITERATIONS.
!
!     MODIFIED TO GET RID OF ALL COMPLEX ARITHMETIC, C. MOLER, 6/27/79.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
use M_matrix
integer i,j,k,l,m,n,en,ii,ll,nm,nn,igh,ip1,itn,its,low,lp1,enm1,iend,ierr
doubleprecision hr(nm,n),hi(nm,n),wr(n),wi(n),zr(nm,n),zi(nm,n),ortr(igh),orti(igh)
doubleprecision si,sr,ti,tr,xi,xr,yi,yr,zzi,zzr,norm
integer :: job
integer :: jj
   ierr = 0
   !*****
   if (job .eq. 0) goto 150
   !*****
!     .......... INITIALIZE EIGENVECTOR MATRIX ..........
   do i = 1, n
      do j = 1, n
         zr(i,j) = 0.0d0
         zi(i,j) = 0.0d0
         if (i .eq. j) zr(i,j) = 1.0d0
      enddo
   enddo
!     .......... FORM THE MATRIX OF ACCUMULATED TRANSFORMATIONS
!                FROM THE INFORMATION LEFT BY CORTH ..........
   iend = igh - low - 1
   if (iend) 180, 150, 105
!     .......... for i=igh-1 step -1 until low+1 do -- ..........
105 continue
   do ii = 1, iend
      i = igh - ii
      if (ortr(i) .eq. 0.0d0 .and. orti(i) .eq. 0.0d0) cycle
      if (hr(i,i-1) .eq. 0.0d0 .and. hi(i,i-1) .eq. 0.0d0) cycle
!     .......... NORM BELOW IS NEGATIVE OF H FORMED IN CORTH ..........
      norm = mat_flop(hr(i,i-1)*ortr(i) + hi(i,i-1)*orti(i))
      ip1 = i + 1

      do k = ip1, igh
         ortr(k) = hr(k,i-1)
         orti(k) = hi(k,i-1)
      enddo

      do j = i, igh
         sr = 0.0d0
         si = 0.0d0

         do k = i, igh
            sr = mat_flop(sr + ortr(k)*zr(k,j) + orti(k)*zi(k,j))
            si = mat_flop(si + ortr(k)*zi(k,j) - orti(k)*zr(k,j))
         enddo

         sr = mat_flop(sr/norm)
         si = mat_flop(si/norm)

         do k = i, igh
            zr(k,j) = mat_flop(zr(k,j) + sr*ortr(k) - si*orti(k))
            zi(k,j) = mat_flop(zi(k,j) + sr*orti(k) + si*ortr(k))
         enddo

      enddo

   enddo
   !*****
   if (job .eq. 3) goto 1001
   !*****
!     .......... CREATE REAL SUBDIAGONAL ELEMENTS ..........
150 continue
   l = low + 1

   do i = l, igh
      ll = min0(i+1,igh)
      if (hi(i,i-1) .eq. 0.0d0) cycle
      norm = mat_pythag(hr(i,i-1),hi(i,i-1))
      yr = mat_flop(hr(i,i-1)/norm)
      yi = mat_flop(hi(i,i-1)/norm)
      hr(i,i-1) = norm
      hi(i,i-1) = 0.0d0

      do j = i, n
         si = mat_flop(yr*hi(i,j) - yi*hr(i,j))
         hr(i,j) = mat_flop(yr*hr(i,j) + yi*hi(i,j))
         hi(i,j) = si
      enddo

      do j = 1, ll
         si = mat_flop(yr*hi(j,i) + yi*hr(j,i))
         hr(j,i) = mat_flop(yr*hr(j,i) - yi*hi(j,i))
         hi(j,i) = si
      enddo
      !*****
      if (job .eq. 0) cycle
      !*****
      do j = low, igh
         si = mat_flop(yr*zi(j,i) + yi*zr(j,i))
         zr(j,i) = mat_flop(yr*zr(j,i) - yi*zi(j,i))
         zi(j,i) = si
      enddo

   enddo
!     .......... STORE ROOTS ISOLATED BY CBAL ..........
180 continue
   do i = 1, n
      if (i .ge. low .and. i .le. igh) cycle
      wr(i) = hr(i,i)
      wi(i) = hi(i,i)
   enddo

   en = igh
   tr = 0.0d0
   ti = 0.0d0
   itn = 30*n
!     .......... SEARCH FOR NEXT EIGENVALUE ..........
220 continue
   if (en .lt. low) goto 680
   its = 0
   enm1 = en - 1
!     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
!                FOR L=EN STEP -1 UNTIL LOW DO -- ..........
240 continue
   do ll = low, en
      l = en + low - ll
      if (l .eq. low) exit
      !*****
      xr = mat_flop(dabs(hr(l-1,l-1)) + dabs(hi(l-1,l-1)) + dabs(hr(l,l)) +dabs(hi(l,l)))
      yr = mat_flop(xr + dabs(hr(l,l-1)))
      if (xr .eq. yr) exit
      !*****
   enddo
!     .......... FORM SHIFT ..........
   if (l .eq. en) goto 660
   if (itn .eq. 0) goto 1000
   if (its .eq. 10 .or. its .eq. 20) goto 320
   sr = hr(en,en)
   si = hi(en,en)
   xr = mat_flop(hr(enm1,en)*hr(en,enm1))
   xi = mat_flop(hi(enm1,en)*hr(en,enm1))
   if (xr .eq. 0.0d0 .and. xi .eq. 0.0d0) goto 340
   yr = mat_flop((hr(enm1,enm1) - sr)/2.0d0)
   yi = mat_flop((hi(enm1,enm1) - si)/2.0d0)
   call mat_wsqrt(yr**2-yi**2+xr,2.0d0*yr*yi+xi,zzr,zzi)
   if (yr*zzr + yi*zzi .ge. 0.0d0) goto 310
   zzr = -zzr
   zzi = -zzi
310 continue
   call mat_wdiv(xr,xi,yr+zzr,yi+zzi,zzr,zzi)
   sr = mat_flop(sr - zzr)
   si = mat_flop(si - zzi)
   goto 340
!     .......... FORM EXCEPTIONAL SHIFT ..........
320 continue
   sr = mat_flop(dabs(hr(en,enm1)) + dabs(hr(enm1,en-2)))
   si = 0.0d0

340 continue
   do i = low, en
      hr(i,i) = mat_flop(hr(i,i) - sr)
      hi(i,i) = mat_flop(hi(i,i) - si)
   enddo

   tr = mat_flop(tr + sr)
   ti = mat_flop(ti + si)
   its = its + 1
   itn = itn - 1
!     .......... REDUCE TO TRIANGLE (ROWS) ..........
   lp1 = l + 1

   do i = lp1, en
      sr = hr(i,i-1)
      hr(i,i-1) = 0.0d0
      norm= mat_flop(dabs(hr(i-1,i-1)) + dabs(hi(i-1,i-1)) + dabs(sr))
      norm= mat_flop(norm*dsqrt((hr(i-1,i-1)/norm)**2 + (hi(i-1,i-1)/norm)**2 + (sr/norm)**2))
      xr = mat_flop(hr(i-1,i-1)/norm)
      wr(i-1) = xr
      xi = mat_flop(hi(i-1,i-1)/norm)
      wi(i-1) = xi
      hr(i-1,i-1) = norm
      hi(i-1,i-1) = 0.0d0
      hi(i,i-1) = mat_flop(sr/norm)

      do j = i, n
         yr = hr(i-1,j)
         yi = hi(i-1,j)
         zzr = hr(i,j)
         zzi = hi(i,j)
         hr(i-1,j) = mat_flop(xr*yr + xi*yi + hi(i,i-1)*zzr)
         hi(i-1,j) = mat_flop(xr*yi - xi*yr + hi(i,i-1)*zzi)
         hr(i,j) = mat_flop(xr*zzr - xi*zzi - hi(i,i-1)*yr)
         hi(i,j) = mat_flop(xr*zzi + xi*zzr - hi(i,i-1)*yi)
      enddo

   enddo

   si = hi(en,en)
   if (si .eq. 0.0d0) goto 540
   norm = mat_pythag(hr(en,en),si)
   sr = mat_flop(hr(en,en)/norm)
   si = mat_flop(si/norm)
   hr(en,en) = norm
   hi(en,en) = 0.0d0
   if (en .eq. n) goto 540
   ip1 = en + 1

   do j = ip1, n
      yr = hr(en,j)
      yi = hi(en,j)
      hr(en,j) = mat_flop(sr*yr + si*yi)
      hi(en,j) = mat_flop(sr*yi - si*yr)
   enddo
!     .......... INVERSE OPERATION (COLUMNS) ..........
540 continue
   do j = lp1, en
      xr = wr(j-1)
      xi = wi(j-1)

      do i = 1, j
         yr = hr(i,j-1)
         yi = 0.0d0
         zzr = hr(i,j)
         zzi = hi(i,j)
         if (i .eq. j) goto 560
         yi = hi(i,j-1)
         hi(i,j-1) = mat_flop(xr*yi + xi*yr + hi(j,j-1)*zzi)
560      continue
         hr(i,j-1) = mat_flop(xr*yr - xi*yi + hi(j,j-1)*zzr)
         hr(i,j) = mat_flop(xr*zzr + xi*zzi - hi(j,j-1)*yr)
         hi(i,j) = mat_flop(xr*zzi - xi*zzr - hi(j,j-1)*yi)
      enddo
!*****
      if (job .eq. 0) cycle
!*****
      do i = low, igh
         yr = zr(i,j-1)
         yi = zi(i,j-1)
         zzr = zr(i,j)
         zzi = zi(i,j)
         zr(i,j-1) = mat_flop(xr*yr - xi*yi + hi(j,j-1)*zzr)
         zi(i,j-1) = mat_flop(xr*yi + xi*yr + hi(j,j-1)*zzi)
         zr(i,j) = mat_flop(xr*zzr + xi*zzi - hi(j,j-1)*yr)
         zi(i,j) = mat_flop(xr*zzi - xi*zzr - hi(j,j-1)*yi)
      enddo

   enddo

   if (si .eq. 0.0d0) goto 240

   do i = 1, en
      yr = hr(i,en)
      yi = hi(i,en)
      hr(i,en) = mat_flop(sr*yr - si*yi)
      hi(i,en) = mat_flop(sr*yi + si*yr)
   enddo
!*****
   if (job .eq. 0) goto 240
!*****
   do i = low, igh
      yr = zr(i,en)
      yi = zi(i,en)
      zr(i,en) = mat_flop(sr*yr - si*yi)
      zi(i,en) = mat_flop(sr*yi + si*yr)
   enddo

   goto 240
!     .......... A ROOT FOUND ..........
660 continue
   hr(en,en) = mat_flop(hr(en,en) + tr)
   wr(en) = hr(en,en)
   hi(en,en) = mat_flop(hi(en,en) + ti)
   wi(en) = hi(en,en)
   en = enm1
   goto 220
!     .......... ALL ROOTS FOUND.  BACKSUBSTITUTE TO FIND
!                VECTORS OF UPPER TRIANGULAR FORM ..........
!
!*****  THE FOLLOWING SECTION CHANGED FOR OVERFLOW CONTROL
!       C. MOLER, 3/16/82
!
680 continue
   if (job .ne. 2) goto 1001

   norm = 0.0d0
   do i = 1, n
      do j = i, n
         tr = mat_flop(dabs(hr(i,j))) + mat_flop(dabs(hi(i,j)))
         if (tr .gt. norm) norm = tr
      enddo
   enddo
   if (n .eq. 1 .or. norm .eq. 0.0d0) goto 1001
!     .......... FOR EN=N STEP -1 UNTIL 2 DO -- ..........
   do nn = 2, n
      en = n + 2 - nn
      xr = wr(en)
      xi = wi(en)
      hr(en,en) = 1.0d0
      hi(en,en) = 0.0d0
      enm1 = en - 1
!     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- ..........
      do ii = 1, enm1
         i = en - ii
         zzr = 0.0d0
         zzi = 0.0d0
         ip1 = i + 1
         do j = ip1, en
            zzr = mat_flop(zzr + hr(i,j)*hr(j,en) - hi(i,j)*hi(j,en))
            zzi = mat_flop(zzi + hr(i,j)*hi(j,en) + hi(i,j)*hr(j,en))
         enddo
         yr = mat_flop(xr - wr(i))
         yi = mat_flop(xi - wi(i))
         if (yr .ne. 0.0d0 .or. yi .ne. 0.0d0) goto 765
         yr = norm
760      continue
         yr = mat_flop(yr/100.0d0)
         yi = mat_flop(norm + yr)
         if (yi .ne. norm) goto 760
         yi = 0.0d0
765      continue
         call mat_wdiv(zzr,zzi,yr,yi,hr(i,en),hi(i,en))
         tr = mat_flop(dabs(hr(i,en))) + mat_flop(dabs(hi(i,en)))
         if (tr .eq. 0.0d0) cycle
         if (tr + 1.0d0/tr .gt. tr)cycle
         do j = i, en
            hr(j,en) = mat_flop(hr(j,en)/tr)
            hi(j,en) = mat_flop(hi(j,en)/tr)
         enddo
      enddo
   enddo
!*****
!     .......... END BACKSUBSTITUTION ..........
   enm1 = n - 1
!     .......... VECTORS OF ISOLATED ROOTS ..........
   do  i = 1, enm1
      if (i .ge. low .and. i .le. igh) cycle
      ip1 = i + 1

      do j = ip1, n
         zr(i,j) = hr(i,j)
         zi(i,j) = hi(i,j)
      enddo

   enddo
!     .......... MULTIPLY BY TRANSFORMATION MATRIX TO GIVE
!                VECTORS OF ORIGINAL FULL MATRIX.
!                FOR J=N STEP -1 UNTIL LOW+1 DO -- ..........
   do jj = low, enm1
      j = n + low - jj
      m = min0(j,igh)

      do i = low, igh
         zzr = 0.0d0
         zzi = 0.0d0

         do k = low, m
            zzr = mat_flop(zzr + zr(i,k)*hr(k,j) - zi(i,k)*hi(k,j))
            zzi = mat_flop(zzi + zr(i,k)*hi(k,j) + zi(i,k)*hr(k,j))
         enddo

         zr(i,j) = zzr
         zi(i,j) = zzi
      enddo
   enddo
!
   goto 1001
!     .......... SET ERROR -- NO CONVERGENCE TO AN
!                EIGENVALUE AFTER 30 ITERATIONS ..........
1000 continue
   ierr = en
1001 continue
end subroutine ml_comqr3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WSVDC(xr,xi,LDX,N,P,sr,si,er,ei,ur,ui,LDU,vr,vi,LDV,workr,worki,JOB,INFO)
      use M_matrix
      INTEGER LDX,N,P,LDU,LDV,JOB,INFO
      DOUBLEPRECISION XR(LDX,*),XI(LDX,*),SR(*),SI(*),ER(*),EI(*), UR(LDU,*),UI(LDU,*),VR(LDV,*),VI(LDV,*), WORKR(*),WORKI(*)
!
!
!     WSVDC IS A SUBROUTINE TO REDUCE A DOUBLE-COMPLEX NXP MATRIX X BY
!     UNITARY TRANSFORMATIONS U AND V TO DIAGONAL FORM.  THE
!     DIAGONAL ELEMENTS S(I) ARE THE SINGULAR VALUES OF X.  THE
!     COLUMNS OF U ARE THE CORRESPONDING LEFT SINGULAR VECTORS,
!     AND THE COLUMNS OF V THE RIGHT SINGULAR VECTORS.
!
!     ON ENTRY
!
!         X         DOUBLE-COMPLEX(LDX,P), WHERE LDX.GE.N.
!                   X CONTAINS THE MATRIX WHOSE SINGULAR VALUE
!                   DECOMPOSITION IS TO BE COMPUTED.  X IS
!                   DESTROYED BY WSVDC.
!
!         LDX       INTEGER.
!                   LDX IS THE LEADING DIMENSION OF THE ARRAY X.
!
!         N         INTEGER.
!                   N IS THE NUMBER OF COLUMNS OF THE MATRIX X.
!
!         P         INTEGER.
!                   P IS THE NUMBER OF ROWS OF THE MATRIX X.
!
!         LDU       INTEGER.
!                   LDU IS THE LEADING DIMENSION OF THE ARRAY U
!                   (SEE BELOW).
!
!         LDV       INTEGER.
!                   LDV IS THE LEADING DIMENSION OF THE ARRAY V
!                   (SEE BELOW).
!
!         WORK      DOUBLE-COMPLEX(N).
!                   WORK IS A SCRATCH ARRAY.
!
!         JOB       INTEGER.
!                   JOB CONTROLS THE COMPUTATION OF THE SINGULAR
!                   VECTORS.  IT HAS THE DECIMAL EXPANSION AB
!                   WITH THE FOLLOWING MEANING
!
!     A.EQ.0    DO NOT COMPUTE THE LEFT SINGULAR
!               VECTORS.
!     A.EQ.1    RETURN THE N LEFT SINGULAR VECTORS
!               IN U.
!     A.GE.2    RETURNS THE FIRST MIN(N,P)
!               LEFT SINGULAR VECTORS IN U.
!     B.EQ.0    DO NOT COMPUTE THE RIGHT SINGULAR
!               VECTORS.
!     B.EQ.1    RETURN THE RIGHT SINGULAR VECTORS
!               IN V.
!
!     ON RETURN
!
!         S         DOUBLE-COMPLEX(MM), WHERE MM=MIN(N+1,P).
!                   THE FIRST MIN(N,P) ENTRIES OF S CONTAIN THE
!                   SINGULAR VALUES OF X ARRANGED IN DESCENDING
!                   ORDER OF MAGNITUDE.
!
!         E         DOUBLE-COMPLEX(P).
!                   E ORDINARILY CONTAINS ZEROS.  HOWEVER SEE THE
!                   DISCUSSION OF INFO FOR EXCEPTIONS.
!
!         U         DOUBLE-COMPLEX(LDU,K), WHERE LDU.GE.N.
!                   IF JOBA.EQ.1 THEN K.EQ.N,
!                   IF JOBA.EQ.2 THEN K.EQ.MIN(N,P).
!                   U CONTAINS THE MATRIX OF RIGHT SINGULAR VECTORS.
!                   U IS NOT REFERENCED IF JOBA.EQ.0.  IF N.LE.P
!                   OR IF JOBA.GT.2, THEN U MAY BE IDENTIFIED WITH X
!                   IN THE SUBROUTINE CALL.
!
!         V         DOUBLE-COMPLEX(LDV,P), WHERE LDV.GE.P.
!                   V CONTAINS THE MATRIX OF RIGHT SINGULAR VECTORS.
!                   V IS NOT REFERENCED IF JOBB.EQ.0.  IF P.LE.N,
!                   THEN V MAY BE IDENTIFIED WHTH X IN THE
!                   SUBROUTINE ML_CALL.
!
!         INFO      INTEGER.
!                   THE SINGULAR VALUES (AND THEIR CORRESPONDING
!                   SINGULAR VECTORS) S(INFO+1),S(INFO+2),...,S(M)
!                   ARE CORRECT (HERE M=MIN(N,P)).  THUS IF
!                   INFO.EQ.0, ALL THE SINGULAR VALUES AND THEIR
!                   VECTORS ARE CORRECT.  IN ANY EVENT, THE MATRIX
!                   B = CTRANS(U)*X*V IS THE BIDIAGONAL MATRIX
!                   WITH THE ELEMENTS OF S ON ITS DIAGONAL AND THE
!                   ELEMENTS OF E ON ITS SUPER-DIAGONAL (CTRANS(U)
!                   IS THE CONJUGATE-TRANSPOSE OF U).  THUS THE
!                   SINGULAR VALUES OF X AND B ARE THE SAME.
!
!     LINPACK. THIS VERSION DATED 07/03/79 .
!     G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.
!
!     WSVDC USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.
!
!     BLAS    matX_waxpy,mat_pythag,mat_wdotcr,mat_wdotci,mat_wscal,WSWAP,
!             mat_rrotg,mat_wnrm2
!     FORTRAN DABS,DIMAG,DMAX1
!     FORTRAN MAX0,MIN0,MOD,DSQRT
!
!     INTERNAL VARIABLES
!
      INTEGER I,ITER,J,JOBU,K,KASE,KK,L,LL,LLS,LM1,LP1,LS,LU,M,MAXIT,MM,MM1,MP1,NCT,NCTP1,NCU,NRT,NRTP1
      DOUBLEPRECISION TR,TI,RR,RI
      DOUBLEPRECISION B,C,CS,EL,EMM1,F,G,SCALE,SHIFT,SL,SM,SN,SMM1,T1,TEST,ZTEST,SMALL
      LOGICAL WANTU,WANTV
!
      DOUBLEPRECISION ZDUMR,ZDUMI
      DOUBLEPRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     SET THE MAXIMUM NUMBER OF ITERATIONS.
!
      MAXIT = 75
!
!     SMALL NUMBER, ROUGHLY MACHINE EPSILON, USED TO AVOID UNDERFLOW
!
      SMALL = 1.D0/2.D0**48
!
!     DETERMINE WHAT IS TO BE COMPUTED.
!
      WANTU = .FALSE.
      WANTV = .FALSE.
      JOBU = MOD(JOB,100)/10
      NCU = N
      IF (JOBU .GT. 1) NCU = MIN0(N,P)
      IF (JOBU .NE. 0) WANTU = .TRUE.
      IF (MOD(JOB,10) .NE. 0) WANTV = .TRUE.
!
!     REDUCE X TO BIDIAGONAL FORM, STORING THE DIAGONAL ELEMENTS
!     IN S AND THE SUPER-DIAGONAL ELEMENTS IN E.
!
      INFO = 0
      NCT = MIN0(N-1,P)
      NRT = MAX0(0,MIN0(P-2,N))
      LU = MAX0(NCT,NRT)
      IF (LU .LT. 1) GOTO 190
      DO 180 L = 1, LU
         LP1 = L + 1
         IF (L .GT. NCT) GOTO 30
!
!           COMPUTE THE TRANSFORMATION FOR THE L-TH COLUMN AND
!           PLACE THE L-TH DIAGONAL IN S(L).
!
            SR(L) = mat_wnrm2(N-L+1,XR(L,L),XI(L,L),1)
            SI(L) = 0.0D0
            IF (CABS1(SR(L),SI(L)) .EQ. 0.0D0) GOTO 20
               IF (CABS1(XR(L,L),XI(L,L)) .EQ. 0.0D0) GOTO 10
                  CALL mat_wsign(SR(L),SI(L),XR(L,L),XI(L,L),SR(L),SI(L))
   10          CONTINUE
               CALL mat_wdiv(1.0D0,0.0D0,SR(L),SI(L),TR,TI)
               CALL mat_wscal(N-L+1,TR,TI,XR(L,L),XI(L,L),1)
               XR(L,L) = mat_flop(1.0D0 + XR(L,L))
   20       CONTINUE
            SR(L) = -SR(L)
            SI(L) = -SI(L)
   30    CONTINUE
         IF (P .LT. LP1) GOTO 60
         DO 50 J = LP1, P
            IF (L .GT. NCT) GOTO 40
            IF (CABS1(SR(L),SI(L)) .EQ. 0.0D0) GOTO 40
!
!              APPLY THE TRANSFORMATION.
!
               TR= -mat_wdotcr(N-L+1,XR(L,L),XI(L,L),1,XR(L,J),XI(L,J),1)
               TI= -mat_wdotci(N-L+1,XR(L,L),XI(L,L),1,XR(L,J),XI(L,J),1)
               CALL mat_wdiv(TR,TI,XR(L,L),XI(L,L),TR,TI)
               CALL matX_waxpy(N-L+1,TR,TI,XR(L,L),XI(L,L),1,XR(L,J),XI(L,J),1)
   40       CONTINUE
!
!           PLACE THE L-TH ROW OF X INTO  E FOR THE
!           SUBSEQUENT CALCULATION OF THE ROW TRANSFORMATION.
!
            ER(J) = XR(L,J)
            EI(J) = -XI(L,J)
   50    CONTINUE
   60    CONTINUE
         IF (.NOT.WANTU .OR. L .GT. NCT) GOTO 80
!
!           PLACE THE TRANSFORMATION IN U FOR SUBSEQUENT BACK
!           MULTIPLICATION.
!
            DO I = L, N
               UR(I,L) = XR(I,L)
               UI(I,L) = XI(I,L)
            enddo
   80    CONTINUE
         IF (L .GT. NRT) GOTO 170
!
!           COMPUTE THE L-TH ROW TRANSFORMATION AND PLACE THE
!           L-TH SUPER-DIAGONAL IN E(L).
!
            ER(L) = mat_wnrm2(P-L,ER(LP1),EI(LP1),1)
            EI(L) = 0.0D0
            IF (CABS1(ER(L),EI(L)) .EQ. 0.0D0) GOTO 100
               IF (CABS1(ER(LP1),EI(LP1)) .EQ. 0.0D0) GOTO 90
                  CALL mat_wsign(ER(L),EI(L),ER(LP1),EI(LP1),ER(L),EI(L))
   90          CONTINUE
               CALL mat_wdiv(1.0D0,0.0D0,ER(L),EI(L),TR,TI)
               CALL mat_wscal(P-L,TR,TI,ER(LP1),EI(LP1),1)
               ER(LP1) = mat_flop(1.0D0 + ER(LP1))
  100       CONTINUE
            ER(L) = -ER(L)
            EI(L) = +EI(L)
            IF (LP1 .GT. N .OR. CABS1(ER(L),EI(L)) .EQ. 0.0D0) GOTO 140
!
!              APPLY THE TRANSFORMATION.
!
               DO I = LP1, N
                  WORKR(I) = 0.0D0
                  WORKI(I) = 0.0D0
               enddo
               DO J = LP1, P
                  CALL matX_waxpy(N-L,ER(J),EI(J),XR(LP1,J),XI(LP1,J),1, WORKR(LP1),WORKI(LP1),1)
               enddo
               DO J = LP1, P
                  CALL mat_wdiv(-ER(J),-EI(J),ER(LP1),EI(LP1),TR,TI)
                  CALL matX_waxpy(N-L,TR,-TI,WORKR(LP1),WORKI(LP1),1, XR(LP1,J),XI(LP1,J),1)
               enddo
  140       CONTINUE
            IF (.NOT.WANTV) GOTO 160
!
!              PLACE THE TRANSFORMATION IN V FOR SUBSEQUENT
!              BACK MULTIPLICATION.
!
               DO I = LP1, P
                  VR(I,L) = ER(I)
                  VI(I,L) = EI(I)
               enddo
  160       CONTINUE
  170    CONTINUE
  180 CONTINUE
  190 CONTINUE
!
!     SET UP THE FINAL BIDIAGONAL MATRIX OR ORDER M.
!
      M = MIN0(P,N+1)
      NCTP1 = NCT + 1
      NRTP1 = NRT + 1
      IF (NCT .GE. P) GOTO 200
         SR(NCTP1) = XR(NCTP1,NCTP1)
         SI(NCTP1) = XI(NCTP1,NCTP1)
  200 CONTINUE
      IF (N .GE. M) GOTO 210
         SR(M) = 0.0D0
         SI(M) = 0.0D0
  210 CONTINUE
      IF (NRTP1 .GE. M) GOTO 220
         ER(NRTP1) = XR(NRTP1,M)
         EI(NRTP1) = XI(NRTP1,M)
  220 CONTINUE
      ER(M) = 0.0D0
      EI(M) = 0.0D0
!
!     IF REQUIRED, GENERATE U.
!
      IF (.NOT.WANTU) GOTO 350
         IF (NCU .LT. NCTP1) GOTO 250
         DO J = NCTP1, NCU
            DO I = 1, N
               UR(I,J) = 0.0D0
               UI(I,J) = 0.0D0
            enddo
            UR(J,J) = 1.0D0
            UI(J,J) = 0.0D0
         enddo
  250    CONTINUE
         IF (NCT .LT. 1) GOTO 340
         DO LL = 1, NCT
            L = NCT - LL + 1
            IF (CABS1(SR(L),SI(L)) .EQ. 0.0D0) GOTO 300
               LP1 = L + 1
               IF (NCU .LT. LP1) GOTO 270
               DO J = LP1, NCU
                  TR = -mat_wdotcr(N-L+1,UR(L,L),UI(L,L),1,UR(L,J), UI(L,J),1)
                  TI = -mat_wdotci(N-L+1,UR(L,L),UI(L,L),1,UR(L,J), UI(L,J),1)
                  CALL mat_wdiv(TR,TI,UR(L,L),UI(L,L),TR,TI)
                  CALL matX_waxpy(N-L+1,TR,TI,UR(L,L),UI(L,L),1,UR(L,J), UI(L,J),1)
               enddo
  270          CONTINUE
               CALL mat_wrscal(N-L+1,-1.0D0,UR(L,L),UI(L,L),1)
               UR(L,L) = mat_flop(1.0D0 + UR(L,L))
               LM1 = L - 1
               IF (LM1 .LT. 1) GOTO 290
               DO I = 1, LM1
                  UR(I,L) = 0.0D0
                  UI(I,L) = 0.0D0
               enddo
  290          CONTINUE
            GOTO 320
  300       CONTINUE
               DO I = 1, N
                  UR(I,L) = 0.0D0
                  UI(I,L) = 0.0D0
               enddo
               UR(L,L) = 1.0D0
               UI(L,L) = 0.0D0
  320       CONTINUE
         enddo
  340    CONTINUE
  350 CONTINUE
!
!     IF IT IS REQUIRED, GENERATE V.
!
      IF (.NOT.WANTV) GOTO 400
         DO LL = 1, P
            L = P - LL + 1
            LP1 = L + 1
            IF (L .GT. NRT) GOTO 370
            IF (CABS1(ER(L),EI(L)) .EQ. 0.0D0) GOTO 370
               DO J = LP1, P
                  TR = -mat_wdotcr(P-L,VR(LP1,L),VI(LP1,L),1,VR(LP1,J),VI(LP1,J),1)
                  TI = -mat_wdotci(P-L,VR(LP1,L),VI(LP1,L),1,VR(LP1,J),VI(LP1,J),1)
                  CALL mat_wdiv(TR,TI,VR(LP1,L),VI(LP1,L),TR,TI)
                  CALL matX_waxpy(P-L,TR,TI,VR(LP1,L),VI(LP1,L),1,VR(LP1,J),VI(LP1,J),1)
               enddo
  370       CONTINUE
            DO I = 1, P
               VR(I,L) = 0.0D0
               VI(I,L) = 0.0D0
            enddo
            VR(L,L) = 1.0D0
            VI(L,L) = 0.0D0
         enddo
  400 CONTINUE
!
!     TRANSFORM S AND E SO THAT THEY ARE REAL.
!
      DO I = 1, M
            TR = mat_pythag(SR(I),SI(I))
            IF (TR .EQ. 0.0D0) GOTO 405
            RR = SR(I)/TR
            RI = SI(I)/TR
            SR(I) = TR
            SI(I) = 0.0D0
            IF (I .LT. M) CALL mat_wdiv(ER(I),EI(I),RR,RI,ER(I),EI(I))
            IF (WANTU) CALL mat_wscal(N,RR,RI,UR(1,I),UI(1,I),1)
  405    CONTINUE
!     ...EXIT
         IF (I .EQ. M) exit
            TR = mat_pythag(ER(I),EI(I))
            IF (TR .EQ. 0.0D0) GOTO 410
            CALL mat_wdiv(TR,0.0D0,ER(I),EI(I),RR,RI)
            ER(I) = TR
            EI(I) = 0.0D0
            CALL mat_wmul(SR(I+1),SI(I+1),RR,RI,SR(I+1),SI(I+1))
            IF (WANTV) CALL mat_wscal(P,RR,RI,VR(1,I+1),VI(1,I+1),1)
  410    CONTINUE
      enddo
!
!     MAIN ITERATION LOOP FOR THE SINGULAR VALUES.
!
      MM = M
      ITER = 0
  440 CONTINUE
!
!        QUIT IF ALL THE SINGULAR VALUES HAVE BEEN FOUND.
!
!     ...EXIT
         IF (M .EQ. 0) GOTO 700
!
!        IF TOO MANY ITERATIONS HAVE BEEN PERFORMED, SET
!        FLAG AND RETURN.
!
         IF (ITER .LT. MAXIT) GOTO 450
            INFO = M
!     ......EXIT
            GOTO 700
  450    CONTINUE
!
!        THIS SECTION OF THE PROGRAM INSPECTS FOR
!        NEGLIGIBLE ELEMENTS IN THE S AND E ARRAYS.  ON
!        COMPLETION THE VARIABLE KASE IS SET AS FOLLOWS.
!
!           KASE = 1     IF SR(M) AND ER(L-1) ARE NEGLIGIBLE AND L.LT.M
!           KASE = 2     IF SR(L) IS NEGLIGIBLE AND L.LT.M
!           KASE = 3     IF ER(L-1) IS NEGLIGIBLE, L.LT.M, AND
!     SR(L), ..., SR(M) ARE NOT NEGLIGIBLE (QR STEP).
!           KASE = 4     IF ER(M-1) IS NEGLIGIBLE (CONVERGENCE).
!
         DO LL = 1, M
            L = M - LL
!        ...EXIT
            IF (L .EQ. 0) GOTO 480
            TEST = mat_flop(DABS(SR(L)) + DABS(SR(L+1)))
            ZTEST = mat_flop(TEST + DABS(ER(L))/2.0D0)
            IF (SMALL*ZTEST .NE. SMALL*TEST) GOTO 460
               ER(L) = 0.0D0
!        ......EXIT
               GOTO 480
  460       CONTINUE
         enddo
  480    CONTINUE
         IF (L .NE. M - 1) GOTO 490
            KASE = 4
         GOTO 560
  490    CONTINUE
            LP1 = L + 1
            MP1 = M + 1
            DO LLS = LP1, MP1
               LS = M - LLS + LP1
!           ...EXIT
               IF (LS .EQ. L) GOTO 520
               TEST = 0.0D0
               IF (LS .NE. M) TEST = mat_flop(TEST + DABS(ER(LS)))
               IF (LS .NE. L + 1) TEST = mat_flop(TEST + DABS(ER(LS-1)))
               ZTEST = mat_flop(TEST + DABS(SR(LS))/2.0D0)
               IF (SMALL*ZTEST .NE. SMALL*TEST) GOTO 500
                  SR(LS) = 0.0D0
!           ......EXIT
                  GOTO 520
  500          CONTINUE
            enddo
  520       CONTINUE
            IF (LS .NE. L) GOTO 530
               KASE = 3
            GOTO 550
  530       CONTINUE
            IF (LS .NE. M) GOTO 540
               KASE = 1
            GOTO 550
  540       CONTINUE
               KASE = 2
               L = LS
  550       CONTINUE
  560    CONTINUE
         L = L + 1
!
!        PERFORM THE TASK INDICATED BY KASE.
!
         GOTO (570, 600, 620, 650), KASE
!
!        DEFLATE NEGLIGIBLE SR(M).
!
  570    CONTINUE
            MM1 = M - 1
            F = ER(M-1)
            ER(M-1) = 0.0D0
            DO KK = L, MM1
               K = MM1 - KK + L
               T1 = SR(K)
               CALL mat_rrotg(T1,F,CS,SN)
               SR(K) = T1
               IF (K .EQ. L) GOTO 580
                  F = mat_flop(-(SN*ER(K-1)))
                  ER(K-1) = mat_flop(CS*ER(K-1))
  580          CONTINUE
               IF (WANTV) CALL mat_rrot(P,VR(1,K),1,VR(1,M),1,CS,SN)
               IF (WANTV) CALL mat_rrot(P,VI(1,K),1,VI(1,M),1,CS,SN)
            enddo
         GOTO 690
!
!        SPLIT AT NEGLIGIBLE SR(L).
!
  600    CONTINUE
            F = ER(L-1)
            ER(L-1) = 0.0D0
            DO K = L, M
               T1 = SR(K)
               CALL mat_rrotg(T1,F,CS,SN)
               SR(K) = T1
               F = mat_flop(-(SN*ER(K)))
               ER(K) = mat_flop(CS*ER(K))
               IF (WANTU) CALL mat_rrot(N,UR(1,K),1,UR(1,L-1),1,CS,SN)
               IF (WANTU) CALL mat_rrot(N,UI(1,K),1,UI(1,L-1),1,CS,SN)
            enddo
         GOTO 690
!
!        PERFORM ONE QR STEP.
!
  620    CONTINUE
!
!           CALCULATE THE SHIFT.
!
            SCALE = DMAX1(DABS(SR(M)),DABS(SR(M-1)),DABS(ER(M-1)), DABS(SR(L)),DABS(ER(L)))
            SM = SR(M)/SCALE
            SMM1 = SR(M-1)/SCALE
            EMM1 = ER(M-1)/SCALE
            SL = SR(L)/SCALE
            EL = ER(L)/SCALE
            B = mat_flop(((SMM1 + SM)*(SMM1 - SM) + EMM1**2)/2.0D0)
            C = mat_flop((SM*EMM1)**2)
            SHIFT = 0.0D0
            IF (B .EQ. 0.0D0 .AND. C .EQ. 0.0D0) GOTO 630
               SHIFT = mat_flop(DSQRT(B**2+C))
               IF (B .LT. 0.0D0) SHIFT = -SHIFT
               SHIFT = mat_flop(C/(B + SHIFT))
  630       CONTINUE
            F = mat_flop((SL + SM)*(SL - SM) - SHIFT)
            G = mat_flop(SL*EL)
!
!           CHASE ZEROS.
!
            MM1 = M - 1
            DO K = L, MM1
               CALL mat_rrotg(F,G,CS,SN)
               IF (K .NE. L) ER(K-1) = F
               F = mat_flop(CS*SR(K) + SN*ER(K))
               ER(K) = mat_flop(CS*ER(K) - SN*SR(K))
               G = mat_flop(SN*SR(K+1))
               SR(K+1) = mat_flop(CS*SR(K+1))
               IF (WANTV) CALL mat_rrot(P,VR(1,K),1,VR(1,K+1),1,CS,SN)
               IF (WANTV) CALL mat_rrot(P,VI(1,K),1,VI(1,K+1),1,CS,SN)
               CALL mat_rrotg(F,G,CS,SN)
               SR(K) = F
               F = mat_flop(CS*ER(K) + SN*SR(K+1))
               SR(K+1) = mat_flop(-(SN*ER(K)) + CS*SR(K+1))
               G = mat_flop(SN*ER(K+1))
               ER(K+1) = mat_flop(CS*ER(K+1))
               IF (WANTU .AND. K .LT. N) CALL mat_rrot(N,UR(1,K),1,UR(1,K+1),1,CS,SN)
               IF (WANTU .AND. K .LT. N) CALL mat_rrot(N,UI(1,K),1,UI(1,K+1),1,CS,SN)
            enddo
            ER(M-1) = F
            ITER = ITER + 1
         GOTO 690
!
!        CONVERGENCE
!
  650    CONTINUE
!
!           MAKE THE SINGULAR VALUE  POSITIVE
!
            IF (SR(L) .GE. 0.0D0) GOTO 660
               SR(L) = -SR(L)
             IF (WANTV) CALL mat_wrscal(P,-1.0D0,VR(1,L),VI(1,L),1)
  660       CONTINUE
!
!           ORDER THE SINGULAR VALUE.
!
  670       IF (L .EQ. MM) GOTO 680
!           ...EXIT
               IF (SR(L) .GE. SR(L+1)) GOTO 680
               TR = SR(L)
               SR(L) = SR(L+1)
               SR(L+1) = TR
               IF (WANTV .AND. L .LT. P)CALL mat_wswap(P,VR(1,L),VI(1,L),1,VR(1,L+1),VI(1,L+1),1)
               IF (WANTU .AND. L .LT. N)CALL mat_wswap(N,UR(1,L),UI(1,L),1,UR(1,L+1),UI(1,L+1),1)
               L = L + 1
            GOTO 670
  680       CONTINUE
            ITER = 0
            M = M - 1
  690    CONTINUE
      GOTO 440
  700 CONTINUE
      END SUBROUTINE ML_WSVDC
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WQRDC(XR,XI,LDX,N,P,QRAUXR,QRAUXI,JPVT,WORKR,WORKI, JOB)
      use M_matrix
      INTEGER LDX,N,P,JOB
      INTEGER JPVT(*)
      DOUBLEPRECISION XR(LDX,*),XI(LDX,*),QRAUXR(*),QRAUXI(*), WORKR(*),WORKI(*)
!
!     WQRDC USES HOUSEHOLDER TRANSFORMATIONS TO COMPUTE THE QR
!     FACTORIZATION OF AN N BY P MATRIX X.  COLUMN PIVOTING
!     BASED ON THE 2-NORMS OF THE REDUCED COLUMNS MAY BE
!     PERFORMED AT THE USERS OPTION.
!
!     ON ENTRY
!
!        X       DOUBLE-COMPLEX(LDX,P), WHERE LDX .GE. N.
!                X CONTAINS THE MATRIX WHOSE DECOMPOSITION IS TO BE
!                COMPUTED.
!
!        LDX     INTEGER.
!                LDX IS THE LEADING DIMENSION OF THE ARRAY X.
!
!        N       INTEGER.
!                N IS THE NUMBER OF ROWS OF THE MATRIX X.
!
!        P       INTEGER.
!                P IS THE NUMBER OF COLUMNS OF THE MATRIX X.
!
!        JPVT    INTEGER(P).
!                JPVT CONTAINS INTEGERS THAT CONTROL THE SELECTION
!                OF THE PIVOT COLUMNS.  THE K-TH COLUMN X(K) OF X
!                IS PLACED IN ONE OF THREE CLASSES ACCORDING TO THE
!                VALUE OF JPVT(K).
!
!                   IF JPVT(K) .GT. 0, THEN X(K) IS AN INITIAL
!                   COLUMN.
!
!                   IF JPVT(K) .EQ. 0, THEN X(K) IS A FREE COLUMN.
!
!                   IF JPVT(K) .LT. 0, THEN X(K) IS A FINAL COLUMN.
!
!                BEFORE THE DECOMPOSITION IS COMPUTED, INITIAL COLUMNS
!                ARE MOVED TO THE BEGINNING OF THE ARRAY X AND FINAL
!                COLUMNS TO THE END.  BOTH INITIAL AND FINAL COLUMNS
!                ARE FROZEN IN PLACE DURING THE COMPUTATION AND ONLY
!                FREE COLUMNS ARE MOVED.  AT THE K-TH STAGE OF THE
!                REDUCTION, IF X(K) IS OCCUPIED BY A FREE COLUMN
!                IT IS INTERCHANGED WITH THE FREE COLUMN OF LARGEST
!                REDUCED NORM.  JPVT IS NOT REFERENCED IF
!                JOB .EQ. 0.
!
!        WORK    DOUBLE-COMPLEX(P).
!                WORK IS A WORK ARRAY.  WORK IS NOT REFERENCED IF
!                JOB .EQ. 0.
!
!        JOB     INTEGER.
!                JOB IS AN INTEGER THAT INITIATES COLUMN PIVOTING.
!                IF JOB .EQ. 0, NO PIVOTING IS DONE.
!                IF JOB .NE. 0, PIVOTING IS DONE.
!
!     ON RETURN
!
!        X       X CONTAINS IN ITS UPPER TRIANGLE THE UPPER
!                TRIANGULAR MATRIX R OF THE QR FACTORIZATION.
!                BELOW ITS DIAGONAL X CONTAINS INFORMATION FROM
!                WHICH THE UNITARY PART OF THE DECOMPOSITION
!                CAN BE RECOVERED.  NOTE THAT IF PIVOTING HAS
!                BEEN REQUESTED, THE DECOMPOSITION IS NOT THAT
!                OF THE ORIGINAL MATRIX X BUT THAT OF X
!                WITH ITS COLUMNS PERMUTED AS DESCRIBED BY JPVT.
!
!        QRAUX   DOUBLE-COMPLEX(P).
!                QRAUX CONTAINS FURTHER INFORMATION REQUIRED TO RECOVER
!                THE UNITARY PART OF THE DECOMPOSITION.
!
!        JPVT    JPVT(K) CONTAINS THE INDEX OF THE COLUMN OF THE
!                ORIGINAL MATRIX THAT HAS BEEN INTERCHANGED INTO
!                THE K-TH COLUMN, IF PIVOTING WAS REQUESTED.
!
!     LINPACK. THIS VERSION DATED 07/03/79 .
!     G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.
!
!     WQRDC USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.
!
!     BLAS matX_waxpy,mat_pythag,mat_wdotcr,mat_wdotci,mat_wscal
!     blas mat_wswap ,mat_wnrm2
!     FORTRAN DABS,DIMAG,DMAX1,MIN0
!
!     INTERNAL VARIABLES
!
integer :: jj
      INTEGER J,JP,L,LP1,LUP,MAXJ,PL,PU
      DOUBLEPRECISION MAXNRM,TT
      DOUBLEPRECISION NRMXLR,NRMXLI,TR,TI
      LOGICAL NEGJ,SWAPJ
!
      DOUBLEPRECISION ZDUMR,ZDUMI
      DOUBLEPRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
      PL = 1
      PU = 0
      IF (JOB .EQ. 0) GOTO 60
!
!        PIVOTING HAS BEEN REQUESTED.  REARRANGE THE COLUMNS
!        ACCORDING TO JPVT.
!
         DO 20 J = 1, P
            SWAPJ = JPVT(J) .GT. 0
            NEGJ = JPVT(J) .LT. 0
            JPVT(J) = J
            IF (NEGJ) JPVT(J) = -J
            IF (.NOT.SWAPJ) GOTO 10
               IF (J .NE. PL) CALL mat_wswap(N,XR(1,PL),XI(1,PL),1,XR(1,J),XI(1,J),1)
               JPVT(J) = JPVT(PL)
               JPVT(PL) = J
               PL = PL + 1
   10       CONTINUE
   20    CONTINUE
         PU = P
         DO 50 JJ = 1, P
            J = P - JJ + 1
            IF (JPVT(J) .GE. 0) GOTO 40
               JPVT(J) = -JPVT(J)
               IF (J .EQ. PU) GOTO 30
                  CALL mat_wswap(N,XR(1,PU),XI(1,PU),1,XR(1,J),XI(1,J),1)
                  JP = JPVT(PU)
                  JPVT(PU) = JPVT(J)
                  JPVT(J) = JP
   30          CONTINUE
               PU = PU - 1
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE
!
!     COMPUTE THE NORMS OF THE FREE COLUMNS.
!
      IF (PU .LT. PL) GOTO 80
      DO 70 J = PL, PU
         QRAUXR(J) = mat_wnrm2(N,XR(1,J),XI(1,J),1)
         QRAUXI(J) = 0.0D0
         WORKR(J) = QRAUXR(J)
         WORKI(J) = QRAUXI(J)
   70 CONTINUE
   80 CONTINUE
!
!     PERFORM THE HOUSEHOLDER REDUCTION OF X.
!
      LUP = MIN0(N,P)
      DO 210 L = 1, LUP
         IF (L .LT. PL .OR. L .GE. PU) GOTO 120
!
!           LOCATE THE COLUMN OF LARGEST NORM AND BRING IT
!           INTO THE PIVOT POSITION.
!
            MAXNRM = 0.0D0
            MAXJ = L
            DO J = L, PU
               IF (QRAUXR(J) .LE. MAXNRM) cycle
               MAXNRM = QRAUXR(J)
               MAXJ = J
            enddo
            IF (MAXJ .EQ. L) GOTO 110
              CALL mat_wswap(N,XR(1,L),XI(1,L),1,XR(1,MAXJ),XI(1,MAXJ),1)
              QRAUXR(MAXJ) = QRAUXR(L)
              QRAUXI(MAXJ) = QRAUXI(L)
              WORKR(MAXJ) = WORKR(L)
              WORKI(MAXJ) = WORKI(L)
              JP = JPVT(MAXJ)
              JPVT(MAXJ) = JPVT(L)
              JPVT(L) = JP
  110       CONTINUE
  120    CONTINUE
         QRAUXR(L) = 0.0D0
         QRAUXI(L) = 0.0D0
         IF (L .EQ. N) GOTO 200
!
!           COMPUTE THE HOUSEHOLDER TRANSFORMATION FOR COLUMN L.
!
            NRMXLR = mat_wnrm2(N-L+1,XR(L,L),XI(L,L),1)
            NRMXLI = 0.0D0
            IF (CABS1(NRMXLR,NRMXLI) .EQ. 0.0D0) GOTO 190
              IF (CABS1(XR(L,L),XI(L,L)) .EQ. 0.0D0) GOTO 130
              CALL mat_wsign(NRMXLR,NRMXLI,XR(L,L),XI(L,L),NRMXLR,NRMXLI)
  130         CONTINUE
              CALL mat_wdiv(1.0D0,0.0D0,NRMXLR,NRMXLI,TR,TI)
              CALL mat_wscal(N-L+1,TR,TI,XR(L,L),XI(L,L),1)
              XR(L,L) = mat_flop(1.0D0 + XR(L,L))
!
!             APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS,
!             UPDATING THE NORMS.
!
              LP1 = L + 1
              IF (P .LT. LP1) GOTO 180
              DO 170 J = LP1, P
                  TR = -mat_wdotcr(N-L+1,XR(L,L),XI(L,L),1,XR(L,J), XI(L,J),1)
                  TI = -mat_wdotci(N-L+1,XR(L,L),XI(L,L),1,XR(L,J), XI(L,J),1)
                  CALL mat_wdiv(TR,TI,XR(L,L),XI(L,L),TR,TI)
                  CALL matX_waxpy(N-L+1,TR,TI,XR(L,L),XI(L,L),1,XR(L,J), XI(L,J),1)
                  IF (J .LT. PL .OR. J .GT. PU) GOTO 160
                  IF (CABS1(QRAUXR(J),QRAUXI(J)) .EQ. 0.0D0) GOTO 160
                    TT=1.0D0 - (mat_pythag(XR(L,J),XI(L,J))/QRAUXR(J))**2
                    TT=DMAX1(TT,0.0D0)
                    TR=mat_flop(TT)
                    TT=mat_flop(1.0D0+0.05D0*TT*(QRAUXR(J)/WORKR(J))**2)
                    IF (TT .EQ. 1.0D0) GOTO 140
                     QRAUXR(J) = QRAUXR(J)*DSQRT(TR)
                     QRAUXI(J) = QRAUXI(J)*DSQRT(TR)
                     GOTO 150
  140                CONTINUE
                     QRAUXR(J) = mat_wnrm2(N-L,XR(L+1,J),XI(L+1,J),1)
                     QRAUXI(J) = 0.0D0
                     WORKR(J) = QRAUXR(J)
                     WORKI(J) = QRAUXI(J)
  150                CONTINUE
  160             CONTINUE
  170          CONTINUE
  180          CONTINUE
!
!              SAVE THE TRANSFORMATION.
!
               QRAUXR(L) = XR(L,L)
               QRAUXI(L) = XI(L,L)
               XR(L,L) = -NRMXLR
               XI(L,L) = -NRMXLI
  190       CONTINUE
  200    CONTINUE
  210 CONTINUE
      END SUBROUTINE ML_WQRDC
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WQRSL(XR,XI,LDX,N,K,QRAUXR,QRAUXI,YR,YI,QYR,QYI,QTYR,QTYI,BR,BI,RSDR,RSDI,XBR,XBI,JOB,INFO)
use M_matrix
IMPLICIT NONE
INTEGER LDX,N,K,JOB,INFO
DOUBLEPRECISION XR(LDX,*),XI(LDX,*),QRAUXR(*),QRAUXI(*),YR(*),     &
   &                YI(*),QYR(*),QYI(*),QTYR(*),QTYI(*),BR(*),BI(*),   &
   &                RSDR(*),RSDI(*),XBR(*),XBI(*)
!
!     WQRSL APPLIES THE OUTPUT OF WQRDC TO COMPUTE COORDINATE
!     TRANSFORMATIONS, PROJECTIONS, AND LEAST SQUARES SOLUTIONS.
!     FOR K .LE. MIN(N,P), LET XK BE THE MATRIX
!
!            XK = (X(JPVT(1)),X(JPVT(2)), ... ,X(JPVT(K)))
!
!     FORMED FROM COLUMNS JPVT(1), ... ,JPVT(K) OF THE ORIGINAL
!     N X P MATRIX X THAT WAS INPUT TO WQRDC (IF NO PIVOTING WAS
!     DONE, XK CONSISTS OF THE FIRST K COLUMNS OF X IN THEIR
!     ORIGINAL ORDER).  WQRDC PRODUCES A FACTORED UNITARY MATRIX Q
!     AND AN UPPER TRIANGULAR MATRIX R SUCH THAT
!
!              XK = Q * (R)
!    (0)
!
!     THIS INFORMATION IS CONTAINED IN CODED FORM IN THE ARRAYS
!     X AND QRAUX.
!
!     ON ENTRY
!
!        X      DOUBLE-COMPLEX(LDX,P).
!               X CONTAINS THE OUTPUT OF WQRDC.
!
!        LDX    INTEGER.
!               LDX IS THE LEADING DIMENSION OF THE ARRAY X.
!
!        N      INTEGER.
!               N IS THE NUMBER OF ROWS OF THE MATRIX XK.  IT MUST
!               HAVE THE SAME VALUE AS N IN WQRDC.
!
!        K      INTEGER.
!               K IS THE NUMBER OF COLUMNS OF THE MATRIX XK.  K
!               MUST NNOT BE GREATER THAN MIN(N,P), WHERE P IS THE
!               SAME AS IN THE CALLING SEQUENCE TO WQRDC.
!
!        QRAUX  DOUBLE-COMPLEX(P).
!               QRAUX CONTAINS THE AUXILIARY OUTPUT FROM WQRDC.
!
!        Y      DOUBLE-COMPLEX(N)
!               Y CONTAINS AN N-VECTOR THAT IS TO BE MANIPULATED
!               BY WQRSL.
!
!        JOB    INTEGER.
!               JOB SPECIFIES WHAT IS TO BE COMPUTED.  JOB HAS
!               THE DECIMAL EXPANSION ABCDE, WITH THE FOLLOWING
!               MEANING.
!
! IF A.NE.0, COMPUTE QY.
! IF B,C,D, OR E .NE. 0, COMPUTE QTY.
! IF C.NE.0, COMPUTE B.
! IF D.NE.0, COMPUTE RSD.
! IF E.NE.0, COMPUTE XB.
!
!               NOTE THAT A REQUEST TO COMPUTE B, RSD, OR XB
!               AUTOMATICALLY TRIGGERS THE COMPUTATION OF QTY, FOR
!               WHICH AN ARRAY MUST BE PROVIDED IN THE CALLING
!               SEQUENCE.
!
!     ON RETURN
!
!        QY     DOUBLE-COMPLEX(N).
!               QY CONTAINS Q*Y, IF ITS COMPUTATION HAS BEEN
!               REQUESTED.
!
!        QTY    DOUBLE-COMPLEX(N).
!               QTY CONTAINS CTRANS(Q)*Y, IF ITS COMPUTATION HAS
!               BEEN REQUESTED.  HERE CTRANS(Q) IS THE CONJUGATE
!               TRANSPOSE OF THE MATRIX Q.
!
!        B      DOUBLE-COMPLEX(K)
!               B CONTAINS THE SOLUTION OF THE LEAST SQUARES PROBLEM
!
! MINIMIZE NORM2(Y - XK*B),
!
!               IF ITS COMPUTATION HAS BEEN REQUESTED.  (NOTE THAT
!               IF PIVOTING WAS REQUESTED IN WQRDC, THE J-TH
!               COMPONENT OF B WILL BE ASSOCIATED WITH COLUMN JPVT(J)
!               OF THE ORIGINAL MATRIX X THAT WAS INPUT INTO WQRDC.)
!
!        RSD    DOUBLE-COMPLEX(N).
!               RSD CONTAINS THE LEAST SQUARES RESIDUAL Y - XK*B,
!               IF ITS COMPUTATION HAS BEEN REQUESTED.  RSD IS
!               ALSO THE ORTHOGONAL PROJECTION OF Y ONTO THE
!               ORTHOGONAL COMPLEMENT OF THE COLUMN SPACE OF XK.
!
!        XB     DOUBLE-COMPLEX(N).
!               XB CONTAINS THE LEAST SQUARES APPROXIMATION XK*B,
!               IF ITS COMPUTATION HAS BEEN REQUESTED.  XB IS ALSO
!               THE ORTHOGONAL PROJECTION OF Y ONTO THE COLUMN SPACE
!               OF X.
!
!        INFO   INTEGER.
!               INFO IS ZERO UNLESS THE COMPUTATION OF B HAS
!               BEEN REQUESTED AND R IS EXACTLY SINGULAR.  IN
!               THIS CASE, INFO IS THE INDEX OF THE FIRST ZERO
!               DIAGONAL ELEMENT OF R AND B IS LEFT UNALTERED.
!
!     THE PARAMETERS QY, QTY, B, RSD, AND XB ARE NOT REFERENCED
!     IF THEIR COMPUTATION IS NOT REQUESTED AND IN THIS CASE
!     CAN BE REPLACED BY DUMMY VARIABLES IN THE CALLING PROGRAM.
!     TO SAVE STORAGE, THE USER MAY IN SOME CASES USE THE SAME
!     ARRAY FOR DIFFERENT PARAMETERS IN THE CALLING SEQUENCE.  A
!     FREQUENTLY OCCURRING EXAMPLE IS WHEN ONE WISHES TO COMPUTE
!     ANY OF B, RSD, OR XB AND DOES NOT NEED Y OR QTY.  IN THIS
!     CASE ONE MAY IDENTIFY Y, QTY, AND ONE OF B, RSD, OR XB, WHILE
!     PROVIDING SEPARATE ARRAYS FOR ANYTHING ELSE THAT IS TO BE
!     COMPUTED.  THUS THE CALLING SEQUENCE
!
!          CALL ML_WQRSL(X,LDX,N,K,QRAUX,Y,DUM,Y,B,Y,DUM,110,INFO)
!
!     WILL RESULT IN THE COMPUTATION OF B AND RSD, WITH RSD
!     OVERWRITING Y.  MORE GENERALLY, EACH ITEM IN THE FOLLOWING
!     LIST CONTAINS GROUPS OF PERMISSIBLE IDENTIFICATIONS FOR
!     A SINGLE CALLING SEQUENCE.
!
!          1. (Y,QTY,B) (RSD) (XB) (QY)
!
!          2. (Y,QTY,RSD) (B) (XB) (QY)
!
!          3. (Y,QTY,XB) (B) (RSD) (QY)
!
!          4. (Y,QY) (QTY,B) (RSD) (XB)
!
!          5. (Y,QY) (QTY,RSD) (B) (XB)
!
!          6. (Y,QY) (QTY,XB) (B) (RSD)
!
!     IN ANY GROUP THE VALUE RETURNED IN THE ARRAY ALLOCATED TO
!     THE GROUP CORRESPONDS TO THE LAST MEMBER OF THE GROUP.
!
!     LINPACK. THIS VERSION DATED 07/03/79 .
!     G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.
!
!     ML_WQRSL USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.
!
!     BLAS matX_waxpy,WCOPY,mat_wdotcr,mat_wdotci
!     FORTRAN DABS,DIMAG,MIN0,MOD
!
!     INTERNAL VARIABLES
!
   INTEGER I,J,JJ,JU,KP1
   DOUBLEPRECISION TR,TI,TEMPR,TEMPI
   LOGICAL CB,CQY,CQTY,CR,CXB
!
   DOUBLEPRECISION ZDUMR,ZDUMI
   DOUBLEPRECISION CABS1
   CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     SET INFO FLAG.
!
   INFO = 0
!
!     DETERMINE WHAT IS TO BE COMPUTED.
!
   CQY = JOB/10000 .NE. 0
   CQTY = MOD(JOB,10000) .NE. 0
   CB = MOD(JOB,1000)/100 .NE. 0
   CR = MOD(JOB,100)/10 .NE. 0
   CXB = MOD(JOB,10) .NE. 0
   JU = MIN0(K,N-1)
!
!     SPECIAL ACTION WHEN N=1.
!
   IF (JU .NE. 0) GOTO 80
   IF (.NOT.CQY) GOTO 10
   QYR(1) = YR(1)
   QYI(1) = YI(1)
10 CONTINUE
   IF (.NOT.CQTY) GOTO 20
   QTYR(1) = YR(1)
   QTYI(1) = YI(1)
20 CONTINUE
   IF (.NOT.CXB) GOTO 30
   XBR(1) = YR(1)
   XBI(1) = YI(1)
30 CONTINUE
   IF (.NOT.CB) GOTO 60
   IF (CABS1(XR(1,1),XI(1,1)) .NE. 0.0D0) GOTO 40
   INFO = 1
   GOTO 50
40 CONTINUE
   CALL mat_wdiv(YR(1),YI(1),XR(1,1),XI(1,1),BR(1),BI(1))
50 CONTINUE
60 CONTINUE
   IF (.NOT.CR) GOTO 70
   RSDR(1) = 0.0D0
   RSDI(1) = 0.0D0
70 CONTINUE
   GOTO 290
80 CONTINUE
!
!        SET UP TO COMPUTE QY OR QTY.
!
   IF (CQY) CALL mat_wcopy(N,YR,YI,1,QYR,QYI,1)
   IF (CQTY) CALL mat_wcopy(N,YR,YI,1,QTYR,QTYI,1)
   IF (.NOT.CQY) GOTO 110
!
!           COMPUTE QY.
!
   DO JJ = 1, JU
      J = JU - JJ + 1
      IF (CABS1(QRAUXR(J),QRAUXI(J)) .EQ. 0.0D0) cycle
      TEMPR = XR(J,J)
      TEMPI = XI(J,J)
      XR(J,J) = QRAUXR(J)
      XI(J,J) = QRAUXI(J)
      TR=-mat_wdotcr(N-J+1,XR(J,J),XI(J,J),1,QYR(J),QYI(J),1)
      TI=-mat_wdotci(N-J+1,XR(J,J),XI(J,J),1,QYR(J),QYI(J),1)
      CALL mat_wdiv(TR,TI,XR(J,J),XI(J,J),TR,TI)
      CALL matX_waxpy(N-J+1,TR,TI,XR(J,J),XI(J,J),1,QYR(J), QYI(J),1)
      XR(J,J) = TEMPR
      XI(J,J) = TEMPI
   enddo
110 CONTINUE
   IF (.NOT.CQTY) GOTO 140
!
!           COMPUTE CTRANS(Q)*Y.
!
   DO J = 1, JU
      IF (CABS1(QRAUXR(J),QRAUXI(J)) .EQ. 0.0D0) cycle
      TEMPR = XR(J,J)
      TEMPI = XI(J,J)
      XR(J,J) = QRAUXR(J)
      XI(J,J) = QRAUXI(J)
      TR = -mat_wdotcr(N-J+1,XR(J,J),XI(J,J),1,QTYR(J), QTYI(J),1)
      TI = -mat_wdotci(N-J+1,XR(J,J),XI(J,J),1,QTYR(J), QTYI(J),1)
      CALL mat_wdiv(TR,TI,XR(J,J),XI(J,J),TR,TI)
      CALL matX_waxpy(N-J+1,TR,TI,XR(J,J),XI(J,J),1,QTYR(J), QTYI(J),1)
      XR(J,J) = TEMPR
      XI(J,J) = TEMPI
   enddo
140 CONTINUE
!
!        SET UP TO COMPUTE B, RSD, OR XB.
!
   IF (CB) CALL mat_wcopy(K,QTYR,QTYI,1,BR,BI,1)
   KP1 = K + 1
   IF (CXB) CALL mat_wcopy(K,QTYR,QTYI,1,XBR,XBI,1)
   IF (CR .AND. K .LT. N)CALL mat_wcopy(N-K,QTYR(KP1),QTYI(KP1),1,RSDR(KP1),RSDI(KP1),1)
   IF (.NOT.CXB .OR. KP1 .GT. N) GOTO 160
   DO I = KP1, N
      XBR(I) = 0.0D0
      XBI(I) = 0.0D0
   enddo
160 CONTINUE
   IF (.NOT.CR) GOTO 180
   DO I = 1, K
      RSDR(I) = 0.0D0
      RSDI(I) = 0.0D0
   enddo
180 CONTINUE
   IF (.NOT.CB) GOTO 230
!
!           COMPUTE B.
!
   DO JJ = 1, K
      J = K - JJ + 1
      IF (CABS1(XR(J,J),XI(J,J)) .NE. 0.0D0) GOTO 190
      INFO = J
!                 ......EXIT
!           ......EXIT
      GOTO 220
190   CONTINUE
      CALL mat_wdiv(BR(J),BI(J),XR(J,J),XI(J,J),BR(J),BI(J))
      IF (J .EQ. 1) GOTO 200
      TR = -BR(J)
      TI = -BI(J)
      CALL matX_waxpy(J-1,TR,TI,XR(1,J),XI(1,J),1,BR,BI,1)
200   CONTINUE
   enddo
220 CONTINUE
230 CONTINUE
   IF (.NOT.CR .AND. .NOT.CXB) GOTO 280
!
!           COMPUTE RSD OR XB AS REQUIRED.
!
   DO JJ = 1, JU
      J = JU - JJ + 1
      IF (CABS1(QRAUXR(J),QRAUXI(J)) .EQ. 0.0D0) cycle
      TEMPR = XR(J,J)
      TEMPI = XI(J,J)
      XR(J,J) = QRAUXR(J)
      XI(J,J) = QRAUXI(J)
      IF (CR) then
         TR = -mat_wdotcr(N-J+1,XR(J,J),XI(J,J),1,RSDR(J), RSDI(J),1)
         TI = -mat_wdotci(N-J+1,XR(J,J),XI(J,J),1,RSDR(J), RSDI(J),1)
         CALL mat_wdiv(TR,TI,XR(J,J),XI(J,J),TR,TI)
         CALL matX_waxpy(N-J+1,TR,TI,XR(J,J),XI(J,J),1,RSDR(J), RSDI(J),1)
      endif
      IF (CXB) then
         TR = -mat_wdotcr(N-J+1,XR(J,J),XI(J,J),1,XBR(J), XBI(J),1)
         TI = -mat_wdotci(N-J+1,XR(J,J),XI(J,J),1,XBR(J), XBI(J),1)
         CALL mat_wdiv(TR,TI,XR(J,J),XI(J,J),TR,TI)
         CALL matX_waxpy(N-J+1,TR,TI,XR(J,J),XI(J,J),1,XBR(J), XBI(J),1)
      endif
      XR(J,J) = TEMPR
      XI(J,J) = TEMPI
   enddo
280 CONTINUE
290 CONTINUE
END SUBROUTINE ML_WQRSL
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
