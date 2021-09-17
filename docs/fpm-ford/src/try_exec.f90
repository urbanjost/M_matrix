    program try_exec
    use M_matrix, only : lala, put_into_lala, get_from_lala, ifin_lala, printit
    !real,allocatable             :: r
    !complex,allocatable          :: cvec(:)
    integer,allocatable          :: iarr(:,:)
    character(len=:),allocatable :: t(:)
    integer                      :: ierr

    ! store some data into lala(3)
    call put_into_lala('A',[1,2,3,4,5]*10.5,ierr)
    write(*,*)'is A defined in LALA?',ifin_lala('A')
    call lala('A/2.0')
 
    ! pass some commands to lala(3f)
    call lala([character(len=80) :: &
    &'PI=atan(1)*4               ', &
    &'mytitle=''this is my title'';', &
    &'littlearray=<              ', &
    &'   1 2 3;                  ', &
    &'   4 5 6;                  ', &
    &'   7 8 9;                  ', &
    &'>                          ', &
    &'S=sum(A)                   ', &
    &'I=inv(littlearray);        ', &
    &'B=littlearray*sin(PI/3)    ', &
    &'save(''keepB'',B)            ', &
    &''])

    ! read a file containing lala(3f) commands
    call lala('exec(''mycommands'');')

    ! interactively interact with lala(3f) interpreter
    !!call printit()
    call lala() 
    !!call printit()

    ! get some data from LALA into the calling program
    call get_from_lala('littlearray',iarr,ierr)
    if(ierr.eq.0)then
       write(*,'(*(g0))')'IN CALLING PROGRAM IARR=',size(iarr)
       write(*,'(1x,*(g0,1x))')(IARR(i,:),new_line('A'),i=1,size(iarr,dim=1))
    endif

    call get_from_lala('mytitle',t,ierr)
    write(*,*)'IN CALLING PROGRAM T=',t

    call get_from_lala('notthere',t,ierr)
    write(*,*)'IN CALLING PROGRAM T=',t

    end program try_exec
