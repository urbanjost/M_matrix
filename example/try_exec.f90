    program try_exec
    use M_matrix, only : laff, put_into_laff, get_from_laff, ifin_laff, printit
    !real,allocatable             :: r
    !complex,allocatable          :: cvec(:)
    integer,allocatable          :: iarr(:,:)
    character(len=:),allocatable :: t(:)
    integer                      :: ierr

    ! store some data into laff(3)
    call put_into_laff('A',[1,2,3,4,5]*10.5,ierr)
    write(*,*)'is A defined in LAFF?',ifin_laff('A')
    call laff('A/2.0')
 
    ! pass some commands to laff(3f)
    call laff([character(len=80) :: &
    &'PI=atan(1)*4               ', &
    &'mytitle="this is my title";', &
    &'littlearray=<              ', &
    &'   1 2 3;                  ', &
    &'   4 5 6;                  ', &
    &'   7 8 9;                  ', &
    &'>                          ', &
    &'S=sum(A)                   ', &
    &'I=inv(littlearray);        ', &
    &'B=littlearray*sin(PI/3)    ', &
    &'save("keepB",B)            ', &
    &''])

    ! read a file containing laff(3f) commands
    call laff('exec("mycommands");')

    ! interactively interact with laff(3f) interpreter
    !!call printit()
    call laff() 
    !!call printit()

    ! get some data from LAFF into the calling program
    call get_from_laff('littlearray',iarr,ierr)
    if(ierr.eq.0)then
       write(*,'(*(g0))')'IN CALLING PROGRAM IARR=',size(iarr)
       write(*,'(1x,*(g0,1x))')(IARR(i,:),new_line('A'),i=1,size(iarr,dim=1))
    endif

    call get_from_laff('mytitle',t,ierr)
    write(*,*)'IN CALLING PROGRAM T=',t

    call get_from_laff('notthere',t,ierr)
    write(*,*)'IN CALLING PROGRAM T=',t

    end program try_exec
