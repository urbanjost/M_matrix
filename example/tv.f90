! based on
! small module to do text mode graphics using ANSI terminal escape sequences
! Copyright (c) 2013 Axel Kohlmeyer <akohlmey@gmail.com>
! v0.1 2013-06-22, initial public version
! v0.2 2013-06-24, better autoscale and overflow protection

module text_viz
implicit none
private
public                      :: viz_init, viz_pos, viz_plot, viz_done

character, parameter        :: esc = achar(27)
character(len=*), parameter :: cli = esc//'['
integer,   parameter        :: maxplot = 10
character, parameter        :: plot(0:maxplot) = [' ', '.', ',', ':', '=', '+', 'o', 'x', 'X', '#', '@']
integer                     :: rows, cols
character(len=*), parameter :: gen = '(*(g0))'

contains
subroutine viz_put(string)
character(len=*),intent(in) :: string
    write(*,fmt=gen,advance='no')string
end subroutine viz_put

subroutine viz_init(x,y)
! initialize package and set size of plot area
integer, intent(in):: x,y
    cols = x
    rows = y
    call viz_put(cli//'?25l') ! hide the cursor to reduce flicker
   call viz_put(cli//'2J')     ! clear the screen
end subroutine viz_init

subroutine viz_done
! restore settings to something sane.
    ! place cursor in last line
    CALL viz_pos(1,rows)
    ! set forground color to principal color
    ! re-enable the cursor
    ! and call for a reset
    call viz_put(cli//'30m'//cli//'?25h'//cli//'0m')
    write(*,*)
end subroutine viz_done

subroutine add_code(code,c,n)
character(len=1),intent(inout) :: code(*)
character(len=1),intent(in)    :: c
integer,intent(inout)          :: n
   n = n+1
   code(n) = c
end subroutine add_code

subroutine viz_pos(x,y)
! position cursor at a given location within screen.
! top left corner is (1,1)
integer, intent(in) :: x,y
integer :: i,n
character(len=1) :: code(7)

    n = 0

    i = y
    if (i < 1) i = 1
    if (i > rows) i = rows
    call add_code(code,'[',n)
    if (i > 9) call add_code(code,achar(48+i/10),n)
    call add_code(code,achar(48+mod(i,10)),n)
    call add_code(code,';',n)

    i = x
    if (i < 1) i = 1
    if (i > cols) i = cols
    if (i > 9) call add_code(code,achar(48+i/10),n)
    call add_code(code,achar(48+mod(i,10)),n)
    call add_code(code,'H',n)
    write(*,fmt=gen,advance='no')esc,code(1:n)
end subroutine viz_pos

subroutine viz_plot(val,nx,ny,max)
real, intent(in)          :: val(:,:)
integer, intent(in)       :: nx,ny
real, intent(in),optional :: max
integer                   :: i, j, k, l, m, n, dx, dy
real                      :: vmax, scalef, tmp

    ! set blocksize for averaging
    dx = nx / cols
    if (dx < 1) dx = 1
    dy = ny / rows
    if (dy < 1) dy = 1

    ! set or determine scaling factor for data points
    vmax=1.0e-30
    if (present(max)) then
       vmax = abs(max)
    else
       ! find absolute maximum value for scaling
       do j=1,rows
          do i=1,cols
             ! average over cells
             tmp = 0.0
             n = 0
             do k=(j-1)*dy+1,j*dy
                do l=(i-1)*dx+1,i*dx
                   tmp = tmp + val(l,k)
                   n = n + 1
                enddo
             enddo

             tmp = ABS(tmp)/REAL(n)
             if (vmax < tmp) vmax = tmp
          enddo
       enddo
    endif
    scalef = real(maxplot)/vmax

    ! now plot
    do j=1,rows
        call viz_pos(1,j)
        do i=1,cols
            ! average over cells
            tmp = 0.0
            n = 0
            do k=(j-1)*dy+1,j*dy
                do l=(i-1)*dx+1,i*dx
                    tmp = tmp + val(l,k)
                    n = n + 1
                enddo
            enddo
            ! convert absolute value into character
            m = int(scalef*abs(tmp)/real(n)+0.5)
            if (m > maxplot) m = 10
            if (tmp < 0.0) then
                if (m > 5) then
                    call viz_put(esc//'[36m'//plot(m))
                else
                    call viz_put(esc//'[34m'//plot(m))
                endif
            else
                if (m > 5) then
                    call viz_put(esc//'[31m'//plot(m))
                else
                    call viz_put(esc//'[30m'//plot(m))
                endif
            endif
        enddo
        call viz_put(esc//'[30m|')
    enddo
    call viz_pos(1,rows)
    call viz_put(esc//'[30m')
    DO i=1,cols+1
       call viz_put('-')
    enddo

end subroutine viz_plot
end module text_viz


program demo_user
use M_matrix
use text_viz
implicit none
integer, parameter :: nx = 200, ny = 200
real :: val(nx,ny)
integer :: i,j
integer :: ierr

! create a dataset in the program to pass to LAFF()
   do j=1,ny ! fill array with data
      do i=1,nx
         val(i,j) = sin(real(i)*0.05)*sin(real(j)*0.033)
      enddo
   enddo
   call put_into_laff('val',val,ierr)


call set_usersub(laff_text_viz) ! set user routine

call laff("user(val,48,24);") ! display using user routine
write(*,*)'user added routine does normalized pixelized text plot'
write(*,*)'optional parameters give character cell dimensiones'
write(*,*)'the default is the size of the array'
call laff()  ! enter interactive mode
contains

subroutine laff_text_viz(a,m,n,s,t)  ! sample usersub_placeholder routine
implicit none
integer                    :: m,n
doubleprecision            :: a(:)
doubleprecision            :: s,t
integer                    :: display_cols,display_rows

   display_rows=nint(s)
   display_cols=nint(t)
   if(display_cols.eq.0)display_cols=m
   if(display_rows.eq.0)display_rows=n
   call viz_init(display_cols,display_rows) ! initialize package and set size of plot area
   call viz_plot(reshape(real(a),[m,n]),m,n)
   call viz_done
end subroutine laff_text_viz

end program demo_user
