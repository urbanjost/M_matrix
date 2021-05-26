
program demo_user
use M_matrix
implicit none
call set_usersub(laff_user)
call laff()
contains

subroutine laff_user(a,m,n,s,t)  ! sample usersub_placeholder routine
implicit none
integer                    :: m,n
doubleprecision            :: a(:)
doubleprecision            :: s,t
integer                    :: i, j, k
   write(*,*)'MY ROUTINE'
   write(*,*)'M=',m
   write(*,*)'N=',n
   write(*,*)'S=',s
   write(*,*)'T=',t
   k=0
   do i = 1, m
      do j = 1, n
         k=k+1
         write(*,*)i,j,a(k)
      enddo
   enddo
   k=0
   if(s.eq.0)s=1
   do i = 1, m
      do j = 1, n
         k=k+1
         a(k)=a(k)*s+t
      enddo
   enddo
end subroutine laff_user

end program demo_user
