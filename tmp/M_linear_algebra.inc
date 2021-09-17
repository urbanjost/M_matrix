!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_inverse_hilbert(a,lda,n)

$@(#) M_matrix::ml_hilbr(3fp): generate doubleprecision inverse hilbert matrix
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
$@(#) M_matrix::mat_magic(3fp): Algorithms for magic squares

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
