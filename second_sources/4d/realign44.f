      program REALIGN44

c    Testing REALIGN directive
       
      print *,'===START OF realign44===================='
C --------------------------------------------------
c 441 ALIGN arrB[i][j][n][m] WITH arrA[i][j][n][m]
c     REALIGN arrB[][j][k][] WITH arrA[j][k][1][3]
      call realign441
C -------------------------------------------------
c 442 ALIGN arrB[][j][n][i] WITH arrA[i][j][ ][n]
c     REALIGN arrB[i][j][ ][m] WITH arrA[i][j][2][m]
      call realign442
C --------------------------------------------------
c 443 ALIGN arrB[i][j][n][m] WITH arrA[i][2*j][3*n][4*m]
c     REALIGN arrB[i][j][n][m] WITH arrA[i+1][j+2][n+3][m+4]
      call realign443
C -------------------------------------------------
c 444 ALIGN arrB[i][j][n][m] WITH arrA[m][i+1][j][2*n]
c     REALIGN arrB[i][j][n][m] WITH arrA[i+2][3*j-2][2*n-2][m+1]
      call realign444
C -------------------------------------------------
C
      print *,'=== END OF realign44 ===================='

      end

C ----------------------------------------------------realign441
c 441 ALIGN arrB[i][j][n][m] WITH arrA[i][j][n][m]
c     REALIGN arrB[][j][n][] WITH arrA[j][n][1][3]

      subroutine realign441
      integer, parameter ::  AN1=6,AN2=8,AN3=5,AN4=7
      integer, parameter ::  BN1=2,BN2=5,BN3=4,BN4=3
      integer, parameter ::  NL=10000,ER=100000
c     parameters for ALIGN arrB[i][j][n][m] WITH arrA[k1i*i+li][k2j*j+lj][k3n*n+ln][k4m*m+lm]
      integer, parameter ::  k1i=1,li=0
      integer, parameter ::  k2j=1,lj=0
      integer, parameter ::  k3n=1,ln=0
      integer, parameter ::  k4m=1,lm=0
c     parameters for REALIGN arrB[*][j][n][*] WITH arrA[kr2j*j+lrj][kr3n*n+lrn][lri][lrm]
      integer, parameter ::  kr1i=0,lri=1
      integer, parameter ::  kr2j=1,lrj=0
      integer, parameter ::  kr3n=1,lrn=0
      integer, parameter ::  kr4m=0,lrm=3
      integer, allocatable :: A4(:,:,:,:),B4(:,:,:,:)
      integer :: s=0,cs,erria=ER, errib=ER,
     >           i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb

      character(10) :: tname='realign441'

!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B4(i,j,n,m) WITH A4(k1i*i+li,k2j*j+lj,k3n*n+ln,k4m*m+lm)
!dvm$ DYNAMIC B4

      allocate (A4(AN1,AN2,AN3,AN4),B4(BN1,BN2,BN3,BN4))

!dvm$ region  out(A4,B4)
!dvm$ parallel (i,j,n,m) on B4(i,j,n,m)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                do m=1,BN4
                    B4(i,j,n,m) = 0
                enddo
             enddo
          enddo 
      enddo

!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), private(ib,jb, nb, mb)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
              do m=1,AN4
                A4(i,j,n,m) = i*NL/10+j*NL/100+n*NL/1000+m
                      if (
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k2j) * k2j)) .and.
     *                  ((n-ln) .eq.(((n-ln)/k3n) * k3n)) .and.
     *                  ((m-lm) .eq.(((m-lm)/k4m) * k4m)) .and.
     *                  (((i-li)/k1i) .gt. 0)  .and.
     *                  (((j-lj)/k2j) .gt. 0)  .and.
     *                  (((n-ln)/k3n) .gt. 0)  .and.
     *                  (((m-lm)/k4m) .gt. 0)  .and.
     *                  (((i-li)/k1i) .le. BN1)  .and.
     *                  (((j-lj)/k2j) .le. BN2)  .and.
     *                  (((n-ln)/k3n) .le. BN3)  .and.
     *                  (((m-lm)/k4m) .le. BN4)
     *                  )  then 
                        ib = (i-li)/k1i
                        jb = (j-lj)/k2j
                        nb = (n-ln)/k3n
                        mb = (m-lm)/k4m
                        B4(ib,jb,nb,mb)=ib*NL/10+jb*NL/100+nb*NL/1000+mb 
                      endif 
              enddo
            enddo
          enddo 
      enddo
!dvm$ end region

!dvm$ REALIGN B4(*,j,n,*) WITH A4(kr2j*j+lrj,kr3n*n+lrn,lri,lrm)

!dvm$ actual(erria, errib, s)

!dvm$ region

!dvm$ parallel (i,j,n,m) on B4(i,j,n,m),
!dvm$*         reduction(min(erria),min(errib),sum(s)),
!dvm$*         private(ia,ja, na, ma)
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                do m=1,BN4
                  s = s + B4(i,j,n,m)
                  if (B4(i,j,n,m)/= (i*NL/10+j*NL/100+n*NL/1000+m))then     
                    errib = min(errib,i*NL/10+j*NL/100+n*NL/1000+m)
                  endif
                  ia=kr2j*j+lrj
                  ja=kr3n*n+lrn
                  na=lri
                  ma=lrm
                  if (A4(ia,ja,na,ma) /=
     *              (ia*NL/10+ja*NL/100+na*NL/1000+ma))then     
                     erria = min(erria,ia*NL/10+ja*NL/100+na*NL/1000+ma)
                  endif
                enddo
            enddo
          enddo 
      enddo
!dvm$ end region
  
      cs = 0
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                do m=1,BN4
                    cs = cs + i*NL/10 + j*NL/100+ n*NL/1000+ m
                enddo
            enddo
          enddo
      enddo
     
!dvm$ get_actual(erria, errib, s)

      if ((erria == ER) .and. (errib == ER) .and.
     *   (s == cs)) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 

      deallocate (B4,A4)

      end

C ----------------------------------------------------realign442
c 442 ALIGN arrB[*][j][n][i] WITH arrA[i][j][*][n] 
c     REALIGN arrB[i][j][*][m] WITH arrA[i][j][2][m]  

      subroutine realign442
      integer, parameter ::  AN1=5,AN2=5,AN3=5,AN4=5
      integer, parameter ::  BN1=2,BN2=2,BN3=2,BN4=2
      integer, parameter ::  NL=10000,ER=100000
c     parameters for ALIGN arrB[*][j][n][i] WITH arrA4(k1i*i+li,k2j*j+lj,*,k3m*n+lm)                                               
      integer, parameter ::  k1i=1, li=0
      integer, parameter ::  k2j=1, lj=0
      integer, parameter ::  k3n=0, ln=0
      integer, parameter ::  k3m=1, lm=0
c     parameters for REALIGN arrB[i][j][*][m] WITH arrA(kr1i*i+lri,kr2j*j+lrj,lrn,kr4m*m+lrm)                                               
      integer, parameter ::  kr1i=1, lri=0
      integer, parameter ::  kr2j=1, lrj=0
      integer, parameter ::  kr3n=0, lrn=2
      integer, parameter ::  kr4m=1, lrm=0
      integer, allocatable :: A4(:,:,:,:),B4(:,:,:,:)
      integer :: s=0,cs,erria=ER, errib=ER,
     >           i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb
      character(10) :: tname='realign442'
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B4(*,j,n,i) WITH A4(k1i*i+li,k2j*j+lj,*,k3m*n+lm)
!dvm$ DYNAMIC B4

      allocate (A4(AN1,AN2,AN3,AN4),B4(BN1,BN2,BN3,BN4))

!dvm$ region inout(A4, B4)
!dvm$ parallel (i,j,n,m) on B4(i,j,n,m)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                do m=1,BN4
                    B4(i,j,n,m) = 0     
                enddo
             enddo
          enddo 
      enddo

!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), private(ib,jb, nb, mb,k)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
              do m=1,AN4
                A4(i,j,n,m) = i*NL/10+j*NL/100+n*NL/1000+m
                    do k = 1,BN1
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *                  ((m-lm) .eq.(((m-lm)/k3m) *k3m)) .and.
     *                  (((i-li)/k1i) .gt. 0)  .and.
     *                  (((j-lj)/k2j) .gt. 0)  .and.
     *                  (((m-lm)/k3m) .gt. 0)  .and.
     *                  (((i-li)/k1i) .le. BN4)  .and.
     *                  (((j-lj)/k2j) .le. BN2)  .and.
     *                  (((m-lm)/k3m) .le. BN3)
     *                  )  then 
                        mb = (i-li)/k1i
                        jb = (j-lj)/k2j
                        ib = k
                        nb = (m-lm)/k3m
                        B4(ib,jb,nb,mb)=ib*NL/10+jb*NL/100+nb*NL/1000+mb 
                      endif 
                   enddo
              enddo
            enddo
          enddo 
      enddo
!dvm$ end region

!dvm$ REALIGN B4(i,j,*,m) WITH A4(kr1i*i+lri,kr2j*j+lrj,lrn,kr4m*m+lrm)

!dvm$ actual(erria, errib, s)

!dvm$ region

!dvm$ parallel (i,j,n,m) on B4(i,j,n,m), 
!dvm$*         reduction(min(erria),min(errib),sum(s)),
!dvm$*         private(ia,ja,na,ma)
      do i=1,BN1
        do j=1,BN2
          do n=1,BN3
            do m=1,BN4
                s = s + B4(i,j,n,m)
                if (B4(i,j,n,m) /= (i*NL/10+j*NL/100+n*NL/1000+m)) then
                   errib = min(errib,i*NL/10 + j*NL/100+ n*NL/1000+ m)
                endif
                ia=kr1i*i+lri
                ja=kr2j*j+lrj
                na=lrn
                ma=kr4m*m+lrm
                if (A4(ia,ja,na,ma) /=
     *             (ia*NL/10+ja*NL/100+na*NL/1000+ma)) then
                    erria = min(erria,i*NL/10 + j*NL/100+ n*NL/1000+ m)
                endif
              enddo
          enddo
        enddo 
      enddo
!dvm$ end region
  
      cs = 0              
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                do m=1,BN4
                    cs = cs + i*NL/10 + j*NL/100+ n*NL/1000+ m
                enddo
            enddo
          enddo
      enddo
     
!dvm$ get_actual(erria, errib, s)

      if ((erria == ER) .and. (errib == ER) .and.
     *     (s == cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (B4,A4)

      end

C ----------------------------------------------------realign443
c 443 ALIGN arrB[i][j][n][m] WITH arrA[i][2*j][3*n][4*m]    
c     REALIGN arrB[i][j][n][m] WITH arrA[i+1][j+2][n+3][m+4] 

      subroutine realign443
      integer, parameter ::  AN1=10,AN2=8,AN3=15,AN4=12
      integer, parameter ::  BN1=4,BN2=3,BN3=5,BN4=3
      integer, parameter ::  NL=10000,ER=100000
c     parameters for ALIGN arrB[i][j][n][m] WITH arrA[k1i*i+li][k2j*j+lj][k3n*n+ln][k4m*m+lm]
      integer, parameter ::  k1i=1,li=0
      integer, parameter ::  k2j=2,lj=0
      integer, parameter ::  k3n=3,ln=0
      integer, parameter ::  k4m=4,lm=0
c     parameters for REALIGN arrB[i][j][n][m] WITH arrA[kr1i*i+lri][kr2j*j+lrj][kr3n*n+lrn][kr4m*m+lrm]
      integer, parameter ::  kr1i=1,lri=1
      integer, parameter ::  kr2j=1,lrj=2
      integer, parameter ::  kr3n=1,lrn=3
      integer, parameter ::  kr4m=1,lrm=4

      integer, allocatable :: A4(:,:,:,:),B4(:,:,:,:)
      integer :: s=0,cs,erria=ER, errib=ER,
     >           i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb

      character(10) :: tname='realign443'

               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B4(i,j,n,m) WITH A4(k1i*i+li,k2j*j+lj,k3n*n+ln,k4m*m+lm)
!dvm$ DYNAMIC B4

      allocate (A4(AN1,AN2,AN3,AN4),B4(BN1,BN2,BN3,BN4))

!dvm$ region  out(A4,B4)
!dvm$ parallel (i,j,n,m) on B4(i,j,n,m)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                do m=1,BN4
                    B4(i,j,n,m) = 5     
                enddo
             enddo
          enddo 
      enddo

!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), private(ib,jb, nb, mb)
      do i=1,AN1
         do j=1,AN2
            do n=1,AN3
               do m=1,AN4
                  A4(i,j,n,m) = i*NL/10+j*NL/100+n*NL/1000+m+1
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k2j) * k2j)) .and.
     *                  ((n-ln) .eq.(((n-ln)/k3n) * k3n)) .and.
     *                  ((m-lm) .eq.(((m-lm)/k4m) * k4m)) .and.
     *                  (((i-li)/k1i) .gt. 0)  .and.
     *                  (((j-lj)/k2j) .gt. 0)  .and.
     *                  (((n-ln)/k3n) .gt. 0)  .and.
     *                  (((m-lm)/k4m) .gt. 0)  .and.
     *                  (((i-li)/k1i) .le. BN1)  .and.
     *                  (((j-lj)/k2j) .le. BN2)  .and.
     *                  (((n-ln)/k3n) .le. BN3)  .and.
     *                  (((m-lm)/k4m) .le. BN4)
     *                  )  then 
                        ib = (i-li)/k1i
                        jb = (j-lj)/k2j
                        nb = (n-ln)/k3n
                        mb = (m-lm)/k4m
                        B4(ib,jb,nb,mb)=B4(ib,jb,nb,mb)+
     *                               ib*NL/10+jb*NL/100+nb*NL/1000+mb 
                      endif 
                enddo
             enddo
          enddo 
      enddo
!dvm$ end region

!dvm$ REALIGN B4(i,j,n,m)
!dvm$*   WITH A4(kr1i*i+lri,kr2j*j+lrj,kr3n*n+lrn,kr4m*m+lrm)

!dvm$ actual(erria, errib, s)

!dvm$ region

!dvm$ parallel (i,j,n,m) on B4(i,j,n,m),
!dvm$*         reduction(min(erria),min(errib),sum(s)),
!dvm$*         private(ia,ja, na, ma)
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                do m=1,BN4
                  s = s + B4(i,j,n,m)
                  if (B4(i,j,n,m)/= (i*NL/10+j*NL/100+n*NL/1000+m+5))
     *            then     
                    errib = min(errib,i*NL/10+j*NL/100+n*NL/1000+m)
                  endif
                  ia=kr1i*i+lri
                  ja=kr2j*j+lrj
                  na=kr3n*n+lrn
                  ma=kr4m*m+lrm
                  if (A4(ia,ja,na,ma) /=
     *               (ia*NL/10+ja*NL/100+na*NL/1000+ma+1)) then     
                      erria = min(erria,i*NL/10+j*NL/100+n*NL/1000+m)
                  endif
                enddo
            enddo
          enddo 
      enddo
!dvm$ end region
  
      cs = 0              
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                do m=1,BN4
                    cs = cs + i*NL/10 + j*NL/100+ n*NL/1000 + m + 5
                enddo
            enddo
          enddo
      enddo
     
!dvm$ get_actual(erria, errib, s)

      if ((erria == ER) .and. (errib == ER) .and.
     *     (s == cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (B4,A4)

      end

C ----------------------------------------------------realign444
c 444 ALIGN arrB[i][j][n][m] WITH arrA[m][i+1][j][2*n] 
c     REALIGN arrB[i][j][n][m] WITH arrA[i+2][3*j-2][2*n-2][m+1]  

      subroutine realign444
      integer, parameter ::  AN1=12,AN2=15,AN3=16,AN4=10
      integer, parameter ::  BN1=4,BN2=4,BN3=5,BN4=3
      integer, parameter ::  NL=10000,ER=100000
c     parameters for ALIGN arrB[i][j][n][m] WITH arrA4(k4m*m+lm,k1i*i+li,k2j*j+lj,k3n*n+ln)
      integer, parameter ::  k1i=1, li=1
      integer, parameter ::  k2j=1, lj=0
      integer, parameter ::  k3n=2, ln=0
      integer, parameter ::  k4m=1, lm=0
c     parameters for REALIGN arrB[i][j][n][m] WITH arrA(kr1i*i+lri,kr2j*j+lrj,k3n*n+lrn,kr4m*m+lrm)
      integer, parameter ::  kr1i=1, lri=2
      integer, parameter ::  kr2j=3, lrj=-2
      integer, parameter ::  kr3n=2, lrn=-1
      integer, parameter ::  kr4m=1, lrm=1

      integer, allocatable :: A4(:,:,:,:),B4(:,:,:,:)
      integer :: s=0,cs,erria=ER, errib=ER,
     >           i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb
      character(10) :: tname='realign444'
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B4(i,j,n,m) WITH A4(k4m*m+lm,k1i*i+li,k2j*j+lj,k3n*n+ln)
!dvm$ DYNAMIC B4

      allocate (A4(AN1,AN2,AN3,AN4),B4(BN1,BN2,BN3,BN4))

!dvm$ region inout(A4, B4)
!dvm$ parallel (i,j,n,m) on B4(i,j,n,m)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                do m=1,BN4
                    B4(i,j,n,m) = 4     
                enddo
             enddo
          enddo 
      enddo

!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), private(ib, jb, nb, mb)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
              do m=1,AN4
                A4(i,j,n,m) = 10+i*NL/10+j*NL/100+n*NL/1000+m
                  if ( 
     *               ((i-lm) == (((i-lm)/k4m) * k4m)) .and.
     *               ((j-li) == (((j-li)/k1i) * k1i)) .and.
     *               ((n-lj) == (((n-lj)/k2j) * k2j)) .and.
     *               ((m-ln) == (((m-ln)/k3n) * k3n)) .and.
     *               (((i-lm)/k4m) > 0) .and.
     *               (((j-li)/k1i) > 0) .and.
     *               (((n-lj)/k2j) > 0) .and.
     *               (((m-ln)/k3n) > 0) .and.
     *               (((i-lm)/k4m) <= BN4) .and.
     *               (((j-li)/k1i) <= BN1) .and.
     *               (((n-lj)/k2j) <= BN2) .and.
     *               (((m-ln)/k3n) <= BN3) 
     *               ) then
                        ib = (j-li)/k1i
                        jb = (n-lj)/k2j
                        nb = (m-ln)/k3n
                        mb = (i-lm)/k4m
                        B4(ib,jb,nb,mb) = B4(ib,jb,nb,mb) +
     *                          ib*NL/10+jb*NL/100+nb*NL/1000+mb;
                      endif
                enddo
             enddo
          enddo 
      enddo
!dvm$ end region

!dvm$ REALIGN B4(i,j,n,m)
!dvm$*        WITH A4(kr1i*i+lri,kr2j*j+lrj,kr3n*n+lrn,kr4m*m+lrm)

!dvm$ actual(erria, errib, s)

!dvm$ region

!dvm$ parallel (i,j,n,m) on B4(i,j,n,m), 
!dvm$*         reduction(min(erria),min(errib),sum(s)),
!dvm$*         private(ia,ja,na,ma)
      do i=1,BN1
        do j=1,BN2
          do n=1,BN3
            do m=1,BN4
                s = s + B4(i,j,n,m)
                if (B4(i,j,n,m) /= (i*NL/10+j*NL/100+n*NL/1000+m+4))then     
                   errib = min(errib,i*NL/10 + j*NL/100+ n*NL/1000 + m)
                endif
                ia=kr1i*i+lri;
                ja=kr2j*j+lrj;
                na=kr3n*n+lrn;
                ma=kr4m*m+lrm;
                if (A4(ia,ja,na,ma) /=
     *             (ia*NL/10+ja*NL/100+na*NL/1000+ma+10))then     
                    erria = min(erria,i*NL/10 + j*NL/100+ n*NL/1000+ m)
                endif
              enddo
          enddo
        enddo 
      enddo
!dvm$ end region
  
      cs = 0              
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                do m=1,BN4
                    cs = cs + i*NL/10 + j*NL/100+ n*NL/1000+ m + 4
                enddo
            enddo
          enddo
      enddo
     
!dvm$ get_actual(erria, errib, s)

      if ((erria == ER) .and. (errib == ER) .and.
     *     (s == cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (B4,A4)

      end

C -------------------------------------------------
      subroutine ansyes(name)
      character(*) name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character(*) name
      print *,name,'  -  ***error'
      end
