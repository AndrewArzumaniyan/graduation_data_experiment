      program ALIGN44
	
c    TESTING align CLAUSE .       

      print *,'===START OF align44========================'
C --------------------------------------------------
c 441 arrA4[BLOCK][BLOCK] [BLOCK] [BLOCK] arrB4[][][][] 
c       ALIGN arrB[i][j][k][l] WITH arrA[i][ j][k][l] normal      
       call align441
C -------------------------------------------------
c 442     ALIGN arrB[i][j][k][l] WITH arrA[l][i][j][k]  rotation
       call align442
C -------------------------------------------------
c 443     ALIGN arrB[i][j][k][l] WITH arrA[i][2* j][k][3*l] stretching
       call align443
C -------------------------------------------------
c 444     ALIGN arrB[i][j][k][l] WITH arrA[i+2][ j][k][ l+3]  shift
       call align444
C -------------------------------------------------
c 445     ALIGN arrB[i][j][k][l] WITH arrA[i][ j][-k+8][- l+8]  reverse
c      call align445
C -------------------------------------------------
c 446     ALIGN arrB[i][j][ ][l] WITH arrA[i][ j][2][ l]  
c                                               compression and replication            
       call align446
C -------------------------------------------------
c 447     ALIGN arrB[][j][k][i] WITH arrA[i][ j][ ][ k] 
c                                               compression and replication            
      call align447
C -------------------------------------------------
c 448     ALIGN arrB[][i][j][] WITH arrA[i][ j][1][3] 
c                                               compression and replication            
      call align448
C -------------------------------------------------
C
      print *,'=== END OF align44 ========================= '    
      end

C ----------------------------------------------------align441
c 441 arrA4[BLOCK][BLOCK] [BLOCK] [BLOCK] arrB4[][][][] 
c       ALIGN arrB[i][j][n][m] WITH arrA[i][ j][n][m] normal      

      subroutine align441
      integer, parameter :: AN1=5,AN2=5,AN3=5,AN4=5
      integer, parameter :: BN1=2,BN2=2,BN3=2,BN4=2
      integer, parameter :: NL=10000,ER=100000
c     parameters for ALIGN arrB[i][j][n][m] WITH arrA[k1i*i+li][k2j*j+lj]                                               
      integer, parameter :: k1i=1,k2i=0,k3i=0,k4i=0,li=0
      integer, parameter :: k1j=0,k2j=1,k3j=0,k4j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=1,k4n=0,ln=0
      integer, parameter :: k1m=0,k2m=0,k3m=0,k4m=1,lm=0
      character*9 tname
      integer, allocatable :: A4(:,:,:,:),B4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B4(i,j,n,m) WITH A4(k1i*i+li,k2j*j+lj,k3n*n+ln,k4m*m+lm)


      tname='align441'
      allocate (A4(AN1,AN2,AN3,AN4),B4(BN1,BN2,BN3,BN4))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A4,B4)
!dvm$ parallel (i,j,n,m) on B4(i,j,n,m)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                do m=1,BN4
                    B4(i,j,n,m) =0     
                enddo 
             enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), private(ib,jb,nb,mb)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
              do m=1,AN4
                A4(i,j,n,m) = i*NL/10+j*NL/100+n*NL/1000+m
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *                  ((n-ln) .eq.(((n-ln)/k3n) * k3n)) .and.
     *                  ((m-lm) .eq.(((m-lm)/k4m) *k4m)) .and.
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

!dvm$ parallel (i,j,n,m) on B4(i,j,n,m), reduction( min( erri ),sum(s) )
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
               do m=1,BN4
                  s = s + B4(i,j,n,m)
                  if (B4(i,j,n,m).eq.(i*NL/10+j*NL/100+n*NL/1000+m))then     
                  else
                      erri = min(erri,i*NL/10 + j*NL/100+ n*NL/1000+ m)
                  endif
                enddo
            enddo
          enddo
      enddo 
!dvm$ end region
!dvm$ get_actual(erri,s)
  
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
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
c           write (*,*) erri,s,cs
c          print *,B4  
      endif 
      deallocate (B4,A4)

      end

C ----------------------------------------------------align442
c 442     ALIGN arrB[i][j][k][l] WITH arrA[l][i][j][k]  rotation

      subroutine align442
      integer, parameter :: AN1=4,AN2=4,AN3=4,AN4=4
      integer, parameter :: BN1=4,BN2=4,BN3=4,BN4=4
      integer, parameter :: NL=10000,ER=100000
c     parameters for ALIGN arrB[i][j][n][m] WITH arrA4(k4i*n+li,k1j*i+lj,k2n*j+ln,k3m*n+lm)                                              
      integer, parameter :: k1i=0,k2i=0,k3i=0,k4i=1,li=0
      integer, parameter :: k1j=1,k2j=0,k3j=0,k4j=0,lj=0
      integer, parameter :: k1n=0,k2n=1,k3n=0,k4n=0,ln=0
      integer, parameter :: k1m=0,k2m=0,k3m=1,k4m=0,lm=0
      character*9 tname
      integer, allocatable :: A4(:,:,:,:),B4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B4(i,j,n,m) WITH A4(k4i*m+li,k1j*i+lj,k2n*j+ln,k3m*n+lm)


      tname='align442'
      allocate (A4(AN1,AN2,AN3,AN4),B4(BN1,BN2,BN3,BN4))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A4,B4)
!dvm$ parallel (i,j,n,m) on B4(i,j,n,m)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                do m=1,BN4
                    B4(i,j,n,m) =0     
                enddo 
             enddo 
          enddo 
      enddo 
!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), private(ib,jb,nb,mb)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
              do m=1,AN4
                A4(i,j,n,m) = i*NL/10+j*NL/100+n*NL/1000+m
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k4i) * k4i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k1j) *k1j)) .and.
     *                  ((n-ln) .eq.(((n-ln)/k2n) * k2n)) .and.
     *                  ((m-lm) .eq.(((m-lm)/k3m) *k3m)) .and.
     *                  (((i-li)/k4i) .gt. 0)  .and.
     *                  (((j-lj)/k1j) .gt. 0)  .and.
     *                  (((n-ln)/k2n) .gt. 0)  .and.
     *                  (((m-lm)/k3m) .gt. 0)  .and.
     *                  (((i-li)/k4i) .le. BN4)  .and.
     *                  (((j-lj)/k1j) .le. BN1)  .and.
     *                  (((n-ln)/k2n) .le. BN2)  .and.
     *                  (((m-lm)/k3m) .le. BN3)
     *                  )  then 
                        mb = (i-li)/k4i
                        ib = (j-lj)/k1j
                        jb = (n-ln)/k2n
                        nb = (m-lm)/k3m
                        B4(ib,jb,nb,mb)=ib*NL/10+jb*NL/100+nb*NL/1000+mb 
                      endif 
              enddo
            enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j,n,m) on B4(i,j,n,m), reduction( min( erri ),sum(s) )
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                do m=1,BN4
                  s = s + B4(i,j,n,m)
                  if (B4(i,j,n,m).eq.(i*NL/10+j*NL/100+n*NL/1000+m))then     
                  else
                      erri = min(erri,i*NL/10 + j*NL/100+ n*NL/1000+ m)
                  endif
                enddo
            enddo
          enddo
      enddo 
!dvm$ end region
!dvm$ get_actual(erri,s)
  
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
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
c          write (*,*) erri,s,cs
c          print *,B4  
      endif 
      deallocate (B4,A4)

      end

C ----------------------------------------------------align443
c 443     ALIGN arrB[i][j][k][l] WITH arrA[i][2* j][k][3*l] stretching

      subroutine align443
      integer, parameter :: AN1=3,AN2=4,AN3=3,AN4=6
      integer, parameter :: BN1=2,BN2=2,BN3=2,BN4=2
      integer, parameter :: NL=10000,ER=100000
c     parameters for ALIGN arrB[i][j][n][m] WITH arrA4(k1i*i+li,k2j*j+lj,k3n*n+ln,k4m*m+lm)                                               
      integer, parameter :: k1i=1,k2i=0,k3i=0,k4i=0,li=0
      integer, parameter :: k1j=0,k2j=2,k3j=0,k4j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=1,k4n=0,ln=0
      integer, parameter :: k1m=0,k2m=0,k3m=0,k4m=3,lm=0
      character*9 tname
      integer, allocatable :: A4(:,:,:,:),B4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B4(i,j,n,m) WITH A4(k1i*i+li,k2j*j+lj,k3n*n+ln,k4m*m+lm)

      tname='align443'
      allocate (A4(AN1,AN2,AN3,AN4),B4(BN1,BN2,BN3,BN4))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A4,B4)
!dvm$ parallel (i,j,n,m) on B4(i,j,n,m)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                do m=1,BN4
                    B4(i,j,n,m) =0     
                enddo 
             enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), private(ib,jb,nb,mb)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
              do m=1,AN4
                A4(i,j,n,m) = i*NL/10+j*NL/100+n*NL/1000+m
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *                  ((n-ln) .eq.(((n-ln)/k3n) * k3n)) .and.
     *                  ((m-lm) .eq.(((m-lm)/k4m) *k4m)) .and.
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

!dvm$ parallel (i,j,n,m) on B4(i,j,n,m), reduction( min( erri ),sum(s) )
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                do m=1,BN4
                  s = s + B4(i,j,n,m)
                  if (B4(i,j,n,m).eq.(i*NL/10+j*NL/100+n*NL/1000+m))then     
                  else
                      erri = min(erri,i*NL/10 + j*NL/100+ n*NL/1000+ m)
                  endif
                enddo
            enddo
          enddo
      enddo 
!dvm$ end region
!dvm$ get_actual(erri,s)
  
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
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
c          write (*,*) erri,s,cs
c          print *,B4  
      endif 
      deallocate (B4,A4)

      end

C ----------------------------------------------------align444
c 444     ALIGN arrB[i][j][k][l] WITH arrA[i+2][ j][k][ l+3]  shift

      subroutine align444
      integer, parameter :: AN1=4,AN2=4,AN3=3,AN4=6
      integer, parameter :: BN1=2,BN2=2,BN3=2,BN4=2
      integer, parameter :: NL=10000,ER=100000
c     parameters for ALIGN arrB[i][j][n][m] WITH arrA4(k1i*i+li,k2j*j+lj,k3n*n+ln,k4m*m+lm)                                               
      integer, parameter :: k1i=1,k2i=0,k3i=0,k4i=0,li=2
      integer, parameter :: k1j=0,k2j=1,k3j=0,k4j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=1,k4n=0,ln=0
      integer, parameter :: k1m=0,k2m=0,k3m=0,k4m=1,lm=3
      character*9 tname
      integer, allocatable :: A4(:,:,:,:),B4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B4(i,j,n,m) WITH A4(k1i*i+li,k2j*j+lj,k3n*n+ln,k4m*m+lm)


      tname='align444'
      allocate (A4(AN1,AN2,AN3,AN4),B4(BN1,BN2,BN3,BN4))
      erri= ER
      NNL=NL 
      s = 0

!dvm$ actual(erri,s)
!dvm$ region local(A4,B4)
!dvm$ parallel (i,j,n,m) on B4(i,j,n,m)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                do m=1,BN4
                    B4(i,j,n,m) =0     
                enddo 
             enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), private(ib,jb,nb,mb)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
              do m=1,AN4
                A4(i,j,n,m) = i*NL/10+j*NL/100+n*NL/1000+m
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *                  ((n-ln) .eq.(((n-ln)/k3n) * k3n)) .and.
     *                  ((m-lm) .eq.(((m-lm)/k4m) *k4m)) .and.
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

!dvm$ parallel (i,j,n,m) on B4(i,j,n,m), reduction( min( erri ),sum(s) )
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                do m=1,BN4
                  s = s + B4(i,j,n,m)
                  if (B4(i,j,n,m).eq.(i*NL/10+j*NL/100+n*NL/1000+m))then     
                  else
                      erri = min(erri,i*NL/10 + j*NL/100+ n*NL/1000+ m)
                  endif
                enddo
            enddo
          enddo
      enddo 
!dvm$ end region
!dvm$ get_actual(erri,s)
  
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
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
c          write (*,*) erri,s,cs
c          print *,B4  
      endif 
      deallocate (B4,A4)

      end

C ----------------------------------------------------align445
c 445     ALIGN arrB[i][j][k][l] WITH arrA[i][ j][-k+4][- l+3]  reverse

      subroutine align445
      integer, parameter :: AN1=4,AN2=4,AN3=8,AN4=6
      integer, parameter :: BN1=3,BN2=3,BN3=3,BN4=2
      integer, parameter :: NL=10000,ER=100000
c     parameters for ALIGN arrB[i][j][n][m] WITH arrA4(k1i*i+li,k2j*j+lj,k3n*n+ln,k4m*m+lm)                                               
      integer, parameter :: k1i=1,k2i=0,k3i=0,k4i=0,li=0
      integer, parameter :: k1j=0,k2j=1,k3j=0,k4j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=-1,k4n=0,ln=4
      integer, parameter :: k1m=0,k2m=0,k3m=0,k4m=-1,lm=3
      character*9 tname
      integer, allocatable :: A4(:,:,:,:),B4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B4(i,j,n,m) WITH A4(k1i*i+li,k2j*j+lj,k3n*n+ln,k4m*m+lm)


      tname='align445'
      allocate (A4(AN1,AN2,AN3,AN4),B4(BN1,BN2,BN3,BN4))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A4,B4)
!dvm$ parallel (i,j,n,m) on B4(i,j,n,m)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                do m=1,BN4
                    B4(i,j,n,m) =0     
                enddo 
             enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), private(ib,jb,nb,mb)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
              do m=1,AN4
                A4(i,j,n,m) = i*NL/10+j*NL/100+n*NL/1000+m
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *                  ((n-ln) .eq.(((n-ln)/k3n) * k3n)) .and.
     *                  ((m-lm) .eq.(((m-lm)/k4m) *k4m)) .and.
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

!dvm$ parallel (i,j,n,m) on B4(i,j,n,m), reduction( min( erri ),sum(s) )
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                do m=1,BN4
                  s = s + B4(i,j,n,m)
                  if (B4(i,j,n,m).eq.(i*NL/10+j*NL/100+n*NL/1000+m))then     
                  else
                      erri = min(erri,i*NL/10 + j*NL/100+ n*NL/1000+ m)
                  endif
                enddo
            enddo
          enddo
      enddo 
!dvm$ end region
!dvm$ get_actual(erri,s)
  
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
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
c          write (*,*) erri,s,cs
c          print *,B4  
      endif 
      deallocate (B4,A4)

      end

C ----------------------------------------------------align446
c 446     ALIGN arrB[i][j][ ][l] WITH arrA[i][ j][2][ l]  
c                                               compression and replication            !!

      subroutine align446
      integer, parameter :: AN1=4,AN2=4,AN3=4,AN4=4
      integer, parameter :: BN1=2,BN2=2,BN3=2,BN4=2
      integer, parameter :: NL=10000,ER=100000
c     parameters for ALIGN arrB[i][j][*][m] WITH arrA4(k1i*i+li,k2j*j+lj,ln,k4m*m+lm)                                               
      integer, parameter :: k1i=1,k2i=0,k3i=0,k4i=0,li=0
      integer, parameter :: k1j=0,k2j=1,k3j=0,k4j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=0,k4n=0,ln=2
      integer, parameter :: k1m=0,k2m=0,k3m=0,k4m=1,lm=0
      character*9 tname
      integer, allocatable :: A4(:,:,:,:),B4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B4(i,j,*,m) WITH A4(k1i*i+li,k2j*j+lj,ln,k4m*m+lm)


      tname='align446'
      allocate (A4(AN1,AN2,AN3,AN4),B4(BN1,BN2,BN3,BN4))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A4,B4)
!dvm$ parallel (i,j,n,m) on B4(i,j,n,m)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                do m=1,BN4
                    B4(i,j,n,m) =0     
                enddo 
             enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), private(k,ib,jb,nb,mb)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
              do m=1,AN4
                A4(i,j,n,m) = i*NL/10+j*NL/100+n*NL/1000+m
                  if (n .eq. ln ) then
                    do k = 1,BN3
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *                  ((m-lm) .eq.(((m-lm)/k4m) *k4m)) .and.
     *                  (((i-li)/k1i) .gt. 0)  .and.
     *                  (((j-lj)/k2j) .gt. 0)  .and.
     *                  (((m-lm)/k4m) .gt. 0)  .and.
     *                  (((i-li)/k1i) .le. BN1)  .and.
     *                  (((j-lj)/k2j) .le. BN2)  .and.
     *                  (((m-lm)/k4m) .le. BN4)
     *                  )  then 
                        ib = (i-li)/k1i
                        jb = (j-lj)/k2j
                        nb = k
                        mb = (m-lm)/k4m
                        B4(ib,jb,nb,mb)=ib*NL/10+jb*NL/100+nb*NL/1000+mb 
                      endif
                   enddo 
                 endif
              enddo
            enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j,n,m) on B4(i,j,n,m), reduction( min( erri ),sum(s) )
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                do m=1,BN4
                  s = s + B4(i,j,n,m)
                  if (B4(i,j,n,m).eq.(i*NL/10+j*NL/100+n*NL/1000+m))then     
                  else
                      erri = min(erri,i*NL/10 + j*NL/100+ n*NL/1000+ m)
                  endif
                enddo
            enddo
          enddo
      enddo 
!dvm$ end region
!dvm$ get_actual(erri,s)
  
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
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
c          write (*,*) erri,s,cs
c          print *,B4  
      endif 
      deallocate (B4,A4)

      end

C ----------------------------------------------------align447
c 447     ALIGN arrB[][j][k][i] WITH arrA[i][ j][ ][ k] 
c                                               compression and replication            !!

      subroutine align447
      integer, parameter :: AN1=4,AN2=4,AN3=4,AN4=4
      integer, parameter :: BN1=4,BN2=4,BN3=4,BN4=4
      integer, parameter :: NL=10000,ER=100000
c     parameters for ALIGN arrB[*][j][n][i] WITH arrA4(k1i*i+li,k2j*j+lj,*,k3m*n+lm)
      integer, parameter :: k1i=1,k2i=0,k3i=0,k4i=0,li=0
      integer, parameter :: k1j=0,k2j=1,k3j=0,k4j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=0,k4n=0,ln=0
      integer, parameter :: k1m=0,k2m=0,k3m=1,k4m=0,lm=0
      character*9 tname
      integer, allocatable :: A4(:,:,:,:),B4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B4(*,j,n,i) WITH A4(k1i*i+li,k2j*j+lj,*,k3m*n+lm)

      tname='align447'
      allocate (A4(AN1,AN2,AN3,AN4),B4(BN1,BN2,BN3,BN4))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A4,B4)
!dvm$ parallel (i,j,n,m) on B4(i,j,n,m)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                do m=1,BN4
                    B4(i,j,n,m) =0     
                enddo 
             enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), private(k,ib,jb,nb,mb)
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

!dvm$ parallel (i,j,n,m) on B4(i,j,n,m), reduction( min( erri ),sum(s) )
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                do m=1,BN4
                  s = s + B4(i,j,n,m)
                  if (B4(i,j,n,m).eq.(i*NL/10+j*NL/100+n*NL/1000+m))then     
                  else
                      erri = min(erri,i*NL/10 + j*NL/100+ n*NL/1000+ m)
                  endif
                enddo
            enddo
          enddo
      enddo 
!dvm$ end region
!dvm$ get_actual(erri,s)
   
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
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
          write (*,*) erri,s,cs
          print *,B4  
      endif 
      deallocate (B4,A4)

      end
C ----------------------------------------------------align448
c 448     ALIGN arrB[][i][j][] WITH arrA[i][ j][1][3] 
c                                               compression and replication            

      subroutine align448
      integer, parameter :: AN1=4,AN2=4,AN3=4,AN4=4
      integer, parameter :: BN1=4,BN2=4,BN3=4,BN4=4
      integer, parameter :: NL=10000,ER=100000
c     parameters for ALIGN arrB[*][i][j][*] WITH arrA4(k1i*i+li,k2j*j+lj,ln,lm)                                               
      integer, parameter :: k1i=1,k2i=0,k3i=0,k4i=0,li=0
      integer, parameter :: k1j=0,k2j=1,k3j=0,k4j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=0,k4n=0,ln=1
      integer, parameter :: k1m=0,k2m=0,k3m=0,k4m=0,lm=3
      character*9 tname
      integer, allocatable :: A4(:,:,:,:),B4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B4(*,i,j,*) WITH A4(k1i*i+li,k2j*j+lj,ln,lm)

      tname='align448'
      allocate (A4(AN1,AN2,AN3,AN4),B4(BN1,BN2,BN3,BN4))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A4,B4)
!dvm$ parallel (i,j,n,m) on B4(i,j,n,m)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                do m=1,BN4
                    B4(i,j,n,m) =0     
                enddo 
             enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), private(k,l,ib,jb,nb,mb)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
              do m=1,AN4
                A4(i,j,n,m) = i*NL/10+j*NL/100+n*NL/1000+m
                  if ((n .eq. ln ) .and. (m .eq. lm)) then
                    do k = 1,BN1
                    do l = 1,BN4
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *                  (((i-li)/k1i) .gt. 0)  .and.
     *                  (((j-lj)/k2j) .gt. 0)  .and.
     *                  (((i-li)/k1i) .le. BN2)  .and.
     *                  (((j-lj)/k2j) .le. BN3)  
     *                  )  then 
                        jb = (i-li)/k1i
                        nb = (j-lj)/k2j
                        ib = k
                        mb = l
                        B4(ib,jb,nb,mb)=ib*NL/10+jb*NL/100+nb*NL/1000+mb 
                      endif 
                   enddo 
                   enddo 
                 endif
              enddo
            enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j,n,m) on B4(i,j,n,m), reduction( min( erri ),sum(s) )
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                do m=1,BN4
                  s = s + B4(i,j,n,m)
                  if (B4(i,j,n,m).eq.(i*NL/10+j*NL/100+n*NL/1000+m))then     
                  else
                      erri = min(erri,i*NL/10 + j*NL/100+ n*NL/1000+ m)
                  endif
                enddo
            enddo
          enddo
      enddo 
!dvm$ end region
!dvm$ get_actual(erri,s)
  
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
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
c          write (*,*) erri,s,cs
c          print *,B4  
      endif 
      deallocate (B4,A4)

      end

C -------------------------------------------------
      subroutine ansyes(name)
      character*9 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*9 name
      print *,name,'  -  ***error'
      end