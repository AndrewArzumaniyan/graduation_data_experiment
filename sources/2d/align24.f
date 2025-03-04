      program ALIGN24

c    TESTING align CLAUSE .       

      print *,'===START OF align24========================'
C --------------------------------------------------
c      call forcat     
C --------------------------------------------------
c 241 arrA2[BLOCK][ BLOCK]  arrB4[ ][ ][ ][ ] ALIGN arrB[i][j][][] WITH arrA[i][j]  
c                                                       matrix compression      
      call align241
C -------------------------------------------------
c 242     ALIGN arrB[ ][ j][][i] WITH arrA[i+4][ 2*j] matrix compression
      call align2421
      call align2422
C -------------------------------------------------
c 243     ALIGN arrB[ ][ ][i][] WITH arrA[1][i] matrix compression 
c                                                       and replication     !!
      call align243
C -------------------------------------------------
      print *,'=== END OF align24 ========================'
      end

C ----------------------------------------------------align241
c 241 arrA2[BLOCK][ BLOCK]  arrB4[ ][ ][ ][ ] ALIGN arrB[i][j][][] WITH arrA[i][j]  
c                                                       matrix compression      

      subroutine align241
      integer, parameter :: AN1=5,AN2=5,BN1=2,BN2=2,BN3=2,BN4=2
      integer, parameter :: PN=2,NL=10000,ER=100000
c     parameters for ALIGN arrB[i][j][][] WITH arrA[k1i*i+li][k2j*j+lj]                                               
      integer, parameter :: k1i=1,k2i=0,k3i=0,li=0
      integer, parameter :: k1j=0,k2j=1,k3j=0,lj=0
      character*9 tname
      integer, allocatable :: A2(:,:),B4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A2(BLOCK,BLOCK)   
!dvm$ ALIGN B4(i,j,*,*) WITH A2(k1i * i + li,k2j * j + lj)


      tname='align241'
      allocate (A2(AN1,AN2), B4(BN1,BN2,BN3,BN4))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A2,B4)
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

!dvm$ parallel (i,j) on A2(i,j), private(ib,jb,n,m,nb,mb)
      do i=1,AN1
          do j=1,AN2
                A2(i,j) = i*NL/10 + j
                do n=1,BN3
                    do m=1,BN4
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *                  (((i-li)/k1i) .gt. 0)  .and.
     *                  (((j-lj)/k2j) .gt. 0)  .and.
     *                  (((i-li)/k1i) .le. BN1)  .and.
     *                  (((j-lj)/k2j) .le. BN2)
     *                  )  then 
                        ib = (i-li)/k1i
                        jb = (j-lj)/k2j
                        nb = n
                        mb = m  
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
          write (*,*) erri,s,cs
          print *,B4  
      endif 
      deallocate (B4,A2)
      end

C ----------------------------------------------------align242
c 242     ALIGN arrB[ ][ j][][i] WITH arrA[i+4][ 2*j] matrix compression

      subroutine align242

      integer, parameter :: AN1=3,AN2=4,BN1=2,BN2=2,BN3=2,BN4=2
      integer, parameter :: PN=2,NL=10000,ER=100000
c     parameters for ALIGN arrB[][j][][i] WITH arrA[k1i*i+li][k2j*j+lj]                                               
      integer, parameter :: k1i=1,k2i=0,k3i=0,li=1
      integer, parameter :: k1j=0,k2j=2,k3j=0,lj=0
      character*9 tname
      integer, allocatable :: A2(:,:),B4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A2(BLOCK,BLOCK)    
!dvm$ ALIGN B4(*,j,*,i) WITH A2(k1i * i + li,k2j * j + lj)

      tname='align242'
      allocate (A2(AN1,AN2), B4(BN1,BN2,BN3,BN4))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A2,B4)
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

!dvm$ parallel (i,j) on A2(i,j), private(ib,jb,n,m,nb,mb)
      do i=1,AN1
          do j=1,AN2
                A2(i,j) = i*NL/10 + j
                do n=1,BN1
                    do m=1,BN3
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *                  (((i-li)/k1i) .gt. 0)  .and.
     *                  (((j-lj)/k2j) .gt. 0)  .and.
     *                  (((i-li)/k1i) .le. BN4)  .and.
     *                  (((j-lj)/k2j) .le. BN2)
     *                  )  then 
                        mb = (i-li)/k1i
                        jb = (j-lj)/k2j
                        ib = n
                        nb = m  
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
      deallocate (B4,A2)

      end

C ----------------------------------------------------align2421
c 2421      ALIGN arrB[ ][ i][][j] WITH arrA[j+4][ 2*i] matrix compression

      subroutine align2421
      integer, parameter :: AN1=12,AN2=9,BN1=4,BN2=4,BN3=4,BN4=4
      integer, parameter :: PN=2,NL=10000,ER=100000
c     parameters for ALIGN arrB[][i][][j] WITH arrA[k2i*j+li][k1j*i+lj]                                               
      integer, parameter :: k1i=0,k2i=1,k3i=0,li=4
      integer, parameter :: k1j=2,k2j=0,k3j=0,lj=0
      character*9 tname
      integer, allocatable :: A2(:,:),B4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A2(BLOCK,BLOCK)    
!dvm$ ALIGN B4(*,i,*,j) WITH A2(k2i * j + li,k1j * i + lj)

      tname='align2421'
      allocate (A2(AN1,AN2), B4(BN1,BN2,BN3,BN4))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A2,B4)
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

!dvm$ parallel (i,j) on A2(i,j), private(ib,jb,nb,mb,n,m)
      do i=1,AN1
          do j=1,AN2
                A2(i,j) = i*NL/10 + j
                do n=1,BN1
                    do m=1,BN3
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k2i) * k2i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k1j) *k1j)) .and.
     *                  (((i-li)/k2i) .gt. 0)  .and.
     *                  (((j-lj)/k1j) .gt. 0)  .and.
     *                  (((i-li)/k2i) .le. BN4)  .and.
     *                  (((j-lj)/k1j) .le. BN2)
     *                  )  then 
                        mb = (i-li)/k2i
                        jb = (j-lj)/k1j
                        ib = n
                        nb = m
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
      deallocate (B4,A2)

      end

C ----------------------------------------------------align2422
c 2422      ALIGN arrB[ ][ i][][j] WITH arrA[j+1][ 2*i] matrix compression

      subroutine align2422
      integer, parameter :: AN1=3,AN2=4,BN1=2,BN2=2,BN3=2,BN4=2
      integer, parameter :: PN=2,NL=10000,ER=100000
c     parameters for ALIGN arrB[][i][][j] WITH arrA[k2i*j+li][k1j*i+lj]                                               
      integer, parameter :: k1i=0,k2i=1,k3i=0,li=1
      integer, parameter :: k1j=2,k2j=0,k3j=0,lj=0
      character*9 tname
      integer, allocatable :: A2(:,:),B4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A2(BLOCK,BLOCK)    
!dvm$ ALIGN B4(*,i,*,j) WITH A2(k2i * j + li,k1j * i + lj)


      tname='align2422'
      allocate (A2(AN1,AN2), B4(BN1,BN2,BN3,BN4))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A2,B4)
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

!dvm$ parallel (i,j) on A2(i,j), private(ib,jb,nb,mb,n,m)
      do i=1,AN1
          do j=1,AN2
                A2(i,j) = i*NL/10 + j
                do n=1,BN1
                    do m=1,BN3
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k2i) * k2i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k1j) *k1j)) .and.
     *                  (((i-li)/k2i) .gt. 0)  .and.
     *                  (((j-lj)/k1j) .gt. 0)  .and.
     *                  (((i-li)/k2i) .le. BN4)  .and.
     *                  (((j-lj)/k1j) .le. BN2)
     *                  )  then 
                        mb = (i-li)/k2i
                        jb = (j-lj)/k1j
                        ib = n
                        nb = m
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
      endif 
      deallocate (B4,A2)

      end

C ----------------------------------------------------align243
c 243     ALIGN arrB[ ][ ][i][] WITH arrA[1][i] matrix compression 
c                                                       and replication     !!

      subroutine align243
      integer, parameter :: AN1=3,AN2=4,BN1=2,BN2=2,BN3=2,BN4=2
      integer, parameter :: PN=2,NL=10000,ER=100000

c     parameters for ALIGN arrB[][ ][i][ ] WITH arrA[li][k1j*i+lj]                                               
      integer, parameter :: k1i=0,k2i=0,k3i=0,li=1
      integer, parameter :: k1j=1,k2j=0,k3j=0,lj=0
      character*9 tname
      integer, allocatable :: A2(:,:),B4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A2(BLOCK,BLOCK)    
!dvm$ ALIGN B4(*,*,i,*) WITH A2(li,k1j * i + lj)


      tname='align243'
      allocate (A2(AN1,AN2), B4(BN1,BN2,BN3,BN4))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A2,B4)
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

!dvm$ parallel (i,j) on A2(i,j), private(ib,jb,nb,mb,n,m,k)
      do i=1,AN1
          do j=1,AN2
                A2(i,j) = i*NL/10 + j
                if (i .eq. (li)) then
                  do n=1,BN1
                    do m=1,BN2
                     do k=1,BN4
                      if ( 
     *                  ((j-lj) .eq.(((j-lj)/k1j) *k1j)) .and.
     *                  (((j-lj)/k1j) .gt. 0)  .and.
     *                  (((j-lj)/k1j) .le. BN3)
     *                  )  then 
                        mb = k
                        jb = m
                        ib = n
                        nb = ((j-lj)/k1j)  
                        B4(ib,jb,nb,mb)=ib*NL/10+jb*NL/100+nb*NL/1000+mb 
                      endif 
                     enddo
                    enddo
                  enddo
                endif
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
      endif 
      deallocate (B4,A2)

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