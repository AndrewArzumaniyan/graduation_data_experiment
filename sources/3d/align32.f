      program ALIGN32

c    TESTING align CLAUSE .       

      print *,'===START OF align32========================'
C --------------------------------------------------
c 321	arrA3[BLOCK][ BLOCK] [ BLOCK]	arrB2[][]	ALIGN arrB[i][j] WITH arrA[i][j][1]	
c                                                   matrix on section
      call align321
C -------------------------------------------------
c 322			ALIGN arrB[i][j] WITH arrA[j][i][5]	matrix on section with rotation
      call align322
C -------------------------------------------------
c 323			ALIGN arrB[i][j] WITH arrA[j][1][2*i]	matrix on section with 
c                                                       rotation and stretching
      call align323
C -------------------------------------------------
c 324			ALIGN arrB[i][j] WITH arrA[][i][j]	matrix replication
      call align324
C -------------------------------------------------
      print *,'=== END OF align32 ========================'
      end

C ----------------------------------------------------align321
c 321	arrA3[BLOCK][ BLOCK] [ BLOCK]	arrB2[][]	ALIGN arrB[i][j] WITH arrA[i][j][1]	
c                                                   matrix on section

      subroutine align321
      integer, parameter :: AN1=5,AN2=5,AN3=5,BN1=4,BN2=4,NL=1000,ER=10000
c     parameters for ALIGN arrB[i][j] WITH arrA[k1i * i + li][k2j * j + lj][ln]                                                 
      integer, parameter :: k1i=1,k2i=0,k3i=0,li=0
      integer, parameter :: k1j=0,k2j=1,k3j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=0,ln=1
      character*9 tname
      integer, allocatable :: A3(:,:,:),B2(:,:)
      integer s,cs,erri,i,j,n,ia,ja,na,ib,jb,nb,Avalue,Bvalue
               
!dvm$ distribute A3(BLOCK,BLOCK,BLOCK)   
!dvm$ ALIGN B2(i,j) WITH A3(k1i * i + li,k2j * j + lj,ln)

      tname='align321'
      allocate (A3(AN1,AN2,AN3), B2(BN1,BN2))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A3,B2)
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) =0     
          enddo 
      enddo 

!dvm$ parallel (i,j,n) on A3(i,j,n), private(ib,jb)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                A3(i,j,n) = i*NL/10 + j*NL/100 + n
                if ( (n .eq. ln ) .and.
     *              ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *              ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *              (((i-li)/k1i) .gt. 0)  .and.
     *              (((j-lj)/k2j) .gt. 0)  .and.
     *              (((i-li)/k1i) .le. BN1)  .and.
     *              (((j-lj)/k2j) .le. BN2)
     *              )  then 
                    ib = (i-li)/k1i
                    jb = (j-lj)/k2j  
                    B2(ib,jb) = ib*NL/10 + jb*NL/100 
                endif 
            enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j) on B2(i,j), reduction( min( erri ),sum(s) ),
!dvm$* private(ia,ja,na)
      do i=1,BN1
          do j=1,BN2
            s = s + B2(i,j)
            if (B2(i,j) .eq.(i*NL/10 + j*NL/100)) then     
            else
               erri = min(erri,i*NL/10 + j*NL/100)
            endif
            ia=k1i * i + li
            ja=k2j * j + lj
            na = ln 
            if (A3(ia,ja,na) .eq.(ia*NL/10 + ja*NL/100 + na)) then     
            else
               erri = min(erri,ia*NL/10 + ja*NL/100 + na)
            endif 
          enddo 
      enddo 
!dvm$ end region
!dvm$ get_actual(erri,s)
  
      cs = 0              
      do i=1,BN1
          do j=1,BN2
                cs = cs + i*NL/10 + j*NL/100
          enddo 
      enddo 
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
          else
          call ansno(tname)
c          write (*,*) erri,s,cs
      endif 
      deallocate (B2,A3)

      end

C ----------------------------------------------------align322
c 322			ALIGN arrB[i][j] WITH arrA[j][i][5]	matrix on section with rotation

      subroutine align322
      integer, parameter :: AN1=5,AN2=5,AN3=5,BN1=4,BN2=4,NL=1000,ER=10000
c     parameters for ALIGN arrB[i][j] WITH arrA[k2i * j + li][k1j * i + lj][ln]                                                 
      integer, parameter :: k1i=0,k2i=1,k3i=0,li=0
      integer, parameter :: k1j=1,k2j=0,k3j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=0,ln=5
      character*9 tname
      integer, allocatable :: A3(:,:,:),B2(:,:)
      integer s,cs,erri,i,j,n,ia,ja,na,ib,jb,nb,Avalue,Bvalue
               
!dvm$ distribute A3(BLOCK,BLOCK,BLOCK)   
!dvm$ ALIGN B2(i,j) WITH A3(k2i * j + li,k1j * i + lj,ln)


      tname='align322'
      allocate (A3(AN1,AN2,AN3), B2(BN1,BN2))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A3,B2)
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) =0     
          enddo 
      enddo 

!dvm$ parallel (i,j,n) on A3(i,j,n), private(ib,jb)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                A3(i,j,n) = i*NL/10 + j*NL/100 + n
                if ( (n .eq. ln ) .and.
     *              ((i-li) .eq.(((i-li)/k2i) * k2i)) .and.
     *              ((j-lj) .eq.(((j-lj)/k1j) *k1j)) .and.
     *              (((i-li)/k2i) .gt. 0)  .and.
     *              (((j-lj)/k1j) .gt. 0)  .and.
     *              (((i-li)/k2i) .le. BN2)  .and.
     *              (((j-lj)/k1j) .le. BN1)
     *              )  then 
                    ib = (j-lj)/k1j
                    jb = (i-li)/k2i  
                    B2(ib,jb) = ib*NL/10 + jb*NL/100 
                endif 
            enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j) on B2(i,j), reduction( min( erri ),sum(s) ),
!dvm$* private(ia,ja,na)
      do i=1,BN1
          do j=1,BN2
            s = s + B2(i,j)
            if (B2(i,j) .eq.(i*NL/10 + j*NL/100)) then     
            else
               erri = min(erri,i*NL/10 + j*NL/100)
            endif
            ia=k2i * j + li
            ja=k1j * i + lj
            na = ln 
            if (A3(ia,ja,na) .eq.(ia*NL/10 + ja*NL/100 + na)) then     
            else
               erri = min(erri,ia*NL/10 + ja*NL/100 + na)
            endif 
          enddo 
      enddo 
!dvm$ end region
!dvm$ get_actual(erri,s)
  
      cs = 0              
      do i=1,BN1
          do j=1,BN2
                cs = cs + i*NL/10 + j*NL/100
          enddo 
      enddo 
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B2,A3)

      end

C ----------------------------------------------------align323
c 323			ALIGN arrB[i][j] WITH arrA[j][1][2*i]	matrix on section with 
c                                                       rotation and stretching
      subroutine align323
      integer, parameter :: AN1=5,AN2=2,AN3=8,BN1=4,BN2=4,NL=1000,ER=10000
c     parameters for ALIGN arrB[i][j] WITH arrA[k2i * j + li][lj][k1n * i + ln]                                                 
      integer, parameter :: k1i=0,k2i=1,k3i=0,li=0
      integer, parameter :: k1j=0,k2j=0,k3j=0,lj=1
      integer, parameter :: k1n=2,k2n=0,k3n=0,ln=0
      character*9 tname
      integer, allocatable :: A3(:,:,:),B2(:,:)
      integer s,cs,erri,i,j,n,ia,ja,na,ib,jb,nb,Avalue,Bvalue
               
!dvm$ distribute A3(BLOCK,BLOCK,BLOCK)   
!dvm$ ALIGN B2(i,j) WITH A3(k2i * j + li,lj,k1n * i + ln)

      tname='align323'
      allocate (A3(AN1,AN2,AN3), B2(BN1,BN2))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A3,B2)
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) =0     
          enddo 
      enddo 

!dvm$ parallel (i,j,n) on A3(i,j,n), private(ib,jb)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                A3(i,j,n) = i*NL/10 + j*NL/100 + n
                if ( (j .eq. lj ) .and.
     *              ((i-li) .eq.(((i-li)/k2i) * k2i)) .and.
     *              ((n-ln) .eq.(((n-ln)/k1n) *k1n)) .and.
     *              (((i-li)/k2i) .gt. 0)  .and.
     *              (((n-ln)/k1n) .gt. 0)  .and.
     *              (((i-li)/k2i) .le. BN2)  .and.
     *              (((n-ln)/k1n) .le. BN1)
     *              )  then 
                    ib = (n-ln)/k1n
                    jb = (i-li)/k2i  
                    B2(ib,jb) = ib*NL/10 + jb*NL/100 
                endif 
            enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j) on B2(i,j), reduction( min( erri ),sum(s) ),
!dvm$* private(ia,ja,na)
      do i=1,BN1
          do j=1,BN2
            s = s + B2(i,j)
            if (B2(i,j) .eq.(i*NL/10 + j*NL/100)) then     
            else
               erri = min(erri,i*NL/10 + j*NL/100)
            endif
            ia=k2i * j + li
            ja=lj
            na = k1n * i + ln 
            if (A3(ia,ja,na) .eq.(ia*NL/10 + ja*NL/100 + na)) then     
            else
               erri = min(erri,ia*NL/10 + ja*NL/100 + na)
            endif 
          enddo 
      enddo 
!dvm$ end region
!dvm$ get_actual(erri,s)
  
      cs = 0              
      do i=1,BN1
          do j=1,BN2
                cs = cs + i*NL/10 + j*NL/100
          enddo 
      enddo 
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
          else
          call ansno(tname)
c          write (*,*) erri,s,cs
      endif 
      deallocate (B2,A3)

      end
C ----------------------------------------------------align324
c 324			ALIGN arrB[i][j] WITH arrA[][i][j]	matrix replication
      subroutine align324
      integer, parameter :: AN1=4,AN2=6,AN3=6,BN1=4,BN2=4,NL=1000,ER=10000
c     parameters for ALIGN arrB[i][j] WITH arrA[*,k1j * i + lj,k2n * j + ln]                                                 
      integer, parameter :: k1i=0,k2i=0,k3i=0,li=0
      integer, parameter :: k1j=1,k2j=0,k3j=0,lj=0
      integer, parameter :: k1n=0,k2n=1,k3n=0,ln=0
      character*9 tname
      integer, allocatable :: A3(:,:,:),B2(:,:)
      integer s,cs,erri,i,j,n,ia,ja,na,ib,jb,nb,Avalue,Bvalue
               
!dvm$ distribute A3(BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B2(i,j) WITH A3(*,k1j * i + lj,k2n * j + ln)


      tname='align324'
      allocate (A3(AN1,AN2,AN3), B2(BN1,BN2))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A3,B2)
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) =i*NL/10 + j*NL/100     
          enddo 
      enddo 

!dvm$ parallel (i,j,n) on A3(i,j,n), reduction( min( erri ) ),
!dvm$* private(ib,jb)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                A3(i,j,n) = i*NL/10 + j*NL/100 + n
                if ( 
     *              ((j-lj) .eq.(((j-lj)/k1j) * k1j)) .and.
     *              ((n-ln) .eq.(((n-ln)/k2n) *k2n)) .and.
     *              (((j-lj)/k1j) .gt. 0)  .and.
     *              (((n-ln)/k2n) .gt. 0)  .and.
     *              (((j-lj)/k1j) .le. BN1)  .and.
     *              (((n-ln)/k2n) .le. BN2)
     *              )  then 
                    jb = (n-ln)/k2n
                    ib = (j-lj)/k1j  
                    if (B2(ib,jb) .eq.(ib*NL/10 + jb*NL/100)) then     
                    else
                        erri = ib*NL/10 + jb*NL/100
                    endif
                endif 
            enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j) on B2(i,j), reduction( min( erri ),sum(s) )
      do i=1,BN1
          do j=1,BN2
            s = s + B2(i,j)
            if (B2(i,j) .eq.(i*NL/10 + j*NL/100)) then     
            else
               erri = min(erri,i*NL/10 + j*NL/100)
            endif
          enddo 
      enddo 
!dvm$ end region
!dvm$ get_actual(erri,s)
  
      cs = 0              
      do i=1,BN1
          do j=1,BN2
                cs = cs + i*NL/10 + j*NL/100
          enddo 
      enddo 
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B2,A3)

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