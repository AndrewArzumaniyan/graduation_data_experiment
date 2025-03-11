      program ALIGNPLUS33

c    TESTING align CLAUSE .       

      print *, '====START OF alignplus33================'
c --------------------------------------------------
c 331 arrA3[*][BLOCK] [BLOCK]  arrB3[][][]
c       ALIGN arrB[i][j][k] WITH arrA[i][ j][k] normal      
       call align331
C --------------------------------------------------
c 332 arrA3[*][BLOCK] [BLOCK]  arrB3[][][]
c       ALIGN arrB[*][i][*] WITH arrA[*][ 3][i]       
       call align332
c --------------------------------------------------
c 333 arrA3[BLOCK][*] [BLOCK]  arrB3[][][]
c       ALIGN arrB[i][j][k] WITH arrA[i+4][2*j+1][3*k+1]       
C       call align333
C --------------------------------------------------
c 334 arrA3[BLOCK][BLOCK] [*]  arrB3[][][]
c       ALIGN arrB[*][i][*] WITH arrA[*][ 7][2*i-1]       
       call align334
C --------------------------------------------------
c 335 arrA3[BLOCK][*] [BLOCK]  arrB3[][][]
c       ALIGN arrB[*][i][*] WITH arrA[*][ 1][i]       
       call align335
C -------------------------------------------------
      print *, '==== END OF alignplus33 ================'
C
      end

C ----------------------------------------------------align331
c 331 arrA3[*][BLOCK] [BLOCK]  arrB3[][][]
c       ALIGN arrB[i][j][n] WITH arrA[i][ j][n] normal      

      subroutine align331
      integer, parameter :: AN1=5,AN2=5,AN3=5,BN1=2,BN2=2,BN3=2
      integer, parameter :: PN=2,NL=10000,ER=100000
c     parameters for ALIGN arrB[i][j][n] WITH arrA[k1i*i+li][k2j*j+lj][k3n*n+ln]                                               
      integer, parameter :: k1i=1,k2i=0,k3i=0,li=0
      integer, parameter :: k1j=0,k2j=1,k3j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=1,ln=0
      character*9 tname
      integer, allocatable :: A3(:,:,:),B3(:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
cdvm$ distribute A3(*,BLOCK,BLOCK)    
cdvm$ ALIGN B3(i,j,n) WITH A3(k1i*i+li,k2j*j+lj,k3n*n+ln)

      tname='align331'
      allocate (A3(AN1,AN2,AN3),B3(BN1,BN2,BN3))
      erri= ER
      NNL=NL 
      s=0
      m=-1

!dvm$ actual(erri,s)
!dvm$ region local(A3,B3)
*dvm$ parallel (i,j,n) on B3(i,j,n)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                    B3(i,j,n) =0     
             enddo 
          enddo 
      enddo 

*dvm$ parallel (i,j,n) on A3(i,j,n), private(ib,jb,nb)
      do i=1,AN1
         do j=1,AN2
            do n=1,AN3
                A3(i,j,n) = i*NL/10+j*NL/100+n*NL/1000+m
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *                  ((n-ln) .eq.(((n-ln)/k3n) * k3n)) .and.
     *                  (((i-li)/k1i) .gt. 0)  .and.
     *                  (((j-lj)/k2j) .gt. 0)  .and.
     *                  (((n-ln)/k3n) .gt. 0)  .and.
     *                  (((i-li)/k1i) .le. BN1)  .and.
     *                  (((j-lj)/k2j) .le. BN2)  .and.
     *                  (((n-ln)/k3n) .le. BN3)  
     *                  )  then 
                        ib = (i-li)/k1i
                        jb = (j-lj)/k2j
                        nb = (n-ln)/k3n
                        B3(ib,jb,nb)=ib*NL/10+jb*NL/100+nb*NL/1000 
                      endif 
            enddo 
          enddo 
      enddo 

*dvm$ parallel (i,j,n) on B3(i,j,n), reduction( min( erri ),sum(s) )
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                  s = s + B3(i,j,n)
                  if (B3(i,j,n).eq.(i*NL/10+j*NL/100+n*NL/1000))then     
                  else
                      erri = min(erri,i*NL/10 + j*NL/100+ n*NL/1000)
                  endif
            enddo
          enddo 
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri,s) 
  
      cs = 0              
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                    cs = cs + i*NL/10 + j*NL/100+ n*NL/1000
            enddo
          enddo
      enddo
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
c           write (*,*) erri,s,cs
c          print *,B3  
      endif 
      deallocate (B3,A3)

      end

C ----------------------------------------------------align332
c 332 arrA3[*][BLOCK] [BLOCK]  arrB3[][][]
c       ALIGN arrB[*][i][*] WITH arrA[*][ 3][i] normal      

      subroutine align332
      integer, parameter :: AN1=4,AN2=4,AN3=4,BN1=2,BN2=2,BN3=2
      integer, parameter :: PN=2,NL=10000,ER=100000
c     parameters for ALIGN arrB[*][i][*] WITH arrA[*][lj][k1n*i+ln]                                               
      integer, parameter :: k1i=0,k2i=0,k3i=0,li=0
      integer, parameter :: k1j=0,k2j=0,k3j=0,lj=3
      integer, parameter :: k1n=1,k2n=0,k3n=0,ln=0
      character*9 tname
      integer, allocatable :: A3(:,:,:),B3(:,:,:)
      integer s,cs,erri,i,j,n,m,k,l,ia,ja,na,ma,ib,jb,nb,mb,
     * Avalue,Bvalue
               
cdvm$ distribute A3(*,BLOCK,BLOCK)    
cdvm$ ALIGN B3(*,i,*) WITH A3(*,lj,k1n*i+ln)

      tname='align332'
      allocate (A3(AN1,AN2,AN3),B3(BN1,BN2,BN3))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A3,B3)
*dvm$ parallel (i,j,n) on B3(i,j,n)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                    B3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j,n) on A3(i,j,n), private(k,l,ib,jb,nb),
!dvm$& reduction(min(erri))
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                A3(i,j,n) = i*NL/10+j*NL/100+n*NL/1000
                  if ((j .eq. lj ) ) then
                    do k = 1,BN1
                    do l = 1,BN3
                      if ( 
     *                  ((n-ln) .eq.(((n-ln)/k1n) * k1n)) .and.
     *                  (((n-ln)/k1n) .gt. 0)  .and.
     *                  (((n-ln)/k1n) .le. BN2)  
     *                  )  then 
                        ib = k
                        jb = ((n-ln)/k1n)
                        nb = l
                        if (B3(ib,jb,nb).eq.
     *                      (ib*NL/10+jb*NL/100+nb*NL/1000))then     
                        else
                        erri = min(erri,i*NL/10 + j*NL/100+ n*NL/1000)
                        endif
                      endif 
                   enddo 
                   enddo 
                 endif
            enddo 
          enddo 
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri) 

      cs=0  
      s=0 
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
c           write (*,*) erri,s,cs
c          print *,B3  
      endif 
      deallocate (B3,A3)

      end

C ----------------------------------------------------align333
c 333 arrA3[BLOCK][*] [BLOCK]  arrB3[][][]
c       ALIGN arrB[i][j][k] WITH arrA[i+4][2*j+1][3*k+1]       

      subroutine align333
      integer, parameter :: AN1=8,AN2=8,AN3=13,BN1=4,BN2=3,BN3=4
      integer, parameter :: PN=2,NL=10000,ER=100000
c     parameters for ALIGN arrB[i][j][n] WITH arrA[k1i*i+li][k2j*j+lj][k3n*n+ln]                                               
      integer, parameter :: k1i=1,k2i=0,k3i=0,li=4
      integer, parameter :: k1j=0,k2j=2,k3j=0,lj=1
      integer, parameter :: k1n=0,k2n=0,k3n=3,ln=1
      character*9 tname
      integer, allocatable :: A3(:,:,:),B3(:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
cdvm$ distribute A3(BLOCK,*,BLOCK)    
cdvm$ ALIGN B3(i,j,n) WITH A3(k1i*i+li,k2j*j+lj,k3n*n+ln)

      tname='align333'
      allocate (A3(AN1,AN2,AN3),B3(BN1,BN2,BN3))
      erri= ER
      NNL=NL 
      s=0 

!dvm$ actual(erri,s)
!dvm$ region local(A3,B3)
*dvm$ parallel (i,j,n) on B3(i,j,n)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                    B3(i,j,n) =0     
             enddo 
          enddo 
      enddo 

*dvm$ parallel (i,j,n) on A3(i,j,n), private(ib,jb,nb)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                A3(i,j,n) = i*NL/10+j*NL/100+n*NL/1000+m
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *                  ((n-ln) .eq.(((n-ln)/k3n) * k3n)) .and.
     *                  (((i-li)/k1i) .gt. 0)  .and.
     *                  (((j-lj)/k2j) .gt. 0)  .and.
     *                  (((n-ln)/k3n) .gt. 0)  .and.
     *                  (((i-li)/k1i) .le. BN1)  .and.
     *                  (((j-lj)/k2j) .le. BN2)  .and.
     *                  (((n-ln)/k3n) .le. BN3)  
     *                  )  then 
                        ib = (i-li)/k1i
                        jb = (j-lj)/k2j
                        nb = (n-ln)/k3n
                        B3(ib,jb,nb)=ib*NL/10+jb*NL/100+nb*NL/1000 
                      endif 
            enddo 
          enddo 
      enddo 

*dvm$ parallel (i,j,n) on B3(i,j,n), reduction( min( erri ),sum(s) )
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                  s = s + B3(i,j,n)
                  if (B3(i,j,n).eq.(i*NL/10+j*NL/100+n*NL/1000))then     
                  else
                      erri = min(erri,i*NL/10 + j*NL/100+ n*NL/1000)
                  endif
            enddo
          enddo
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri,s) 
  
      cs = 0              
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                    cs = cs + i*NL/10 + j*NL/100+ n*NL/1000
            enddo
          enddo
      enddo
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
c           write (*,*) erri,s,cs
c          print *,B3  
      endif 
      deallocate (B3,A3)

      end

C ----------------------------------------------------align334
c 334 arrA3[BLOCK][BLOCK] [*]  arrB3[][][]
c       ALIGN arrB[*][i][*] WITH arrA[*][ 7][2*i-1]       

      subroutine align334
      integer, parameter :: AN1=5,AN2=7,AN3=9,BN1=4,BN2=3,BN3=5
      integer, parameter :: PN=2,NL=10000,ER=100000
c     parameters for ALIGN arrB[*][i][*] WITH arrA[*][lj][k1n*i+ln]                                               
      integer, parameter :: k1i=0,k2i=0,k3i=0,li=0
      integer, parameter :: k1j=0,k2j=0,k3j=0,lj=7
      integer, parameter :: k1n=2,k2n=0,k3n=0,ln=-1
      character*9 tname
      integer, allocatable :: A3(:,:,:),B3(:,:,:)
      integer s,cs,erri,i,j,n,m,k,l,ia,ja,na,ma,ib,jb,nb,mb,
     * Avalue,Bvalue
               
cdvm$ distribute A3(BLOCK,BLOCK,*)    
cdvm$ ALIGN B3(*,i,*) WITH A3(*,lj,k1n*i+ln)

      tname='align334'
      allocate (A3(AN1,AN2,AN3),B3(BN1,BN2,BN3))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A3,B3)
*dvm$ parallel (i,j,n) on B3(i,j,n)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                    B3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j,n) on A3(i,j,n), private(k,l,ib,jb,nb),
!dvm$& reduction(min(erri))
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                A3(i,j,n) = i*NL/10+j*NL/100+n*NL/1000
                  if ((j .eq. lj ) ) then
                    do k = 1,BN1
                    do l = 1,BN3
                      if ( 
     *                  ((n-ln) .eq.(((n-ln)/k1n) * k1n)) .and.
     *                  (((n-ln)/k1n) .gt. 0)  .and.
     *                  (((n-ln)/k1n) .le. BN2)  
     *                  )  then 
                        ib = k
                        jb = ((n-ln)/k1n)
                        nb = l
                        if (B3(ib,jb,nb).eq.
     *                      (ib*NL/10+jb*NL/100+nb*NL/1000))then     
                        else
                        erri = min(erri,i*NL/10 + j*NL/100+ n*NL/1000)
                        endif
                      endif 
                   enddo 
                   enddo 
                 endif
            enddo 
          enddo 
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri) 

      cs=0  
      s=0 
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
c           write (*,*) erri,s,cs
c          print *,B3  
      endif 
      deallocate (B3,A3)

      end

C ----------------------------------------------------align335
c 335 arrA3[BLOCK][*] [BLOCK]  arrB3[][][]
c       ALIGN arrB[*][i][*] WITH arrA[*][ 1][i]       

      subroutine align335
      integer, parameter :: AN1=5,AN2=7,AN3=9,BN1=4,BN2=3,BN3=5
      integer, parameter :: PN=2,NL=10000,ER=100000
c     parameters for ALIGN arrB[*][i][*] WITH arrA[*][lj][k1n*i+ln]                                               
      integer, parameter :: k1i=0,k2i=0,k3i=0,li=0
      integer, parameter :: k1j=0,k2j=0,k3j=0,lj=1
      integer, parameter :: k1n=1,k2n=0,k3n=0,ln=0
      character*9 tname
      integer, allocatable :: A3(:,:,:),B3(:,:,:)
      integer s,cs,erri,i,j,n,m,k,l,ia,ja,na,ma,ib,jb,nb,mb,
     * Avalue,Bvalue
              
cdvm$ distribute A3(BLOCK,*,BLOCK)    
cdvm$ ALIGN B3(*,i,*) WITH A3(*,lj,k1n*i+ln)

      tname='align335'
      allocate (A3(AN1,AN2,AN3),B3(BN1,BN2,BN3))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A3,B3)
*dvm$ parallel (i,j,n) on B3(i,j,n)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                    B3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j,n) on A3(i,j,n), private(k,l,ib,jb,nb),
!dvm$& reduction(min(erri))
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                A3(i,j,n) = i*NL/10+j*NL/100+n*NL/1000
                  if ((j .eq. lj ) ) then
                    do k = 1,BN1
                    do l = 1,BN3
                      if ( 
     *                  ((n-ln) .eq.(((n-ln)/k1n) * k1n)) .and.
     *                  (((n-ln)/k1n) .gt. 0)  .and.
     *                  (((n-ln)/k1n) .le. BN2)  
     *                  )  then 
                        ib = k
                        jb = ((n-ln)/k1n)
                        nb = l
                        if (B3(ib,jb,nb).eq.
     *                      (ib*NL/10+jb*NL/100+nb*NL/1000))then     
                        else
                        erri = min(erri,i*NL/10 + j*NL/100+ n*NL/1000)
                        endif
                      endif 
                   enddo 
                   enddo 
                 endif
            enddo 
          enddo 
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri) 

      cs=0  
      s=0 
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
c           write (*,*) erri,s,cs
c          print *,B3  
      endif 
      deallocate (B3,A3)

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