      program ALIGNPLUS21

c    TESTING align CLAUSE .       
c    arrA2[*][ BLOCK] arrB1[]
c    or arrA2[ BLOCK][*] arrB1[]
      print *, '===START OF alignplus21=================='
C --------------------------------------------------
c 211       ALIGN arrB[i] WITH arrA[1][i]   vector arrB on section 
*                                                 (the first line of arrA)
      call align211
C -------------------------------------------------
c 212     ALIGN arrB[i] WITH arrA[2*i+2][2] vector arrB on section 
*                                       (the second column of arrA) with stretching and shift
      call align212
C -------------------------------------------------
c 213     ALIGN arrB[i] WITH arrA[][i]  vector replication on every line of arrA
      call align213
C -------------------------------------------------
c 214     ALIGN arrB[i] WITH arrA[2*i+2][ ] vector arrB on replication on 
*                                          every column of arrA with stretching and shift
      call align214
C --------------------------------------------------
c 215       ALIGN arrB[i] WITH arrA[1][i]   vector arrB on section 
*                                                 (the first line of arrA)
      call align215
C -------------------------------------------------
c 216     ALIGN arrB[i] WITH arrA[2*i+2][2] vector arrB on section 
*                                       (the second column of arrA) with stretching and shift
      call align216
C -------------------------------------------------
c 217     ALIGN arrB[i] WITH arrA[][i]  vector replication on every line of arrA
      call align217
C -------------------------------------------------
c 218     ALIGN arrB[i] WITH arrA[2*i+2][ ] vector arrB on replication on 
*                                          every column of arrA with stretching and shift
      call align218
C -------------------------------------------------
      print *, '=== END OF alignplus21 =================='
C
C
      end

C ----------------------------------------------------align211
c 211 arrA2[*][ BLOCK]  arrB1[] ALIGN arrB[i] WITH arrA[1][i]vector arrB on section 
*                                                                   (the first line of arrA)
      subroutine align211
      integer, parameter :: AN1=8,AN2=8,BN1=4,NL=1000,ER=10000
c     parameters for ALIGN arrB[i] WITH arrA(1,i)                                                
      character*9 tname
      integer, allocatable :: A2(:,:), B1(:)
      integer erri,i,j,ia,ja,ib,jb
               
!dvm$ distribute A2(*,BLOCK)    
!dvm$ ALIGN B1(i) WITH A2(1,i)

      tname='align211'
      allocate (A2(AN1,AN2),B1(BN1))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2,B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) =0     
      enddo 

!dvm$ parallel (i,j) on A2(i,j), private(ib)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             if ((i .eq. 1) ) then
                      if ( 
     *                  (j .le. BN1)  
     *                  )  then 
                        ib = j
                        B1(ib) = ib
                      endif 
             endif   
          enddo 
      enddo 

!dvm$ parallel (i) on B1(i), reduction( min( erri ) ), private(ia,ja)
      do i=1,BN1
            if (B1(i) .eq.(i)) then     
            else
               erri = min(erri,i)
            endif
            ia=1
            ja=i
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
                erri = i*NL/10+j
            endif 
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri) 
     
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B1,A2)

      end
C ----------------------------------------------------align212
c 212     ALIGN arrB[i] WITH arrA[2*i+2][2] vector arrB on section 
*                                       (the second column of arrA) with stretching and shift
      subroutine align212
      integer, parameter :: AN1=14,AN2=3,BN1=6,NL=1000,ER=10000
c     parameters for ALIGN arrB[i] WITH arrA(k1i*i+li,lj)                                                
      integer, parameter :: k1i=2,k2i=0,li=2,k1j=0,k2j=0,lj=2
      character*9 tname
      integer, allocatable :: A2(:,:), B1(:)
      integer :: erri, i
               
!dvm$ distribute A2(*,BLOCK)    
!dvm$ ALIGN B1(i) WITH A2(k1i*i+li,lj)

      tname='align212'
      allocate (A2(AN1,AN2),B1(BN1))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2,B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) =0     
      enddo 

!dvm$ parallel (i,j) on A2(i,j), private(ib)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             if ((j .eq. lj) .and. 
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  (((i-li)/k1i) .gt. 0)  .and.
     *                  (((i-li)/k1i) .le. BN1)  
     *                  )  then 
               ib = (i-li)/k1i
               B1(ib) = ib
             endif   
          enddo 
      enddo 

!dvm$ parallel (i) on B1(i), reduction( min( erri ) ), private(ia,ja)
      do i=1,BN1
            if (B1(i) .eq.(i)) then     
            else
               erri = min(erri,i)
            endif
            ia=k1i*i+li
            ja=lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
                erri = min(erri,i*NL/10+j)
            endif 
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri) 
     
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B1,A2)

      end
C ----------------------------------------------------align213
c 213     ALIGN arrB[i] WITH arrA[][i]  vector replication on every line of arrA
      subroutine align213
      integer, parameter :: AN1=8,AN2=8,BN1=6,NL=1000,ER=10000
c     parameters for ALIGN arrB[i] WITH arrA[][k1j * i + lj]                                                
      integer, parameter :: k1i=0,k2i=0,li=0,k1j=1,k2j=0,lj=0
      character*9 tname
      integer, allocatable :: A2(:,:), B1(:)
      integer s,cs,erri,i,j,ia,ja,ib,jb
               
!dvm$ distribute A2(*,BLOCK)    
!dvm$ ALIGN B1(i) WITH A2(*,k1j * i + lj)

      tname='align213'
      allocate (A2(AN1,AN2),B1(BN1))
      erri= ER
      NNL=NL 
      s=0

!dvm$ actual(erri,s)
!dvm$ region local(A2,B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) =i     
      enddo 

!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) ), private(ib)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             if (
     *          ((j-lj) .eq.(((j-lj)/k1j) *k1j)) .and.
     *          (((j-lj)/k1j) .gt. 0)  .and.
     *          (((j-lj)/k1j) .le. BN1)  )then
                ib = (j-lj)/k1j
                if (B1(ib) .eq.(ib)) then     
                else
                    erri = min(erri,ib)
                endif
             endif   
          enddo 
      enddo 

!dvm$ parallel (i) on B1(i), reduction( min( erri ),sum(s) )
      do i=1,BN1
            s = s + B1(i)
            if (B1(i) .eq.(i)) then     
            else
               erri = min(erri,i)
            endif
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri,s) 
  
      cs = ((1 + BN1)* BN1/ 2)
c      write (*,*) erri,s,cs
  
      if ((erri .eq.ER) .and.
     *     (s .eq.cs )) then     
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (B1,A2)

      end
C ----------------------------------------------------align214
c 214     ALIGN arrB[i] WITH arrA[2*i+2][ ] vector arrB on replication on 
*                                          every column of arrA with stretching and shift
      subroutine align214
      integer, parameter :: AN1=28,AN2=8,BN1=5,NL=1000,ER=10000
c     parameters for ALIGN arrB[i] WITH arrA(k1i*i+li,*)                                                
      integer, parameter :: k1i=2,k2i=0,li=2,k1j=0,k2j=0,lj=0
      character*9 tname
      integer, allocatable :: A2(:,:), B1(:)
      integer s,erri,i,j,ia,ja,ib,jb
               
!dvm$ distribute A2(*,BLOCK)    
!dvm$ ALIGN B1(i) WITH A2(k1i*i+li,*)

      tname='align214'
      allocate (A2(AN1,AN2),B1(BN1))
      erri= ER
      NNL=NL 
      s=0  

!dvm$ actual(erri,s)
!dvm$ region local(A2,B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) =i
      enddo 

!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri )), private(ib)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             if (  
     *          ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)  )then
                ib = (i-li)/k1i
                if (B1(ib) .eq.(ib)) then     
                else
                    erri = min(erri,ib)
                endif
             endif   
          enddo 
      enddo 

!dvm$ parallel (i) on B1(i), reduction( min( erri ),sum(s) )
      do i=1,BN1
            s = s + B1(i)
            if (B1(i) .eq.(i)) then     
            else
               erri = min(erri,i)
            endif
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri,s) 

      if ((erri .eq.ER) .and.
     *     (s .eq. ((1 + BN1)* BN1/ 2))) then     
          call ansyes(tname)
          else
          call ansno(tname)
          write (*,*) erri,s
      endif 
      deallocate(B1,A2)
      end
C ----------------------------------------------------align215
c 215 arrA2[*][ BLOCK]  arrB1[] ALIGN arrB[i] WITH arrA[1][i]vector arrB on section 
*                                                                   (the first line of arrA)
      subroutine align215
      integer, parameter :: AN1=8,AN2=8,BN1=4,NL=1000,ER=10000
c     parameters for ALIGN arrB[i] WITH arrA(1,i)                                                
      character*9 tname
      integer, allocatable :: A2(:,:), B1(:)
      integer erri,i,j,ia,ja,ib,jb
               
!dvm$ distribute A2(BLOCK,*)    
!dvm$ ALIGN B1(i) WITH A2(1,i)

      tname='align215'
      allocate (A2(AN1,AN2),B1(BN1))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2,B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) =0     
      enddo 

!dvm$ parallel (i,j) on A2(i,j), private(ib)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             if ((i .eq. 1) ) then
                      if ( 
     *                  (j .le. BN1)  
     *                  )  then 
                        ib = j
                        B1(ib) = ib
                      endif 
             endif   
          enddo 
      enddo 

!dvm$ parallel (i) on B1(i), reduction( min( erri ) ), private(ia,ja)
      do i=1,BN1
            if (B1(i) .eq.(i)) then     
            else
               erri = min(erri,i)
            endif
            ia=1
            ja=i
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
                erri = min(erri,i*NL/10+j)
            endif 
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri) 
  
      if (erri .eq.ER) then     
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (B1,A2)

      end
C ----------------------------------------------------align216
c 216     ALIGN arrB[i] WITH arrA[2*i+2][2] vector arrB on section 
*                                       (the second column of arrA) with stretching and shift
      subroutine align216
      integer, parameter :: AN1=14,AN2=3,BN1=6,NL=1000,ER=10000
c     parameters for ALIGN arrB[i] WITH arrA(k1i*i+li,lj)                                                
      integer, parameter :: k1i=2,k2i=0,li=2,k1j=0,k2j=0,lj=2
      character*9 tname
      integer, allocatable :: A2(:,:), B1(:)
      integer :: erri, i
               
!dvm$ distribute A2(BLOCK,*)   
!dvm$ ALIGN B1(i) WITH A2(k1i*i+li,lj)

      tname='align216'
      allocate (A2(AN1,AN2),B1(BN1))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2,B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) =0     
      enddo 

!dvm$ parallel (i,j) on A2(i,j), private(ib)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             if ((j .eq. lj) .and. 
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  (((i-li)/k1i) .gt. 0)  .and.
     *                  (((i-li)/k1i) .le. BN1)  
     *                  )  then 
               ib = (i-li)/k1i
               B1(ib) = ib
             endif   
          enddo 
      enddo 

!dvm$ parallel (i) on B1(i), reduction( min( erri ) ), private(ia,ja)
      do i=1,BN1
            if (B1(i) .eq.(i)) then     
            else
               erri = min(erri,i)
            endif
            ia=k1i*i+li
            ja=lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
                erri = min(erri,i*NL/10+j)
            endif 
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri) 
  
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B1,A2)

      end
C ----------------------------------------------------align217
c 217     ALIGN arrB[i] WITH arrA[][i]  vector replication on every line of arrA
      subroutine align217
      integer, parameter :: AN1=8,AN2=8,BN1=6,NL=1000,ER=10000
c     parameters for ALIGN arrB[i] WITH arrA[][k1j * i + lj]                                                
      integer, parameter :: k1i=0,k2i=0,li=0,k1j=1,k2j=0,lj=0
      character*9 tname
      integer, allocatable :: A2(:,:), B1(:)
      integer s,cs,erri,i,j,ia,ja,ib,jb
               
!dvm$ distribute A2(BLOCK,*)    
!dvm$ ALIGN B1(i) WITH A2(*,k1j * i + lj)

      tname='align217'
      allocate (A2(AN1,AN2),B1(BN1))
      erri= ER
      NNL=NL 
      s=0

!dvm$ actual(erri,s)
!dvm$ region local(A2,B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) =i     
      enddo 

!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) ), private(ib)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             if (
     *          ((j-lj) .eq.(((j-lj)/k1j) *k1j)) .and.
     *          (((j-lj)/k1j) .gt. 0)  .and.
     *          (((j-lj)/k1j) .le. BN1)  )then
                ib = (j-lj)/k1j
                if (B1(ib) .eq.(ib)) then     
                else
                    erri = min(erri,ib)
                endif
             endif   
          enddo 
      enddo 

!dvm$ parallel (i) on B1(i), reduction( min( erri ),sum(s) )
      do i=1,BN1
            s = s + B1(i)
            if (B1(i) .eq.(i)) then     
            else
               erri = min(erri,i)
            endif
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri,s) 
  
      cs = ((1 + BN1)* BN1/ 2)
      if ((erri .eq.ER) .and.
     *     (s .eq.cs )) then     
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (B1,A2)

      end
C ----------------------------------------------------align218
c 218     ALIGN arrB[i] WITH arrA[2*i+2][ ] vector arrB on replication on 
*                                          every column of arrA with stretching and shift
      subroutine align218
      integer, parameter :: AN1=28,AN2=8,BN1=5,NL=1000,ER=10000
c     parameters for ALIGN arrB[i] WITH arrA(k1i*i+li,*)                                                
      integer, parameter :: k1i=2,k2i=0,li=2,k1j=0,k2j=0,lj=0
      character*9 tname
      integer, allocatable :: A2(:,:), B1(:)
      integer s,erri,i,j,ia,ja,ib,jb
               
!dvm$ distribute A2(BLOCK,*)    
!dvm$ ALIGN B1(i) WITH A2(k1i*i+li,*)

      tname='align218'
      allocate (A2(AN1,AN2),B1(BN1))
      erri= ER
      NNL=NL 
      s=0  

!dvm$ actual(erri,s)
!dvm$ region local(A2,B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) =i
      enddo 

!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri )), private(ib)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             if (  
     *          ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)  )then
                ib = (i-li)/k1i
                if (B1(ib) .eq.(ib)) then     
                else
                    erri = min(erri,ib)
                endif
             endif   
          enddo 
      enddo 

!dvm$ parallel (i) on B1(i), reduction( min( erri ),sum(s) )
      do i=1,BN1
            s = s + B1(i)
            if (B1(i) .eq.(i)) then     
            else
               erri = min(erri,i)
            endif
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri,s) 

      if ((erri .eq.ER) .and.
     *     (s .eq. ((1 + BN1)* BN1/ 2))) then     
          call ansyes(tname)
          else
          call ansno(tname)
          write (*,*) erri,s
      endif 
      deallocate (B1,A2)

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