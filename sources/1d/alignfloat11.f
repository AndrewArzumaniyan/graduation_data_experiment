      program ALIGNFLOAT11

c    TESTING align CLAUSE .       

      print *,'===START OF alignfloat11========================'
C --------------------------------------------------
c 111 arrA1[BLOCK]  arrB1[ ]  ALIGN arrB[i] WITH arrA[i]  normal 
      call align111
C --------------------------------------------------
c 1111  arrA1[BLOCK]  arrB1[ ]  ALIGN arrB[i] WITH arrA[i]  small array 
      call align1111
C --------------------------------------------------
c 1112  arrA1[BLOCK]  arrB1[ ]  ALIGN arrB[i] WITH arrA[2*i+1]  small array 
      call align1112
C --------------------------------------------------
c 112                         ALIGN arrB[i] WITH arrA[i+4]  shift along i 
      call align112
C --------------------------------------------------
c 113                         ALIGN arrB[i] WITH arrA[-i+9] reverse on i
c      call align113
C --------------------------------------------------
c 114                         ALIGN arrB[i] WITH arrA[2*i+8]  stretching along i
      call align114
C --------------------------------------------------
c 115                             ALIGN arrB[*] WITH arrA[*]   
      call align115
C --------------------------------------------------
C
C
      print *,'=== END OF alignfloat11 ========================= '    
      end

C ----------------------------------------------------align111
c 111 arrA1[BLOCK]  arrB1[ ]  ALIGN arrB[i] WITH arrA[i]  normal 
      subroutine align111
      integer, parameter :: AN1=8,BN1=8,NL=1000,ER=10000
c     parameters for ALIGN arrB[i] WITH arrA[k1i * i + li]                                                 
      integer, parameter :: k1i=1,k2i=0,li=0
      character*9 tname
      integer, allocatable :: A1(:)
      real, allocatable :: B1(:)
      integer erri,i
               
!dvm$ distribute A1(BLOCK)    
!dvm$ ALIGN B1(i) WITH A1(k1i * i + li)

      tname='align111'
      allocate (A1(AN1),B1(BN1))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A1,B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) =0     
      enddo 

!dvm$ parallel (i) on A1(i), private(ib)
      do i=1,AN1
             A1(i) = i
             if (((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)) then
                ib = (i-li)/k1i
                B1(ib) = ib
             endif 
      enddo 

!dvm$ parallel (i) on B1(i), reduction( min( erri ) ), private(ia)
      do i=1,BN1
            if (B1(i) .eq.(i)) then     
            else
               erri = min(erri,i)
            endif 
            ia=k1i * i + li
            if (A1(ia) .eq.(ia)) then     
            else
               erri = min(erri,i)
            endif 
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri)   
     
      if (erri .eq.ER) then     
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (B1,A1)

      end
C ----------------------------------------------------align1111
c 1111  arrA1[BLOCK]  arrB1[ ]  ALIGN arrB[i] WITH arrA[i]  small array 
      subroutine align1111
      integer, parameter :: AN1=5,BN1=2,NL=1000,ER=10000
c     parameters for ALIGN arrB[i] WITH arrA[k1i * i + li]                                                 
      integer, parameter :: k1i=1,k2i=0,li=0
      character*9 tname
      real, allocatable :: A1(:)
      integer, allocatable :: B1(:)
      integer erri,i
               
!dvm$ distribute A1(BLOCK)    
!dvm$ ALIGN B1(i) WITH A1(k1i * i + li)

      tname='align1111'
      allocate (A1(AN1),B1(BN1))
      erri= ER
c      call stralign1111 
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A1,B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) =0     
      enddo 

!dvm$ parallel (i) on A1(i), private(ib)
      do i=1,AN1
             A1(i) = i
             if (((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)) then
                ib = (i-li)/k1i
                B1(ib) = ib
             endif 
      enddo 

!dvm$ parallel (i) on B1(i), reduction( min( erri ) ), private(ia)
      do i=1,BN1
            if (B1(i) .eq.(i)) then     
            else
               erri = min(erri,i)
            endif 
            ia=k1i * i + li
            if (A1(ia) .eq.(ia)) then     
            else
               erri = min(erri,i)
            endif 
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri) 
     
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B1,A1)

      end
C ----------------------------------------------------align1112
c 1112  arrA1[BLOCK]  arrB1[ ]  ALIGN arrB[i] WITH arrA[2*i+1]  small array 
      subroutine align1112
      integer, parameter :: AN1=5,BN1=2,NL=1000,ER=10000
c     parameters for ALIGN arrB[i] WITH arrA[k1i * i + li]                                                 
      integer, parameter :: k1i=2,k2i=0,li=1
      character*9 tname
      complex, allocatable :: A1(:)
      integer, allocatable :: B1(:)
      integer erri,i
               
!dvm$ distribute A1(BLOCK)    
!dvm$ ALIGN B1(i) WITH A1(k1i * i + li)

      tname='align1112'
      allocate (A1(AN1),B1(BN1))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A1,B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) =0     
      enddo 

!dvm$ parallel (i) on A1(i), private(ib)
      do i=1,AN1
             A1(i) = i
             if (((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)) then
                ib = (i-li)/k1i
                B1(ib) = ib
             endif 
      enddo 

!dvm$ parallel (i) on B1(i), reduction( min( erri ) ), private(ia)
      do i=1,BN1
            if (B1(i) .eq.(i)) then     
            else
               erri = min(erri,i)
            endif 
            ia=k1i * i + li
            if (A1(ia) .eq.(ia)) then     
            else
               erri = min(erri,i)
            endif 
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri) 
     
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B1,A1)

      end
C ----------------------------------------------------align112
c 112                         ALIGN arrB[i] WITH arrA[i+4]  shift along i 
      subroutine align112
      integer, parameter :: AN1=8,BN1=4,NL=1000,ER=10000
c     parameters for ALIGN arrB[i] WITH arrA[k1i * i + li]                                                 
      integer, parameter :: k1i=1,k2i=0,li=4
      character*9 tname
      real, allocatable :: B1(:)
      complex, allocatable ::  A1(:)
      integer erri,i
               
!dvm$ distribute A1(BLOCK)    
!dvm$ ALIGN B1(i) WITH A1(k1i * i + li)

      tname='align112'
      allocate (A1(AN1),B1(BN1))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A1,B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) =0     
      enddo 

!dvm$ parallel (i) on A1(i), private(ib)
      do i=1,AN1
             A1(i) = i
             if (((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)) then
                ib = (i-li)/k1i
                B1(ib) = ib
             endif 
      enddo 

!dvm$ parallel (i) on B1(i), reduction( min( erri ) ), private(ia)
      do i=1,BN1
            if (B1(i) .eq.(i)) then     
            else
               erri = min(erri,i)
            endif 
            ia=k1i * i + li
            if (A1(ia) .eq.(ia)) then     
            else
               erri = min(erri,i)
            endif 
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri) 
      
      if (erri .eq.ER) then     
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (B1,A1)

      end
C ----------------------------------------------------align113
c 113     ALIGN arrB[i] WITH arrA[-i+9] reverse on i
      subroutine align113
      integer, parameter :: AN1=8,BN1=8,NL=1000,ER=10000
c     parameters for ALIGN arrB[i] WITH arrA[k1i * i + li]                                                 
      integer, parameter :: k1i=-1,k2i=0,li=9
      character*9 tname
      real, allocatable :: A1(:)
      integer, allocatable :: B1(:)
      integer erri,i
               
!dvm$ distribute A1(BLOCK)    
!dvm$ ALIGN B1(i) WITH A1(k1i * i + li)

      tname='align113'
      allocate (A1(AN1),B1(BN1))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A1,B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) =0     
      enddo 

!dvm$ parallel (i) on A1(i), private(ib)
      do i=1,AN1
             A1(i) = i
             if (((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)) then
                ib = (i-li)/k1i
                B1(ib) = ib
             endif 
      enddo 

!dvm$ parallel (i) on B1(i), reduction( min( erri ) ), private(ia)
      do i=1,BN1
            if (B1(i) .eq.(i)) then     
            else
               erri = min(erri,i)
            endif 
            ia=k1i * i + li
            if (A1(ia) .eq.(ia)) then     
            else
               erri = min(erri,i)
            endif 
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri) 
     
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B1,A1)

      end
C ----------------------------------------------------align114
c 114     ALIGN arrB[i] WITH arrA[2*i+8]  stretching along i
      subroutine align114
      integer, parameter :: AN1=24,BN1=8,NL=1000,ER=10000
c     parameters for ALIGN arrB[i] WITH arrA[k1i * i + li]                                                 
      integer, parameter :: k1i=2,k2i=0,li=8
      character*9 tname
      integer, allocatable :: A1(:)
      complex, allocatable :: B1(:)
      integer erri,i
               
!dvm$ distribute A1(BLOCK)    
!dvm$ ALIGN B1(i) WITH A1(k1i * i + li)

      tname='align114'
      allocate (A1(AN1),B1(BN1))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A1,B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) =0     
      enddo 

!dvm$ parallel (i) on A1(i), private(ib)
      do i=1,AN1
             A1(i) = i
             if (((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)) then
                ib = (i-li)/k1i
                B1(ib) = ib
             endif 
      enddo 

!dvm$ parallel (i) on B1(i), reduction( min( erri ) ), private(ia)
      do i=1,BN1
            if (B1(i) .eq.(i)) then     
            else
               erri = min(erri,i)
            endif 
            ia=k1i * i + li
            if (A1(ia) .eq.(ia)) then     
            else
               erri = min(erri,i)
            endif 
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri) 
     
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B1,A1)

      end
C ----------------------------------------------------align115
c 115                             ALIGN arrB[*] WITH arrA[*]   
      subroutine align115
      integer, parameter :: AN1=24,BN1=8,NL=1000,ER=10000
c     parameters for ALIGN arrB[*] WITH arrA[*]                                                 
      integer, parameter :: k1i=0,k2i=0,li=0
      character*9 tname
      integer, allocatable :: A1(:)
      real, allocatable :: B1(:)
      integer erri,i
               
!dvm$ distribute A1(BLOCK)    
!dvm$ ALIGN B1(*) WITH A1(*)

      tname='align115'
      allocate (A1(AN1),B1(BN1))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A1,B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) =i     
      enddo 

!dvm$ parallel (i) on A1(i), reduction(min(erri)), private(j)
      do i=1,AN1
        do j=1,BN1
            if (B1(j) .eq.(j)) then     
            else
               erri = min(erri,j)
            endif 
        enddo 
      enddo 
!dvm$ end region   
!dvm$ get_actual(erri) 

      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B1,A1)

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