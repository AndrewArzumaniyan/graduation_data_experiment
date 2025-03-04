      program PARALLEL1

c    TESTING parallel CLAUSE .       

      print *,'===START OF parallel1========================'
C --------------------------------------------------
c 11  arrA1[BLOCK]  PARALLEL ON arrA[i+4] normal
      call parallel11
C --------------------------------------------------
c 12  arrA1[BLOCK]  PARALLEL ON arrA[-i+8]  reverse
c      call parallel12
C --------------------------------------------------
c 13  arrA1[BLOCK]  PARALLEL ON arrA[2*i+8] stretch
      call parallel13
C --------------------------------------------------
c 131 arrA1[BLOCK]  PARALLEL ON arrA[2*i+1] small array
      call parallel13
C --------------------------------------------------
c 14  arrA1[BLOCK]  PARALLEL ON arrA[] 
      call parallel14
C --------------------------------------------------
c 15  arrA1[BLOCK]  PARALLEL ON arrA[2] 
      call parallel15
C --------------------------------------------------
      print *,'=== END OF parallel1 ========================= '    
      end

C ----------------------------------------------------parallel11
c 11  arrA1[BLOCK]  PARALLEL ON arrA[i+4] normal
      subroutine parallel11
      integer, parameter :: AN1=8,NL=1000,ER=10000
c     parameters for PARALLEL arrA1[k1i * i + li]                                                 
      integer, parameter :: k1i=1,k2i=0,li=4      
      character*9 tname
      integer, allocatable :: A1(:)
      integer erri,i
               
!dvm$ distribute A1(BLOCK)    

      tname='paral11'
      allocate (A1(AN1))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A1)
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) =i     
      enddo

!dvm$ parallel (i) on A1(k1i * i + li), reduction( min( erri ) )
!dvm$*, private(ia)
      do i=1,((AN1-li)/k1i)
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
      deallocate (A1)

      end
C ----------------------------------------------------parallel12
c 12  arrA1[BLOCK]  PARALLEL ON arrA[-i+8]  reverse
      subroutine parallel12
      integer, parameter :: AN1=8,NL=1000,ER=10000
c     parameters for PARALLEL arrA1[k1i * i + li]                                                 
      integer, parameter :: k1i=-1,k2i=0,li=9      
      character*9 tname
      integer, allocatable :: A1(:)
      integer erri,i
               
!dvm$ distribute A1(BLOCK)   

      tname='paral12'
      allocate (A1(AN1))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A1)
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) =i     
      enddo

!dvm$ parallel (i) on A1(k1i * i + li), reduction( min( erri ) )
!dvm$*, private(ia)
      do i=1,AN1
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
      deallocate (A1)

      end
C ----------------------------------------------------parallel13
c 13  arrA1[BLOCK]  PARALLEL ON arrA[2*i+8] stretch
      subroutine parallel13
      integer, parameter :: AN1=20,NL=1000,ER=10000
c     parameters for PARALLEL arrA1[k1i * i + li]                                                 
      integer, parameter :: k1i=2,k2i=0,li=8      
      character*9 tname
      integer, allocatable :: A1(:)
      integer erri,i
               
!dvm$ distribute A1(BLOCK)   

      tname='paral13'
      allocate (A1(AN1))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A1)
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) =i     
      enddo

!dvm$ parallel (i) on A1(k1i * i + li), reduction( min( erri ) )
!dvm$*, private(ia)
      do i=1,((AN1-li)/k1i)
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
      deallocate (A1)

      end
C ----------------------------------------------------parallel131
c 131  arrA1[BLOCK]  PARALLEL ON arrA[2*i+1] small array
      subroutine parallel131
      integer, parameter :: AN1=5,NL=1000,ER=10000
c     parameters for PARALLEL arrA1[k1i * i + li]                                                 
      integer, parameter :: k1i=2,k2i=0,li=1      
      character*9 tname
      integer, allocatable :: A1(:)
      integer erri,i
               
!dvm$ distribute A1(BLOCK)   

      tname='paral131'
      allocate (A1(AN1))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A1)
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) =i     
      enddo

!dvm$ parallel (i) on A1(k1i * i + li), reduction( min( erri ) )
!dvm$*, private(ia)
      do i=1,((AN1-li)/k1i)
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
      deallocate (A1)

      end
C ----------------------------------------------------parallel14
c 14  arrA1[BLOCK]  PARALLEL ON arrA[] 
      subroutine parallel14
      integer, parameter :: AN1=20,BN1=10,NL=1000,ER=10000
c     parameters for PARALLEL arrA1[*]                                                 
      integer, parameter :: k1i=0,k2i=0,li=0
      character*9 tname
      integer, allocatable :: A1(:),B1(:)
      integer erri,i
               
!dvm$ distribute A1(BLOCK)   
!dvm$ distribute B1(*)

      tname='paral14'
      allocate (A1(AN1),B1(BN1))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A1,B1)
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) =i     
      enddo

!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) =i     
      enddo

!dvm$ parallel (i) on A1(*), reduction( min( erri ) )
      do i=1,BN1
            if (B1(i) .eq.(i)) then     
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
      deallocate (A1,B1)

      end
C ----------------------------------------------------parallel15
c 15  arrA1[BLOCK]  PARALLEL ON arrA[2] 
      subroutine parallel15
      integer, parameter :: AN1=20,NL=1000,ER=10000
c     parameters for PARALLEL arrA1[li]                                                 
      integer, parameter :: k1i=0,k2i=0,li=2
      character*9 tname
      integer, allocatable :: A1(:)
      integer erri,i
               
!dvm$ distribute A1(BLOCK)   

      tname='paral15'
      allocate (A1(AN1))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A1)
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) =i     
      enddo

!dvm$ parallel (i) on A1(li), reduction( min( erri ) )
!dvm$*, private(ia)
      do i=1,AN1
            ia=li
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
      deallocate (A1)

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
