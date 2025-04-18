      program REALIGN11

c    Testing REALIGN directive       

      print *,'===START OF realign11========================'
C --------------------------------------------------
C 111 ALIGN arrB(i) WITH arrA(i)  REALIGN arrB(i) WITH arrA(2*i+8) 
       call realign111
C --------------------------------------------------
C 112 ALIGN arrB(i) WITH arrA(i+4)  REALIGN arrB(i) WITH arrA(i+8) 
       call realign112
C --------------------------------------------------
C 112r ALIGN arrB(i) WITH arrA(i+4)  REALIGN arrB(i) WITH arrA(-i+8) 
c      call realign112r
C --------------------------------------------------
C 113  ALIGN arrB(i) WITH arrA(3*i-2) REALIGN arrB(i) WITH arrA(2*i+1)  
       call realign113
C --------------------------------------------------
C 113r ALIGN arrB(i) WITH arrA(-i+8) REALIGN arrB(i) WITH arrA(3*i-2) 
c       call realign113r
C --------------------------------------------------
C 114  ALIGN arrB(i) WITH arrA(2*i+8)  REALIGN arrB(i) WITH arrA(i) 
       call realign114
C --------------------------------------------------
C 115  ALIGN arrB(*) WITH arrA(*)  REALIGN arrB(i) WITH arrA(i+4) 
       call realign115
C --------------------------------------------------
C 116  ALIGN arrB(i) WITH arrA(4*i-3)  REALIGN arrB(i) WITH arrA(*) 
       call realign116
C --------------------------------------------------
C
      print *,'=== END OF realign11 ========================= '    
      end

C ----------------------------------------------------realign111
 
C 111 ALIGN arrB(i) WITH arrA(i)  REALIGN arrB(i) WITH arrA(2*i+8) 
      subroutine realign111
      integer, parameter ::  AN1=25,BN1=8,NL=1000,ER=10000
      integer ::   erria = ER, errib = ER
c     parameters for ALIGN                                                  
      integer, parameter ::  k1i=1,li=0
c     parameters for REALIGN                                                 
      integer, parameter ::  kr1i=2,lri=8
      integer, allocatable :: A1(:),B1(:)
      character(*), parameter :: tname = 'realign111'
               
!dvm$ distribute A1(BLOCK)    
!dvm$ ALIGN B1(i) WITH A1(k1i * i + li)
!dvm$ DYNAMIC B1

      allocate (A1(AN1),B1(BN1))

!dvm$ region  out(A1,B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) = 0     
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
!dvm$ end region   

!dvm$ REALIGN B1(i) WITH A1(kr1i * i + lri)

!dvm$ actual(erria, errib)

!dvm$ region inlocal(A1,B1)
!dvm$ parallel (i) on B1(i), private(ia),
!dvm$*                reduction(min(erria),min(errib))
      do i=1,BN1
            if (B1(i) /= i) then     
               errib = min(errib,i)
            endif 
            ia=kr1i * i + lri
            if (A1(ia) /= ia) then     
               erria = min(erria,i)
            endif 
      enddo
!dvm$ end region   

!dvm$ get_actual(erria,errib) 

      if ((erria == ER) .and. (errib == ER)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (B1,A1)

      end subroutine realign111
C ----------------------------------------------------realign112

C 112 ALIGN arrB(i) WITH arrA(i+4)  REALIGN arrB(i) WITH arrA(i+8)   
      subroutine realign112
      integer, parameter ::  AN1=16,BN1=4,NL=1000,ER=10000
      integer ::   erria = ER, errib = ER
c     parameters for ALIGN                                                 
      integer, parameter ::  k1i=1,li=4
c     parameters for REALIGN                                                 
      integer, parameter ::  kr1i=1,lri=8
      integer, allocatable :: A1(:),B1(:)
      character(*), parameter :: tname = 'realign112'
               
!dvm$ distribute A1(BLOCK)    
!dvm$ ALIGN B1(i) WITH A1(k1i * i + li)
!dvm$ DYNAMIC B1

      allocate (A1(AN1),B1(BN1))

      B1 = 1     

!dvm$ actual (B1)

!dvm$ region inout (B1), out(A1)
!dvm$ parallel (i) on A1(i), private(ib)
      do i=1,AN1
             A1(i) = i * 2
             if (((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)) then
                ib = (i-li)/k1i
                B1(ib) = B1(ib) + ib
             endif 
      enddo
!dvm$ end region 

!dvm$ REALIGN B1(i) WITH A1(kr1i * i + lri)

!dvm$ actual(erria, errib)

!dvm$ region  
!dvm$ parallel (i) on B1(i), private(ia), 
!dvm$*                reduction(min(erria),min(errib))
      do i=1,BN1
            if (B1(i) /= i+1) then     
               errib = min(errib,i)
            endif 
            ia=kr1i * i + lri
            if (A1(ia) /= ia*2) then     
               erria = min(erria,i)
            endif 
      enddo
!dvm$ end region 
  
!dvm$ get_actual(erria,errib) 

      if ((erria == ER) .and. (errib == ER)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (B1,A1)

      end subroutine realign112
C ----------------------------------------------------realign112r
C 112r ALIGN arrB(i) WITH arrA(i+4)  REALIGN arrB(i) WITH arrA(-i+8)
      subroutine realign112r
      integer, parameter ::  AN1=16,BN1=4,NL=1000,ER=10000
      integer ::   erria = ER, errib = ER
c     parameters for ALIGN                                                 
      integer, parameter ::  k1i=1,li=4
c     parameters for REALIGN                                                  
      integer, parameter ::  kr1i=-1,lri=8
      integer, allocatable :: A1(:),B1(:)
      character(*), parameter :: tname = 'realign112r'
               
!dvm$ distribute A1(BLOCK)    
!dvm$ ALIGN B1(i) WITH A1(k1i * i + li)
!dvm$ DYNAMIC B1

      allocate (A1(AN1),B1(BN1))

      B1 = 1     

!dvm$ actual (B1)

!dvm$ region inout (B1), out(A1)
!dvm$ parallel (i) on A1(i), private(ib)
      do i=1,AN1
             A1(i) = i * 2
             if (((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)) then
                ib = (i-li)/k1i
                B1(ib) = B1(ib) + ib
             endif 
      enddo
!dvm$ end region 

!dvm$ REALIGN B1(i) WITH A1(kr1i * i + lri)

!dvm$ actual(erria, errib)

!dvm$ region  
!dvm$ parallel (i) on B1(i), private(ia), 
!dvm$*                reduction(min(erria),min(errib))
      do i=1,BN1
            if (B1(i) /= i+1) then     
               errib = min(errib,i)
            endif 
            ia=kr1i * i + lri
            if (A1(ia) /= ia*2) then     
               erria = min(erria,i)
            endif 
      enddo
!dvm$ end region 
  
!dvm$ get_actual(erria,errib) 

      if ((erria == ER) .and. (errib == ER)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (B1,A1)

      end subroutine realign112r
C ----------------------------------------------------realign113
C 113 ALIGN arrB(i) WITH arrA(3*i-2) REALIGN arrB(i) WITH arrA(2*i+1)   
      subroutine realign113
      integer, parameter ::  AN1=30,BN1=6,NL=1000,ER=10000
      integer ::   erria = ER, errib = ER
c     parameters for ALIGN                                                  
      integer, parameter ::  k1i=3,li=-2
c     parameters for REALIGN                                                 
      integer, parameter ::  kr1i=2,lri=1
      integer, allocatable :: A1(:),B1(:)
      character(*), parameter :: tname = 'realign113'
               
!dvm$ distribute A1(BLOCK)   
!dvm$ ALIGN B1(i) WITH A1(k1i * i + li)
!dvm$ DYNAMIC B1

      allocate (A1(AN1),B1(BN1))

!dvm$ region 
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) = 5     
      enddo

!dvm$ end region   

!dvm$ region in(B1), out(A1,B1)
!dvm$ parallel (i) on A1(i), private(ib)
      do i=1,AN1
             A1(i) = i + 3
             if (((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)) then
                ib = (i-li)/k1i
                B1(ib) = B1(ib) + ib
             endif 
      enddo
!dvm$ end region   

!dvm$ REALIGN B1(i) WITH A1(kr1i * i + lri)

!dvm$ actual(erria, errib)

!dvm$ region 
!dvm$ parallel (i) on B1(i), private(ia), 
!dvm$*                reduction(min(erria),min(errib))
      do i=1,BN1
            if (B1(i) /= (i+5)) then     
               errib = min(errib,i)
            endif 
            ia=kr1i * i + lri
            if (A1(ia) /= (ia+3)) then     
               erria = min(erria,i)
            endif 
      enddo
!dvm$ end region   

!dvm$ get_actual(erria,errib) 

      if ((erria == ER) .and. (errib == ER)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B1,A1)

      end subroutine realign113
C ----------------------------------------------------realign113r
C 113r ALIGN arrB(i) WITH arrA(-i+8) REALIGN arrB(i) WITH arrA(3*i-2) 
      subroutine realign113r
      integer, parameter ::  AN1=30,BN1=6,NL=1000,ER=10000
      integer ::   erria = ER, errib = ER
c     parameters for ALIGN                                                  
      integer, parameter ::  k1i=-1,li=8
c     parameters for REALIGN                                                 
      integer, parameter ::  kr1i=3,lri=-2
      integer, allocatable :: A1(:),B1(:)
      character(*), parameter :: tname = 'realign113r'
               
!dvm$ distribute A1(BLOCK)   
!dvm$ ALIGN B1(i) WITH A1(k1i * i + li)
!dvm$ DYNAMIC B1

      allocate (A1(AN1),B1(BN1))

!dvm$ region 
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) = 5     
      enddo

!dvm$ end region   

!dvm$ region in(B1), out(A1,B1)
!dvm$ parallel (i) on A1(i), private(ib)
      do i=1,AN1
             A1(i) = i + 3
             if (((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)) then
                ib = (i-li)/k1i
                B1(ib) = B1(ib) + ib
             endif 
      enddo
!dvm$ end region   

!dvm$ REALIGN B1(i) WITH A1(kr1i * i + lri)

!dvm$ actual(erria, errib)

!dvm$ region 
!dvm$ parallel (i) on B1(i), private(ia), 
!dvm$*                reduction(min(erria),min(errib))
      do i=1,BN1
            if (B1(i) /= (i+5)) then     
               errib = min(errib,i)
            endif 
            ia=kr1i * i + lri
            if (A1(ia) /= (ia+3)) then     
               erria = min(erria,i)
            endif 
      enddo
!dvm$ end region   

!dvm$ get_actual(erria,errib) 

      if ((erria == ER) .and. (errib == ER)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B1,A1)

      end subroutine realign113r
C ----------------------------------------------------realign114
C 114 ALIGN arrB(i) WITH arrA(2*i+8)  REALIGN arrB(i) WITH arrA(i) 
      subroutine realign114
      integer, parameter ::  AN1=24,BN1=8,NL=1000,ER=10000
      integer ::   erria = ER, errib = ER
c     parameters for ALIGN                                                 
      integer, parameter ::  k1i=2,li=8
c     parameters for REALIGN                                                  
      integer, parameter ::  kr1i=1,lri=0
      integer, allocatable :: A1(:),B1(:)
      character(*), parameter :: tname = 'realign114'
               
!dvm$ distribute A1(BLOCK)   
!dvm$ ALIGN B1(i) WITH A1(k1i * i + li)
!dvm$ DYNAMIC B1

      allocate (A1(AN1),B1(BN1))

!dvm$ region  out(A1, B1)
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) = 0     
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
!dvm$ end region   

!dvm$ REALIGN B1(i) WITH A1(kr1i * i + lri)

!dvm$ actual(erria, errib)

!dvm$ region 
!dvm$ parallel (i) on B1(i), reduction(min(erria),min(errib)),
!dvm$*      private(ia)      
      do i=1,BN1
            if (B1(i) /= (i)) then     
               errib = min(errib,i)
            endif 
            ia=kr1i * i + lri
            if (A1(ia) /= (ia)) then     
               erria = min(erria,i)
            endif 
      enddo
!dvm$ end region   
     
!dvm$ get_actual(erria,errib) 

      if ((erria == ER) .and. (errib == ER)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (B1,A1)

      end subroutine realign114
C ----------------------------------------------------realign115
C 115 ALIGN arrB(*) WITH arrA(*)  REALIGN arrB(i) WITH arrA(i+4)
      subroutine realign115
      integer, parameter ::  AN1=24,BN1=8,NL=1000,ER=10000
      integer ::   erria = ER, errib = ER
c     parameters for ALIGN                                                  
      integer, parameter ::  k1i=0,li=0
c     parameters for REALIGN                                                 
      integer, parameter ::  kr1i=1,lri=4
      integer, allocatable :: A1(:),B1(:)
      character(*), parameter :: tname = 'realign115'
               
!dvm$ distribute A1(BLOCK)    
!dvm$ ALIGN B1(*) WITH A1(*)
!dvm$ DYNAMIC B1

      allocate (A1(AN1),B1(BN1))

      do i=1,BN1
            B1(i) = i+4     
      enddo

!dvm$ region 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
             A1(i) = (i+1) ** 2
      enddo
!dvm$ end region 

!dvm$ REALIGN B1(i) WITH A1(kr1i * i + lri)

!dvm$ actual(erria, errib)

!dvm$ region 
!dvm$ parallel (i) on B1(i),
!dvm$*      private(ia),      
!dvm$*      reduction(min(erria),min(errib))
      do i=1,BN1
            if (B1(i) /= (i+4)) then     
               errib = min(errib,i)
            endif 
            ia=kr1i * i + lri
            if (A1(ia) /= (ia+1)**2) then     
              erria = min(erria,i)
            endif 
      enddo
!dvm$ end region   
     
!dvm$ get_actual(erria,errib) 

      if ((erria == ER) .and. (errib == ER)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (B1,A1)

      end subroutine realign115
C ----------------------------------------------------realign116
C 116  ALIGN arrB(i) WITH arrA(4*i-3)  REALIGN arrB(i) WITH arrA(*) 
      subroutine realign116
      integer, parameter ::  AN1=36,BN1=8,NL=1000,ER=10000
      integer ::   erria = ER, errib = ER
c     parameters for ALIGN                                                  
      integer, parameter ::  k1i=4,li=-3
c     parameters for REALIGN                                                  
      integer, parameter ::  kr1i=0,lri=0
      integer, allocatable :: A1(:),B1(:)
      character(*), parameter :: tname = 'realign116'
               
!dvm$ distribute A1(BLOCK)    
!dvm$ ALIGN B1(i) WITH A1(k1i * i + li)
!dvm$ DYNAMIC B1

      allocate (A1(AN1),B1(BN1))

!dvm$ region 
!dvm$ parallel (i) on B1(i)
      do i=1,BN1
            B1(i) = i+6     
      enddo

!dvm$ parallel (i) on A1(i)
      do i=1,AN1
             A1(i) = (i+1) ** 3
      enddo
!dvm$ end region 

!dvm$ REALIGN B1(*) WITH A1(*)

!dvm$ actual(erria, errib)

!dvm$ region 
!dvm$ parallel (i) on B1(i), reduction(min(errib))
      do i=1,BN1
            if (B1(i) /= (i+6)) then     
               errib = min(errib,i)
            endif 
      enddo
!dvm$ parallel (i) on A1(i),reduction(min(erria))
      do i=1,AN1
            if (A1(i) /= ((i+1)**3)) then     
               erria = min(erria,i)
            endif 
      enddo
!dvm$ end region   
     
!dvm$ get_actual(erria,errib) 

      if ((erria == ER) .and. (errib == ER)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (B1,A1)

      end subroutine realign116
C -------------------------------------------------


      subroutine ansyes(name)
      character(*) name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character(*) name
      print *,name,'  -  ***error'
      end
   