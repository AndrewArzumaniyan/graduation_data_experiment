      program DISTRM2

!    TESTING distribute and redistribute directive       
!            MULT_BLOCK distribution
       
      print *,'===START OF distrmult2========================'

C -------------------------------------------------
c 24  DISTRIBUTE arrA2[MULT_BLOCK][MULT_BLOCK]  REDISTRIBUTE arrA2[*][*]
      call distrm24
C -------------------------------------------------
c 25  DISTRIBUTE arrA2[*][*]  REDISTRIBUTE arrA2[MULT_BLOCK][MULT_BLOCK]
      call distrm25
C -------------------------------------------------
c 26  DISTRIBUTE arrA2[MULT_BLOCK][MULT_BLOCK] 
C                             REDISTRIBUTE arrA2[BLOCK][BLOCK]
      call distrm26
C -------------------------------------------------
c 27  DISTRIBUTE arrA2[BLOCK][BLOCK]
c                             REDISTRIBUTE arrA2[MULT_BLOCK][MULT_BLOCK]
      call distrm27
C -------------------------------------------------
c 28  DISTRIBUTE arrA2[MULT_BLOCK][BLOCK]
c                         REDISTRIBUTE arrA2[BLOCK][MULT_BLOCK]
      call distrm28
C -------------------------------------------------
c 29  DISTRIBUTE arrA2[BLOCK][MULT_BLOCK]
c                         REDISTRIBUTE arrA2[MULT_BLOCK][BLOCK]
      call distrm29
C -------------------------------------------------
c 210  DISTRIBUTE arrA2[MULT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [MULT_BLOCK][MULT_BLOCK]  other m1,m2
      call distrm210
C -------------------------------------------------
c 32  DISTRIBUTE  arrA3[MULT_BLOCK][MULT_BLOCK] [*] 
c                         REDISTRIBUTE arrA3[*][MULT_BLOCK][MULT_BLOCK]
      call distrm32
C -------------------------------------------------
c 33  DISTRIBUTE  arrA3[MULT_BLOCK][MULT_BLOCK][*]
c                          REDISTRIBUTE arrA3[MULT_BLOCK][*][BLOCK]
      call distrm33
C -------------------------------------------------
c 34  DISTRIBUTE  arrA3[MULT_BLOCK][*][MULT_BLOCK] 
c                         REDISTRIBUTE arrA3[MULT_BLOCK][MULT_BLOCK][*]
      call distrm34
C -------------------------------------------------
c 35  DISTRIBUTE  arrA3[MULT_BLOCK][MULT_BLOCK][*]
c                         REDISTRIBUTE arrA3[*][*]MULT_BLOCK]
      call distrm35
C -------------------------------------------------
c 36  DISTRIBUTE  arrA3[MULT_BLOCK][*][BLOCK]
c                         REDISTRIBUTE arrA3[BLOCK][*][MULT_BLOCK]
      call distrm36
C -------------------------------------------------
c 37  DISTRIBUTE  arrA3[MULT_BLOCK][BLOCK][*]
c                         REDISTRIBUTE arrA3[BLOCK][*][MULT_BLOCK]
      call distrm37
C -------------------------------------------------
c 38  DISTRIBUTE  arrA3[BLOCK][*][MULT_BLOCK]
c                         REDISTRIBUTE arrA3[*][MULT_BLOCK][BLOCK]
      call distrm38
C -------------------------------------------------
c 41  DISTRIBUTE arrA4[*][*][MULT_BLOCK][MULT_BLOCK]
c                         REDISTRIBUTE arrA4[*][*][*][*]
      call distrm41
C -------------------------------------------------
c 42  DISTRIBUTE arrA4[MULT_BLOCK][*][MULT_BLOCK][*]
c                        REDISTRIBUTE arrA4[*][MULT_BLOCK][MULT_BLOCK][*]
      call distrm42                           
C -------------------------------------------------
C
      print *,'=== END OF distrmult2 ========================= '    

      end

C ----------------------------------------------------distrm24
c 24  DISTRIBUTE arrA2[MULT_BLOCK][MULT_BLOCK]  REDISTRIBUTE arrA2[*][*]

      subroutine distrm24

      integer, parameter :: AN1=15,AN2=12,NL=1000,ER=10000
      integer :: erri= ER,i,j
      integer, parameter :: m1 = 5, m2 = 3
      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrm24  '
               
!dvm$ distribute A2(MULT_BLOCK(m1),MULT_BLOCK(m2))   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

      A2 = 4 

!dvm$ actual (A2)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j)+ (i*NL+j)     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(*,*)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)+4 ) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A2)

      end subroutine distrm24

C ----------------------------------------------------distrm25
c 25  DISTRIBUTE arrA2[*][*]  REDISTRIBUTE arrA2[MULT_BLOCK][MULT_BLOCK]

      subroutine distrm25

      integer, parameter :: AN1=18,AN2=8,NL=1000,ER=10000
      integer :: erri= ER,i,j
      integer, parameter :: m1 = 3, m2 = 2
      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrm25  '
               
!dvm$ distribute A2(*,*)   
!dvm$ dynamic A2

       allocate (A2(AN1,AN2))

       A2 = 5

!dvm$  actual(A2)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) + (i*NL+j)     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(MULT_BLOCK(m1), MULT_BLOCK(m2))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) - 5     
            if (A2(i,j) /= (i*NL+j)) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A2)

      end subroutine distrm25

C ----------------------------------------------------distrm26
c 26  DISTRIBUTE arrA2[MULT_BLOCK][MULT_BLOCK]
c                REDISTRIBUTE arrA2[BLOCK][BLOCK]

      subroutine distrm26

      integer, parameter :: AN1=49,AN2=12,NL=1000,ER=10000
      integer :: erri= ER,i,j
      integer, parameter :: m1 = 7, m2 = 4
      integer, allocatable :: A2(:,:)
      character(10), parameter :: tname='distrm26  '
               
!dvm$ distribute A2(MULT_BLOCK(m1),MULT_BLOCK(m2))   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(BLOCK,BLOCK)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A2)

      end subroutine distrm26

C ----------------------------------------------------distrm27
c 27  DISTRIBUTE arrA2[BLOCK][BLOCK]  REDISTRIBUTE arrA2[MULT_BLOCK][MULT_BLOCK]

      subroutine distrm27

      integer, parameter :: AN1=8,AN2= 64,NL=1000,ER=10000
      integer :: erri= ER,i,j
      integer, parameter :: m1 = 1, m2 = 8
      integer, allocatable :: A2(:,:)
      character(10), parameter :: tname='distrm27'
               
!dvm$ distribute A2(BLOCK,BLOCK)   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =(i*NL+j)*2     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(MULT_BLOCK(m1),MULT_BLOCK(m2))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j)  /= (i*NL+j)*2) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A2)

      end subroutine distrm27

C ----------------------------------------------------distrm28
c 28  DISTRIBUTE arrA2[MULT_BLOCK][BLOCK]  REDISTRIBUTE arrA2[BLOCK][MULT_BLOCK]

      subroutine distrm28

      integer, parameter :: AN1=20,AN2=20,NL=1000,ER=10000
      integer :: erri= ER,i,j
      integer, parameter :: m1 = 5, m2 = 4
      integer, allocatable :: A2(:,:)
      character(10) :: tname='distrm28  '
               
!dvm$ distribute A2(MULT_BLOCK(m1),BLOCK)   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =(i*NL+j)*3     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(BLOCK,MULT_BLOCK(m2))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)*3) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A2)

      end subroutine distrm28

C ----------------------------------------------------distrm29
c 29  DISTRIBUTE arrA2[BLOCK][MULT_BLOCK]
c                REDISTRIBUTE arrA2[MULT_BLOCK][BLOCK]

      subroutine distrm29

      integer, parameter :: AN1=30,AN2=60,NL=1000,ER=10000
      integer :: erri= ER,i,j
      integer, parameter :: m1 = 10, m2 = 10
      integer, allocatable :: A2(:,:)
      character(10), parameter :: tname='distrm29'
               
!dvm$ distribute A2(BLOCK,MULT_BLOCK(m2))   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

      A2 = -1

!dvm$ actual (A2)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) + (i*NL+j)     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(MULT_BLOCK(m1),BLOCK)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)-1) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A2)

      end subroutine distrm29

C ----------------------------------------------------distrm210
c 210  DISTRIBUTE arrA2[MULT_BLOCK][MULT_BLOCK]
c                 REDISTRIBUTE [MULT_BLOCK][MULT_BLOCK]  other m1, m2

      subroutine distrm210

      integer, parameter :: AN1=24,AN2=24,NL=1000,ER=10000
      integer :: erri= ER,i,j
      integer, parameter :: m1 = 3, m2 = 2
      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrm210 '
               
!dvm$ distribute A2(MULT_BLOCK(m1),MULT_BLOCK(m2))   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(MULT_BLOCK(m2),MULT_BLOCK(m1))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region
                                                    
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A2)

      end subroutine distrm210

C ----------------------------------------------------distrm32
c 32  DISTRIBUTE  arrA3[MULT_BLOCK][MULT_BLOCK] [*]
c                 REDISTRIBUTE arrA3[*][MULT_BLOCK][MULT_BLOCK]

      subroutine distrm32

      integer, parameter :: AN1=16,AN2=12,AN3=8,NL=1000,ER=10000
      integer :: erri = ER,i,j,k
      integer, parameter :: m1 = 2, m2 = 3 , m3 = 4
      integer, allocatable ::  A3(:,:,:)
      character(10), parameter :: tname='distrm32  '
               
!dvm$ distribute A3(MULT_BLOCK(m1),MULT_BLOCK(m2),*)   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = i*NL/10 + j*NL/100 + k    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A3(*,MULT_BLOCK(m2),MULT_BLOCK(m3))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region
 
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end

C ----------------------------------------------------distrm33
c 33  DISTRIBUTE  arrA3[MULT_BLOCK][MULT_BLOCK][*]
c                 REDISTRIBUTE arrA3[MULT_BLOCK][*][BLOCK]

      subroutine distrm33

      integer, parameter :: AN1=16,AN2=16,AN3=8,NL=1000,ER=10000
      integer :: erri = ER,i,j,k
      integer, parameter :: m1 = 4, m2 = 2, m3 = 2
      integer, allocatable :: A3(:,:,:)
      character(*), parameter :: tname='distrm33  '
               
!dvm$ distribute A3(MULT_BLOCK(m1),MULT_BLOCK(m2),*)   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = i*NL/10 + j*NL/100 + k*2    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A3(MULT_BLOCK(m1),*,MULT_BLOCK(m3))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k*2)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region
 
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end

C ----------------------------------------------------distrm34
c 34  DISTRIBUTE  arrA3[MULT_BLOCK][*][MULT_BLOCK]
c                 REDISTRIBUTE arrA3[MULT_BLOCK][MULT_BLOCK][*]

      subroutine distrm34

      integer, parameter :: AN1=8,AN2=8,AN3=8,NL=1000,ER=10000
      integer :: erri=ER,i,j,k
      integer, parameter :: m1 = 2, m2 = 1, m3 = 4
      integer, allocatable ::  A3(:,:,:)
      character(10) :: tname='distrm34'
               
!dvm$ distribute A3(MULT_BLOCK(m1),*,MULT_BLOCK(m3))   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

      A3 = 3
     
!dvm$ actual(A3)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) + i*NL/10 + j*NL/100 + k    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A3(MULT_BLOCK(m1),MULT_BLOCK(m2),*)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k + 3)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region
 
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end

C ----------------------------------------------------distrm35
c 35  DISTRIBUTE  arrA3[MULT_BLOCK][MULT_BLOCK][*]
c                         REDISTRIBUTE arrA3[*][*][MULT_BLOCK]

      subroutine distrm35

      integer, parameter :: AN1=18,AN2=28,AN3=38,NL=1000,ER=10000
      integer :: erri=ER,i,j,k
      integer, parameter :: m1 = 3, m2 = 7 , m3 = 19
      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrm35  '
               
!dvm$ distribute A3(MULT_BLOCK(m1),MULT_BLOCK(m2),*)   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = i*NL/10 + j*NL/100 + k    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A3(*,*,MULT_BLOCK(m3))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region
 
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A3)

      end

C ----------------------------------------------------distrm36
c 36  DISTRIBUTE  arrA3[MULT_BLOCK][*][BLOCK]
c                         REDISTRIBUTE arrA3[BLOCK][*][MULT_BLOCK]

      subroutine distrm36

      integer, parameter :: AN1=121,AN2=12,AN3=35,NL=1000,ER=10000
      integer :: erri=ER,i,j,k
      integer, parameter :: m1 = 11, m2 = 2, m3 = 7
      integer, allocatable ::  A3(:,:,:)
      character(10), parameter :: tname='distrm36  '
               
!dvm$ distribute A3(MULT_BLOCK(m1),*,BLOCK)   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

      A3 = 10

!dvm$ actual (A3)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) + i*NL/10 + j*NL/100 + k    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A3(BLOCK,*,MULT_BLOCK(m3))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k + 10)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region
 
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end

C ----------------------------------------------------distrm37
c 37  DISTRIBUTE  arrA3[MULT_BLOCK][BLOCK][*]
c                         REDISTRIBUTE arrA3[BLOCK][*][MULT_BLOCK]

      subroutine distrm37

      integer, parameter :: AN1=8,AN2=28,AN3=8,NL=1000,ER=10000
      integer :: erri=ER,i,j,k
      integer, parameter :: m1 = 2, m2 = 4, m3 = 2
      integer, allocatable :: A3(:,:,:)
      character(*), parameter :: tname='distrm37  '
               
!dvm$ distribute A3(MULT_BLOCK(m1),BLOCK,*)   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = i*NL/10 + j*NL/100 + k    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A3(BLOCK,*,MULT_BLOCK(m3))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region
 
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end

C ----------------------------------------------------distrm38
c 38  DISTRIBUTE  arrA3[BLOCK][*][MULT_BLOCK] REDISTRIBUTE arrA3[*][MULT_BLOCK][MULT_BLOCK]

      subroutine distrm38

      integer, parameter :: AN1=50,AN2=40,AN3=30,NL=1000,ER=10000
      integer :: erri=ER,i,j,k
      integer, parameter :: m1 = 5, m2 = 4, m3 = 3
      integer, allocatable :: A3(:,:,:)
      character(10) :: tname='distrm38'
               
!dvm$ distribute A3(BLOCK, *, MULT_BLOCK(m3))   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = i*NL/10 + j*NL/100 + k*5    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A3(*,MULT_BLOCK(m2),MULT_BLOCK(m3))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k*5)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region
 
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end

C ----------------------------------------------------distrm41
c 41  DISTRIBUTE arrA4[*][*][MULT_BLOCK][MULT_BLOCK] REDISTRIBUTE arrA4[*][*][*][*]

      subroutine distrm41

      integer, parameter :: AN1=16,AN2=16,AN3=16,AN4=16,NL=1000,ER=100000
      integer :: erri=ER,i,j,n,m
      integer, parameter :: m1 = 2, m2 = 4, m3 = 2, m4 = 4
      integer, allocatable :: A4(:,:,:,:)
      character(10), parameter :: tname='distrm41  '
               
!dvm$ distribute A4(*,*,MULT_BLOCK(m3),MULT_BLOCK(m4))   
!dvm$ dynamic A4

      allocate (A4(AN1,AN2,AN3,AN4))

!dvm$ region
!dvm$ parallel (i,j,n,m) on A4(i,j,n,m)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                do m=1,AN4
                     A4(i,j,n,m) = i*NL/10+j*NL/100+n*NL/1000+m
                enddo
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A4(*,*,*,*)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
              do m=1,AN4
                if (A4(i,j,n,m) /= (i*NL/10+j*NL/100+n*NL/1000+m)) then     
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000+m)
                endif 
              enddo
            enddo
          enddo
      enddo
!dvm$ end region
 
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A4)

      end

C ----------------------------------------------------distrm42
c 42  DISTRIBUTE arrA4[MULT_BLOCK][*][MULT_BLOCK][*] 
c                REDISTRIBUTE arrA4[*][MULT_BLOCK][MULT_BLOCK][*]

      subroutine distrm42

      integer, parameter :: AN1=28,AN2=25,AN3=27,AN4=21,NL=1000,ER=100000
      integer :: erri=ER,i,j,n,m
      integer, parameter :: m1 = 7, m2 = 5, m3 = 9, m4  = 3
      integer, allocatable :: A4(:,:,:,:)
      character(10) :: tname='distrm42  '
               
!dvm$ distribute A4(MULT_BLOCK(m1),*,MULT_BLOCK(m3),*)   
!dvm$ dynamic A4

      allocate (A4(AN1,AN2,AN3,AN4))

      A4 = 6   

!dvm$ actual (A4)

!dvm$ region
!dvm$ parallel (i,j,n,m) on A4(i,j,n,m)
      do i=1,AN1
         do j=1,AN2
            do n=1,AN3
               do m=1,AN4
                A4(i,j,n,m) = A4(i,j,n,m)+ i*NL/10+j*NL/100+n*NL/1000+m
               enddo
            enddo
         enddo
      enddo
!dvm$ end region

!dvm$ redistribute A4(*,MULT_BLOCK(m2),MULT_BLOCK(m3),*)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), reduction( min( erri ) )
      do i=1,AN1
        do j=1,AN2
          do n=1,AN3
            do m=1,AN4
              if (A4(i,j,n,m) /= (i*NL/10+j*NL/100+n*NL/1000+m+6)) then     
                  erri = min(erri,i*NL/10+j*NL/100+n*NL/1000+m)
              endif 
            enddo
          enddo
        enddo
      enddo
!dvm$ end region
 
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A4)

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
