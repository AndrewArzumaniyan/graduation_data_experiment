      program DISTRM1

!    TESTING distribute and redistribute directive       
!            MULT_BLOCK distribution

      print *,'===START OF distrmult1========================'

C --------------------------------------------------
c 11  DISTRIBUTE arrA1[BLOCK]       REDISTRIBUTE arrA1[MULT_BLOCK] 
      call distrm11
C --------------------------------------------------
c 12  DISTRIBUTE arrA1[MULT_BLOCK]  REDISTRIBUTE arrA1[BLOCK] 
      call distrm12
C --------------------------------------------------
c 13  DISTRIBUTE arrA1[BLOCK]       REDISTRIBUTE arrA1[MULT_BLOCK] small array
      call distrm13
C --------------------------------------------------
c 14  DISTRIBUTE arrA1[MULT_BLOCK]  REDISTRIBUTE arrA1[BLOCK] small array
      call distrm14
C --------------------------------------------------
c 15  DISTRIBUTE arrA1[MULT_BLOCK]  REDISTRIBUTE arrA1[MULT_BLOCK] other m
      call distrm15
C --------------------------------------------------
c 16  DISTRIBUTE arrA1[MULT_BLOCK]  REDISTRIBUTE arrA1[*]
      call distrm16
C --------------------------------------------------
c 17  DISTRIBUTE arrA1[*]           REDISTRIBUTE arrA1[MULT_BLOCK] 
      call distrm17
C --------------------------------------------------
c 21  DISTRIBUTE arrA2[MULT_BLOCK][*]   REDISTRIBUTE arrA2[*][MULT_BLOCK]
      call distrm21
C --------------------------------------------------
c 22  DISTRIBUTE arrA2[*][MULT_BLOCK]   REDISTRIBUTE arrA2[*][*]
      call distrm22
C --------------------------------------------------
c 23  DISTRIBUTE arrA2[*][*]            REDISTRIBUTE arrA2[*][MULT_BLOCK]
      call distrm23
C -------------------------------------------------
C
      print *,'=== END OF distrmult1 ========================= '    

      end

C ----------------------------------------------------distrm11
c 11  DISTR arrA1[BLOCK]    REDISTR arrA1[MULT_BLOCK]  

      subroutine distrm11
      integer, parameter :: AN1=25,ER=10000
      integer :: erri=ER,i
      integer, parameter :: m = 5
      integer, allocatable :: A1(:)
      character(*), parameter :: tname='distrm11  '
               
!dvm$ distribute A1(BLOCK)    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = i    
      enddo
!dvm$ end region

!dvm$ redistribute A1(MULT_BLOCK(m))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction(min(erri))
      do i=1,AN1
          if (A1(i) /= i) then     
             erri = min(erri,i)
          endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A1)

      end subroutine distrm11

C ---------------------------------------------distrm12
c 12  DISTRIBUTE arrA1[MULT_BLOCK]    REDISTRIBUTE arrA1[BLOCK] 

      subroutine distrm12

      integer, parameter :: AN1=48,ER=10000
      integer :: erri=ER,i
      integer, parameter :: m = 6
      integer, allocatable :: A1(:)
      character(10), parameter :: tname='distrm12  '
               
!dvm$ distribute A1(MULT_BLOCK(m))    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = i ** 2    
      enddo
!dvm$ end region

!dvm$ redistribute A1(BLOCK)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) /= i**2) then     
               erri = min(erri,i)
            endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A1)

      end subroutine distrm12

C ----------------------------------------------------distrm13
c 13  DISTR arrA1[BLOCK]    REDISTR arrA1[MULT_BLOCK] small array  

      subroutine distrm13

      integer, parameter :: AN1=4,ER=10000
      integer :: erri=ER,i
      integer, parameter :: m = 4
      integer, allocatable :: A1(:)
      character(10) :: tname='distrm13  '
               
!dvm$ distribute A1(BLOCK)    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = i*2     
      enddo
!dvm$ end region

!dvm$ redistribute A1(MULT_BLOCK(m))  
  
!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) /= i*2 ) then     
               erri = min(erri,i)
            endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A1)

      end  subroutine distrm13
C ---------------------------------------------distrm14
c 14  DISTRIBUTE arrA1[MULT_BLOCK]   REDISTRIBUTE arrA1[BLOCK]  small array

      subroutine distrm14

      integer, parameter :: AN1=3,ER=10000
      integer :: erri=ER,i
      integer, parameter :: m = 3
      integer, allocatable :: A1(:)
      character(10) :: tname='distrm14  '

!dvm$ distribute A1(MULT_BLOCK(m))    
!dvm$ dynamic A1

      allocate (A1(AN1))

      A1 = 5

!dvm$ actual (A1)

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i) + i   
      enddo
!dvm$ end region

!dvm$ redistribute A1(BLOCK)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) /= i+5) then     
               erri = min(erri,i)
            endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A1)

      end  subroutine distrm14

C ----------------------------------------------------distrm15
c 15  DISTR arrA1[MULT_BLOCK]    REDISTR arrA1[MULT_BLOCK] other m 

      subroutine distrm15

      integer, parameter :: AN1=24,ER=10000
      integer :: erri=ER,i
      integer, parameter :: m1 = 4, m2 = 3
      integer, allocatable :: A1(:)
      character(*), parameter :: tname='distrm15  '
               
!dvm$ distribute A1(MULT_BLOCK(m1))    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = i     
      enddo
!dvm$ end region

!dvm$ redistribute A1(MULT_BLOCK(m2))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
          if (A1(i) /= i) then     
             erri = min(erri,i)
          endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A1)

      end subroutine distrm15

C ----------------------------------------------------distrm16
c 16  DISTR arrA1[MULT_BLOCK]    REDISTR arrA1[*]  

      subroutine distrm16

      integer, parameter :: AN1=50,ER=10000
      integer :: erri=ER,i
      integer, parameter :: m = 2
      integer, allocatable :: A1(:)
      character(*), parameter :: tname='distrm16  '
               
!dvm$ distribute A1(MULT_BLOCK(m))    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = i * 3    
      enddo
!dvm$ end region

!dvm$ redistribute A1(*)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) /= i*3 ) then     
               erri = min(erri,i)
            endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A1)

      end subroutine distrm16

C ---------------------------------------------distrm17
c 17  DISTRIBUTE arrA1[*]    REDISTRIBUTE arrA1[MULT_BLOCK] 

      subroutine distrm17

      integer, parameter :: AN1=120,ER=10000
      integer :: erri=ER,i
      integer, parameter :: m = 10
      integer, allocatable :: A1(:)
      character(10), parameter :: tname='distrm17  '
               
!dvm$ distribute A1(*)    
!dvm$ dynamic A1

      allocate (A1(AN1))

      A1 = -2

!dvm$ actual (A1)

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = i - A1(i)     
      enddo
!dvm$ end region

!dvm$ redistribute A1(MULT_BLOCK(m))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) /= i+2) then     
               erri = min(erri,i)
            endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A1)

      end subroutine distrm17

C ----------------------------------------------------distrm21
c 21  DISTRIBUTE arrA2[MULT_BLOCK][*]	REDISTRIBUTE arrA2[*][MULT_BLOCK]

      subroutine distrm21

      integer, parameter :: AN1=36,AN2=25,NL=1000,ER=10000
      integer :: erri=ER,i
      integer, parameter :: m1 = 6,  m2 = 5
      integer, allocatable :: A2(:,:)
      character(10) :: tname='distrm21'
               
!dvm$ distribute A2(MULT_BLOCK(m1),*)   
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

!dvm$ redistribute A2(*,MULT_BLOCK(m2))    

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

      end  subroutine distrm21

C ----------------------------------------------------distrm22
c 22  DISTRIBUTE arrA2[*][MULT_BLOCK]	REDISTRIBUTE arrA2[*][*]

      subroutine distrm22

      integer, parameter :: AN1=8,AN2=121,NL=1000,ER=10000
      integer :: erri=ER,i
      integer, parameter :: m2 = 11
      integer, allocatable :: A2(:,:)
      character(10) :: tname='distrm22'
               
!dvm$ distribute A2(*,MULT_BLOCK(m2))   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

      A2 = 4 

!dvm$ actual(A2)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =A2(i,j) + (i*NL+j)     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(*,*)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j+4)) then     
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

      end  subroutine distrm22

C ----------------------------------------------------distrm23
c 23  DISTRIBUTE arrA2[*][*]	REDISTRIBUTE arrA2[*][MULT_BLOCK]

      subroutine distrm23

      integer, parameter :: AN1=8,AN2=63,NL=1000,ER=10000
      integer :: erri=ER,i
      integer, parameter :: m2 = 9
      integer, allocatable :: A2(:,:)
      character(10) :: tname='distrm23'
               
!dvm$ distribute A2(*,*)   
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

!dvm$ redistribute A2(*,MULT_BLOCK(m2))    

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

      end subroutine distrm23

C -------------------------------------------------

      subroutine ansyes(name)
      character(*) name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character(*) name
      print *,name,'  -  ***error'
      end
