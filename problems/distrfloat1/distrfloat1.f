      program DISTRFLOAT1

c    TESTING distribute and redistribute CLAUSE .       

      print *, '===START OF distrfloat1=================='
C --------------------------------------------------
c 11  DISTRIBUTE arrA1[BLOCK]    REDISTRIBUTE arrA1[*] 
      call distrf11
C --------------------------------------------------
c 12  DISTRIBUTE arrA1[*]    REDISTRIBUTE arrA1[BLOCK] 
c      call distr12
C --------------------------------------------------
c 13  DISTRIBUTE arrA1[BLOCK]    REDISTRIBUTE arrA1[*] small array
      call distrf13
C --------------------------------------------------
c 14  DISTRIBUTE arrA1[*]    REDISTRIBUTE arrA1[BLOCK] small array 
c      call distrf14
C --------------------------------------------------
c 21  DISTRIBUTE arrA2[BLOCK][*]	REDISTRIBUTE arrA2[*][ BLOCK]
      call distrf21
C --------------------------------------------------
c 22  DISTRIBUTE arrA2[*][BLOCK]	REDISTRIBUTE arrA2[*][*]
c      call distrf22
C --------------------------------------------------
c 23  DISTRIBUTE arrA2[*][*]	REDISTRIBUTE arrA2[*][ BLOCK]
      call distrf23
C -------------------------------------------------
C
      print *, '=== END OF distrfloat1 =================='
C
      end

C ----------------------------------------------------distrf11
c 11  DISTR arrA1[BLOCK]    REDISTR arrA1[*]  
      subroutine distrf11
      integer, parameter :: AN1=8,NL=1000,ER=10000
      integer :: erri= ER,i
      real, allocatable :: A1(:)
      character(10) :: tname = 'distrf11'
               
!dvm$ distribute A1(BLOCK)    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ region 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) =i     
      enddo
!dvm$ end region   

!dvm$ redistribute A1(*)    

!dvm$ actual(erri)
!dvm$ region 
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) .eq.(i)) then     
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
C ---------------------------------------------distrf12
c 12  DISTRIBUTE arrA1[*]    REDISTRIBUTE arrA1[BLOCK] 
      subroutine distrf12
      integer, parameter :: AN1=8,NL=1000,ER=10000
      integer :: erri= ER,i
      real*8, allocatable :: A1(:)
      character(10) :: tname = 'distrf12'
               
!dvm$ distribute A1(*)    
!dvm$ dynamic A1

      allocate (A1(AN1))

c *dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) =i     
      enddo

!dvm$ redistribute A1(BLOCK)    

!dvm$ actual(erri)
!dvm$ region 
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) .eq.(i)) then     
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
C ----------------------------------------------------distrf13
c 13  DISTR arrA1[BLOCK]    REDISTR arrA1[*] small array  
      subroutine distrf13
      integer, parameter :: AN1=5,NL=1000,ER=10000
      integer :: erri= ER,i
      complex, allocatable :: A1(:)
      character(10) :: tname = 'distrf13'
               
!dvm$ distribute A1(BLOCK)    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ region 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) =i     
      enddo
!dvm$ end region   

!dvm$ redistribute A1(*)    

!dvm$ actual(erri)
!dvm$ region 
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) .eq.(i)) then     
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
C ---------------------------------------------distrf14
c 14  DISTRIBUTE arrA1[*]    REDISTRIBUTE arrA1[BLOCK]  small array
      subroutine distrf14
      integer, parameter :: AN1=5,NL=1000,ER=10000
      integer :: erri= ER,i
      complex*16, allocatable :: A1(:)
      character(10), parameter :: tname = 'distrf14'
               
!dvm$ distribute A1(*)    
!dvm$ dynamic A1

      allocate (A1(AN1))

c *dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) =i     
      enddo

!dvm$ redistribute A1(BLOCK)    

!dvm$ actual(erri)
!dvm$ region 
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) .eq.(i)) then     
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
C ----------------------------------------------------distrf21
c 21  DISTRIBUTE arrA2[BLOCK][*]	REDISTRIBUTE arrA2[*][ BLOCK]
      subroutine distrf21
      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
      integer :: erri= ER,i
      real, allocatable ::  A2(:,:)
      character(10), parameter :: tname = 'distrf21'
               
!dvm$ distribute A2(BLOCK,*)   
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

!dvm$ redistribute A2(*,BLOCK)    

!dvm$ actual(erri)
!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) .eq.(i*NL+j)) then     
            else
               erri = min(erri,i*NL/10+j)
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
      deallocate (A2)

      end

C ----------------------------------------------------distrf22
c 22  DISTRIBUTE arrA2[*][BLOCK]	REDISTRIBUTE arrA2[*][*]
      subroutine distrf22
      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
      integer :: erri= ER,i
      real*8, allocatable :: A2(:,:)
      character(10), parameter :: tname = 'distrf22'
               
!dvm$ distribute A2(*,BLOCK)   
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

!dvm$ redistribute A2(*,*)    

!dvm$ actual(erri)
!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) .eq.(i*NL+j)) then     
            else
               erri = min(erri,i*NL/10+j)
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

      deallocate (A2)

      end

C ----------------------------------------------------distrf23
c 23  DISTRIBUTE arrA2[BLOCK][*]	REDISTRIBUTE arrA2[*][ BLOCK]
      subroutine distrf23
      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
      integer :: erri= ER,i
      complex, allocatable :: A2(:,:)
      character(10), parameter :: tname = 'distrf23'
               
!dvm$ distribute A2(*,*)   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

c *dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo

!dvm$ redistribute A2(*,BLOCK)    

!dvm$ actual(erri)
!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) .eq.(i*NL+j)) then     
            else
               erri = min(erri,i*NL/10+j)
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

      deallocate (A2)

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
