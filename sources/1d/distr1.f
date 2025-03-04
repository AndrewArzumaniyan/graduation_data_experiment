      program DISTR1

c    TESTING distribute and redistribute CLAUSE .       

      print *,'===START OF distr1========================'
C --------------------------------------------------
c 11  DISTRIBUTE arrA1[BLOCK]    REDISTRIBUTE arrA1[*] 
      call distr11
C --------------------------------------------------
c 12  DISTRIBUTE arrA1[*]    REDISTRIBUTE arrA1[BLOCK] 
      call distr12
C --------------------------------------------------
c 13  DISTRIBUTE arrA1[BLOCK]    REDISTRIBUTE arrA1[*] small array
      call distr13
C --------------------------------------------------
c 14  DISTRIBUTE arrA1[*]    REDISTRIBUTE arrA1[BLOCK] small array 
      call distr14
C --------------------------------------------------
c 21  DISTRIBUTE arrA2[BLOCK][*]	REDISTRIBUTE arrA2[*][ BLOCK]
      call distr21
C --------------------------------------------------
c 22  DISTRIBUTE arrA2[*][BLOCK]	REDISTRIBUTE arrA2[*][*]
      call distr22
C --------------------------------------------------
c 23  DISTRIBUTE arrA2[*][*]	REDISTRIBUTE arrA2[*][ BLOCK]
      call distr23
C -------------------------------------------------
C
C
      print *,'=== END OF distr1 ========================= '    
      end

C ----------------------------------------------------distr11
c 11  DISTR arrA1[BLOCK]    REDISTR arrA1[*]  
      subroutine distr11
      integer, parameter :: AN1=8,NL=1000,ER=10000
      integer :: erri= ER, i
      integer, allocatable :: A1(:)
      character(9) :: tname = 'distr11'
               
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
C ---------------------------------------------distr12
c 12  DISTRIBUTE arrA1[*]    REDISTRIBUTE arrA1[BLOCK] 
      subroutine distr12
      integer, parameter :: AN1=8,NL=1000,ER=10000
      integer :: erri= ER,i
      integer, allocatable :: A1(:)
      character(9), parameter :: tname='distr12'

!dvm$ distribute A1(*)    
!dvm$ dynamic A1

      allocate (A1(AN1))

c !dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) =i     
      enddo

!dvm$ redistribute A1(BLOCK)    

!dvm$ actual(erri)
!dvm$ region 
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) /= i) then     
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
C ----------------------------------------------------distr13
c 13  DISTR arrA1[BLOCK]    REDISTR arrA1[*] small array  
      subroutine distr13
      integer, parameter :: AN1=5,NL=1000,ER=10000
      integer :: erri= ER,i
      integer, allocatable :: A1(:)
      character(*), parameter :: tname='distr13  '
               
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
C ---------------------------------------------distr14
c 14  DISTRIBUTE arrA1[*]    REDISTRIBUTE arrA1[BLOCK]  small array
      subroutine distr14
      integer, parameter :: AN1=5,NL=1000,ER=10000
      integer :: erri=ER,i
      integer, allocatable :: A1(:)
      character(9)  ::  tname='distr14'

!dvm$ distribute A1(*)    
!dvm$ dynamic A1

      allocate (A1(AN1))

c !dvm$ parallel (i) on A1(i)
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
C ----------------------------------------------------distr21
c 21  DISTRIBUTE arrA2[BLOCK][*]	REDISTRIBUTE arrA2[*][ BLOCK]
      subroutine distr21
      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
      integer :: erri=ER,i
      integer, allocatable :: A2(:,:)
      character(9), parameter :: tname='distr21'
               
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

C ----------------------------------------------------distr22
c 22  DISTRIBUTE arrA2[*][BLOCK]	REDISTRIBUTE arrA2[*][*]
      subroutine distr22
      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
      integer :: erri=ER,i
      integer, allocatable :: A2(:,:)
      character(9) :: tname='distr22'
               
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

C ----------------------------------------------------distr23
c 23  DISTRIBUTE arrA2[BLOCK][*]	REDISTRIBUTE arrA2[*][ BLOCK]
      subroutine distr23
      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
      integer :: erri=ER,i
      integer, allocatable :: A2(:,:)
      character(9) :: tname='distr23'
               
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
!dvm$  region 
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
      character*9 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*9 name
      print *,name,'  -  ***error'
      end
