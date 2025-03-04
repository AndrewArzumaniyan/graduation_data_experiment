      program DISTRMIX1

      integer nproc
      number_of_processors()=1  

!    Testing DISTRIBUTE and REDISTRIBUTE directive       
!            GEN_BLOCK, WGT_BLOCK, MULT_BLOCK distributions

      print *,'===START OF distrmix1========================'

      nproc = number_of_processors()  

C --------------------------------------------------
c 11  DISTRIBUTE arrA1[MULT_BLOCK]
c                     REDISTRIBUTE arrA1[WGT_BLOCK] 
c                     REDISTRIBUTE arrA1[MULT_BLOCK]  
      call distrmix11
C --------------------------------------------------
c 12  DISTRIBUTE arrA1[WGT_BLOCK]
c                     REDISTRIBUTE arrA1[MULT_BLOCK]  
c                     REDISTRIBUTE arrA1[WGT_BLOCK]  
      call distrmix12
C --------------------------------------------------

      if (nproc > 4 ) then     ! may be temporary
         goto 1
      endif

C --------------------------------------------------
c 13  DISTRIBUTE arrA1[MULT_BLOCK]
c                     REDISTRIBUTE arrA1[GEN_BLOCK] 
c                     REDISTRIBUTE arrA1[MULT_BLOCK]  
      call distrmix13 (nproc)
C --------------------------------------------------
c 14  DISTRIBUTE arrA1[GEN_BLOCK]
c                     REDISTRIBUTE arrA1[MULT_BLOCK]
c                     REDISTRIBUTE arrA1[GEN_BLOCK] 
      call distrmix14 (nproc)
C --------------------------------------------------
c 15  DISTRIBUTE arrA1[WGT_BLOCK]    
c                     REDISTRIBUTE arrA1[GEN_BLOCK] 
c                     REDISTRIBUTE arrA1[WGT_BLOCK] 
      call distrmix15 (nproc)
C --------------------------------------------------
c 16  DISTRIBUTE arrA1[GEN_BLOCK]
c                     REDISTRIBUTE arrA1[WGT_BLOCK] 
c                     REDISTRIBUTE arrA1[GEN_BLOCK] 
      call distrmix16 (nproc)
C -------------------------------------------------
C
 1    print *,'=== END OF distrmix1 ========================= '    

      end

C ----------------------------------------------------distrmix11
c 11  DISTRIBUTE arrA1[MULT_BLOCK]
c                     REDISTRIBUTE arrA1[WGT_BLOCK] 
c                     REDISTRIBUTE arrA1[MULT_BLOCK]  
 
      subroutine distrmix11
      integer nproc
      
      integer, parameter :: AN1=64,ER=10000
      integer :: erri=ER,i

      integer, parameter :: m1 = 4, m2 = 2

      double precision :: WB(7) = (/2.1,4.6,3.,2.0,1.5,2.,3.1/)

      integer, allocatable :: A1(:)
      character(*), parameter :: tname='distrmix11  '
               
!dvm$ distribute :: A1    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ redistribute A1(MULT_BLOCK(m1))    

      A1 = 5

!dvm$ actual(A1)

!dvm$ region 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i) + i    
      enddo
!dvm$ end region   

!dvm$ redistribute A1(WGT_BLOCK(WB,7))    

!dvm$ region 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i) + 5 
      enddo
!dvm$ end region   

!dvm$ redistribute A1(MULT_BLOCK(m2))    

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i) on A1(i), reduction(min(erri))
      do i=1,AN1
          if (A1(i) /= i + 10) then     
             erri = min(erri,i)
          endif 
      enddo
!dvm$ end region   

!dvm$ get_actual(erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A1)

      end subroutine distrmix11

C ---------------------------------------------distrmix12
c 12  DISTRIBUTE arrA1[WGT_BLOCK]
c                     REDISTRIBUTE arrA1[MULT_BLOCK]  
c                     REDISTRIBUTE arrA1[WGT_BLOCK]  

      subroutine distrmix12

      integer, parameter :: AN1=75,ER=10000
      integer :: erri=ER,i

      integer, parameter :: m1 = 15

      double precision :: WB1(6) = (/3.1,1.6,2.,3.0,0.5,2./)
      double precision :: WB2(8) 
     >                    = (/1.5,2.1,2.6,4.2,2.5,3.5,1.,2.1/)

      integer, allocatable :: A1(:)
      character(*), parameter :: tname='distrmix12  '
               
!dvm$ distribute :: A1    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ redistribute A1(WGT_BLOCK(WB1,6))    

      A1 = 0

!dvm$ actual(A1)

!dvm$ region 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i) + i    
      enddo
!dvm$ end region   

!dvm$ redistribute A1(MULT_BLOCK(m1))    

!dvm$ region 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i)**2 
      enddo
!dvm$ end region   

!dvm$ redistribute A1(WGT_BLOCK(WB2,8))    

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i) on A1(i), reduction(min(erri))
      do i=1,AN1
          if (A1(i) /= i**2) then     
             erri = min(erri,i)
          endif 
      enddo
!dvm$ end region   

!dvm$ get_actual(erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A1)

      end subroutine distrmix12

C ----------------------------------------------------distrm13
c 13  DISTRIBUTE arrA1[MULT_BLOCK]
c                     REDISTRIBUTE arrA1[GEN_BLOCK] 
c                     REDISTRIBUTE arrA1[MULT_BLOCK]  

      subroutine distrmix13 (nproc)
      integer nproc

      integer, parameter :: AN1=30,ER=10000
      integer :: erri=ER,i

      integer, parameter :: m1 = 5, m2 = 3

      integer :: BS1(1) = (/30/)
      integer :: BS2(2) = (/25,5/)
      integer :: BS3(3) = (/3,15,12/)
      integer :: BS4(4) = (/14,3,11,2/)

      integer, allocatable :: A1(:)
      character(*), parameter :: tname='distrmix13  '
               
!dvm$ distribute A1(MULT_BLOCK(m1))    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ region   
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = i
      enddo
!dvm$ end region   

!!!!dvm$ redistribute A1(GEN_BLOCK(BSnproc))    

      select case(nproc)
      case(1) 
!dvm$ redistribute A1(GEN_BLOCK(BS1))    
      case(2)
!dvm$ redistribute A1(GEN_BLOCK(BS2))    
      case (3)
!dvm$ redistribute A1(GEN_BLOCK(BS3))    
      case(4)
!dvm$ redistribute A1(GEN_BLOCK(BS4))    
      case default 
         goto 10
      endselect  

!dvm$ region 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i)*2
      enddo
!dvm$ end region   

!dvm$ redistribute A1(MULT_BLOCK(m2))    

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i) on A1(i), reduction( min(erri) )
      do i=1,AN1
          if (A1(i) /= i*2) then     
             erri = min(erri,i)
          endif 
      enddo
!dvm$ end region   

!dvm$ get_actual(erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
 10   deallocate (A1)

      end subroutine distrmix13

C ----------------------------------------------------distrmmix14
c 14  DISTRIBUTE arrA1[GEN_BLOCK]
c                     REDISTRIBUTE arrA1[MULT_BLOCK]
c                     REDISTRIBUTE arrA1[GEN_BLOCK] 

      subroutine distrmix14 (nproc)
      integer nproc

      integer, parameter :: AN1=35,ER=10000
      integer :: m1 = 7
      integer :: erri= ER, i

      integer :: BS11(1) = (/35/)
      integer :: BS12(1) = (/35/)
      integer :: BS21(2) = (/15,20/)
      integer :: BS22(2) = (/8,27/)
      integer :: BS31(3) = (/12,17,6/)
      integer :: BS32(3) = (/14,4,17/)
      integer :: BS41(4) = (/5,7,12,11/)  
      integer :: BS42(4) = (/14,10,5,6/)

      integer, allocatable :: A1(:)
      character(*), parameter ::  tname='distrmix14  '
     
!dvm$ distribute :: A1   
!dvm$ dynamic A1
       
      allocate (A1(AN1))

!!!!dvm$ redistribute A1(GEN_BLOCK(BS1))   

      select case(nproc)
      case(1) 
!dvm$ redistribute A1(GEN_BLOCK(BS11))    
      case(2)
!dvm$ redistribute A1(GEN_BLOCK(BS21))    
      case (3)
!dvm$ redistribute A1(GEN_BLOCK(BS31))    
      case(4)
!dvm$ redistribute A1(GEN_BLOCK(BS41))    
      case default 
         goto 10
      endselect  

!dvm$ region 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) =i*4     
      enddo
!dvm$ end region   

!dvm$ redistribute A1(MULT_BLOCK(m1))    

!dvm$ region 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i) + 4     
      enddo
!dvm$ end region   

!!!!dvm$ redistribute A1(GEN_BLOCK(BS2))   

      select case(nproc)
      case(1) 
!dvm$ redistribute A1(GEN_BLOCK(BS12))    
      case(2)
!dvm$ redistribute A1(GEN_BLOCK(BS22))    
      case (3)
!dvm$ redistribute A1(GEN_BLOCK(BS32))    
      case(4)
!dvm$ redistribute A1(GEN_BLOCK(BS42))    
      case default 
         goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) /= i*4 + 4) then     
               erri = min(erri,i)
            endif 
      enddo
!dvm$ end region   

!dvm$ get_actual(erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10   deallocate (A1)

      end subroutine distrmix14

C ----------------------------------------------------distrmix15
c 15  DISTRIBUTE arrA1[WGT_BLOCK]    
c                     REDISTRIBUTE arrA1[GEN_BLOCK] 
c                     REDISTRIBUTE arrA1[WGT_BLOCK] 

      subroutine distrmix15 (nproc)
      integer nproc
       
      integer, parameter :: AN1=10,ER=10000
      integer :: erri= ER, i

      integer :: BS1(1) = (/10/)
      integer :: BS2(2) = (/6,4/)
      integer :: BS3(3) = (/2,4,4/)
      integer :: BS4(4) = (/3,1,4,2/)

      double precision, dimension(6) :: WB1=(/1.0, 2., 2., 3.0, 1., 1./)     
      double precision, dimension(5) :: WB2=(/2.0, 1., 2., 2.0, 2./)     

      integer, allocatable :: A1(:)
      character(*), parameter :: tname='distrmix15  '

!dvm$ distribute A1(WGT_BLOCK(WB1,6))    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ region 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = i
      enddo
!dvm$ end region   
     
      select case(nproc)
      case(1) 
!dvm$ redistribute A1(GEN_BLOCK(BS1))    
      case(2)
!dvm$ redistribute A1(GEN_BLOCK(BS2))    
      case (3)
!dvm$ redistribute A1(GEN_BLOCK(BS3))    
      case(4)
!dvm$ redistribute A1(GEN_BLOCK(BS4))    
      case default 
         goto 10
      endselect  

!dvm$ region 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i)*A1(i)
      enddo
!dvm$ end region   

!dvm$ redistribute A1(WGT_BLOCK(WB2,5))    

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) .ne.i**2) then     
                erri = min(erri,i)
            endif 
      enddo
!dvm$ end region   

!dvm$ get_actual(erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10   deallocate (A1)

      end subroutine distrmix15

C ----------------------------------------------------distrmix16
c 16  DISTRIBUTE arrA1[GEN_BLOCK]     
c                     REDISTRIBUTE arrA1[WGT_BLOCK] 
c                     REDISTRIBUTE arrA1[GEN_BLOCK] 

      subroutine distrmix16 (nproc)
      integer nproc

      integer, parameter :: AN1=12,ER=10000
      integer :: erri= ER, i

      integer :: BS11(1) = (/12/)
      integer :: BS12(1) = (/12/)
      integer :: BS21(2) = (/8,4/)
      integer :: BS22(2) = (/2,10/)	!rem
      integer :: BS31(3) = (/4,4,4/)
      integer :: BS32(3) = (/2,3,7/)
      integer :: BS41(4) = (/2,3,4,3/)
      integer :: BS42(4) = (/6,1,3,2/)

      double precision, dimension(7) ::
     >         WB1=(/1.0, 2., 2., 3.0, 1., 1., 0.5/)     
      double precision, dimension(6) ::
     >         WB2=(/2.0, 0.1, 2.5, 2.0, 2., 0.7/)     

      integer, allocatable :: A1(:)
      character(*), parameter :: tname='distrmix16  '

!dvm$ distribute :: A1    
!dvm$ dynamic A1

      allocate (A1(AN1))

!!!!dvm$ redistribute A1(GEN_BLOCK(BS1))   

      select case(nproc)
      case(1) 
!dvm$ redistribute A1(GEN_BLOCK(BS11))    
      case(2)
!dvm$ redistribute A1(GEN_BLOCK(BS21))    
      case (3)
!dvm$ redistribute A1(GEN_BLOCK(BS31))    
      case(4)
!dvm$ redistribute A1(GEN_BLOCK(BS41))    
      case default 
         goto 10
      endselect  

!dvm$ region 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = i
      enddo
!dvm$ end region   

!dvm$ redistribute A1(WGT_BLOCK(WB1,7))    

!dvm$ region 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i) + A1(i)
      enddo
!dvm$ end region   

!!!!dvm$ redistribute A1(GEN_BLOCK(BS2))   

      select case(nproc)
      case(1) 
!dvm$ redistribute A1(GEN_BLOCK(BS12))    
      case(2)
!dvm$ redistribute A1(GEN_BLOCK(BS22))    
      case (3)
!dvm$ redistribute A1(GEN_BLOCK(BS32))    
      case(4)
!dvm$ redistribute A1(GEN_BLOCK(BS42))    
      case default 
         goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) .ne.i*2) then     
               erri = min(erri,i)
            endif 
      enddo
!dvm$ end region   

!dvm$ get_actual(erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10   deallocate (A1)

      end subroutine distrmix16

C -------------------------------------------------

      subroutine ansyes(name)
      character(*) name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character(*) name
      print *,name,'  -  ***error'
      end
