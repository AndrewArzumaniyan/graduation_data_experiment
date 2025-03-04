      program DISTRGEN1

!    Testing DISTRIBUTE and REDISTRIBUTE directives
!            GEN_BLOCK, BLOCK, *  distributions
      
      integer nproc
      number_of_processors()=1  

      print *,'===START OF distrgen1 ========================'

      nproc = number_of_processors()  

      if (nproc > 4 ) then  ! may be temporary
         goto 1
      endif

C --------------------------------------------------
c 11  DISTRIBUTE arrA1 [GEN_BLOCK]  
c                 REDISTRIBUTE arrA1[BLOCK] 
c                 REDISTRIBUTE arrA1[GEN_BLOCK] 
        call distrg11 (nproc)
C --------------------------------------------------
c 12 DISTRIBUTE arrA1[BLOCK]  
c                REDISTRIBUTE arrA1[GEN_BLOCK] 
c                REDISTRIBUTE arrA1[BLOCK] 
       call distrg12 (nproc)
C --------------------------------------------------
c 13  DISTRIBUTE arrA1 [GEN_BLOCK] 
c                REDISTRIBUTE arrA1[*] 
c                REDISTRIBUTE arrA1[GEN_BLOCK] 
        call distrg13 (nproc)
C --------------------------------------------------
c 14  DISTRIBUTE arrA1[*]    
c                 REDISTRIBUTE arrA1[GEN_BLOCK] 
c                 REDISTRIBUTE arrA1[*] 
        call distrg14 (nproc)
C --------------------------------------------------
c 15  DISTRIBUTE arrA1[GEN_BLOCK] 
c                REDISTRIBUTE arrA1[GEN_BLOCK] 
        call distrg15 (nproc)
C --------------------------------------------------
c 151 DISTRIBUTE arrA1[GEN_BLOCK] 
c                                with 0 in BS.1
c                REDISTRIBUTE arrA1[GEN_BLOCK] 
        call distrg151 (nproc)
C --------------------------------------------------
c 152 DISTRIBUTE arrA1[GEN_BLOCK] 
c                                with 0 in BS.2
c                REDISTRIBUTE arrA1[GEN_BLOCK] 
        call distrg152 (nproc)
C --------------------------------------------------
c 21  DISTRIBUTE arrA2[BLOCK][*] 
c                REDISTRIBUTE arrA2[*][GEN_BLOCK]
        call distrg21 (nproc)
C --------------------------------------------------
c 22  DISTRIBUTE arrA2[*][BLOCK]
c                 REDISTRIBUTE arrA2[GEN_BLOCK][*]
        call distrg22 (nproc)
C --------------------------------------------------
c 23  DISTRIBUTE arrA2[*][GEN_BLOCK]
c                 REDISTRIBUTE arrA2[*][*]
        call distrg23 (nproc)
C --------------------------------------------------
c 24  DISTRIBUTE arrA2[*][*]
c                 REDISTRIBUTE arrA2[GEN_BLOCK][*]
        call distrg24 (nproc)
C -------------------------------------------------

 1    print *,'=== END OF distrgen1 ========================= '    

      end

C ----------------------------------------------------distrg11
c 11  DISTRIBUTE arrA1 [GEN_BLOCK]  
c                 REDISTRIBUTE arrA1[BLOCK] 
c                 REDISTRIBUTE arrA1[GEN_BLOCK] 

      subroutine distrg11 (nproc)
      integer, parameter :: AN1=16,ER=10000
      integer :: erri= ER, i

      integer :: BS11(1) = (/16/)
      integer :: BS12(1) = (/16/)
      integer :: BS21(2) = (/6,10/)
      integer :: BS22(2) = (/8,8/)
      integer :: BS31(3) = (/3,7,6/)
      integer :: BS32(3) = (/4,5,7/)
      integer :: BS41(4) = (/3,4,8,1/)  
      integer :: BS42(4) = (/4,4,5,3/)

      integer, allocatable :: A1(:)
      character(*), parameter :: tname='distrg11  '

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

      A1 = 5

!dvm$ actual(A1)

!dvm$ region  
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i) + i     
      enddo
!dvm$ end region   

!dvm$ redistribute A1(BLOCK)    

!dvm$ region 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i) + 2      
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
            if (A1(i) /= (i+7)) then     
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

      end subroutine distrg11

C ----------------------------------------------------distrg12
c 12 DISTRIBUTE arrA1[BLOCK]                    
c                REDISTRIBUTE arrA1[GEN_BLOCK]  
c                REDISTRIBUTE arrA1[BLOCK] 

      subroutine distrg12 (nproc)
      integer nproc
      
      integer, parameter :: AN1=14,ER=10000
      integer :: erri= ER, i

      integer :: BS1(1) = (/14/)
      integer :: BS2(2) = (/6,8/)
      integer :: BS3(3) = (/3,5,6/)
      integer :: BS4(4) = (/4,3,5,2/)

      integer, allocatable :: A1(:)
      character(*), parameter :: tname='distrg12  '

!dvm$ distribute A1(BLOCK)   
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ region inout (A1) 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = i
      enddo
!dvm$ end region   

!!!!dvm$ redistribute A1(GEN_BLOCK(BS))    

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
            A1(i) = A1(i) + 2
      enddo
!dvm$ end region   

!dvm$ redistribute A1(BLOCK)   

!dvm$ actual(erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            A1(i) = A1(i) - 2
            if (A1(i) /= i) then     
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

      end subroutine distrg12

C ----------------------------------------------------distrg13
c 13  DISTRIBUTE arrA1 [GEN_BLOCK] 
c                REDISTRIBUTE arrA1[*] 
c                REDISTRIBUTE arrA1[GEN_BLOCK] 

      subroutine distrg13 (nproc)
      integer, parameter :: AN1=24,ER=10000
      integer :: erri= ER, i

      integer :: BS11(1) = (/24/)
      integer :: BS12(1) = (/24/)
      integer :: BS21(2) = (/3,21/)
      integer :: BS22(2) = (/17,7/)
      integer :: BS31(3) = (/13,1,10/)
      integer :: BS32(3) = (/4,12,8/)
      integer :: BS41(4) = (/5,7,3,9/)  
      integer :: BS42(4) = (/10,1,12,1/)  

      integer, allocatable :: A1(:)
      character(*), parameter :: tname='distrg13  '

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
      
      A1 = 3

!dvm$ actual(A1)

!dvm$ region inout(A1(:AN1)) 
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i)*i     
      enddo
!dvm$ end region   

!dvm$ redistribute A1(*)    

!dvm$ region  inout(A1)
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i)*2     
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

!dvm$ region inlocal(A1)
!dvm$ parallel (i) on A1(i), reduction( min(erri) )
      do i=1,AN1
            if (A1(i) /= (i*6)) then     
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

      end subroutine distrg13

C ---------------------------------------------distrg14
c 14  DISTRIBUTE arrA1[*]    
c                 REDISTRIBUTE arrA1[GEN_BLOCK] 
c                 REDISTRIBUTE arrA1[*] 

      subroutine distrg14 (nproc)
      integer nproc
      integer, parameter :: AN1=13,ER=10000
      integer :: erri= ER, i

      integer :: BS1(1) = (/13/)
      integer :: BS2(2) = (/6,7/)
      integer :: BS3(3) = (/2,1,10/)
      integer :: BS4(4) = (/4,3,5,1/)

      integer, allocatable :: A1(:)
      character(*), parameter ::  tname='distrg14  '
     
!dvm$ distribute A1(*)   
!dvm$ dynamic A1
       
      allocate (A1(AN1))
      
      A1 = 4

!dvm$ actual(A1)

!dvm$ region  inout(A1(1:AN1))
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) =A1(i)+i     
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

!dvm$ region inout (A1)
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i) - 2     
      enddo
!dvm$ end region   

!dvm$ redistribute A1(*)   

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            A1(i) = A1(i) - 2     
            if (A1(i) /= (i)) then     
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

      end

C ---------------------------------------------distrg15
c 15  DISTRIBUTE arrA1[GEN_BLOCK]   different BS1 and BS2
c                REDISTRIBUTE arrA1[GEN_BLOCK] 

      subroutine distrg15 (nproc)
      integer, parameter :: AN1=15,ER=10000
      integer :: erri= ER, i

      integer :: BS11(1) = (/15/)
      integer :: BS12(1) = (/15/)
      integer :: BS21(2) = (/5,10/)
      integer :: BS22(2) = (/8,7/)
      integer :: BS31(3) = (/2,7,6/)
      integer :: BS32(3) = (/4,4,7/)
      integer :: BS41(4) = (/3,4,7,1/)  
      integer :: BS42(4) = (/4,4,5,2/)

      integer, allocatable :: A1(:)
      character(*), parameter ::  tname='distrg15  '
     
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

!dvm$ region inout (A1(1:5), A1(6:AN1))
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) =i*4     
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

!dvm$ region  in(A1)
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) /= i*4) then     
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

      end subroutine distrg15

C ---------------------------------------------distrg151
c 151 DISTRIBUTE arrA1[GEN_BLOCK] 
c                                with 0 in BS.1
c                REDISTRIBUTE arrA1[GEN_BLOCK] 

      subroutine distrg151 (nproc) 
      integer, parameter :: AN1=9,ER=10000
      integer :: erri= ER, i
     
      integer :: BS11(1) = (/9/)  ! (/0/) causes RTS err 036.027 
      integer :: BS12(1) = (/9/)
      integer :: BS21(2) = (/0,9/)
      integer :: BS22(2) = (/8,1/)
      integer :: BS31(3) = (/2,0,7/)
      integer :: BS32(3) = (/3,5,1/)
      integer :: BS41(4) = (/3,4,2,0/)  
      integer :: BS42(4) = (/4,3,1,1/)

      integer, allocatable :: A1(:)
      character(10), parameter ::  tname='distrg151 '
     
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
      
!dvm$ region in(A1), out(A1)
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) =i*6     
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

!dvm$ region inlocal (A1(1:AN1))
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) /= i*6) then     
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

 10     deallocate (A1)

      end subroutine distrg151

C ---------------------------------------------distrg152
c 152 DISTRIBUTE arrA1[GEN_BLOCK] 
c                                with 0 in BS.2
c                REDISTRIBUTE arrA1[GEN_BLOCK] 

      subroutine distrg152 (nproc)
      integer, parameter :: AN1=10,ER=10000
      integer :: erri= ER, i

      integer :: BS11(1) = (/10/)
      integer :: BS12(1) = (/10/)  ! (/0/) causes RTS err 036.027
      integer :: BS21(2) = (/1,9/)
      integer :: BS22(2) = (/10,0/)
      integer :: BS31(3) = (/2,1,7/)
      integer :: BS32(3) = (/3,7,0/)
      integer :: BS41(4) = (/3,4,2,1/)  
      integer :: BS42(4) = (/4,3,0,3/)

      integer, allocatable :: A1(:)
      character(10), parameter ::  tname='distrg152 '
     
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
            A1(i) = i*2     
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

      end subroutine distrg152

C ----------------------------------------------------distrg21
c 21  DISTRIBUTE arrA2[BLOCK][*] 
c                REDISTRIBUTE arrA2[*][GEN_BLOCK]

      subroutine distrg21 (nproc)
      integer nproc

      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
      integer :: erri= ER, i
      
      integer :: BSj1(1) = (/8/)
      integer :: BSj2(2) = (/6,2/)
      integer :: BSj3(3) = (/2,5,1/)
      integer :: BSj4(4) = (/2,3,1,2/)

      integer, allocatable :: A2(:,:)
      character(*), parameter ::  tname='distrg21  '

!dvm$ distribute A2(BLOCK,*)   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!dvm$ region inout(A2)
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo
!dvm$ end region   

      select case(nproc)
      case(1) 
!dvm$ redistribute A2(*, GEN_BLOCK(BSj1))    
      case(2)
!dvm$ redistribute A2(*, GEN_BLOCK(BSj2))    
      case (3)
!dvm$ redistribute A2(*, GEN_BLOCK(BSj3))    
      case(4)
!dvm$ redistribute A2(*, GEN_BLOCK(BSj4))    
      case default 
      goto 10
      endselect  

!dvm$ actual(erri)

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

!dvm$ get_actual(erri) 

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10   deallocate (A2)

      end subroutine distrg21

C ----------------------------------------------------distrg22
c 22  DISTRIBUTE arrA2[*][BLOCK]
c                 REDISTRIBUTE arrA2[GEN_BLOCK][*]

      subroutine distrg22 (nproc)
      integer nproc
      integer, parameter :: AN1=7,AN2=12,NL=1000,ER=10000
      integer :: erri= ER, i

      integer :: BSi1(1) = (/7/)
      integer :: BSi2(2) = (/6,1/)
      integer :: BSi3(3) = (/2,4,1/)
      integer :: BSi4(4) = (/2,2,1,2/)

      integer, allocatable :: A2(:,:)
      character(10), parameter ::  tname='distrg22  '

!dvm$ distribute A2(*,BLOCK)   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =2*i*NL+j     
          enddo
      enddo
!dvm$ end region   

!!!!dvm$ redistribute A2(GEN_BLOCK(BSi),*)   
       
      select case(nproc)
      case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi1), * )    
      case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi2), * )    
      case (3)
!dvm$ redistribute A2(GEN_BLOCK(BSi3), * )    
      case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi4), * )    
      case default 
      goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region  inlocal(A1)
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (2*i*NL+j)) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region   

!dvm$ get_actual(erri) 

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10   deallocate (A2)

      end subroutine distrg22

C ----------------------------------------------------distr23
c 23  DISTRIBUTE arrA2[*][GEN_BLOCK]
c                 REDISTRIBUTE arrA2[*][*]

      subroutine distrg23 (nproc)
      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
      integer :: erri= ER, i

      integer :: BSj1(1) = (/8/)
      integer :: BSj2(2) = (/6,2/)
      integer :: BSj3(3) = (/2,2,4/)
      integer :: BSj4(4) = (/1,2,2,3/)  

      integer, allocatable :: A2(:,:)
      character(10) :: tname='distrg23  '

!dvm$ distribute :: A2   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!!!!dvm$ redistribute A2(*,GEN_BLOCK(BSj))   
       
      select case(nproc)
      case(1) 
!dvm$ redistribute A2(*, GEN_BLOCK(BSj1))    
      case(2)
!dvm$ redistribute A2(*, GEN_BLOCK(BSj2))    
      case (3)
!dvm$ redistribute A2(*, GEN_BLOCK(BSj3))    
      case(4)
!dvm$ redistribute A2(*, GEN_BLOCK(BSj4))    
      case default 
      goto 10
      endselect  

!dvm$ region    in(A2), out(A2)
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =3*i*NL+j     
          enddo
      enddo
!dvm$ end region   

!dvm$ redistribute A2(*,*)    

!dvm$ actual(erri)

!dvm$ region in(A2), local(A2)
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (3*i*NL+j)) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region   

!dvm$ get_actual(erri) 

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10   deallocate (A2)

      end subroutine distrg23

C ----------------------------------------------------distrg24
c 24  DISTRIBUTE arrA2[*][*]
c                 REDISTRIBUTE arrA2[GEN_BLOCK][*]
      subroutine distrg24 (nproc)
      integer, parameter :: AN1=6,AN2=24,NL=1000,ER=10000

      integer :: erri= ER, i

      integer :: BSi1(1) = (/6/)
      integer :: BSi2(2) = (/5,1/)
      integer :: BSi3(3) = (/2,3,1/)
      integer :: BSi4(4) = (/2,1,1,2/)

      integer, allocatable :: A2(:,:)
      character(10) ::  tname='distrg24  '

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

!!!!dvm$ redistribute A2(GEN_BLOCK(BS1),*)    

      select case(nproc)
      case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi1),*)    
      case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),*)    
      case (3)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),*)    
      case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),*)    
      case default 
      goto 10
      endselect  
                                                                    
!dvm$ actual(erri)

!dvm$ region  inout(A2)
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region   
     
!dvm$ get_actual(erri) 

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10     deallocate (A2)

      end subroutine distrg24

C -------------------------------------------------
      subroutine ansyes(name)
      character(*) name
      print *,name,'  -  complete'
      end

      subroutine ansno(name)
      character(*) name
      print *,name,'  -  ***error'
      end
