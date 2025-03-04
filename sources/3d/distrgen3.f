        program DISTRG3

!    Testing DISTRIBUTE and REDISTRIBUTE directives
!            GEN_BLOCK, BLOCK, *  distributions
      
      integer PROCESSORS_RANK, PROCESSORS_SIZE 
      integer psize(3), rank

      PROCESSORS_RANK() = 3
      PROCESSORS_SIZE(i) = 1

      print *,'===START OF distrgen3========================'

      rank = PROCESSORS_RANK()

      do i=1,rank
         psize(i)=PROCESSORS_SIZE(i)
         if (psize(i) > 4) then   !may be temporary
             goto 1
         endif
      enddo
C -------------------------------------------------
      if
     > ((psize(1) == 1 .and. psize(2) == 1 .and. psize(3) == 1)  !range 1 1 1
     >.or.
     >  (psize(1) == 1 .and. psize(2) == 2 .and. psize(3) == 3)  !range 1 2 3
     >.or.
     >  (psize(1) == 2 .and. psize(2) == 3 .and. psize(3) == 2)  !range 2 3 2
     >.or.
     >  (psize(1) == 3 .and. psize(2) == 1 .and. psize(3) == 4)  !range 3 1 4
     >.or.
     >  (psize(1) == 4 .and. psize(2) == 2 .and. psize(3) == 2)) !range 4 2 2
     >then
! 31  DISTRIBUTE arrA3[GEN_BLOCK][GEN_BLOCK][GEN_BLOCK]  
!                REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK][GEN_BLOCK] other blocks
        call distrg31 (psize)
      endif
! -------------------------------------------------
      if
     > ((psize(1) == 1 .and. psize(2) == 1 .and. psize(3) == 1)  !range 1 1 1
     >.or.
     >  (psize(1) == 1 .and. psize(2) == 3 .and. psize(3) == 4)  !range 1 3 4
     >.or.
     >  (psize(1) == 2 .and. psize(2) == 2 .and. psize(3) == 3)  !range 2 2 3
     >.or.
     >  (psize(1) == 3 .and. psize(2) == 4 .and. psize(3) == 1)  !range 3 4 1
     >.or.
     >  (psize(1) == 4 .and. psize(2) == 2 .and. psize(3) == 2)) !range 4 2 2
     >then
! 32 DISTRIBUTE arrA3[GEN_BLOCK][GEN_BLOCK][GEN_BLOCK] 
!                REDISTRIBUTE [BLOCK][BLOCK][BLOCK] 
!                REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK][GEN_BLOCK] 
        call distrg32 (psize)
      endif
! -------------------------------------------------
! 33  DISTRIBUTE arrA3[BLOCK][GEN_BLOCK][GEN_BLOCK]   allocatable 
!     DISTRIBUTE arrB3[BLOCK][GEN_BLOCK][GEN_BLOCK]   static
!                REDISTRIBUTE [GEN_BLOCK][BLOCK][BLOCK]
!                REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK][BLOCK]
        call distrg33  (psize)
! -------------------------------------------------
      if
     > ((psize(1) == 1 .and. psize(2) == 1 .and. psize(3) == 1)  !range 1 1 1
     >.or.
     >  (psize(1) == 1 .and. psize(2) == 2 .and. psize(3) == 2)  !range 1 2 2
     >.or.
     >  (psize(1) == 2 .and. psize(2) == 4 .and. psize(3) == 2)  !range 2 4 2
     >.or.
     >  (psize(1) == 3 .and. psize(2) == 1 .and. psize(3) == 2)  !range 3 1 2
     >.or.
     >  (psize(1) == 4 .and. psize(2) == 1 .and. psize(3) == 4) !range 4 1 4
     >) then
! 34 DISTRIBUTE arrA3[GEN_BLOCK][GEN_BLOCK][GEN_BLOCK]  !static 
!                REDISTRIBUTE [GEN_BLOCK][*][BLOCK]
!      DISTRIBUTE arrB3[GEN_BLOCK][*][BLOCK]            !static
!                 REDISTRIBUTE [BLOCK][GEN_BLOCK][*]
        call distrg34 (psize)
      endif
! -------------------------------------------------
      if
     > ((psize(1) == 1 .and. psize(2) == 1 .and. psize(3) == 1)  !range 1 1 1
     >.or.
     >  (psize(1) == 1 .and. psize(2) == 3 .and. psize(3) == 2)  !range 1 3 2
     >.or.
     >  (psize(1) == 2 .and. psize(2) == 2 .and. psize(3) == 4)  !range 2 2 4
     >.or.
     >  (psize(1) == 3 .and. psize(2) == 2 .and. psize(3) == 2)  !range 3 2 2
     >.or.
     >  (psize(1) == 4 .and. psize(2) == 2 .and. psize(3) == 2)) !range 4 2 2
     >then

! 35  DISTRIBUTE arrA3[GEN_BLOCK][GEN_BLOCK][GEN_BLOCK] 
!                REDISTRIBUTE arrA2[*][*][*]
!                REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK][GEN_BLOCK]
        call distrg35 (psize)
      endif
! -------------------------------------------------
! 36 DISTRIBUTE arrA3[GEN_BLOCK][BLOCK][GEN_BLOCK]   
!                REDISTRIBUTE [BLOCK][GEN_BLOCK][BLOCK]
!                REDISTRIBUTE [BLOCK][GEN_BLOCK][GEN_BLOCK]
!                REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK][BLOCK]
        call distrg36 (psize)
! -------------------------------------------------
! 37 DISTRIBUTE arrA3[GEN_BLOCK][GEN_BLOCK][BLOCK]
!                REDISTRIBUTE [BLOCK][GEN_BLOCK][BLOCK]
!                REDISTRIBUTE [BLOCK][GEN_BLOCK][GEN_BLOCK]
!                REDISTRIBUTE [GEN_BLOCK][BLOCK][GEN_BLOCK]
      call distrg37 (psize)
! -------------------------------------------------
! 38  DISTRIBUTE arrA3 [GEN_BLOCK][*][GEN_BLOCK] 
!                REDISTRIBUTE [*][GEN_BLOCK][*]
!                REDISTRIBUTE[GEN_BLOCK][GEN_BLOCK][*]
!                REDISTRIBUTE[*][GEN_BLOCK][GEN_BLOCK]
      call distrg38 (psize)
! -------------------------------------------------
! 39 DISTRIBUTE arrA3 [*][GEN_BLOCK][GEN_BLOCK]  
!                REDISTRIBUTE [GEN_BLOCK][*][*]
!                REDISTRIBUTE[GEN_BLOCK][*][GEN_BLOCK]
      call distrg39 (psize)
! -------------------------------------------------
! 310 DISTRIBUTE arrA3 [GEN_BLOCK][GEN_BLOCK][BLOCK]  
!                REDISTRIBUTE [*][*][GEN_BLOCK]
!                REDISTRIBUTE[*][GEN_BLOCK][BLOCK]
       call distrg310 (psize)
! -------------------------------------------------
! 311 DISTRIBUTE arrA3 [GEN_BLOCK][*][*]  
!                REDISTRIBUTE [*][*][*]
!                REDISTRIBUTE [BLOCK][*][GEN_BLOCK]
        call distrg311 (psize)

! -------------------------------------------------

 1    print *,'=== END OF distrgen3 ========================= '    

      end
                                             
! ----------------------------------------------------distrg31
! 31  DISTRIBUTE arrA3[GEN_BLOCK][GEN_BLOCK][GEN_BLOCK]     range 1 1 1
!        REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK][GEN_BLOCK]     range 1 2 3
!                                                           range 2 3 2
!                                                           range 3 1 4
!                                                           range 4 2 2
      subroutine distrg31 (psize)
      integer psize(3)

      integer, parameter :: AN1=6,AN2=6,AN3=6,NL=1000,ER=10000
      integer :: erri=ER,i,j,k
 
      integer, dimension(1) :: BSi111=(/6/)       !range 1 1 1  
      integer, dimension(1) :: BSj111=(/6/)     
      integer, dimension(1) :: BSk111=(/6/)     

      integer, dimension(1) :: BSi11=(/6/)        !range 1 2 3  
      integer, dimension(1) :: BSi12=(/6/)           
      integer, dimension(2) :: BSj11=(/5,1/)     
      integer, dimension(2) :: BSj12=(/2,4/)     
      integer, dimension(3) :: BSk11=(/1,4,1/)     
      integer, dimension(3) :: BSk12=(/1,2,3/)     

      integer, dimension(2) :: BSi21=(/4,2/)      !range 2 3 2  
      integer, dimension(2) :: BSi22=(/1,5/)     
      integer, dimension(3) :: BSj21=(/3,2,1/)     
      integer, dimension(3) :: BSj22=(/5,1,0/)     
      integer, dimension(2) :: BSk21=(/2,4/)     
      integer, dimension(2) :: BSk22=(/5,1/)    

      integer, dimension(3) :: BSi31=(/2,2,2/)    !range 3 1 4  
      integer, dimension(3) :: BSi32=(/3,2,1/)     
      integer, dimension(1) :: BSj31=(/6/)     
      integer, dimension(1) :: BSj32=(/6/)     
      integer, dimension(4) :: BSk31=(/1,2,2,1/)     
      integer, dimension(4) :: BSk32=(/2,2,1,1/)     

      integer, dimension(4) :: BSi41=(/2,1,2,1/)  !range 4 2 2  
      integer, dimension(4) :: BSi42=(/1,2,1,2/)     
      integer, dimension(2) :: BSj41=(/5,1/)     
      integer, dimension(2) :: BSj42=(/2,4/)     
      integer, dimension(2) :: BSk41=(/4,2/)     
      integer, dimension(2) :: BSk42=(/6,0/)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrg31  '

!dvm$ distribute :: A3
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!!!!dvm$ distribute A3(GEN_BLOCK(BSi1),GEN_BLOCK(BSj1),GEN_BLOCK(BSk1))   

      select case(psize(1))
      case(1) 
        if (psize(2) == 1 .and. psize(3) == 1) then
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi111),GEN_BLOCK(BSj111),GEN_BLOCK(BSk111))   
        else
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi11),GEN_BLOCK(BSj11),GEN_BLOCK(BSk11))   
        endif
      case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi21),GEN_BLOCK(BSj21),GEN_BLOCK(BSk21))   
      case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi31),GEN_BLOCK(BSj31),GEN_BLOCK(BSk31))   
      case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi41),GEN_BLOCK(BSj41),GEN_BLOCK(BSk41))   
      case default 
      goto 10
      endselect  

!dvm$ region  out(A3)
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = i*NL/10 + j*NL/100 + k    
             enddo
          enddo
      enddo
!dvm$ end region   
  

!!!!dvm$  redistribute A3(GEN_BLOCK(BSi2),GEN_BLOCK(BSj2),GEN_BLOCK(BSk2))   

      select case(psize(1))
      case(1) 
        if (psize(2) == 1 .and. psize(3) == 1) then
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi111),GEN_BLOCK(BSj111),GEN_BLOCK(BSk111))   
        else
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi12),GEN_BLOCK(BSj12),GEN_BLOCK(BSk12))   
        endif
      case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi22),GEN_BLOCK(BSj22),GEN_BLOCK(BSk22))   
      case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi32),GEN_BLOCK(BSj32),GEN_BLOCK(BSk32))   
      case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi42),GEN_BLOCK(BSj42),GEN_BLOCK(BSk42))   
      case default 
      goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region in(A3) 
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
     
!dvm$ get_actual(erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10   deallocate (A3)

      end subroutine distrg31

! ----------------------------------------------------distrg32
! 32  DISTRIBUTE arrA3[GEN_BLOCK][GEN_BLOCK][GEN_BLOCK]         range 1 1 1 
!                REDISTRIBUTE [BLOCK][BLOCK][BLOCK]             range 1 3 4
!                REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK][GEN_BLOCK] range 2 2 3
!                                                               range 3 4 1
!                                                               range 4 2 2
      subroutine distrg32 (psize)
      integer psize(3)

      integer, parameter :: AN1=8,AN2=6,AN3=14,NL=1000,ER=10000
      integer :: erri=ER,i,j,k

      integer, dimension(1) :: BSi111=(/8/)        !range 1 1 1  
      integer, dimension(1) :: BSj111=(/6/)     
      integer, dimension(1) :: BSk111=(/14/)     

      integer, dimension(1) :: BSi11=(/8/)         !range 1 3 4
      integer, dimension(1) :: BSi12=(/8/)           
      integer, dimension(3) :: BSj11=(/2,4,0/)     
      integer, dimension(3) :: BSj12=(/3,2,1/)     
      integer, dimension(4) :: BSk11=(/1,4,3,6/)     
      integer, dimension(4) :: BSk12=(/5,2,4,3/)     

      integer, dimension(2) :: BSi21=(/6,2/)       !range 2 2 3  
      integer, dimension(2) :: BSi22=(/1,7/)     
      integer, dimension(2) :: BSj21=(/3,3/)     
      integer, dimension(2) :: BSj22=(/5,1/)     
      integer, dimension(3) :: BSk21=(/10,3,1/)     
      integer, dimension(3) :: BSk22=(/4,8,2/)    

      integer, dimension(3) :: BSi31=(/3,2,3/)     !range 3 4 1 
      integer, dimension(3) :: BSi32=(/2,4,2/)     
      integer, dimension(4) :: BSj31=(/2,1,1,2/)     
      integer, dimension(4) :: BSj32=(/1,2,3,0/)     
      integer, dimension(1) :: BSk31=(/14/)     
      integer, dimension(1) :: BSk32=(/14/)     

      integer, dimension(4) :: BSi41=(/3,2,1,2/)   !range 4 2 2  
      integer, dimension(4) :: BSi42=(/4,1,2,1/)     
      integer, dimension(2) :: BSj41=(/5,1/)     
      integer, dimension(2) :: BSj42=(/2,4/)     
      integer, dimension(2) :: BSk41=(/7,7/)     
      integer, dimension(2) :: BSk42=(/6,8/)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrg32  '

!dvm$ distribute :: A3   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!!!!dvm$ distribute A3(GEN_BLOCK(BSi),GEN_BLOCK(BSj),GEN_BLOCK(BSk))   

      select case(psize(1))
      case(1) 
        if (psize(2) == 1 .and. psize(3) == 1) then
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi111),GEN_BLOCK(BSj111),GEN_BLOCK(BSk111))   
        else
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi11),GEN_BLOCK(BSj11),GEN_BLOCK(BSk11))   
         endif
      case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi21),GEN_BLOCK(BSj21),GEN_BLOCK(BSk21))   
      case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi31),GEN_BLOCK(BSj31),GEN_BLOCK(BSk31))   
      case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi41),GEN_BLOCK(BSj41),GEN_BLOCK(BSk41))   
      case default 
         goto 10
      endselect  

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = i*NL/10 + j*NL/100 + k + 1   
             enddo
          enddo
      enddo
!dvm$ end region   

!dvm$ redistribute A3(BLOCK,BLOCK,BLOCK)   

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) + 1   
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$ redistribute A3(GEN_BLOCK(BSi),GEN_BLOCK(BSj),GEN_BLOCK(BSk))   

      select case(psize(1))
      case(1) 
        if (psize(2) == 1 .and. psize(3) == 1) then
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi111),GEN_BLOCK(BSj111),GEN_BLOCK(BSk111))   
        else
!dvm$ redistribute 
!dvm$*    A3(GEN_BLOCK(BSi12),GEN_BLOCK(BSj12),GEN_BLOCK(BSk12))   
        endif
      case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi22),GEN_BLOCK(BSj22),GEN_BLOCK(BSk22))   
      case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi32),GEN_BLOCK(BSj32),GEN_BLOCK(BSk32))   
      case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi42),GEN_BLOCK(BSj42),GEN_BLOCK(BSk42))   
      case default 
         goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region   inlocal(A3)
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction(min(erri))
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k + 2)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region   
     
!dvm$ get_actual(erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10   deallocate (A3)

      end subroutine distrg32

! ----------------------------------------------------distrg33
! 33  DISTRIBUTE arrA3[BLOCK][GEN_BLOCK][GEN_BLOCK]   allocatable 
!     DISTRIBUTE arrB3[BLOCK][GEN_BLOCK][GEN_BLOCK]   static
!                REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK][BLOCK]
!                REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK][BLOCK]

      subroutine distrg33 (psize)
      integer psize(3)

      integer, parameter :: AN1=12,AN2=17,AN3=16,NL=1000,ER=10000
      integer, parameter :: BN1=10,BN2=10,BN3=10
      integer :: erria=ER, errib=ER,i,j,k

      integer, dimension(1) :: BSai11=(/12/)      
      integer, dimension(1) :: BSai12=(/12/)           
      integer, dimension(2) :: BSai21=(/3,9/)       
      integer, dimension(2) :: BSai22=(/11,1/)     
      integer, dimension(3) :: BSai31=(/4,0,8/)     
      integer, dimension(3) :: BSai32=(/6,4,2/)     
      integer, dimension(4) :: BSai41=(/1,2,3,6/)   !rem
      integer, dimension(4) :: BSai42=(/3,8,0,1/)     

      integer, dimension(1) :: BSaj11=(/17/)     
      integer, dimension(1) :: BSaj12=(/17/)     
      integer, dimension(2) :: BSaj21=(/3,14/)     
      integer, dimension(2) :: BSaj22=(/4,13/)     
      integer, dimension(3) :: BSaj31=(/6,1,10/)     
      integer, dimension(3) :: BSaj32=(/11,6,0/)     
      integer, dimension(4) :: BSaj41=(/5,0,11,1/)     
      integer, dimension(4) :: BSaj42=(/11,3,1,2/)     

      integer, dimension(1) :: BSak11=(/16/)     
      integer, dimension(1) :: BSak12=(/16/)     
      integer, dimension(2) :: BSak21=(/12,4/)     
      integer, dimension(2) :: BSak22=(/7,9/)    	!rem
      integer, dimension(3) :: BSak31=(/2,4,10/)     
      integer, dimension(3) :: BSak32=(/3,1,12/)     
      integer, dimension(4) :: BSak41=(/6,2,5,3/)     
      integer, dimension(4) :: BSak42=(/1,7,6,2/)     


      integer, dimension(1) :: BSbi11=(/10/)        
      integer, dimension(1) :: BSbi12=(/10/)           
      integer, dimension(2) :: BSbi21=(/3,7/)       
      integer, dimension(2) :: BSbi22=(/1,9/)     
      integer, dimension(3) :: BSbi31=(/3,2,5/)     
      integer, dimension(3) :: BSbi32=(/2,6,2/)     
      integer, dimension(4) :: BSbi41=(/1,2,5,2/)   
      integer, dimension(4) :: BSbi42=(/3,1,0,6/)     

      integer, dimension(1) :: BSbj11=(/10/)     
      integer, dimension(1) :: BSbj12=(/10/)     
      integer, dimension(2) :: BSbj21=(/6,4/)     
      integer, dimension(2) :: BSbj22=(/7,3/)     
      integer, dimension(3) :: BSbj31=(/1,5,4/)     
      integer, dimension(3) :: BSbj32=(/3,1,6/)     
      integer, dimension(4) :: BSbj41=(/5,0,2,3/)     
      integer, dimension(4) :: BSbj42=(/2,3,4,1/)     

      integer, dimension(1) :: BSbk11=(/10/)     
      integer, dimension(1) :: BSbk12=(/10/)     
      integer, dimension(2) :: BSbk21=(/5,5/)     
      integer, dimension(2) :: BSbk22=(/2,8/)    
      integer, dimension(3) :: BSbk31=(/1,1,8/)     
      integer, dimension(3) :: BSbk32=(/3,5,2/)     
      integer, dimension(4) :: BSbk41=(/1,2,3,4/)     
      integer, dimension(4) :: BSbk42=(/4,3,2,1/)     

      integer, allocatable :: A3(:,:,:)
      integer B3(BN1,BN2,BN3)
      character(10), parameter :: tname='distrg33  '

!dvm$ distribute :: A3
!dvm$ distribute :: B3
!dvm$ dynamic A3, B3

      allocate (A3(AN1,AN2,AN3))

!!!!dvm$ redistribute A3(BLOCK,GEN_BLOCK(BSaj1),GEN_BLOCK(BSak1))   
!!!!dvm$ redistribute B3(BLOCK,GEN_BLOCK(BSbj1),GEN_BLOCK(BSbk1))   

      select case(psize(2))
      case(1)
         select case(psize(3))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSaj11),GEN_BLOCK(BSak11))   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj11),GEN_BLOCK(BSbk11))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSaj11),GEN_BLOCK(BSak21))   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj11),GEN_BLOCK(BSbk21))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSaj11),GEN_BLOCK(BSak31))   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj11),GEN_BLOCK(BSbk31))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSaj11),GEN_BLOCK(BSak41))   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj11),GEN_BLOCK(BSbk41))   
         case default 
           goto 10
         endselect  

      case(2)
         select case(psize(3))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSaj21),GEN_BLOCK(BSak11))   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj21),GEN_BLOCK(BSbk11))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSaj21),GEN_BLOCK(BSak21))   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj21),GEN_BLOCK(BSbk21))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSaj21),GEN_BLOCK(BSak31))   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj21),GEN_BLOCK(BSbk31))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSaj21),GEN_BLOCK(BSak41))   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj21),GEN_BLOCK(BSbk41))   
         case default 
           goto 10
         endselect  

      case (3)
         select case(psize(3))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSaj31),GEN_BLOCK(BSak11))   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj31),GEN_BLOCK(BSbk11))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSaj31),GEN_BLOCK(BSak21))   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj31),GEN_BLOCK(BSbk21))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSaj31),GEN_BLOCK(BSak31))   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj31),GEN_BLOCK(BSbk31))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSaj31),GEN_BLOCK(BSak41))   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj31),GEN_BLOCK(BSbk41))   
         case default 
           goto 10
         endselect  

      case (4)
         select case(psize(3))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSaj41),GEN_BLOCK(BSak11))   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj41),GEN_BLOCK(BSbk11))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSaj41),GEN_BLOCK(BSak21))   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj41),GEN_BLOCK(BSbk21))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSaj41),GEN_BLOCK(BSak31))   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj41),GEN_BLOCK(BSbk31))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSaj41),GEN_BLOCK(BSak41))   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj41),GEN_BLOCK(BSbk41))   
         case default 
           goto 10
         endselect  

      case default 
        goto 10
      endselect  

!dvm$ region   out (A3, B3)
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = i*NL/10 + j*NL/100 + k    
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,k) on B3(i,j,k)
      do i=1,BN1
          do j=1,BN2
             do k=1,BN3
                B3(i,j,k) = (i*NL/10 + j*NL/100 + k) * 2   
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$ redistribute A3(GEN_BLOCK(BSai2),BLOCK,BLOCK)   
!!!!dvm$ redistribute B3(GEN_BLOCK(BSbi2),BLOCK,BLOCK)   

      select case(psize(1))
      case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai11),BLOCK,BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi11),BLOCK,BLOCK)   
      case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai21),BLOCK,BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi21),BLOCK,BLOCK)   
      case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai31),BLOCK,BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi31),BLOCK,BLOCK)   
      case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai41),BLOCK,BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi41),BLOCK,BLOCK)   
      case default 
         goto 10
      endselect  

!dvm$ region  inout (A3, B3)
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) * 2   
             enddo
          enddo
      enddo
!dvm$ parallel (i,j,k) on B3(i,j,k)
      do i=1,BN1
          do j=1,BN2
             do k=1,BN3
                B3(i,j,k) = B3(i,j,k)/ 2   
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$ redistribute A3(GEN_BLOCK(BSai2),GEN_BLOCK(BSaj2),BLOCK)   
!!!!dvm$ redistribute A3(GEN_BLOCK(BSbi2),GEN_BLOCK(BSbj2),BLOCK)   

      select case(psize(1))
      case(1)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai12),GEN_BLOCK(BSaj12),BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi12),GEN_BLOCK(BSbj12),BLOCK)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai12),GEN_BLOCK(BSaj22),BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi12),GEN_BLOCK(BSbj22),BLOCK)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai12),GEN_BLOCK(BSaj32),BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi12),GEN_BLOCK(BSbj32),BLOCK)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai12),GEN_BLOCK(BSaj42),BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi12),GEN_BLOCK(BSbj42),BLOCK)   
         case default 
            goto 10
         endselect  

      case(2)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai22),GEN_BLOCK(BSaj12),BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi22),GEN_BLOCK(BSbj12),BLOCK)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai22),GEN_BLOCK(BSaj22),BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi22),GEN_BLOCK(BSbj22),BLOCK)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai22),GEN_BLOCK(BSaj32),BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi22),GEN_BLOCK(BSbj32),BLOCK)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai22),GEN_BLOCK(BSaj42),BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi22),GEN_BLOCK(BSbj42),BLOCK)   
         case default 
            goto 10
         endselect  

      case (3)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai32),GEN_BLOCK(BSaj12),BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi32),GEN_BLOCK(BSbj12),BLOCK)   !rem
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai32),GEN_BLOCK(BSaj22),BLOCK)   !rem
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi32),GEN_BLOCK(BSbj22),BLOCK)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai32),GEN_BLOCK(BSaj32),BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi32),GEN_BLOCK(BSbj32),BLOCK)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai32),GEN_BLOCK(BSaj42),BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi32),GEN_BLOCK(BSbj42),BLOCK)   
         case default 
           goto 10
         endselect  

      case (4)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai42),GEN_BLOCK(BSaj12),BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi42),GEN_BLOCK(BSbj12),BLOCK)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai42),GEN_BLOCK(BSaj22),BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi42),GEN_BLOCK(BSbj22),BLOCK)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai42),GEN_BLOCK(BSaj32),BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi42),GEN_BLOCK(BSbj32),BLOCK)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai42),GEN_BLOCK(BSaj42),BLOCK)   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi42),GEN_BLOCK(BSbj42),BLOCK)   
         case default 
            goto 10
         endselect  

      case default 
         goto 10
      endselect  

!dvm$ actual(erria, errib)

!dvm$ region inlocal (a3), inlocal (B3) 
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction(min(erria))
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                A3(i,j,k) = A3(i,j,k)/ 2   
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k)) then     
                    erria = min(erria,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ parallel (i,j,k) on B3(i,j,k), reduction(min(errib))
      do i=1,BN1
          do j=1,BN2
             do k=1,BN3
                if (B3(i,j,k) /= (i*NL/10 + j*NL/100 + k)) then     
                    errib = min(errib,i*NL/10 + j*NL/100 + k)
                endif 
             enddo
          enddo
      enddo
!dvm$ end region   

!dvm$ get_actual(erria, errib)

      if ((erria == ER) .and. (errib == ER)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10   deallocate (A3)

      end subroutine distrg33

! ----------------------------------------------------distrg34
! 34 DISTRIBUTE arrA3[GEN_BLOCK][GEN_BLOCK][GEN_BLOCK]    static  range 1 1 1
!                REDISTRIBUTE [GEN_BLOCK][*][BLOCK]               range 1 2 2
!     DISTRIBUTE arrB3[GEN_BLOCK][*][BLOCK]               static  range 2 4 2
!                 REDISTRIBUTE [BLOCK][GEN_BLOCK][*]              range 3 1 2
!                                                                 range 4 1 4
      subroutine distrg34 (psize)
      integer psize(3)

      integer, parameter :: AN1=16,AN2=16,AN3=16,NL=1000,ER=10000
      integer, parameter :: BN1=12,BN2=17,BN3=11
      integer :: erria=ER, errib=ER,i,j,k

      integer, dimension(1) :: BSai111=(/16/)       !range 1 1 1  
      integer, dimension(1) :: BSaj111=(/16/)     
      integer, dimension(1) :: BSak111=(/16/)     

      integer, dimension(1) :: BSai11=(/16/)        !range 1 2 2  
      integer, dimension(1) :: BSai12=(/16/)           
      integer, dimension(2) :: BSaj11=(/6,10/)     
      integer, dimension(2) :: BSaj12=(/4,12/)     
      integer, dimension(2) :: BSak11=(/10,6/)     
      integer, dimension(2) :: BSak12=(/12,4/)     

      integer, dimension(2) :: BSai21=(/2,14/)      !range 2 4 2  
      integer, dimension(2) :: BSai22=(/13,3/)     
      integer, dimension(4) :: BSaj21=(/3,4,3,6/)     
      integer, dimension(4) :: BSaj22=(/6,6,2,2/)     
      integer, dimension(2) :: BSak21=(/1,15/)     
      integer, dimension(2) :: BSak22=(/7,9/)    

      integer, dimension(3) :: BSai31=(/3,2,11/)    !range 3 1 2  
      integer, dimension(3) :: BSai32=(/2,12,2/)     
      integer, dimension(1) :: BSaj31=(/16/)     
      integer, dimension(1) :: BSaj32=(/16/)     
      integer, dimension(2) :: BSak31=(/3,13/)     
      integer, dimension(2) :: BSak32=(/4,12/)     

      integer, dimension(4) :: BSai41=(/1,2,5,8/)   !range 4 1 4  
      integer, dimension(4) :: BSai42=(/3,11,0,2/)     
      integer, dimension(1) :: BSaj41=(/16/)     
      integer, dimension(1) :: BSaj42=(/16/)     
c      integer, dimension(4) :: BSak41=(/1,5,0,10/)     
      integer, dimension(4) :: BSak41=(/1,5,2,8/)     !rem
      integer, dimension(4) :: BSak42=(/6,2,5,3/)     

      integer, dimension(1) :: BSbi111=(/12/)        !range 1 1 1  
      integer, dimension(1) :: BSbj111=(/17/)     
      integer, dimension(1) :: BSbk111=(/11/)     


      integer, dimension(1) :: BSbi11=(/12/)        !range 1 2 2  
      integer, dimension(1) :: BSbi12=(/12/)           
      integer, dimension(2) :: BSbj11=(/6,11/)     
      integer, dimension(2) :: BSbj12=(/14,3/)     
      integer, dimension(2) :: BSbk11=(/10,1/)     
      integer, dimension(2) :: BSbk12=(/4,7/)     

      integer, dimension(2) :: BSbi21=(/5,7/)       !range 2 4 2  
      integer, dimension(2) :: BSbi22=(/3,9/)     
      integer, dimension(4) :: BSbj21=(/5,2,8,2/)     
      integer, dimension(4) :: BSbj22=(/7,3,2,5/)     
      integer, dimension(2) :: BSbk21=(/5,6/)     
      integer, dimension(2) :: BSbk22=(/3,8/)    

      integer, dimension(3) :: BSbi31=(/3,4,5/)     !range 3 1 2  
      integer, dimension(3) :: BSbi32=(/4,6,2/)     
      integer, dimension(1) :: BSbj31=(/17/)     
      integer, dimension(1) :: BSbj32=(/17/)     
      integer, dimension(2) :: BSbk31=(/4,7/)     
      integer, dimension(2) :: BSbk32=(/8,3/)     

      integer, dimension(4) :: BSbi41=(/4,1,5,2/)   !range 4 1 4  
c      integer, dimension(4) :: BSbi42=(/3,4,2,4/)     
      integer, dimension(4) :: BSbi42=(/3,4,2,3/)     
      integer, dimension(1) :: BSbj41=(/17/)     
      integer, dimension(1) :: BSbj42=(/17/)     
c      integer, dimension(4) :: BSbk41=(/1,4,2,5/)     
      integer, dimension(4) :: BSbk41=(/1,4,2,4/)     
      integer, dimension(4) :: BSbk42=(/2,3,4,2/)     

      integer A3(AN1,AN2,AN3), B3(BN1,BN2,BN3)
      character(10), parameter :: tname='distrg34  '

!dvm$ distribute :: A3
!dvm$ distribute :: B3
!dvm$ dynamic A3, B3

!!!!dvm$ redistribute A3(GEN_BLOCK(BSai1),GEN_BLOCK(BSaj1),GEN_BLOCK(BSak1))   
!!!!dvm$ redistribute B3(GEN_BLOCK(BSbi1),*, BLOCK)   

      select case(psize(1))
      case(1) 
        if (psize(2) == 1 .and. psize(3) == 1) then
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai111),GEN_BLOCK(BSaj111),GEN_BLOCK(BSak111))   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi111),*, BLOCK)   
         else
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai11),GEN_BLOCK(BSaj11),GEN_BLOCK(BSak11))   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi11),*, BLOCK)   
         endif
      case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai21),GEN_BLOCK(BSaj21),GEN_BLOCK(BSak21))   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi21),*, BLOCK)   
      case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai31),GEN_BLOCK(BSaj31),GEN_BLOCK(BSak31))   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi31),*, BLOCK)   
      case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai41),GEN_BLOCK(BSaj41),GEN_BLOCK(BSak41))   
!dvm$ redistribute
!dvm$*    B3(GEN_BLOCK(BSbi41),*, BLOCK)   
      case default 
        goto 10
      endselect  
      A3 = 10
      B3 = 7
!dvm$ actual(A3, B3)

!dvm$ region  in(A3,B3), out(A3,B3) 
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) + i*NL/10 + j*NL/100 + k  
             enddo
          enddo
      enddo
   
!dvm$ parallel (i,j,k) on B3(i,j,k)
      do i=1,BN1
          do j=1,BN2
             do k=1,BN3
                B3(i,j,k) = B3(i,j,k) + (i*NL/10 + j*NL/100 + k)   
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$ redistribute A3(GEN_BLOCK(BSai1),*,BLOCK)   
!!!!dvm$ redistribute B3(BLOCK, GEN_BLOCK(BSbj2),*)   

      select case(psize(1))
      case(1) 
        if (psize(2) == 1 .and. psize(3) == 1) then
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai111),*,BLOCK)   
!dvm$ redistribute
!dvm$*    B3(BLOCK, GEN_BLOCK(BSbj111),*)   
         else
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai12),*,BLOCK)   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj12),*)   
          endif
      case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai22),*,BLOCK)   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj22),*)   
      case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai32),*,BLOCK)   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj32),*)   
      case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSai42),*,BLOCK)   
!dvm$ redistribute
!dvm$*    B3(BLOCK,GEN_BLOCK(BSbj42),*)   
      case default 
      goto 10
      endselect  

!dvm$ actual(erria, errib)

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction(min(erria))
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                A3(i,j,k) = A3(i,j,k) - 10   
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k)) then     
                    erria = min(erria,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ parallel (i,j,k) on B3(i,j,k), reduction (min(errib))
      do i=1,BN1
          do j=1,BN2
             do k=1,BN3
                if (B3(i,j,k) /= (i*NL/10 + j*NL/100 + k+ 7)) then     
                    errib = min(errib,i*NL/10 + j*NL/100 + k)
                endif 
             enddo
          enddo
      enddo
!dvm$ end region   

!dvm$ get_actual(erria, errib)

      if ((erria == ER) .and. (errib == ER)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 10   continue
      end subroutine distrg34

! ----------------------------------------------------distrg35
! 35  DISTRIBUTE arrA3[GEN_BLOCK][GEN_BLOCK][GEN_BLOCK]         range 1 1 1
!                REDISTRIBUTE arrA2[*][*][*]                    range 1 3 2
!                REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK][GEN_BLOCK] range 2 2 4
!                                                               range 3 2 2
!                                                               range 4 2 2
      subroutine distrg35 (psize)
      integer psize(3)

      integer, parameter :: AN1=16,AN2=16,AN3=16,NL=1000,ER=10000
      integer :: erri=ER,i,j,k

      integer, dimension(1) :: BSi111=(/16/)        !range 1 1 1  
      integer, dimension(1) :: BSj111=(/16/)     
      integer, dimension(1) :: BSk111=(/16/)     

      integer, dimension(1) :: BSi11=(/16/)         !range 1 3 2  
      integer, dimension(1) :: BSi12=(/16/)           
      integer, dimension(3) :: BSj11=(/6,5,5/)     
      integer, dimension(3) :: BSj12=(/3,2, 11/)     
      integer, dimension(2) :: BSk11=(/12,4/)     
      integer, dimension(2) :: BSk12=(/10,6/)     

      integer, dimension(2) :: BSi21=(/6,10/)       !range 2 2 4 
      integer, dimension(2) :: BSi22=(/1,15/)     
      integer, dimension(2) :: BSj21=(/4,12/)     
      integer, dimension(2) :: BSj22=(/5,11/)     
      integer, dimension(4) :: BSk21=(/10,4,1,1/)     
      integer, dimension(4) :: BSk22=(/5,3,2,6/)    

      integer, dimension(3) :: BSi31=(/3,2,11/)     !range 3 2 2  
      integer, dimension(3) :: BSi32=(/12,1,3/)     
      integer, dimension(2) :: BSj31=(/6,10/)     
      integer, dimension(2) :: BSj32=(/4,12/)     
      integer, dimension(2) :: BSk31=(/3,13/)     
      integer, dimension(2) :: BSk32=(/15,1/)     

      integer, dimension(4) :: BSi41=(/3,2,1,10/)   !range 4 2 2  
      integer, dimension(4) :: BSi42=(/4,8,2,2/)     
      integer, dimension(2) :: BSj41=(/13,3/)     
      integer, dimension(2) :: BSj42=(/12,4/)     
      integer, dimension(2) :: BSk41=(/7,9/)     
      integer, dimension(2) :: BSk42=(/10,6/)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrg35  '

!dvm$ distribute ::A3
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!!!!dvm$ redistribute A3(GEN_BLOCK(BSi1),GEN_BLOCK(BSj1),GEN_BLOCK(BSk1))   

      select case(psize(1))
      case(1) 
        if (psize(2) == 1 .and. psize(3) == 1) then
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi111),GEN_BLOCK(BSj111),GEN_BLOCK(BSk111))   
        else
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi11),GEN_BLOCK(BSj11),GEN_BLOCK(BSk11))   
        endif
      case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi21),GEN_BLOCK(BSj21),GEN_BLOCK(BSk21))   
      case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi31),GEN_BLOCK(BSj31),GEN_BLOCK(BSk31))   
      case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi41),GEN_BLOCK(BSj41),GEN_BLOCK(BSk41))   
      case default 
      goto 10
      endselect  

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = i*NL/10 + j*NL/100 + k + 3   
             enddo
          enddo
      enddo
!dvm$ end region   

!dvm$ redistribute A3(*,*,*)    

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) + 3   
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$ redistribute A3(GEN_BLOCK(BSi2),GEN_BLOCK(BSj2),GEN_BLOCK(BSk2))   

      select case(psize(1))
      case(1) 
        if (psize(2) == 1 .and. psize(3) == 1) then
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi111),GEN_BLOCK(BSj111),GEN_BLOCK(BSk111))   
        else
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi12),GEN_BLOCK(BSj12),GEN_BLOCK(BSk12))   
        endif
      case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi22),GEN_BLOCK(BSj22),GEN_BLOCK(BSk22))   
      case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi32),GEN_BLOCK(BSj32),GEN_BLOCK(BSk32))   
      case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi42),GEN_BLOCK(BSj42),GEN_BLOCK(BSk42))   
      case default 
         goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction(min(erri))
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k + 6)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region   
     
!dvm$ get_actual(erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10   deallocate (A3)

      end subroutine distrg35

! ----------------------------------------------------distrg36
! 36 DISTRIBUTE arrA3[GEN_BLOCK][BLOCK][GEN_BLOCK]   
!                REDISTRIBUTE [BLOCK][GEN_BLOCK][BLOCK]
!                REDISTRIBUTE [BLOCK][GEN_BLOCK][GEN_BLOCK]
!                REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK][BLOCK]

      subroutine distrg36 (psize)
      integer psize(3)

      integer, parameter :: AN1=12,AN2=12,AN3=5,NL=1000,ER=10000
      integer :: erri=ER,i,j,k

      integer, dimension(1) :: BSi11=(/12/)       
      integer, dimension(1) :: BSi12=(/12/)           
      integer, dimension(2) :: BSi21=(/10,2/)     
      integer, dimension(2) :: BSi22=(/4,8/)     
      integer, dimension(3) :: BSi31=(/4,2,6/)    
      integer, dimension(3) :: BSi32=(/6,4,2/)     
      integer, dimension(4) :: BSi41=(/4,2,4,2/)  
      integer, dimension(4) :: BSi42=(/4,1,6,1/)     

      integer, dimension(1) :: BSj11=(/12/)     
      integer, dimension(1) :: BSj12=(/12/)     
      integer, dimension(2) :: BSj21=(/4,8/)     
      integer, dimension(2) :: BSj22=(/5,7/)     
      integer, dimension(3) :: BSj31=(/3,3,6/)     
      integer, dimension(3) :: BSj32=(/6,4,2/)     
      integer, dimension(4) :: BSj41=(/5,1,2,4/)     
      integer, dimension(4) :: BSj42=(/2,1,3,6/)     

      integer, dimension(1) :: BSk11=(/5/)     
      integer, dimension(1) :: BSk12=(/5/)     
      integer, dimension(2) :: BSk21=(/0,5/)     
      integer, dimension(2) :: BSk22=(/3,2/)    
      integer, dimension(3) :: BSk31=(/2,2,1/)     
      integer, dimension(3) :: BSk32=(/1,1,3/)     
      integer, dimension(4) :: BSk41=(/1,0,2,2/)     
      integer, dimension(4) :: BSk42=(/1,0,1,3/)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrg36  '

!dvm$ distribute :: A3
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!!!!dvm$ redistribute A3(GEN_BLOCK(BSi1),BLOCK,GEN_BLOCK(BSk1))   

      select case(psize(1))
      case(1)
          select case(psize(3))
              case(1) 
!dvm$ redistribute A3(GEN_BLOCK(BSi11),BLOCK,GEN_BLOCK(BSk11))    
              case(2)
!dvm$ redistribute A3(GEN_BLOCK(BSi11),BLOCK,GEN_BLOCK(BSk21))    
              case (3)
!dvm$ redistribute A3(GEN_BLOCK(BSi11),BLOCK,GEN_BLOCK(BSk31))    
              case(4)
!dvm$ redistribute A3(GEN_BLOCK(BSi11),BLOCK,GEN_BLOCK(BSk41))    
              case default 
                goto 10
          endselect  

      case(2)
          select case(psize(3))
              case(1) 
!dvm$ redistribute A3(GEN_BLOCK(BSi21),BLOCK,GEN_BLOCK(BSk11))    
              case(2)
!dvm$ redistribute A3(GEN_BLOCK(BSi21),BLOCK,GEN_BLOCK(BSk21))    
              case (3)
!dvm$ redistribute A3(GEN_BLOCK(BSi21),BLOCK,GEN_BLOCK(BSk31))    
              case(4)
!dvm$ redistribute A3(GEN_BLOCK(BSi21),BLOCK,GEN_BLOCK(BSk41))    
              case default 
                goto 10
          endselect  

      case (3)
          select case(psize(3))
              case(1) 
!dvm$ redistribute A3(GEN_BLOCK(BSi31),BLOCK,GEN_BLOCK(BSk11))    
              case(2)
!dvm$ redistribute A3(GEN_BLOCK(BSi31),BLOCK,GEN_BLOCK(BSk21))    
              case (3)
!dvm$ redistribute A3(GEN_BLOCK(BSi31),BLOCK,GEN_BLOCK(BSk31))    
              case(4)
!dvm$ redistribute A3(GEN_BLOCK(BSi31),BLOCK,GEN_BLOCK(BSk41))    
              case default 
                 goto 10
          endselect  

      case(4)
          select case(psize(3))
              case(1) 
!dvm$ redistribute A3(GEN_BLOCK(BSi41),BLOCK,GEN_BLOCK(BSk11))    
              case(2)
!dvm$ redistribute A3(GEN_BLOCK(BSi41),BLOCK,GEN_BLOCK(BSk21))    
              case (3)
!dvm$ redistribute A3(GEN_BLOCK(BSi41),BLOCK,GEN_BLOCK(BSk31))    
              case(4)
!dvm$ redistribute A3(GEN_BLOCK(BSi41),BLOCK,GEN_BLOCK(BSk41))    
              case default 
                 goto 10
          endselect  

      case default 
         goto 10
      endselect  

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

!!!!dvm$ redistribute A3(BLOCK,GEN_BLOCK(BSj1),BLOCK)   

      select case(psize(2))
      case(1) 
!dvm$ redistribute  A3(BLOCK,GEN_BLOCK(BSj11),BLOCK)   
      case(2)
!dvm$ redistribute  A3(BLOCK,GEN_BLOCK(BSj21),BLOCK)   
      case(3)
!dvm$ redistribute  A3(BLOCK,GEN_BLOCK(BSj31),BLOCK)   
      case(4)
!dvm$ redistribute  A3(BLOCK,GEN_BLOCK(BSj41),BLOCK)   
      case default 
         goto 10
      endselect  

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) * 2   
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$  redistribute A3(BLOCK,GEN_BLOCK(BSj2),GEN_BLOCK(BSk2))   

      select case(psize(2))
      case(1)
         select case(psize(3))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj12),GEN_BLOCK(BSk12))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj12),GEN_BLOCK(BSk22))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj12),GEN_BLOCK(BSk32))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj12),GEN_BLOCK(BSk42))   
         case default 
            goto 10
         endselect  

      case(2)
         select case(psize(3))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj22),GEN_BLOCK(BSk12))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj22),GEN_BLOCK(BSk22))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj22),GEN_BLOCK(BSk32))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj22),GEN_BLOCK(BSk42))   
         case default 
            goto 10               
         endselect  

      case (3)
         select case(psize(3))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj32),GEN_BLOCK(BSk12))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj32),GEN_BLOCK(BSk22))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj32),GEN_BLOCK(BSk32))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj32),GEN_BLOCK(BSk42))   
         case default 
            goto 10
         endselect  

      case (4)
         select case(psize(3))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj42),GEN_BLOCK(BSk12))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj42),GEN_BLOCK(BSk22))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj42),GEN_BLOCK(BSk32))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj42),GEN_BLOCK(BSk42))   
         case default 
            goto 10
         endselect  

      case default 
         goto 10
      endselect  

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) * 2   
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$  redistribute A3(GEN_BLOCK(BSi2),GEN_BLOCK(BSj2),BLOCK)   

      select case(psize(1))
      case(1)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi12),GEN_BLOCK(BSj12),BLOCK)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi12),GEN_BLOCK(BSj22),BLOCK)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi12),GEN_BLOCK(BSj32),BLOCK)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi12),GEN_BLOCK(BSj42),BLOCK)   
         case default 
            goto 10
         endselect  

      case(2)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi22),GEN_BLOCK(BSj12),BLOCK)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi22),GEN_BLOCK(BSj22),BLOCK)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi22),GEN_BLOCK(BSj32),BLOCK)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi22),GEN_BLOCK(BSj42),BLOCK)   
         case default 
            goto 10
         endselect  

      case (3)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi32),GEN_BLOCK(BSj12),BLOCK)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi32),GEN_BLOCK(BSj22),BLOCK)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi32),GEN_BLOCK(BSj32),BLOCK)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi32),GEN_BLOCK(BSj42),BLOCK)   
         case default 
            goto 10
         endselect  

      case (4)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi42),GEN_BLOCK(BSj12),BLOCK)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi42),GEN_BLOCK(BSj22),BLOCK)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi42),GEN_BLOCK(BSj32),BLOCK)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi42),GEN_BLOCK(BSj42),BLOCK)   
         case default 
            goto 10
         endselect  

      case default 
         goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction(min(erri))
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                A3(i,j,k) = A3(i,j,k) / 4   
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region   
     
!dvm$ get_actual(erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10   deallocate (A3)

      end subroutine distrg36

! ----------------------------------------------------distrg37
! 37 DISTRIBUTE arrA3[GEN_BLOCK][GEN_BLOCK][BLOCK]    
!                REDISTRIBUTE [BLOCK][GEN_BLOCK][BLOCK]
!                REDISTRIBUTE [BLOCK][GEN_BLOCK][GEN_BLOCK]
!                REDISTRIBUTE [GEN_BLOCK][BLOCK][GEN_BLOCK]

      subroutine distrg37 (psize)
      integer psize(3)

      integer, parameter :: AN1=10,AN2=15,AN3=15,NL=1000,ER=10000
      integer :: erri=ER,i,j,k

      integer, dimension(1) :: BSi11=(/10/)         
      integer, dimension(1) :: BSi12=(/10/)           
      integer, dimension(2) :: BSi21=(/6,4/)        
      integer, dimension(2) :: BSi22=(/2,8/)     
      integer, dimension(3) :: BSi31=(/2,3,5/)      
      integer, dimension(3) :: BSi32=(/3,4,3/)     
      integer, dimension(4) :: BSi41=(/4,1,3,2/)    
      integer, dimension(4) :: BSi42=(/3,3,2,2/)     

      integer, dimension(1) :: BSj11=(/15/)     
      integer, dimension(1) :: BSj12=(/15/)     
      integer, dimension(2) :: BSj21=(/3,12/)     
      integer, dimension(2) :: BSj22=(/10,5/)     
      integer, dimension(3) :: BSj31=(/6,4,5/)     
      integer, dimension(3) :: BSj32=(/3,2,10/)     
      integer, dimension(4) :: BSj41=(/5,2,3,5/)     
      integer, dimension(4) :: BSj42=(/2,4,8,1/)     

      integer, dimension(1) :: BSk11=(/15/)     
      integer, dimension(1) :: BSk12=(/15/)     
      integer, dimension(2) :: BSk21=(/10,5/)     
      integer, dimension(2) :: BSk22=(/7,8/)    
      integer, dimension(3) :: BSk31=(/1,11,3/)     
      integer, dimension(3) :: BSk32=(/5,7,3/)     
      integer, dimension(4) :: BSk41=(/3,4,2,6/)     
      integer, dimension(4) :: BSk42=(/4,2,5,4/)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrg37  '

!dvm$ distribute :: A3 
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!!!!dvm$ distribute A3(GEN_BLOCK(BSi1),GEN_BLOCK(BSj1),BLOCK)   

      select case(psize(1))
      case(1)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi11),GEN_BLOCK(BSj11),BLOCK)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi11),GEN_BLOCK(BSj21),BLOCK)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi11),GEN_BLOCK(BSj31),BLOCK)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi11),GEN_BLOCK(BSj41),BLOCK)   
         case default 
            goto 10
         endselect  

      case(2)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi21),GEN_BLOCK(BSj11),BLOCK)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi21),GEN_BLOCK(BSj21),BLOCK)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi21),GEN_BLOCK(BSj31),BLOCK)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi21),GEN_BLOCK(BSj41),BLOCK)   
         case default 
            goto 10
         endselect  

      case (3)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi31),GEN_BLOCK(BSj11),BLOCK)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi31),GEN_BLOCK(BSj21),BLOCK)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi31),GEN_BLOCK(BSj31),BLOCK)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi31),GEN_BLOCK(BSj41),BLOCK)   
         case default 
            goto 10
         endselect  

      case (4)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi41),GEN_BLOCK(BSj11),BLOCK)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi41),GEN_BLOCK(BSj21),BLOCK)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi41),GEN_BLOCK(BSj31),BLOCK)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi41),GEN_BLOCK(BSj41),BLOCK)   
         case default 
            goto 10
         endselect  

      case default 
         goto 10
      endselect  

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

!!!!dvm$ redistribute A3(BLOCK,GEN_BLOCK(BSj2),BLOCK)   

      select case(psize(2))
      case(1) 
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj12),BLOCK)   
      case(2)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj22),BLOCK)   
      case(3)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj32),BLOCK)   
      case(4)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj42),BLOCK)   
      case default 
         goto 10
      endselect  

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) * 2   
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$ redistribute A3(BLOCK,GEN_BLOCK(BSj1),GEN_BLOCK(BSk1))   

      select case(psize(2))
      case(1)
         select case(psize(3))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj11),GEN_BLOCK(BSk11))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj11),GEN_BLOCK(BSk21))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj11),GEN_BLOCK(BSk31))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj11),GEN_BLOCK(BSk41))   
         case default 
            goto 10
         endselect  

      case(2)
         select case(psize(3))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj21),GEN_BLOCK(BSk11))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj21),GEN_BLOCK(BSk21))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj21),GEN_BLOCK(BSk31))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj21),GEN_BLOCK(BSk41))   
         case default 
            goto 10               
         endselect  

      case (3)
         select case(psize(3))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj31),GEN_BLOCK(BSk11))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj31),GEN_BLOCK(BSk21))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj31),GEN_BLOCK(BSk31))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj31),GEN_BLOCK(BSk41))   
         case default 
            goto 10
         endselect  

      case (4)
         select case(psize(3))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj41),GEN_BLOCK(BSk11))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj41),GEN_BLOCK(BSk21))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj41),GEN_BLOCK(BSk31))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(BLOCK,GEN_BLOCK(BSj41),GEN_BLOCK(BSk41))   
         case default 
            goto 10
         endselect  

      case default 
         goto 10
      endselect  

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) * 2   
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$ redistribute A3(GEN_BLOCK(BSi2),BLOCK,GEN_BLOCK(BSk2))   

      select case(psize(1))
      case(1)
          select case(psize(3))
          case(1) 
!dvm$ redistribute A3(GEN_BLOCK(BSi12),BLOCK,GEN_BLOCK(BSk12))    
          case(2)
!dvm$ redistribute A3(GEN_BLOCK(BSi12),BLOCK,GEN_BLOCK(BSk22))    
          case (3)
!dvm$ redistribute A3(GEN_BLOCK(BSi12),BLOCK,GEN_BLOCK(BSk32))    
          case(4)
!dvm$ redistribute A3(GEN_BLOCK(BSi12),BLOCK,GEN_BLOCK(BSk42))    
          case default 
             goto 10
          endselect  

      case(2)
          select case(psize(3))
          case(1) 
!dvm$ redistribute A3(GEN_BLOCK(BSi22),BLOCK,GEN_BLOCK(BSk12))    
          case(2)
!dvm$ redistribute A3(GEN_BLOCK(BSi22),BLOCK,GEN_BLOCK(BSk22))    
          case (3)
!dvm$ redistribute A3(GEN_BLOCK(BSi22),BLOCK,GEN_BLOCK(BSk32))    
          case(4)
!dvm$ redistribute A3(GEN_BLOCK(BSi22),BLOCK,GEN_BLOCK(BSk42))    
          case default 
             goto 10
          endselect  

      case (3)
          select case(psize(3))
          case(1) 
!dvm$ redistribute A3(GEN_BLOCK(BSi32),BLOCK,GEN_BLOCK(BSk12))    
          case(2)
!dvm$ redistribute A3(GEN_BLOCK(BSi32),BLOCK,GEN_BLOCK(BSk22))    
          case (3)
!dvm$ redistribute A3(GEN_BLOCK(BSi32),BLOCK,GEN_BLOCK(BSk32))    
          case(4)
!dvm$ redistribute A3(GEN_BLOCK(BSi32),BLOCK,GEN_BLOCK(BSk42))    
          case default 
             goto 10
          endselect  

      case(4)
          select case(psize(3))
          case(1) 
!dvm$ redistribute A3(GEN_BLOCK(BSi42),BLOCK,GEN_BLOCK(BSk12))    
          case(2)
!dvm$ redistribute A3(GEN_BLOCK(BSi42),BLOCK,GEN_BLOCK(BSk22))    
          case (3)
!dvm$ redistribute A3(GEN_BLOCK(BSi42),BLOCK,GEN_BLOCK(BSk32))    
          case(4)
!dvm$ redistribute A3(GEN_BLOCK(BSi42),BLOCK,GEN_BLOCK(BSk42))    
          case default 
             goto 10
          endselect  

      case default 
         goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction(min(erri))
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                A3(i,j,k) = A3(i,j,k) / 4   
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region   
     
!dvm$ get_actual(erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10   deallocate (A3)

      end subroutine distrg37

! ----------------------------------------------------distrg38
! 38  DISTRIBUTE arrA3 [GEN_BLOCK][*][GEN_BLOCK] 
!                REDISTRIBUTE [*][GEN_BLOCK][*]
!                REDISTRIBUTE[GEN_BLOCK][GEN_BLOCK][*]
!                REDISTRIBUTE[*][GEN_BLOCK][GEN_BLOCK]

      subroutine distrg38 (psize)
      integer psize(3)

      integer, parameter :: AN1=5,AN2=6,AN3=12,NL=1000,ER=10000
      integer :: erri=ER,i,j,k

      integer, dimension(1) :: BSi11=(/5/)         
      integer, dimension(1) :: BSi12=(/5/)           
      integer, dimension(2) :: BSi21=(/4,1/)      
      integer, dimension(2) :: BSi22=(/3,2/)     
      integer, dimension(3) :: BSi31=(/3,1,1/)     
      integer, dimension(3) :: BSi32=(/1,2,2/)     
      integer, dimension(4) :: BSi41=(/1,2,1,1/)   
      integer, dimension(4) :: BSi42=(/3,1,0,2/)     

      integer, dimension(1) :: BSj11=(/6/)     
      integer, dimension(1) :: BSj12=(/6/)     
      integer, dimension(2) :: BSj21=(/2,4/)     
      integer, dimension(2) :: BSj22=(/4,2/)     
      integer, dimension(3) :: BSj31=(/2,3,1/)     
      integer, dimension(3) :: BSj32=(/1,2,3/)     
      integer, dimension(4) :: BSj41=(/2,1,2,1/)     
      integer, dimension(4) :: BSj42=(/1,1,3,1/)     

      integer, dimension(1) :: BSk11=(/12/)     
      integer, dimension(1) :: BSk12=(/12/)    
      integer, dimension(2) :: BSk21=(/10,2/)     
      integer, dimension(2) :: BSk22=(/5,7/)     
      integer, dimension(3) :: BSk31=(/2,6,4/)     
      integer, dimension(3) :: BSk32=(/3,4,5/)     
      integer, dimension(4) :: BSk41=(/2,4,5,1/)     
      integer, dimension(4) :: BSk42=(/3,2,4,3/)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrg38  '

!dvm$ distribute :: A3   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!!!!dvm$ redistribute A3(GEN_BLOCK(BSi1),*,GEN_BLOCK(BSk1))   

      select case(psize(1))
      case(1)
          select case(psize(2))   ! it's true - psize(2)
          case(1) 
!dvm$ redistribute A3(GEN_BLOCK(BSi11),*,GEN_BLOCK(BSk11))    
          case(2)
!dvm$ redistribute A3(GEN_BLOCK(BSi11),*,GEN_BLOCK(BSk21))    
          case (3)
!dvm$ redistribute A3(GEN_BLOCK(BSi11),*,GEN_BLOCK(BSk31))    
          case(4)
!dvm$ redistribute A3(GEN_BLOCK(BSi11),*,GEN_BLOCK(BSk41))    
          case default 
              goto 10
          endselect  

      case(2)
          select case(psize(2)) ! it's true - psize(2)
          case(1) 
!dvm$ redistribute A3(GEN_BLOCK(BSi21),*,GEN_BLOCK(BSk11))    
          case(2)
!dvm$ redistribute A3(GEN_BLOCK(BSi21),*,GEN_BLOCK(BSk21))    
          case (3)
!dvm$ redistribute A3(GEN_BLOCK(BSi21),*,GEN_BLOCK(BSk31))    
          case(4)
!dvm$ redistribute A3(GEN_BLOCK(BSi21),*,GEN_BLOCK(BSk41))    
          case default 
             goto 10
          endselect  

      case (3)
          select case(psize(2)) ! it's true - psize(2)
          case(1) 
!dvm$ redistribute A3(GEN_BLOCK(BSi31),*,GEN_BLOCK(BSk11))    
          case(2)
!dvm$ redistribute A3(GEN_BLOCK(BSi31),*,GEN_BLOCK(BSk21))    
          case (3)
!dvm$ redistribute A3(GEN_BLOCK(BSi31),*,GEN_BLOCK(BSk31))    
          case(4)
!dvm$ redistribute A3(GEN_BLOCK(BSi31),*,GEN_BLOCK(BSk41))    
          case default 
             goto 10
          endselect  

      case(4)
          select case(psize(2)) ! it's true - psize(2)
          case(1) 
!dvm$ redistribute A3(GEN_BLOCK(BSi41),*,GEN_BLOCK(BSk11))    
          case(2)
!dvm$ redistribute A3(GEN_BLOCK(BSi41),*,GEN_BLOCK(BSk21))    
          case (3)
!dvm$ redistribute A3(GEN_BLOCK(BSi41),*,GEN_BLOCK(BSk31))    
          case(4)
!dvm$ redistribute A3(GEN_BLOCK(BSi41),*,GEN_BLOCK(BSk41))    
          case default 
             goto 10
          endselect  

      case default 
         goto 10
      endselect  

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = i*NL/10 + j*NL/100 + k + 5  
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$  redistribute A3(*,GEN_BLOCK(BSj2),*)   

      select case(psize(1))  ! it's true - psize(1)
      case(1) 
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj12),*)   
      case(2)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj22),*)   
      case(3)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj32),*)   
      case(4)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj42),*)   
      case default 
         goto 10
      endselect  

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) + 5   
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$  redistribute A3(GEN_BLOCK(BSi1),GEN_BLOCK(BSj1),*)   

      select case(psize(1))
      case(1)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi11),GEN_BLOCK(BSj11),*)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi11),GEN_BLOCK(BSj21),*)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi11),GEN_BLOCK(BSj31),*)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi11),GEN_BLOCK(BSj41),*)   
         case default 
            goto 10
         endselect  

      case(2)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi21),GEN_BLOCK(BSj11),*)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi21),GEN_BLOCK(BSj21),*)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi21),GEN_BLOCK(BSj31),*)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi21),GEN_BLOCK(BSj41),*)   
         case default 
            goto 10
         endselect  

      case (3)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi31),GEN_BLOCK(BSj11),*)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi31),GEN_BLOCK(BSj21),*)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi31),GEN_BLOCK(BSj31),*)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi31),GEN_BLOCK(BSj41),*)   
         case default 
            goto 10
         endselect  

      case (4)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi41),GEN_BLOCK(BSj11),*)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi41),GEN_BLOCK(BSj21),*)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi41),GEN_BLOCK(BSj31),*)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi41),GEN_BLOCK(BSj41),*)   
         case default 
            goto 10
         endselect  

      case default 
         goto 10
      endselect  

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) + 5   
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$  redistribute A3(*,GEN_BLOCK(BSj2),GEN_BLOCK(BSk2))   

      select case(psize(1))     ! it's true - psize(1)
      case(1)
         select case(psize(2))  ! it's true - psize(2)
         case(1) 
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj12),GEN_BLOCK(BSk12))   
         case(2)                                     
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj12),GEN_BLOCK(BSk22))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj12),GEN_BLOCK(BSk32))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj12),GEN_BLOCK(BSk42))   
         case default 
            goto 10
         endselect  

      case(2)
         select case(psize(2))  ! it's true - psize(2)
         case(1) 
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj22),GEN_BLOCK(BSk12))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj22),GEN_BLOCK(BSk22))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj22),GEN_BLOCK(BSk32))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj22),GEN_BLOCK(BSk42))   
         case default 
            goto 10               
         endselect  

      case (3)
         select case(psize(2))  ! it's true - psize(2)
         case(1) 
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj32),GEN_BLOCK(BSk12))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj32),GEN_BLOCK(BSk22))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj32),GEN_BLOCK(BSk32))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj32),GEN_BLOCK(BSk42))   
         case default 
            goto 10
         endselect  

      case (4)
         select case(psize(2))  ! it's true - psize(2)
         case(1) 
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj42),GEN_BLOCK(BSk12))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj42),GEN_BLOCK(BSk22))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj42),GEN_BLOCK(BSk32))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj42),GEN_BLOCK(BSk42))   
         case default 
            goto 10
         endselect  

      case default 
         goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction(min(erri))
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k + 15)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region   
     
!dvm$ get_actual(erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10   deallocate (A3)

      end subroutine distrg38

! ----------------------------------------------------distrg39
! 39 DISTRIBUTE arrA3 [*][GEN_BLOCK][GEN_BLOCK]  
!                REDISTRIBUTE [GEN_BLOCK][*][*]
!                REDISTRIBUTE[GEN_BLOCK][*][GEN_BLOCK]

      subroutine distrg39 (psize)
      integer psize(3)

      integer, parameter :: AN1=10,AN2=16,AN3=10,NL=1000,ER=10000
      integer :: erri=ER,i,j,k

      integer, dimension(1) :: BSi11=(/10/)        
      integer, dimension(1) :: BSi12=(/10/)           
      integer, dimension(2) :: BSi21=(/6,4/)       
      integer, dimension(2) :: BSi22=(/2,8/)     
      integer, dimension(3) :: BSi31=(/5,2,3/)    
      integer, dimension(3) :: BSi32=(/2,4,4/)     
      integer, dimension(4) :: BSi41=(/3,2,1,4/)   
      integer, dimension(4) :: BSi42=(/5,1,1,3/)     

      integer, dimension(1) :: BSj11=(/16/)     
      integer, dimension(1) :: BSj12=(/16/)     
      integer, dimension(2) :: BSj21=(/5,11/)     
      integer, dimension(2) :: BSj22=(/12,4/)     
      integer, dimension(3) :: BSj31=(/6,2,8/)     
      integer, dimension(3) :: BSj32=(/10,3,3/)     
      integer, dimension(4) :: BSj41=(/6,3,5,2/)     
      integer, dimension(4) :: BSj42=(/3,2,1,10/)     

      integer, dimension(1) :: BSk11=(/10/)     
      integer, dimension(1) :: BSk12=(/10/)     
      integer, dimension(2) :: BSk21=(/9,1/)     
      integer, dimension(2) :: BSk22=(/4,6/)     
      integer, dimension(3) :: BSk31=(/10,0,0/)     
      integer, dimension(3) :: BSk32=(/5,3,2/)    
      integer, dimension(4) :: BSk41=(/0,2,3,5/)     
      integer, dimension(4) :: BSk42=(/0,4,0,6/)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrg39  '

!dvm$ distribute :: A3   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!!!!dvm$ redistribute A3(*,GEN_BLOCK(BSj1),GEN_BLOCK(BSk1))   

      select case(psize(1))     ! it's true - psize(1)
      case(1)
         select case(psize(2))  ! it's true - psize(2)
         case(1) 
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj11),GEN_BLOCK(BSk11))   
         case(2) 
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj11),GEN_BLOCK(BSk21))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj11),GEN_BLOCK(BSk31))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj11),GEN_BLOCK(BSk41))   
         case default 
            goto 10
         endselect  

      case(2)
         select case(psize(2))  ! it's true - psize(2)
         case(1) 
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj21),GEN_BLOCK(BSk11))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj21),GEN_BLOCK(BSk21))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj21),GEN_BLOCK(BSk31))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj21),GEN_BLOCK(BSk41))   
         case default 
            goto 10               
         endselect  

      case (3)
         select case(psize(2))  ! it's true - psize(2)
         case(1) 
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj31),GEN_BLOCK(BSk11))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj31),GEN_BLOCK(BSk21))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj31),GEN_BLOCK(BSk31))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj31),GEN_BLOCK(BSk41))   
         case default 
            goto 10
         endselect  

      case (4)
         select case(psize(2))  ! it's true - psize(2)
         case(1) 
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj41),GEN_BLOCK(BSk11))   
         case(2)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj41),GEN_BLOCK(BSk21))   
         case(3)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj41),GEN_BLOCK(BSk31))   
         case(4)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj41),GEN_BLOCK(BSk41))   
         case default 
            goto 10
         endselect  

      case default 
         goto 10
      endselect  

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = i*NL/10 + j*NL/100 + k + 5  
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$  redistribute A3(GEN_BLOCK(BSi2),*,*)   

      select case(psize(1))
      case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi12),*,*)   
      case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi22),*,*)   
      case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi32),*,*)   
      case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi42),*,*)   
      case default 
         goto 10
      endselect  

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) + 5   
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$  redistribute A3(GEN_BLOCK(BSi2),*,GEN_BLOCK(BSk2))   

      select case(psize(1))
      case(1)
          select case(psize(2))  ! it's true - psize(2)
              case(1) 
!dvm$ redistribute A3(GEN_BLOCK(BSi12),*,GEN_BLOCK(BSk12))    
              case(2)
!dvm$ redistribute A3(GEN_BLOCK(BSi12),*,GEN_BLOCK(BSk22))    
              case (3)
!dvm$ redistribute A3(GEN_BLOCK(BSi12),*,GEN_BLOCK(BSk32))    
              case(4)
!dvm$ redistribute A3(GEN_BLOCK(BSi12),*,GEN_BLOCK(BSk42))    
              case default 
                 goto 10
          endselect  

      case(2)
          select case(psize(2))  ! it's true - psize(2)
              case(1) 
!dvm$ redistribute A3(GEN_BLOCK(BSi22),*,GEN_BLOCK(BSk12))    
              case(2)
!dvm$ redistribute A3(GEN_BLOCK(BSi22),*,GEN_BLOCK(BSk22))    
              case (3)
!dvm$ redistribute A3(GEN_BLOCK(BSi22),*,GEN_BLOCK(BSk32))    
              case(4)
!dvm$ redistribute A3(GEN_BLOCK(BSi22),*,GEN_BLOCK(BSk42))    
              case default 
                goto 10
          endselect  

      case (3)
          select case(psize(2))  ! it's true - psize(2)
              case(1) 
!dvm$ redistribute A3(GEN_BLOCK(BSi32),*,GEN_BLOCK(BSk12))    
              case(2)
!dvm$ redistribute A3(GEN_BLOCK(BSi32),*,GEN_BLOCK(BSk22))    
              case (3)
!dvm$ redistribute A3(GEN_BLOCK(BSi32),*,GEN_BLOCK(BSk32))    
              case(4)
!dvm$ redistribute A3(GEN_BLOCK(BSi32),*,GEN_BLOCK(BSk42))    
              case default 
                 goto 10
          endselect  

      case(4)
          select case(psize(2))  ! it's true - psize(2)
              case(1) 
!dvm$ redistribute A3(GEN_BLOCK(BSi42),*,GEN_BLOCK(BSk12))    
              case(2)
!dvm$ redistribute A3(GEN_BLOCK(BSi42),*,GEN_BLOCK(BSk22))    
              case (3)
!dvm$ redistribute A3(GEN_BLOCK(BSi42),*,GEN_BLOCK(BSk32))    
              case(4)
!dvm$ redistribute A3(GEN_BLOCK(BSi42),*,GEN_BLOCK(BSk42))    
              case default 
                 goto 10
          endselect  

      case default 
         goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction(min(erri))
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
     
!dvm$ get_actual(erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

  10  deallocate (A3)

      end subroutine distrg39

! ----------------------------------------------------distrg310
! 310 DISTRIBUTE arrA3 [GEN_BLOCK][GEN_BLOCK][BLOCK]  
!                REDISTRIBUTE [*][*][GEN_BLOCK]
!                REDISTRIBUTE[*][GEN_BLOCK][BLOCK]

      subroutine distrg310 (psize)
      integer psize(3)

      integer, parameter :: AN1=20,AN2=15,AN3=10,NL=1000,ER=10000
      integer :: erri=ER,i,j,k

      integer, dimension(1) :: BSi11=(/20/)        
      integer, dimension(1) :: BSi12=(/20/)           
      integer, dimension(2) :: BSi21=(/2,18/)      
      integer, dimension(2) :: BSi22=(/12,8/)     
      integer, dimension(3) :: BSi31=(/3,12,5/)    
      integer, dimension(3) :: BSi32=(/2,4,14/)     
      integer, dimension(4) :: BSi41=(/3,12,3,2/)   
      integer, dimension(4) :: BSi42=(/4,6,2,8/)     

      integer, dimension(1) :: BSj11=(/15/)     
      integer, dimension(1) :: BSj12=(/15/)     
      integer, dimension(2) :: BSj21=(/3,12/)     
      integer, dimension(2) :: BSj22=(/5,10/)     
      integer, dimension(3) :: BSj31=(/6,3,6/)     
      integer, dimension(3) :: BSj32=(/3,1,11/)     
      integer, dimension(4) :: BSj41=(/5,1,3,6/)     
      integer, dimension(4) :: BSj42=(/2,2,6,5/)     

      integer, dimension(1) :: BSk11=(/10/)     
      integer, dimension(1) :: BSk12=(/10/)     
      integer, dimension(2) :: BSk21=(/1,9/)     
      integer, dimension(2) :: BSk22=(/6,4/)    
      integer, dimension(3) :: BSk31=(/3,3,4/)     
      integer, dimension(3) :: BSk32=(/1,2,7/)     
      integer, dimension(4) :: BSk41=(/2,4,1,3/)     
      integer, dimension(4) :: BSk42=(/1,6,2,1/)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrg310 '

!dvm$ distribute :: A3   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!!!!dvm$ redistribute A3(GEN_BLOCK(BSi1),GEN_BLOCK(BSj1),BLOCK)   

      select case(psize(1))
      case(1)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi11),GEN_BLOCK(BSj11),BLOCK)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi11),GEN_BLOCK(BSj21),BLOCK)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi11),GEN_BLOCK(BSj31),BLOCK)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi11),GEN_BLOCK(BSj41),BLOCK)   
         case default 
            goto 10
         endselect  

      case(2)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi21),GEN_BLOCK(BSj11),BLOCK)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi21),GEN_BLOCK(BSj21),BLOCK)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi21),GEN_BLOCK(BSj31),BLOCK)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi21),GEN_BLOCK(BSj41),BLOCK)   
         case default 
            goto 10
         endselect  

      case (3)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi31),GEN_BLOCK(BSj11),BLOCK)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi31),GEN_BLOCK(BSj21),BLOCK)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi31),GEN_BLOCK(BSj31),BLOCK)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi31),GEN_BLOCK(BSj41),BLOCK)   
         case default 
            goto 10
         endselect  

      case (4)
         select case(psize(2))
         case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi41),GEN_BLOCK(BSj11),BLOCK)   
         case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi41),GEN_BLOCK(BSj21),BLOCK)   
         case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi41),GEN_BLOCK(BSj31),BLOCK)   
         case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi41),GEN_BLOCK(BSj41),BLOCK)   
         case default 
            goto 10
         endselect  

      case default 
         goto 10
      endselect  

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = i*NL/10 + j*NL/100 + k + 8  
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$  redistribute A3(*,*,GEN_BLOCK(BSk2))   

      select case(psize(1))  ! it's true - psize(1)
      case(1) 
!dvm$ redistribute
!dvm$*    A3(*,*,GEN_BLOCK(BSk12))   
      case(2)
!dvm$ redistribute
!dvm$*    A3(*,*,GEN_BLOCK(BSk22))   
      case(3)
!dvm$ redistribute
!dvm$*    A3(*,*,GEN_BLOCK(BSk32))   
      case(4)
!dvm$ redistribute
!dvm$*    A3(*,*,GEN_BLOCK(BSk42))   
      case default 
         goto 10
      endselect  

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) + 8   
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$  redistribute A3(*,GEN_BLOCK(BSj2),BLOCK)   

      select case(psize(1))  ! it's true - psize(1)
      case(1) 
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj12),BLOCK)   
      case(2)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj22),BLOCK)   
      case(3)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj32),BLOCK)   
      case(4)
!dvm$ redistribute
!dvm$*    A3(*,GEN_BLOCK(BSj42),BLOCK)   
      case default 
         goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction(min(erri))
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k + 16)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region   
     
!dvm$ get_actual(erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10   deallocate (A3)

      end subroutine distrg310

! ---------------------------------------------------------distrg311
! 311 DISTRIBUTE arrA3 [GEN_BLOCK][*][*]  
!                REDISTRIBUTE [*][*][*]
!                REDISTRIBUTE[BLOCK][*][GEN_BLOCK]

      subroutine distrg311 (psize)
      integer psize(3)

      integer, parameter :: AN1=8,AN2=16,AN3=24,NL=1000,ER=10000
      integer :: erri=ER,i,j,k

      integer, dimension(1) :: BSi11=(/8/)        
      integer, dimension(1) :: BSi12=(/8/)           
      integer, dimension(2) :: BSi21=(/1,7/)     
      integer, dimension(2) :: BSi22=(/5,3/)     
      integer, dimension(3) :: BSi31=(/1,5,2/)   
      integer, dimension(3) :: BSi32=(/2,2,4/)     
      integer, dimension(4) :: BSi41=(/1,2,3,2/)  
      integer, dimension(4) :: BSi42=(/4,1,2,1/)     

      integer, dimension(1) :: BSj11=(/16/)     
      integer, dimension(1) :: BSj12=(/16/)     
      integer, dimension(2) :: BSj21=(/4,12/)     
      integer, dimension(2) :: BSj22=(/7,9/)     
      integer, dimension(3) :: BSj31=(/3,12,1/)     
      integer, dimension(3) :: BSj32=(/6,2,8/)     
      integer, dimension(4) :: BSj41=(/4,1,2,9/)     
      integer, dimension(4) :: BSj42=(/2,3,6,5/)     

      integer, dimension(1) :: BSk11=(/24/)     
      integer, dimension(1) :: BSk12=(/24/)     
      integer, dimension(2) :: BSk21=(/20,4/)     
      integer, dimension(2) :: BSk22=(/10,14/)     
      integer, dimension(3) :: BSk31=(/5,11,8/)     
      integer, dimension(3) :: BSk32=(/6,7,11/)    
      integer, dimension(4) :: BSk41=(/12,4,6,2/)     
      integer, dimension(4) :: BSk42=(/10,8,2,4/)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrg311 '

!dvm$ distribute :: A3   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!!!!dvm$ redistribute A3(GEN_BLOCK(BSi1),*,*)   

      select case(psize(1))
      case(1) 
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi11),*,*)   
      case(2)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi21),*,*)   
      case(3)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi31),*,*)   
      case(4)
!dvm$ redistribute
!dvm$*    A3(GEN_BLOCK(BSi41),*,*)   
      case default 
         goto 10
      endselect  

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = i*NL/10 + j*NL/100 + k + 6  
             enddo
          enddo
      enddo
!dvm$ end region   

!dvm$  redistribute A3(*,*,*)   

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) + 6   
             enddo
          enddo
      enddo
!dvm$ end region   

!!!!dvm$  redistribute A3(BLOCK,*, GEN_BLOCK(BSk2))   

      select case(psize(2))  ! it's true - psize(2)
      case(1) 
!dvm$ redistribute
!dvm$*    A3(BLOCK,*,GEN_BLOCK(BSk12))   
      case(2)
!dvm$ redistribute
!dvm$*    A3(BLOCK,*,GEN_BLOCK(BSk22))   
      case(3)
!dvm$ redistribute
!dvm$*    A3(BLOCK,*,GEN_BLOCK(BSk32))   
      case(4)
!dvm$ redistribute
!dvm$*    A3(BLOCK,*,GEN_BLOCK(BSk42))   
      case default 
         goto 10
      endselect  

!dvm$ get_actual(erri)

!dvm$ region  
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction(min(erri))
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k + 12)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region   
     
!dvm$ get_actual(erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10   deallocate (A3)

      end subroutine distrg311

C -------------------------------------------------

      subroutine ansyes(name)
      character(*) name
      print *,name,'  -  complete'
      end

      subroutine ansno(name)
      character(*) name
      print *,name,'  -  ***error'
      end
 