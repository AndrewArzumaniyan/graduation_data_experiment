      program DISTRMIX2

!    Testing DISTRIBUTE and REDISTRIBUTE directives       
!            GEN_BLOCK, WGT_BLOCK, MULT_BLOCK, BLOCK distributions
       
      integer PROCESSORS_RANK, PROCESSORS_SIZE 
      integer psize(2), rank

      PROCESSORS_RANK() = 2
      PROCESSORS_SIZE(i) = 1

      print *,'===START OF distrmix2========================'

C -------------------------------------------------
c 21  DISTRIBUTE arrA2[MULT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK]  
c              REDISTRIBUTE [MULT_BLOCK][MULT_BLOCK]  
      call distrmix21
C -------------------------------------------------
c 22  DISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]  
c              REDISTRIBUTE [MULT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK]  
      call distrmix22
C -------------------------------------------------
c 23  DISTRIBUTE arrA2[MULT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [WGT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [MULT_BLOCK][WGT_BLOCK]  
      call distrmix23
C -------------------------------------------------
      rank = PROCESSORS_RANK()

      do i=1,rank
         psize(i)=PROCESSORS_SIZE(i)
         if (psize(i) > 4) then    ! may be temporary
            goto 1
         endif  
      enddo

C -------------------------------------------------

c 24  DISTRIBUTE arrA2[MULT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK]  
c              REDISTRIBUTE [MULT_BLOCK][MULT_BLOCK]  
      call distrmix24 (psize)
C -------------------------------------------------
c 25  DISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK]  
c              REDISTRIBUTE [MULT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK]  
      call distrmix25 (psize)
C -------------------------------------------------
c 26  DISTRIBUTE arrA2[WGT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [MULT_BLOCK][WGT_BLOCK]  
c              REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK]  
      call distrmix26 (psize)
C -------------------------------------------------
c 27  DISTRIBUTE arrA2[BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [MULT_BLOCK][WGT_BLOCK]  
c              REDISTRIBUTE [GEN_BLOCK][BLOCK]  
      call distrmix27 (psize)
C -------------------------------------------------
c 28  DISTRIBUTE arrA2[BLOCK][GEN_BLOCK]  
c              REDISTRIBUTE [WGT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [BLOCK][WGT_BLOCK]  
      call distrmix28 (psize)
C -------------------------------------------------
c 29  DISTRIBUTE arrA2[WGT_BLOCK][BLOCK]  
c              REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK]  
c              REDISTRIBUTE [BLOCK][MULT_BLOCK]  
      call distrmix29 (psize)
C -------------------------------------------------
c 210  DISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK] 
c               REDISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK] 
c               REDISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]
       call distrmix210 (psize)
C -------------------------------------------------
c 211  DISTRIBUTE arrA2[WGT_BLOCK][BLOCK] 
c               REDISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK] 
c               REDISTRIBUTE arrA2[BLOCK][WGT_BLOCK]
         call distrmix211 (psize)
C -------------------------------------------------
c 212  DISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK] 
c               REDISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]
c               REDISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK] 
         call distrmix212 (psize)
C -------------------------------------------------
c 213  DISTRIBUTE arrA2[BLOCK][GEN_BLOCK] 
c               REDISTRIBUTE arrA2[WGT_BLOCK][MULT_BLOCK]
c               REDISTRIBUTE arrA2[GEN_BLOCK][BLOCK] 
         call distrmix213 (psize)
C -------------------------------------------------
C
 1    print *,'=== END OF distrmix2 ========================= '    

      end

C ----------------------------------------------------distrmix21
c 21  DISTRIBUTE arrA2[MULT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK]  
c              REDISTRIBUTE [MULT_BLOCK][MULT_BLOCK]  

      subroutine distrmix21

      integer, parameter :: AN1=10,AN2=56,NL=1000,ER=10000
      integer :: erri= ER,i,j

      integer, parameter :: m11 = 2, m21 = 7
      integer, parameter :: m12 = 5, m22 = 8

      double precision, dimension(8) ::
     >                  WB1=(/1.0,2.,1.,3.2,1.0, 1.5, 2.3, 2./)     
      double precision, dimension(7) ::
     >                  WB2=(/1.3, 1.5, 2.2, 1.6, 2.6, 0.5, 1.7/)     

      integer A2(AN1,AN2)  !static array
      character(*), parameter :: tname='distrmix21 '
               
!dvm$ distribute A2(MULT_BLOCK(m11),MULT_BLOCK(m21))   
!dvm$ dynamic A2

      A2 = 3

!dvm$ actual(A2)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) + i*NL+j     
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A2(WGT_BLOCK(WB1,8),WGT_BLOCK(WB2,7))   

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =A2(i,j) + 3     
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A2(MULT_BLOCK(m12),MULT_BLOCK(m22))   

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)+6) then     
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
 
      end subroutine distrmix21

C ----------------------------------------------------distrmix22
c 22  DISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]  
c              REDISTRIBUTE [MULT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK]  

      subroutine distrmix22

      integer, parameter :: AN1=16,AN2=32,NL=1000,ER=10000
      integer :: erri= ER,i,j

      integer, parameter :: m1 = 2, m2 = 4

      double precision, dimension(7) ::
     >                  WB1=(/2.4, 1.2, 3.0, 0.2, 1.5, 2.8, 2.1/)     
      double precision, dimension(6) ::
     >                  WB2=(/2.0, 1.2, 2.6, 1.6, 3.5, 0.7/)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrmix22 '
               
!dvm$ distribute A2(WGT_BLOCK(WB1,7),WGT_BLOCK(WB2,6))   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

      A2 = 4

!dvm$ actual(A2)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) + i*NL+j     
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A2(MULT_BLOCK(m1),MULT_BLOCK(m2))   

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =A2(i,j) - 4     
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A2(WGT_BLOCK(WB2,6),WGT_BLOCK(WB1,7))   

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction( min(erri) )
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

      deallocate(A2) 

      end subroutine distrmix22

C ----------------------------------------------------distrmix23
c 23  DISTRIBUTE arrA2[MULT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [WGT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [MULT_BLOCK][WGT_BLOCK]  

      subroutine distrmix23

      integer, parameter :: AN1=18,AN2=12,NL=1000,ER=10000
      integer :: erri= ER,i,j

      integer, parameter :: m11 = 2, m21 = 2
      integer, parameter :: m12 = 3, m22 = 3

      double precision, dimension(10) ::
     >       WB1=(/2., 1.2, 2., 2.5, 0.2, 1.5, 1., 2.8, 2.1, 3./)
      double precision, dimension(8) ::
     >       WB2=(/3.0, 3.5, 2.0, 1.2, 2.6, 1.6, 3.5, 0.7/)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrmix23 '
               
!dvm$ distribute A2(MULT_BLOCK(m11),MULT_BLOCK(m21))   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

      A2 = 5

!dvm$ actual(A2)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) + i*NL+j     
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A2(WGT_BLOCK(WB1,10),MULT_BLOCK(m22))   

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =A2(i,j) - 4     
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A2(MULT_BLOCK(m12),WGT_BLOCK(WB2,8))   

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction( min(erri) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)+ 1) then     
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

      deallocate(A2)
  
      end subroutine distrmix23

C ----------------------------------------------------distrmix24
c 24  DISTRIBUTE arrA2[MULT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK]  
c              REDISTRIBUTE [MULT_BLOCK][MULT_BLOCK]  

      subroutine distrmix24 (psize)
      integer psize(2)

      integer, parameter :: AN1=30,AN2=30,NL=1000,ER=10000
      integer :: erri= ER,i,j

      integer, parameter :: m1 = 3, m2 = 5

      integer, dimension(1) :: BSi1=(/30/)     
      integer, dimension(2) :: BSi2=(/25,5/)     
      integer, dimension(3) :: BSi3=(/12,4,14/)     
      integer, dimension(4) :: BSi4=(/8,7,5,10/)     

      integer, dimension(1) :: BSj1=(/30/)     
      integer, dimension(2) :: BSj2=(/12,18/)     
      integer, dimension(3) :: BSj3=(/5,16,9/)     
      integer, dimension(4) :: BSj4=(/10,4,14,2/)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrmix24 '
               
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

!!!!dvm$ redistribute A2(GEN_BLOCK(BSi),GEN_BLOCK(BSj))   

      select case(psize(1))

      case(1)
          select case(psize(2))
              case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj1))    
              case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj2))    
              case (3)
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj3))    
              case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj4))    
              case default 
                 goto 10
          endselect  

      case(2)
          select case(psize(2))
              case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj1))    
              case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj2))    
              case (3)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj3))    
              case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj4))    
              case default 
                 goto 10
          endselect  

      case (3)
          select case(psize(2))
              case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj1))    
              case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj2))    
              case (3)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj3))    
              case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj4))    
              case default 
                 goto 10
          endselect  

      case(4)
          select case(psize(2))
              case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj1))    
              case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj2))    
              case (3)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj3))    
              case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj4))    
              case default 
                 goto 10
          endselect  

      case default 
         goto 10
      endselect  

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =A2(i,j) * 2     
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A2(MULT_BLOCK(m2),MULT_BLOCK(m1))   

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) / 2     
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

      end subroutine distrmix24

C ----------------------------------------------------distrmix25
c 25  DISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK]  
c              REDISTRIBUTE [MULT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK]  

      subroutine distrmix25 (psize)
      integer psize(2)

      integer, parameter :: AN1=16,AN2=12,NL=1000,ER=10000
      integer :: erri= ER,i,j

      integer, parameter :: m1 = 2, m2 = 3

      integer, dimension(1) :: BSi11=(/16/)     
      integer, dimension(1) :: BSi12=(/16/)     
      integer, dimension(2) :: BSi21=(/15,1/)     
      integer, dimension(2) :: BSi22=(/6,10/)     
      integer, dimension(3) :: BSi31=(/3,8,5/)     
      integer, dimension(3) :: BSi32=(/7,3,6/)     
      integer, dimension(4) :: BSi41=(/2,3,4,7/)     
      integer, dimension(4) :: BSi42=(/5,1,6,4/)     

      integer, dimension(1) :: BSj11=(/12/)     
      integer, dimension(1) :: BSj12=(/12/)     
      integer, dimension(2) :: BSj21=(/7,5/)     
      integer, dimension(2) :: BSj22=(/5,7/)     
      integer, dimension(3) :: BSj31=(/5,6,1/)     
      integer, dimension(3) :: BSj32=(/2,6,4/)     
      integer, dimension(4) :: BSj41=(/1,4,2,5/)     
      integer, dimension(4) :: BSj42=(/2,4,4,2/)     

      integer  :: A2(AN1,AN2)            ! static array
      character(*), parameter :: tname='distrmix25 '
               
!dvm$ distribute :: A2   
!dvm$ dynamic A2

!!!!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj1))   

      select case(psize(1))

      case(1)
          select case(psize(2))
              case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi11),GEN_BLOCK(BSj11))    
              case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi11),GEN_BLOCK(BSj21))    
              case (3)
!dvm$ redistribute A2(GEN_BLOCK(BSi11),GEN_BLOCK(BSj31))    
              case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi11),GEN_BLOCK(BSj41))    
              case default 
                 goto 10
          endselect  

      case(2)
          select case(psize(2))
              case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi21),GEN_BLOCK(BSj11))    
              case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi21),GEN_BLOCK(BSj21))    
              case (3)
!dvm$ redistribute A2(GEN_BLOCK(BSi21),GEN_BLOCK(BSj31))    
              case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi21),GEN_BLOCK(BSj41))    
              case default 
                 goto 10
          endselect  

      case (3)
          select case(psize(2))
              case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi31),GEN_BLOCK(BSj11))    
              case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi31),GEN_BLOCK(BSj21))    
              case (3)
!dvm$ redistribute A2(GEN_BLOCK(BSi31),GEN_BLOCK(BSj31))    
              case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi31),GEN_BLOCK(BSj41))    
              case default 
                 goto 10
          endselect  

      case(4)
          select case(psize(2))
              case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi41),GEN_BLOCK(BSj11))    
              case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi41),GEN_BLOCK(BSj21))    
              case (3)
!dvm$ redistribute A2(GEN_BLOCK(BSi41),GEN_BLOCK(BSj31))    
              case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi41),GEN_BLOCK(BSj41))    
              case default 
                 goto 10
          endselect  

      case default 
         goto 10
      endselect  

      A2 = 2

!dvm$ actual(A2)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) + i*NL+j     
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A2(MULT_BLOCK(m1),MULT_BLOCK(m2))   

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) + 2     
          enddo
      enddo
!dvm$ end region 

!!!!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj2))    

      select case(psize(1))

      case(1)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi12),GEN_BLOCK(BSj12))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi12),GEN_BLOCK(BSj22))    
          case (3)
!dvm$ redistribute A2(GEN_BLOCK(BSi12),GEN_BLOCK(BSj32))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi12),GEN_BLOCK(BSj42))    
          case default 
             goto 10
          endselect  

      case(2)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi22),GEN_BLOCK(BSj12))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi22),GEN_BLOCK(BSj22))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi22),GEN_BLOCK(BSj32))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi22),GEN_BLOCK(BSj42))    
          case default 
             goto 10
          endselect  

      case (3)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi32),GEN_BLOCK(BSj12))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi32),GEN_BLOCK(BSj22))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi32),GEN_BLOCK(BSj32))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi32),GEN_BLOCK(BSj42))    
          case default 
             goto 10
          endselect  

      case(4)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi42),GEN_BLOCK(BSj12))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi42),GEN_BLOCK(BSj22))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi42),GEN_BLOCK(BSj32))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi42),GEN_BLOCK(BSj42))    
          case default 
             goto 10
          endselect  

      case default 
         goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =A2(i,j)      
            if (A2(i,j) /= (i*NL+j)+ 4) then     
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
 
 10   continue

      end subroutine distrmix25

C ----------------------------------------------------distrmix26
c 26  DISTRIBUTE arrA2[WGT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [MULT_BLOCK][WGT_BLOCK]  
c              REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK]  

      subroutine distrmix26 (psize)
      integer psize(2)

      integer, parameter :: AN1=52,AN2=50,NL=1000,ER=10000
      integer :: erri= ER,i,j

      integer, parameter :: m1 = 13, m2 = 5

      double precision, dimension(6) ::
     >       WB1=(/2.4, 2.2, 0.2, 3.5, 1.2, 1./)     
      double precision, dimension(8) ::
     >       WB2=(/1.0, 2.5, 3.0, 2.8, 1.6, 1., 0.5, 1.7/)     

      integer, dimension(1) :: BSi1=(/52/)     
      integer, dimension(2) :: BSi2=(/15,37/)     
      integer, dimension(3) :: BSi3=(/20,28,4/)     
      integer, dimension(4) :: BSi4=(/6,24,4,18/)     

      integer, dimension(1) :: BSj1=(/50/)     
      integer, dimension(2) :: BSj2=(/16,34/)     
      integer, dimension(3) :: BSj3=(/22,28,0/)     
      integer, dimension(4) :: BSj4=(/11,14,8,17/)     !rem

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrmix26 '
               
!dvm$ distribute A2(WGT_BLOCK(WB1,6),MULT_BLOCK(m2))   

!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = i*NL+j     
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A2(MULT_BLOCK(m1),WGT_BLOCK(WB2,8))   

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) * 5     
          enddo
      enddo
!dvm$ end region 

!!!!dvm$ redistribute A2(GEN_BLOCK(BSi),GEN_BLOCK(BSj))    

      select case(psize(1))

      case(1)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj1))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj2))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj3))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj4))    
          case default 
             goto 10
          endselect  

      case(2)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj1))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj2))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj3))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj4))    
          case default 
             goto 10
          endselect  

      case (3)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj1))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj2))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj3))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj4))    
          case default 
             goto 10
          endselect  

      case(4)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj1))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj2))    
          case (3)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj3))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj4))    
          case default 
             goto 10
          endselect  

      case default 
         goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction( min(erri) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)* 5) then     
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

      end subroutine distrmix26

C ----------------------------------------------------distrmix27
c 27  DISTRIBUTE arrA2[BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [MULT_BLOCK][WGT_BLOCK]  
c              REDISTRIBUTE [GEN_BLOCK][BLOCK]  

      subroutine distrmix27 (psize)
      integer psize (2)

      integer, parameter :: AN1=8,AN2=64,NL=1000,ER=10000
      integer :: erri= ER,i,j

      integer, parameter :: m1 = 2, m2 = 8

      double precision, dimension(7) ::
     >       WB=(/2., 3.2, 2., 3.5, 1.2, 1., 4./)     

      integer, dimension(1) :: BSi1=(/8/)     
      integer, dimension(2) :: BSi2=(/2,6/)     
      integer, dimension(3) :: BSi3=(/4,3,1/)     
      integer, dimension(4) :: BSi4=(/2,3,2,1/)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrmix27 '
               
!dvm$ distribute A2(BLOCK,MULT_BLOCK(m2))   
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

!dvm$ redistribute A2(MULT_BLOCK(m1),WGT_BLOCK(WB,7))    

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =A2(i,j) * 2     
          enddo
      enddo
!dvm$ end region 

!!!!dvm$ redistribute A2(GEN_BLOCK(BSi),BLOCK)    

      select case(psize(1))
      case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi1),BLOCK)    
      case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),BLOCK)    
      case (3)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),BLOCK)    
      case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),BLOCK)    
      case default 
         goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)* 4) then     
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

      end subroutine distrmix27

C ----------------------------------------------------distrmix28
c 28  DISTRIBUTE arrA2[BLOCK][GEN_BLOCK]  
c              REDISTRIBUTE [WGT_BLOCK][MULT_BLOCK]  
c              REDISTRIBUTE [BLOCK][WGT_BLOCK]  

      subroutine distrmix28 (psize)
      integer psize (2)

      integer, parameter :: AN1=42,AN2=16,NL=1000,ER=10000
      integer :: erri= ER,i,j
               
      integer, parameter :: m1 = 3, m2 = 2

      double precision, dimension(6) ::
     >       WB1=(/2., 3., 1.2, 1.5, 1., 1.5/)     
      double precision, dimension(7) ::
     >       WB2=(/2.2, 1.5, 3.0, 2.8, 2.6, 1.4, 0.5/)     

      integer, dimension(1) :: BSj1=(/16/)     
      integer, dimension(2) :: BSj2=(/12,4/)     
      integer, dimension(3) :: BSj3=(/5,1,10/)     
      integer, dimension(4) :: BSj4=(/2,4,6,4/)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrmix28 '

!dvm$ distribute :: A2
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!!!!dvm$ distribute A2(BLOCK(m1),GEN_BLOCK(Bj))   

      select case(psize(2))
      case(1) 
!dvm$ redistribute A2(BLOCK,GEN_BLOCK(BSj1))    
      case(2)
!dvm$ redistribute A2(BLOCK,GEN_BLOCK(BSj2))    
      case (3)
!dvm$ redistribute A2(BLOCK,GEN_BLOCK(BSj3))    
      case(4)
!dvm$ redistribute A2(BLOCK,GEN_BLOCK(BSj4))    
      case default 
         goto 10
      endselect  

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =(i*NL+j)*3     
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A2(WGT_BLOCK(WB1,6),MULT_BLOCK(m2))    

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j)*2     
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A2(BLOCK, WGT_BLOCK(WB2,7))    

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)*6) then     
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

      end

C ----------------------------------------------------distrmix29
c 29  DISTRIBUTE arrA2[WGT_BLOCK][BLOCK]  
c              REDISTRIBUTE [GEN_BLOCK][GEN_BLOCK]  
c              REDISTRIBUTE [BLOCK][MULT_BLOCK]  

      subroutine distrmix29 (psize)
      integer psize(2)

      integer, parameter :: AN1=21,AN2=48, NL=1000,ER=10000
      integer :: erri= ER,i,j
               
      integer, parameter :: m1 = 3, m2 = 2

      double precision, dimension(9) ::
     >     WB=(/2.2, 1.5, 3.0, 2.8, 2.6, 1.4, 0.5, 1., 2./)     

      integer, dimension(1) :: BSi1=(/21/)     
      integer, dimension(2) :: BSi2=(/15,6/)     
      integer, dimension(3) :: BSi3=(/10,6,5/)     
      integer, dimension(4) :: BSi4=(/6,4,8,3/)     

      integer, dimension(1) :: BSj1=(/48/)     
      integer, dimension(2) :: BSj2=(/16,32/)     
      integer, dimension(3) :: BSj3=(/20,18,10/)     
      integer, dimension(4) :: BSj4=(/2,42,1,3/)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrmix29 '

!dvm$ distribute :: A2
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!dvm$ redistribute A2(WGT_BLOCK(WB,9),BLOCK)   

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =(i*NL+j)     
          enddo
      enddo
!dvm$ end region 

!!!!dvm$ redistribute A2(GEN_BLOCK(BSi),GEN_BLOCK(BSj))    

      select case(psize(1))

      case(1)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj1))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj2))    
          case (3)
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj3))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj4))    
          case default 
             goto 10
          endselect  

      case(2)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj1))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj2))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj3))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj4))    
          case default 
             goto 10
          endselect  

      case (3)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj1))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj2))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj3))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj4))    
          case default 
             goto 10
          endselect  

      case(4)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj1))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj2))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj3))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj4))    
          case default 
             goto 10
          endselect  

      case default 
         goto 10
      endselect  

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j)*4     
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A2(BLOCK, MULT_BLOCK(m2))    

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)*4) then     
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

      end subroutine distrmix29

C ----------------------------------------------------distrmix210
c 210  DISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK] 
c               REDISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK] 
c               REDISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]

      subroutine distrmix210 (psize)
      integer psize(2)

      integer, parameter :: AN1=9,AN2=11,NL=1000,ER=10000
      integer :: erri= ER,i,j

      integer, dimension(1) :: BSi1=(/9/)     
      integer, dimension(2) :: BSi2=(/3,6/)     
      integer, dimension(3) :: BSi3=(/1,3,5/)     
      integer, dimension(4) :: BSi4=(/2,3,1,3/)     

      integer, dimension(1) :: BSj1=(/11/)     
      integer, dimension(2) :: BSj2=(/7,4/)     
      integer, dimension(3) :: BSj3=(/5,6,0/)     
      integer, dimension(4) :: BSj4=(/2,3,2,4/)     

      double precision, dimension(6) :: 
     >             WB1=(/1.0, 1.2, 2.5, 1.4, 2.5, 1.3/)     
      double precision, dimension(4) ::
     >             WB2=(/1.0,2.,1.5,1.7/)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrmix210'
               
!dvm$ distribute A2(WGT_BLOCK(WB1,6),WGT_BLOCK(WB2,4))   
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

!!!!dvm$ redistribute A2(GEN_BLOCK(BSi),GEN_BLOCK(BSj))    

      select case(psize(1))

      case(1)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj1))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj2))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj3))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj4))    
          case default 
             goto 10
          endselect  

      case(2)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj1))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj2))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj3))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj4))    
          case default 
             goto 10
          endselect  

      case (3)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj1))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj2))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj3))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj4))    
          case default 
             goto 10
          endselect  

      case(4)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj1))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj2))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj3))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj4))    
          case default 
             goto 10
          endselect  

      case default 
         goto 10
      endselect  

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) * 2     
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A2(WGT_BLOCK(WB2,4),WGT_BLOCK(WB1,6))   

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) .ne.(i*NL+j)*2) then     
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

      end subroutine distrmix210

C ----------------------------------------------------distrmix211
c 211  DISTRIBUTE arrA2[WGT_BLOCK][BLOCK] 
c               REDISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK] 
c               REDISTRIBUTE arrA2[BLOCK][WGT_BLOCK]

      subroutine distrmix211 (psize)
      integer psize(2)

      integer, parameter :: AN1=16,AN2=16,NL=1000,ER=10000
      integer :: erri= ER,i,j

      double precision, dimension(7) :: 
     >             WB=(/1.0, 1.2, 2.5, 1.4, 2.5, 1.3, 2./)

      integer, dimension(1) :: BSi1=(/16/)     
      integer, dimension(2) :: BSi2=(/10,6/)     
      integer, dimension(3) :: BSi3=(/8,3,5/)     
      integer, dimension(4) :: BSi4=(/2,3,4,7/)     !rem

      integer, dimension(1) :: BSj1=(/16/)     
      integer, dimension(2) :: BSj2=(/7,9/)     
      integer, dimension(3) :: BSj3=(/5,6,5/)     
      integer, dimension(4) :: BSj4=(/1,4,8,3/)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrmix211'
               
!dvm$ distribute A2(WGT_BLOCK(WB,7),BLOCK)   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j + 2     
          enddo
      enddo
!dvm$ end region 

!!!!dvm$ redistribute A2(GEN_BLOCK(BSi),GEN_BLOCK(BSj))    

      select case(psize(1))

      case(1)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj1))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj2))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj3))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj4))    
          case default 
             goto 10
          endselect  

      case(2)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj1))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj2))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj3))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj4))    
          case default 
             goto 10
          endselect  

      case (3)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj1))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj2))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj3))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),GEN_BLOCK(BSj4))    
          case default 
             goto 10
          endselect  

      case(4)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj1))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj2))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj3))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),GEN_BLOCK(BSj4))    
          case default 
             goto 10
          endselect  

      case default 
         goto 10
      endselect  

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) + 2     
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A2(BLOCK,WGT_BLOCK(WB,5))   

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) .ne.(i*NL+j + 4)) then     
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

      end subroutine distrmix211

C ----------------------------------------------------distrmix212
c 212  DISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK]  
c               REDISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]
c               REDISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK] 

      subroutine distrmix212 (psize)
      integer psize(2)

      integer, parameter :: AN1=6,AN2=28,NL=1000,ER=10000
      integer :: erri= ER,i

      double precision, dimension(8) :: 
     >             WB1=(/1.0, 1.2, 2.5, 1.4, 2.5, 1.3, 1., 2./)     
      double precision, dimension(5) ::
     >             WB2=(/2., 1.3, 2., 1.0, 1.7/)     

      integer, dimension(1) :: BSi11=(/6/)     
      integer, dimension(1) :: BSi12=(/6/)     
      integer, dimension(2) :: BSi21=(/1,5/)     
      integer, dimension(2) :: BSi22=(/4,2/)     
      integer, dimension(3) :: BSi31=(/0,4,2/)     
      integer, dimension(3) :: BSi32=(/1,3,2/)     
      integer, dimension(4) :: BSi41=(/2,3,1,0/)     
      integer, dimension(4) :: BSi42=(/1,2,1,2/)     

      integer, dimension(1) :: BSj11=(/28/)     
      integer, dimension(1) :: BSj12=(/28/)     
      integer, dimension(2) :: BSj21=(/13,15/)     
      integer, dimension(2) :: BSj22=(/7,21/)     
      integer, dimension(3) :: BSj31=(/8,8,12/)     
      integer, dimension(3) :: BSj32=(/5,18,5/)     
      integer, dimension(4) :: BSj41=(/2,12,3,11/)     
      integer, dimension(4) :: BSj42=(/6,4,8,10/)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrmix212'
               
!dvm$ distribute :: A2   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!!!!dvm$ distribute A2(GEN_BLOCK(BSi1),GEN_BLOCK(BSj1))   

      select case(psize(1))

      case(1)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi11),GEN_BLOCK(BSj11))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi11),GEN_BLOCK(BSj21))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi11),GEN_BLOCK(BSj31))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi11),GEN_BLOCK(BSj41))    
          case default 
             goto 10
          endselect  

      case(2)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi21),GEN_BLOCK(BSj11))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi21),GEN_BLOCK(BSj21))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi21),GEN_BLOCK(BSj31))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi21),GEN_BLOCK(BSj41))    
          case default 
             goto 10
          endselect  

      case (3)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi31),GEN_BLOCK(BSj11))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi31),GEN_BLOCK(BSj21))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi31),GEN_BLOCK(BSj31))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi31),GEN_BLOCK(BSj41))    
          case default 
             goto 10
          endselect  

      case(4)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi41),GEN_BLOCK(BSj11))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi41),GEN_BLOCK(BSj21))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi41),GEN_BLOCK(BSj31))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi41),GEN_BLOCK(BSj41))    
          case default 
             goto 10
          endselect  

      case default 
         goto 10
      endselect  

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2          
            A2(i,j) =i*NL+j     
          enddo
      enddo
!dvm$ end region 

!      print *, 'A2 =' 
!      print *, A2
 
!dvm$ redistribute A2(WGT_BLOCK(WB1,8),WGT_BLOCK(WB2,5))   

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) + 1    
          enddo
      enddo
!dvm$ end region 

!!!!dvm$ redistribute A2(GEN_BLOCK(BSi2),GEN_BLOCK(BSj2))   

      select case(psize(1))

      case(1)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi12),GEN_BLOCK(BSj12))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi12),GEN_BLOCK(BSj22))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi12),GEN_BLOCK(BSj32))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi12),GEN_BLOCK(BSj42))    
          case default 
             goto 10
          endselect  

      case(2)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi22),GEN_BLOCK(BSj12))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi22),GEN_BLOCK(BSj22))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi22),GEN_BLOCK(BSj32))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi22),GEN_BLOCK(BSj42))    
          case default 
             goto 10
          endselect  

      case (3)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi32),GEN_BLOCK(BSj12))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi32),GEN_BLOCK(BSj22))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi32),GEN_BLOCK(BSj32))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi32),GEN_BLOCK(BSj42))    
          case default 
             goto 10
          endselect  

      case(4)
          select case(psize(2))
          case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi42),GEN_BLOCK(BSj12))    
          case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi42),GEN_BLOCK(BSj22))    
          case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi42),GEN_BLOCK(BSj32))    
          case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi42),GEN_BLOCK(BSj42))    
          case default 
             goto 10
          endselect  

      case default 
         goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction(min(erri))
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) - 1    
            if (A2(i,j) /= (i*NL+j)) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region 

!      print *, 'A2 =' 
!      print *, A2

!dvm$ get_actual(erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10    deallocate (A2)

      end subroutine distrmix212

C ----------------------------------------------------distrmix213
c 213  DISTRIBUTE arrA2[BLOCK][GEN_BLOCK] 
c               REDISTRIBUTE arrA2[WGT_BLOCK][MULT_BLOCK]
c               REDISTRIBUTE arrA2[GEN_BLOCK][BLOCK] 

      subroutine distrmix213 (psize)
      integer psize(2)

      integer, parameter :: AN1=27,AN2=14,NL=1000,ER=10000
      integer :: erri= ER,i

      integer, parameter :: m1 = 3, m2 = 2

      double precision, dimension(4) :: 
     >             WB=(/1.2, 1.6, 2.0, 1.8/)     

      integer, dimension(1) :: BSi1=(/27/)     
      integer, dimension(2) :: BSi2=(/13,14/)     
      integer, dimension(3) :: BSi3=(/11,13,3/)     
      integer, dimension(4) :: BSi4=(/3,5,11,8/)     

      integer, dimension(1) :: BSj1=(/14/)     
      integer, dimension(2) :: BSj2=(/12,2/)     
      integer, dimension(3) :: BSj3=(/5,6,3/)     
      integer, dimension(4) :: BSj4=(/2,3,5,4/)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrmix213'
               
!dvm$ distribute ::A2   
!dvm$ dynamic A2    

      allocate (A2(AN1,AN2))

!!!!dvm$ redistribute A2(BLOCK,GEN_BLOCK(BSj))   

      select case(psize(2))
      case(1) 
!dvm$ redistribute A2(BLOCK,GEN_BLOCK(BSj1))    
      case(2)
!dvm$ redistribute A2(BLOCK,GEN_BLOCK(BSj2))    
      case(3)
!dvm$ redistribute A2(BLOCK,GEN_BLOCK(BSj3))    
      case(4)
!dvm$ redistribute A2(BLOCK,GEN_BLOCK(BSj4))    
      case default 
         goto 10
      endselect  

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j + 4     
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A2(WGT_BLOCK(WB,4), MULT_BLOCK(m2))    

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =A2(i,j) + 4     
          enddo
      enddo
!dvm$ end region 

!!!!dvm$ redistribute A2(GEN_BLOCK(BSi),BLOCK)   

      select case(psize(1))
      case(1) 
!dvm$ redistribute A2(GEN_BLOCK(BSi1),BLOCK)    
      case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi2),BLOCK)    
      case(3)
!dvm$ redistribute A2(GEN_BLOCK(BSi3),BLOCK)    
      case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi4),BLOCK)    
      case default 
         goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction(min(erri))
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= i*NL+j+8) then     
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

      end subroutine distrmix213

C -------------------------------------------------

      subroutine ansyes(name)
      character(*) name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character(*) name
      print *,name,'  -  ***error'
      end
