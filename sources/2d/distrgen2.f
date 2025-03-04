      program DISTRG2

!    Testing DISTRIBUTE and REDISTRIBUTE directives
!            GEN_BLOCK, BLOCK, *  distributions
      
      integer PROCESSORS_RANK, PROCESSORS_SIZE 
      integer psize(2), rank

      PROCESSORS_RANK() = 2
      PROCESSORS_SIZE(i) = 1

      print *,'===START OF distrgen2========================'

      rank = PROCESSORS_RANK()

      do i=1,rank
         psize(i)=PROCESSORS_SIZE(i)
         if (psize(i) > 4) then   !may be temporary
             goto 1
         endif
      enddo

C -------------------------------------------------
c 25  DISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK] 
c               REDISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK] other blocks
         call distrg25 (psize)
C -------------------------------------------------
c 26  DISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK] 
c                             REDISTRIBUTE arrA2[BLOCK][BLOCK]
c                             REDISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK]
         call distrg26 (psize)
C -------------------------------------------------
c 27  DISTRIBUTE arrA2[GEN_BLOCK][BLOCK]
c                         REDISTRIBUTE arrA2[BLOCK][GEN_BLOCK]
c                         REDISTRIBUTE arrA2[GEN_BLOCK][BLOCK]
         call distrg27 (psize)
C -------------------------------------------------
c 28  DISTRIBUTE arrA2[BLOCK][GEN_BLOCK]
c                         REDISTRIBUTE arrA2[BLOCK][BLOCK]
c                         REDISTRIBUTE arrA2[GEN_BLOCK][BLOCK]
      call distrg28 (psize)
C -------------------------------------------------
c 29  DISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK] 
c                         REDISTRIBUTE arrA2[*][*]
c                         REDISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK]
      call distrg29 (psize)
C -------------------------------------------------
c 210 DISTRIBUTE arrA2[GEN_BLOCK][*] 
c                         REDISTRIBUTE arrA2[*][*]
c                         REDISTRIBUTE arrA2[*][GEN_BLOCK]
      call distrg210 (psize)
C ----------------------------------------------------

 1    print *,'=== END OF distrgen2 ========================= '    

      end

C ----------------------------------------------------distrg25
c 25  DISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK] 
c               REDISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK] other blocks

      subroutine distrg25 (psize)
      integer psize(2)

      integer, parameter :: AN1=10,AN2=12,NL=1000,ER=10000
      integer :: erri= ER,i,j
 
      integer, dimension(1) :: BSi11=(/10/)     
      integer, dimension(1) :: BSi12=(/10/)     
      integer, dimension(2) :: BSi21=(/5,5/)     
      integer, dimension(2) :: BSi22=(/6,4/)     
      integer, dimension(3) :: BSi31=(/2,3,5/)     
      integer, dimension(3) :: BSi32=(/8,1,1/)     
      integer, dimension(4) :: BSi41=(/2,3,4,1/)     
      integer, dimension(4) :: BSi42=(/2,1,3,4/)     

      integer, dimension(1) :: BSj11=(/12/)     
      integer, dimension(1) :: BSj12=(/12/)     
      integer, dimension(2) :: BSj21=(/7,5/)     
      integer, dimension(2) :: BSj22=(/5,7/)     
      integer, dimension(3) :: BSj31=(/5,6,1/)     
      integer, dimension(3) :: BSj32=(/2,6,4/)     
      integer, dimension(4) :: BSj41=(/1,4,2,5/)     
      integer, dimension(4) :: BSj42=(/2,4,4,2/)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrg25  '
               
!dvm$ distribute :: A2   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

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

      A2 = 1

!dvm$ actual(A2) 

!dvm$ region  
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j)+ i*NL+j     
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
              case (3)
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
              case (3)
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
              case (3)
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
            if (A2(i,j) .ne.(i*NL+j) + 1) then     
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

      end subroutine distrg25

C ----------------------------------------------------distrg26
c 26  DISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK] 
c                             REDISTRIBUTE arrA2[BLOCK][BLOCK]
c                             REDISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK]

      subroutine distrg26 (psize)
      integer psize(2)

      integer, parameter :: AN1=16,AN2=16,NL=1000,ER=10000
      integer :: erri= ER,i

      integer, dimension(1) :: BSi11=(/16/)     
      integer, dimension(1) :: BSi12=(/16/)     
      integer, dimension(2) :: BSi21=(/5,11/)     
      integer, dimension(2) :: BSi22=(/10,6/)     
      integer, dimension(3) :: BSi31=(/6,3,7/)     
      integer, dimension(3) :: BSi32=(/8,4,4/)     
      integer, dimension(4) :: BSi41=(/4,3,4,5/)     
      integer, dimension(4) :: BSi42=(/2,5,3,6/)     

      integer, dimension(1) :: BSj11=(/16/)     
      integer, dimension(1) :: BSj12=(/16/)     
      integer, dimension(2) :: BSj21=(/7,9/)     
      integer, dimension(2) :: BSj22=(/10,6/)     
      integer, dimension(3) :: BSj31=(/5,6,5/)     
      integer, dimension(3) :: BSj32=(/6,6,4/)     
      integer, dimension(4) :: BSj41=(/1,8,2,5/)     
      integer, dimension(4) :: BSj42=(/4,4,4,4/)     

      integer, allocatable :: A2(:,:)
      character(10), parameter :: tname='distrg26  '
               
!dvm$ distribute :: A2   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

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

!dvm$ region out(A2) 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo
!dvm$ end region   

!dvm$ redistribute A2(BLOCK,BLOCK)    

!dvm$ region  
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =A2(i,j) + 1    
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
              case (3)
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
              case (3)
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
              case (3)
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

!dvm$ region in(A2), local(A2) 
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

!dvm$ get_actual(erri) 

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

 10   deallocate (A2)

      end subroutine distrg26

C ----------------------------------------------------distrg27
c 27  DISTRIBUTE arrA2[GEN_BLOCK][BLOCK]
c                         REDISTRIBUTE arrA2[BLOCK][GEN_BLOCK]
c                         REDISTRIBUTE arrA2[GEN_BLOCK][BLOCK]

      subroutine distrg27 (psize)
      integer psize(2)

      integer, parameter :: AN1=11,AN2=15,NL=1000,ER=10000
      integer :: erri= ER,i,j
 
      integer, dimension(1) :: BSi11=(/11/)     
      integer, dimension(1) :: BSi12=(/11/)     
      integer, dimension(2) :: BSi21=(/6,5/)     
      integer, dimension(2) :: BSi22=(/7,4/)     
      integer, dimension(3) :: BSi31=(/4,3,4/)     
      integer, dimension(3) :: BSi32=(/8,2,1/)     
      integer, dimension(4) :: BSi41=(/3,3,4,1/)     
      integer, dimension(4) :: BSi42=(/2,2,3,4/)     

      integer, dimension(1) :: BSj11=(/15/)     
      integer, dimension(1) :: BSj12=(/15/)     
      integer, dimension(2) :: BSj21=(/10,5/)     
      integer, dimension(2) :: BSj22=(/8,7/)     
      integer, dimension(3) :: BSj31=(/5,6,4/)     
      integer, dimension(3) :: BSj32=(/2,7,8/)     
      integer, dimension(4) :: BSj41=(/4,4,2,5/)     
      integer, dimension(4) :: BSj42=(/2,3,7,4/)     

      integer, allocatable :: A2(:,:)
      character(10), parameter :: tname='distrg27  '
               
!dvm$ distribute :: A2   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!!!!dvm$ redistribute A2(GEN_BLOCK(BSi1),BLOCK)   

      select case(psize(1))

      case(1)
!dvm$ redistribute A2(GEN_BLOCK(BSi11),BLOCK)    
      case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi21),BLOCK)    
      case (3)
!dvm$ redistribute A2(GEN_BLOCK(BSi31),BLOCK)    
      case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi41),BLOCK)    
      case default 
      goto 10
      endselect  

      A2 = 5

!dvm$ actual(A2) 

!dvm$ region  inout (A2)
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j)+ i*NL+j     
          enddo
      enddo
!dvm$ end region   

!!!!dvm$ redistribute A2(BLOCK, GEN_BLOCK(BSj1))    

      select case(psize(2))

      case(1)
!dvm$ redistribute A2(BLOCK, GEN_BLOCK(BSj11))    
      case(2)
!dvm$ redistribute A2(BLOCK, GEN_BLOCK(BSj21))    
      case (3)
!dvm$ redistribute A2(BLOCK, GEN_BLOCK(BSj31))    
      case(4)
!dvm$ redistribute A2(BLOCK, GEN_BLOCK(BSj41))    
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

!!!!dvm$ redistribute A2(GEN_BLOCK(BSi2),BLOCK)   

      select case(psize(1))

      case(1)
!dvm$ redistribute A2(GEN_BLOCK(BSi12),BLOCK)    
      case(2)
!dvm$ redistribute A2(GEN_BLOCK(BSi22),BLOCK)    
      case (3)
!dvm$ redistribute A2(GEN_BLOCK(BSi32),BLOCK)    
      case(4)
!dvm$ redistribute A2(GEN_BLOCK(BSi42),BLOCK)    
      case default 
      goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region  in(A2), local(A2)
!dvm$ parallel (i,j) on A2(i,j), reduction(min(erri))
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) / 2    
            if (A2(i,j) /= (i*NL+j+5)) then     
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

      end subroutine distrg27

C ----------------------------------------------------distrg28
c 28  DISTRIBUTE arrA2[BLOCK][GEN_BLOCK]
c                         REDISTRIBUTE arrA2[BLOCK][BLOCK]
c                         REDISTRIBUTE arrA2[GEN_BLOCK][BLOCK]

      subroutine distrg28 (psize)
      integer psize(2)

      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
      integer :: erri= ER,i,j
 
      integer, dimension(1) :: BSi1=(/8/)     
      integer, dimension(2) :: BSi2=(/6,2/)     
      integer, dimension(3) :: BSi3=(/3,3,2/)     
      integer, dimension(4) :: BSi4=(/2,2,2,2/)     

      integer, dimension(1) :: BSj1=(/8/)     
      integer, dimension(2) :: BSj2=(/4,4/)     
      integer, dimension(3) :: BSj3=(/5,1,2/)     
      integer, dimension(4) :: BSj4=(/2,1,2,3/)     

      integer, allocatable :: A2(:,:)
      character(10), parameter :: tname='distrg28  '
               
!dvm$ distribute :: A2   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!!!!dvm$ redistribute A2(BLOCK, GEN_BLOCK(BSj1))   

      select case(psize(2))

      case(1)
!dvm$ redistribute A2(BLOCK, GEN_BLOCK(BSj1))    
      case(2)
!dvm$ redistribute A2(BLOCK, GEN_BLOCK(BSj2))    
      case (3)
!dvm$ redistribute A2(BLOCK, GEN_BLOCK(BSj3))    
      case(4)
!dvm$ redistribute A2(BLOCK, GEN_BLOCK(BSj4))    
      case default 
      goto 10
      endselect  

!dvm$ region out(A2(1:AN1, 1:AN2)) 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo
!dvm$ end region   

!dvm$ redistribute A2(BLOCK, BLOCK)

!dvm$ region  inout(A2)
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =A2(i,j) + 5    
          enddo
      enddo
!dvm$ end region   

!!!!dvm$ redistribute A2(GEN_BLOCK(BSi2),BLOCK)   

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
!dvm$ parallel (i,j) on A2(i,j), reduction(min(erri))
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) - 5    
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

      end subroutine distrg28

C ----------------------------------------------------distrg29
c 29  DISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK] 
c                         REDISTRIBUTE arrA2[*][*]
c                         REDISTRIBUTE arrA2[GEN_BLOCK][GEN_BLOCK]

      subroutine distrg29 (psize)
      integer psize(2)

      integer, parameter :: AN1=10,AN2=14,NL=1000,ER=10000
      integer :: erri= ER,i,j
 
      integer, dimension(1) :: BSi11=(/10/)     
      integer, dimension(1) :: BSi12=(/10/)     
      integer, dimension(2) :: BSi21=(/5,5/)     
      integer, dimension(2) :: BSi22=(/6,4/)     
      integer, dimension(3) :: BSi31=(/2,3,5/)     
      integer, dimension(3) :: BSi32=(/8,1,1/)     
      integer, dimension(4) :: BSi41=(/2,3,4,1/)     
      integer, dimension(4) :: BSi42=(/2,1,3,4/)     

      integer, dimension(1) :: BSj11=(/14/)     
      integer, dimension(1) :: BSj12=(/14/)     
      integer, dimension(2) :: BSj21=(/7,7/)     
      integer, dimension(2) :: BSj22=(/5,9/)     
      integer, dimension(3) :: BSj31=(/5,6,3/)     
      integer, dimension(3) :: BSj32=(/2,6,6/)     
      integer, dimension(4) :: BSj41=(/3,4,2,5/)     
      integer, dimension(4) :: BSj42=(/4,4,5,1/)     

      integer, allocatable :: A2(:,:)
      character(10), parameter :: tname='distrg29  '
               
!dvm$ distribute :: A2   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!!!!dvm$ redistribute A2(GEN_BLOCK(BSi1), GEN_BLOCK(BSj1))   

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

!dvm$ region out (A2(1:AN1, 1:4)), out(A2(1:AN1, 5:AN2)) 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo
!dvm$ end region   

!dvm$ redistribute A2(*,*)

!dvm$ region  in(A2), out (A2)
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =A2(i,j) * 3    
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
              case (3)
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
              case (3)
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
              case (3)
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
            A2(i,j) = A2(i,j) / 3    
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

      end subroutine distrg29

C ----------------------------------------------------distrg210
c 210 DISTRIBUTE arrA2[GEN_BLOCK][*] 
c                         REDISTRIBUTE arrA2[*][*]
c                         REDISTRIBUTE arrA2[*][GEN_BLOCK]

      subroutine distrg210 (psize)
      integer psize(2)

      integer, parameter :: AN1=8,AN2=6,NL=1000,ER=10000
      integer :: erri= ER,i,j
 
      integer, dimension(1) :: BSi1=(/8/)     
      integer, dimension(2) :: BSi2=(/6,2/)     
      integer, dimension(3) :: BSi3=(/3,3,2/)     
      integer, dimension(4) :: BSi4=(/1,2,2,3/)     

      integer, dimension(1) :: BSj1=(/6/)     
      integer, dimension(2) :: BSj2=(/2,4/)     
      integer, dimension(3) :: BSj3=(/4,1,1/)     
      integer, dimension(4) :: BSj4=(/2,1,2,1/)     

      integer, allocatable :: A2(:,:)
      character(10), parameter :: tname='distrg210 '
               
!dvm$ distribute :: A2   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!!!!dvm$ redistribute A2(GEN_BLOCK(BSi1),*)   

      select case(psize(1))
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
      
!dvm$ region out (A2(1:3, 1:AN2), A2(4:AN1, 1:AN2)) 
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo
!dvm$ end region   

!dvm$ redistribute A2(*,*)

!dvm$ region  
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =A2(i,j) * 3    
          enddo
      enddo
!dvm$ end region   

!!!!dvm$ redistribute A2(*,GEN_BLOCK(BSj2))   

      select case(psize(1))    ! it's true - psize(1))
      case(1)
!dvm$ redistribute A2(*,GEN_BLOCK(BSj1))    
      case(2)
!dvm$ redistribute A2(*,GEN_BLOCK(BSj2))    
      case (3)
!dvm$ redistribute A2(*,GEN_BLOCK(BSj3))    
      case(4)
!dvm$ redistribute A2(*,GEN_BLOCK(BSj4))    
      case default 
        goto 10
      endselect  

!dvm$ actual(erri)

!dvm$ region   inlocal (A2)
!dvm$ parallel (i,j) on A2(i,j), reduction(min(erri))
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) / 3    
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

      end subroutine distrg210

C -------------------------------------------------

      subroutine ansyes(name)
      character(*) name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character(*) name
      print *,name,'  -  ***error'
      end
