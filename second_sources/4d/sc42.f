      program SC42
     
c    TESTING OF THE SHADOW DIRECTIVE AND THE SHADOW_COMPUTE CLAUSE       
c    DISTRIBUTED ARRAY A(N,M,K,L) IS TO HAVE DIFFERENT SHADOW WIDTH
c    ON BOTH SIDES 

      print *,'===START OF SC42========================'
C --------------------------------------------------
      call sc4201
C --------------------------------------------------
      call sc4202
C --------------------------------------------------
      call sc4203
C -------------------------------------------------
      call sc4204
C -------------------------------------------------
      call sc4205
C -------------------------------------------------
      call sc4206
C --------------------------------------------------
      call sc4207
C --------------------------------------------------
      call sc4208
C----------------------------------------------------
       call sc4209
C----------------------------------------------------

C
C
      print *,'=== END OF SC42 ========================= '    
      end
C ---------------------------------------------SC4201
      subroutine SC4201
      integer, parameter :: N = 16,M=8,K=8,L=8,NL=1000
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:),C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK,*)       
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A

      tname='SC4201'     
      allocate (B(N,M,K,L),A(N,M,K,L),C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL
!dvm$ actual(nloopi,nloopj,nloopii,nloopjj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),shadow_compute
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo
      enddo                                                
 
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj)
      do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
            do jj=2,L-1
            B(i,j,ii,jj) = A(i+1,j+1,ii+1,jj+1)+A(i-1,j-1,ii-1,jj-1)+
     *   A(i+1,j-1,ii-1,jj-1)+A(i-1,j+1,ii-1,jj-1)+
     *   A(i-1,j-1,ii+1,jj-1)+ A(i-1,j-1,ii-1,jj+1)+
     *   A(i+1,j+1,ii-1,jj-1)+A(i-1,j+1,ii+1,jj-1)+
     *   A(i-1,j-1,ii+1,jj+1)+A(i+1,j-1,ii-1,jj+1)+
     *   A(i+1,j-1,ii+1,jj-1)+A(i-1,j+1,ii-1,jj+1)+
     *   A(i+1,j+1,ii+1,jj-1)+A(i-1,j+1,ii+1,jj+1)+
     *   A(i+1,j-1,ii+1,jj+1)+A(i+1,j+1,ii-1,jj+1)
            enddo
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
!dvm$*,private(isum)
      do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
            do jj=2,L-1
              isum = C(i+1,j+1,ii+1,jj+1)+C(i-1,j-1,ii-1,jj-1)+
     *   C(i+1,j-1,ii-1,jj-1)+ C(i-1,j+1,ii-1,jj-1)+
     *   C(i-1,j-1,ii+1,jj-1)+ C(i-1,j-1,ii-1,jj+1)+
     *   C(i+1,j+1,ii-1,jj-1)+ C(i-1,j+1,ii+1,jj-1)+
     *   C(i-1,j-1,ii+1,jj+1)+ C(i+1,j-1,ii-1,jj+1)+
     *   C(i+1,j-1,ii+1,jj-1)+ C(i-1,j+1,ii-1,jj+1)+
     *   C(i+1,j+1,ii+1,jj-1)+ C(i-1,j+1,ii+1,jj+1)+
     *   C(i+1,j-1,ii+1,jj+1)+ C(i+1,j+1,ii-1,jj+1)         
              if (B(i,j,ii,jj).ne.isum) then
                nloopi=min(nloopi,i)
                nloopj=min(nloopj,j)
                nloopii=min(nloopii,ii)
                nloopjj=min(nloopjj,jj)         
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii,nloopjj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)

      end
C ---------------------------------------------SC4202     

      subroutine SC4202     
      integer, parameter :: N = 16,M=10,K=10,L=10,NL=1000
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:),C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,*,BLOCK)   
!dvm$ shadow(2:2,2:2,2:2,2:2) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A

      tname='SC4202'     
      allocate (B(N,M,K,L),A(N,M,K,L),C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual(nloopi,nloopj,nloopii,nloopjj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),shadow_compute
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo
      enddo                                                
 
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj)
      do i=3,N-2
        do j=3,M-2
          do ii=3,K-2
            do jj=3,L-2
            B(i,j,ii,jj) = A(i+2,j+2,ii+2,jj+2)+A(i-2,j-2,ii-2,jj-2)+
     *   A(i+2,j-2,ii-2,jj-2)+ A(i-2,j+2,ii-2,jj-2)+
     *   A(i-2,j-2,ii+2,jj-2)+ A(i-2,j-2,ii-2,jj+2)+
     *   A(i+2,j+2,ii-2,jj-2)+ A(i-2,j+2,ii+2,jj-2)+
     *   A(i-2,j-2,ii+2,jj+2)+ A(i+2,j-2,ii-2,jj+2)+
     *   A(i+2,j-2,ii+2,jj-2)+ A(i-2,j+2,ii-2,jj+2)+
     *   A(i+2,j+2,ii+2,jj-2)+ A(i-2,j+2,ii+2,jj+2)+
     *   A(i+2,j-2,ii+2,jj+2)+ A(i+2,j+2,ii-2,jj+2)
            enddo
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
!dvm$*,private(isum)
      do i=3,N-2
        do j=3,M-2
          do ii=3,K-2
            do jj=3,L-2
              isum = C(i+2,j+2,ii+2,jj+2)+C(i-2,j-2,ii-2,jj-2)+
     *   C(i+2,j-2,ii-2,jj-2)+ C(i-2,j+2,ii-2,jj-2)+
     *   C(i-2,j-2,ii+2,jj-2)+ C(i-2,j-2,ii-2,jj+2)+
     *   C(i+2,j+2,ii-2,jj-2)+ C(i-2,j+2,ii+2,jj-2)+
     *   C(i-2,j-2,ii+2,jj+2)+ C(i+2,j-2,ii-2,jj+2)+
     *   C(i+2,j-2,ii+2,jj-2)+ C(i-2,j+2,ii-2,jj+2)+
     *   C(i+2,j+2,ii+2,jj-2)+ C(i-2,j+2,ii+2,jj+2)+
     *   C(i+2,j-2,ii+2,jj+2)+ C(i+2,j+2,ii-2,jj+2)
              if (B(i,j,ii,jj).ne.isum) then
                nloopi=min(nloopi,i)
                nloopj=min(nloopj,j)
                nloopii=min(nloopii,ii)
                nloopjj=min(nloopjj,jj)         
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii,nloopjj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)

      end
C -----------------------------------------SC4203      
      subroutine SC4203
      integer, parameter :: N = 16,M=10,K=10,L=10,NL=1000
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:),C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,*,BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2,2:2,2:2) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A

      tname='SC4203'     
      allocate (B(N,M,K,L),A(N,M,K,L),C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual(nloopi,nloopj,nloopii,nloopjj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
!dvm$*,shadow_compute(A(2:0,2:2,2:0,2:0))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo
      enddo                                                
 
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj)
      do i=3,N-2
        do j=3,M-2
          do ii=3,K-2
            do jj=3,L-2
              B(i,j,ii,jj) = A(i-2,j-2,ii-2,jj-2)+
     *   A(i,j-2,ii,jj)+ A(i-2,j-2,ii,jj)+
     *   A(i,j-2,ii,jj)+ A(i,j-2,ii-2,jj)+
     *   A(i-2,j-2,ii,jj)+ A(i,j-2,ii-2,jj-2)+
     *   A(i,j-2,ii,jj-2)+ A(i-2,j-2,ii-2,jj)+
     *   A(i,j-2,ii-2,jj-2)
            enddo
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
!dvm$*,private(isum)
      do i=3,N-2
        do j=3,M-2
          do ii=3,K-2
            do jj=3,L-2
              isum = C(i-2,j-2,ii-2,jj-2)+
     *   C(i,j-2,ii,jj)+ C(i-2,j-2,ii,jj)+
     *   C(i,j-2,ii,jj)+ C(i,j-2,ii-2,jj)+
     *   C(i-2,j-2,ii,jj)+ C(i,j-2,ii-2,jj-2)+
     *   C(i,j-2,ii,jj-2)+ C(i-2,j-2,ii-2,jj)+
     *   C(i,j-2,ii-2,jj-2)
             if (B(i,j,ii,jj).ne.isum) then
               nloopi=min(nloopi,i)
               nloopj=min(nloopj,j)
               nloopii=min(nloopii,ii)
               nloopjj=min(nloopjj,jj)         
             endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii,nloopjj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)

      end      
C ------------------------------------------SC4204   
      subroutine SC4204
      integer, parameter :: N = 16,M=10,K=10,L=10,NL=1000
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:),C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj,isum
      character*6 tname 

!dvm$ distribute B(*,BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(0:2,2:2,0:2,0:2) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A

      tname='SC4204'     
      allocate (B(N,M,K,L),A(N,M,K,L),C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual(nloopi,nloopj,nloopii,nloopjj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),shadow_compute
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo
      enddo                                                
 
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj)
      do i=3,N-2
        do j=3,M-2
          do ii=3,K-2
            do jj=3,L-2
              B(i,j,ii,jj) = A(i+2,j+2,ii+2,jj)+
     *   A(i,j-2,ii,jj)+ A(i+2,j-2,ii,jj)+
     *   A(i,j+2,ii,jj)+ A(i,j-2,ii+2,jj)+
     *   A(i+2,j+2,ii,jj)+ A(i,j-2,ii+2,jj+2)+
     *   A(i,j+2,ii,jj+2)+ A(i+2,j+2,ii+2,jj)+
     *   A(i,j+2,ii+2,jj+2)+A(i+2,j-2,ii+2,jj+2)
            enddo
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
!dvm$*,private(isum)
      do i=3,N-2
        do j=3,M-2
          do ii=3,K-2
            do jj=3,L-2
              isum = C(i+2,j+2,ii+2,jj)+
     *   C(i,j-2,ii,jj)+ C(i+2,j-2,ii,jj)+
     *   C(i,j+2,ii,jj)+ C(i,j-2,ii+2,jj)+
     *   C(i+2,j+2,ii,jj)+ C(i,j-2,ii+2,jj+2)+
     *   C(i,j+2,ii,jj+2)+ C(i+2,j+2,ii+2,jj)+
     *   C(i,j+2,ii+2,jj+2)+C(i+2,j-2,ii+2,jj+2)
              if (B(i,j,ii,jj).ne.isum) then
                nloopi=min(nloopi,i)
                nloopj=min(nloopj,j)
                nloopii=min(nloopii,ii)
                nloopjj=min(nloopjj,jj)         
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii,nloopjj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)

      end  
C ------------------------------------------SC4205
      subroutine SC4205
      integer, parameter :: N = 16,M=16,K=16,L=16,NL=1000
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:),C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK,*)   
!dvm$ shadow(2:2,2:0,0:2,2:2) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A

      tname='SC4205'     
      allocate (B(N,M,K,L),A(N,M,K,L),C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual(nloopi,nloopj,nloopii,nloopjj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
!dvm$*,shadow_compute(A(0:0,0:0,0:0,0:2)) 
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo
      enddo                                                
 
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj)
      do i=3,N-2
        do j=3,M-2
          do ii=3,K-2
            do jj=3,L-2
              B(i,j,ii,jj) = A(i,j,ii,jj+2)
            enddo
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
!dvm$*,private(isum)
      do i=3,N-2
        do j=3,M-2
          do ii=3,K-2
            do jj=3,L-2
              isum = C(i,j,ii,jj+2)
              if (B(i,j,ii,jj).ne.isum) then
                nloopi=min(nloopi,i)
                nloopj=min(nloopj,j)
                nloopii=min(nloopii,ii)
                nloopjj=min(nloopjj,jj)         
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii,nloopjj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)

      end  
C --------------------------------------------SC4206  
      subroutine SC4206
      integer, parameter :: N = 32,M=16,K=16,L=16,NL=1000
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:),C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,*,BLOCK)   
!dvm$ shadow(3:3,3:3,3:3,3:3) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A

      tname='SC4206'     
      allocate (B(N,M,K,L),A(N,M,K,L),C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual(nloopi,nloopj,nloopii,nloopjj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),shadow_compute 
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo
      enddo                                                
 
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj)
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            do jj=4,L-3
            B(i,j,ii,jj) = A(i+3,j+3,ii+3,jj+3)+A(i-3,j-3,ii-3,jj-3)+
     *   A(i+3,j-3,ii-3,jj-3)+ A(i-3,j+3,ii-3,jj-3)+
     *   A(i-3,j-3,ii+3,jj-3)+ A(i-3,j-3,ii-3,jj+3)+
     *   A(i+3,j+3,ii-3,jj-3)+ A(i-3,j+3,ii+3,jj-3)+
     *   A(i-3,j-3,ii+3,jj+3)+ A(i+3,j-3,ii-3,jj+3)+
     *   A(i+3,j-3,ii+3,jj-3)+ A(i-3,j+3,ii-3,jj+3)+
     *   A(i+3,j+3,ii+3,jj-3)+ A(i-3,j+3,ii+3,jj+3)+
     *   A(i+3,j-3,ii+3,jj+3)+ A(i+3,j+3,ii-3,jj+3)
            enddo
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
!dvm$*,private(isum)
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            do jj=4,L-3
              isum = C(i+3,j+3,ii+3,jj+3)+C(i-3,j-3,ii-3,jj-3)+
     *   C(i+3,j-3,ii-3,jj-3)+ C(i-3,j+3,ii-3,jj-3)+
     *   C(i-3,j-3,ii+3,jj-3)+ C(i-3,j-3,ii-3,jj+3)+
     *   C(i+3,j+3,ii-3,jj-3)+ C(i-3,j+3,ii+3,jj-3)+
     *   C(i-3,j-3,ii+3,jj+3)+ C(i+3,j-3,ii-3,jj+3)+
     *   C(i+3,j-3,ii+3,jj-3)+ C(i-3,j+3,ii-3,jj+3)+
     *   C(i+3,j+3,ii+3,jj-3)+ C(i-3,j+3,ii+3,jj+3)+
     *   C(i+3,j-3,ii+3,jj+3)+ C(i+3,j+3,ii-3,jj+3)
              if (B(i,j,ii,jj).ne.isum) then
                nloopi=min(nloopi,i)
                nloopj=min(nloopj,j)
                nloopii=min(nloopii,ii)
                nloopjj=min(nloopjj,jj)         
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii,nloopjj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)

      end
C -------------------------------------------SC4207   
      subroutine SC4207
      integer, parameter :: N = 16,M=16,K=16,L=16,NL=1000
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:),C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,*,BLOCK,BLOCK)   
!dvm$ shadow(0:3,3:3,0:3,0:3) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A

      tname='SC4207'     
      allocate (B(N,M,K,L),A(N,M,K,L),C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual(nloopi,nloopj,nloopii,nloopjj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),shadow_compute 
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo
      enddo                                                
 
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj)
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            do jj=4,L-3
              B(i,j,ii,jj) = A(i+3,j+3,ii+3,jj+3)+A(i,j-3,ii,jj)+
     *   A(i+3,j-3,ii,jj)+ A(i,j+3,ii,jj)+
     *   A(i,j-3,ii+3,jj)+ A(i+3,j+3,ii,jj)+
     *   A(i,j-3,ii+3,jj+3)+ A(i,j+3,ii,jj+3)+
     *   A(i+3,j+3,ii+3,jj)+ A(i,j+3,ii+3,jj+3)+
     *   A(i+3,j-3,ii+3,jj+3)
            enddo
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
!dvm$*,private(isum)
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            do jj=4,L-3
              isum = C(i+3,j+3,ii+3,jj+3)+C(i,j-3,ii,jj)+
     *   C(i+3,j-3,ii,jj)+ C(i,j+3,ii,jj)+
     *   C(i,j-3,ii+3,jj)+ C(i+3,j+3,ii,jj)+
     *   C(i,j-3,ii+3,jj+3)+ C(i,j+3,ii,jj+3)+
     *   C(i+3,j+3,ii+3,jj)+ C(i,j+3,ii+3,jj+3)+
     *   C(i+3,j-3,ii+3,jj+3)
              if (B(i,j,ii,jj).ne.isum) then
                nloopi=min(nloopi,i)
                nloopj=min(nloopj,j)
                nloopii=min(nloopii,ii)
                nloopjj=min(nloopjj,jj)         
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii,nloopjj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)

      end   
C -------------------------------------------SC4208   
      subroutine SC4208
      integer, parameter :: N = 16,M=16,K=16,L=16,NL=1000
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:),C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj,isum
      character*6 tname 

!dvm$ distribute B(*,BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(0:3,3:3,0:3,3:0) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A

      tname='SC4208'     
      allocate (B(N,M,K,L),A(N,M,K,L),C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual(nloopi,nloopj,nloopii,nloopjj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
!dvm$*,shadow_compute(A(0:0,0:0,0:0,3:0)) 
       do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo
      enddo                                                
 
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj)
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            do jj=4,L-3
              B(i,j,ii,jj) = A(i,j,ii,jj-3)
            enddo
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
!dvm$*,private(isum)
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            do jj=4,L-3
              isum =C(i,j,ii,jj-3) 
              if (B(i,j,ii,jj).ne.isum) then
                nloopi=min(nloopi,i)
                nloopj=min(nloopj,j)
                nloopii=min(nloopii,ii)
                nloopjj=min(nloopjj,jj)         
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii,nloopjj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)

      end   
C -------------------------------------------SC4209   
      subroutine SC4209
      integer, parameter :: N = 60,M=60,K=60,L=60,NL=1000
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:),C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK,*)   
!dvm$ shadow(11:11,11:11,11:11,11:11) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A

      tname='SC4209'     
      allocate (B(N,M,K,L),A(N,M,K,L),C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual(nloopi,nloopj,nloopii,nloopjj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),shadow_compute 
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo
      enddo                                                
 
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj)
      do i=12,N-11
        do j=12,M-11
          do ii=12,K-11
            do jj=12,L-11
              B(i,j,ii,jj) = A(i+11,j+11,ii+11,jj+11)+
     *   A(i-11,j-11,ii-11,jj-11)+
     *   A(i+11,j-11,ii-11,jj-11)+ A(i-11,j+11,ii-11,jj-11)+
     *   A(i-11,j-11,ii+11,jj-11)+ A(i-11,j-11,ii-11,jj+11)+
     *   A(i+11,j+11,ii-11,jj-11)+ A(i-11,j+11,ii+11,jj-11)+
     *   A(i-11,j-11,ii+11,jj+11)+ A(i+11,j-11,ii-11,jj+11)+
     *   A(i+11,j-11,ii+11,jj-11)+ A(i-11,j+11,ii-11,jj+11)+
     *   A(i+11,j+11,ii+11,jj-11)+ A(i-11,j+11,ii+11,jj+11)+
     *   A(i+11,j-11,ii+11,jj+11)+ A(i+11,j+11,ii-11,jj+11)
            enddo
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
!dvm$*,private(isum)
      do i=12,N-11
        do j=12,M-11
          do ii=12,K-11
            do jj=12,L-11
              isum = C(i+11,j+11,ii+11,jj+11)+
     *   C(i-11,j-11,ii-11,jj-11)+
     *   C(i+11,j-11,ii-11,jj-11)+ C(i-11,j+11,ii-11,jj-11)+
     *   C(i-11,j-11,ii+11,jj-11)+ C(i-11,j-11,ii-11,jj+11)+
     *   C(i+11,j+11,ii-11,jj-11)+ C(i-11,j+11,ii+11,jj-11)+
     *   C(i-11,j-11,ii+11,jj+11)+ C(i+11,j-11,ii-11,jj+11)+
     *   C(i+11,j-11,ii+11,jj-11)+ C(i-11,j+11,ii-11,jj+11)+
     *   C(i+11,j+11,ii+11,jj-11)+ C(i-11,j+11,ii+11,jj+11)+
     *   C(i+11,j-11,ii+11,jj+11)+ C(i+11,j+11,ii-11,jj+11)
              if (B(i,j,ii,jj).ne.isum) then
                nloopi=min(nloopi,i)
                nloopj=min(nloopj,j)
                nloopii=min(nloopii,ii)
                nloopjj=min(nloopjj,jj)         
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii,nloopjj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)

      end
C -----------------------------------------------         
      subroutine serial4(AR,N,M,K,L,NL)
      integer AR(N,M,K,L)
      integer NL 
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              AR(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo
      enddo                
      end 

      subroutine ansyes(name)
      character*6 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*6 name
      print *,name,'  -  ***error'
      end
