      program ACR42

c    TESTING OF THE ACROSS CLAUSE'.       
c    DISTRIBUTED ARRAY A(N,M,K,L) IS TO HAVE DIFFERENT 
c    FLOW-DEP-LENGTH ON BOTH SIDES      

      print *,'===START OF ACR42========================'
C --------------------------------------------------
      call acr4201
C --------------------------------------------------
      call acr4202
C --------------------------------------------------
      call acr4203
C -------------------------------------------------
      call acr4204
C -------------------------------------------------
      call acr4205
C -------------------------------------------------
      call acr4206
C --------------------------------------------------
      call acr4207
C --------------------------------------------------
      call acr4208
C----------------------------------------------------
      call acr4209
C----------------------------------------------------

C
C
      print *,'=== END OF ACR42 ========================= '    
      end
C ---------------------------------------------ACR4201 
      subroutine ACR4201    
      integer, parameter :: N = 16,M=8,K=8,L=8, NL=1000
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,*,*,*)      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4201'     
      allocate (B(N,M,K,L),A(N,M,K,L),C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      do i=2,N-1
       do j=2,M-1
         do ii=2,K-1
          do jj=2,L-1
           C(i,j,ii,jj)=
     *   C(i+1,j,ii,jj)+ C(i,j+1,ii,jj)+
     *   C(i,j,ii+1,jj)+ C(i,j,ii,jj+1)+
     *   C(i-1,j,ii,jj)+ C(i,j-1,ii,jj)+
     *   C(i,j,ii-1,jj)+ C(i,j,ii,jj-1)
         enddo 
        enddo
       enddo 
      enddo
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual (nloopi,nloopj,nloopii,nloopjj,C)
!dvm$ region in (C),out (A) 

!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj)
      do jj=1,L
        do ii=1,K
         do j=1,M
          do i=1,N
           A(i,j,ii,jj) = NL+i+j+ii+jj
          enddo
         enddo
        enddo
       enddo
 
!dvm$ parallel (jj,ii,j,i) on B(i,j,ii,jj),
!dvm$*across(A(1:1,1:1,1:1,1:1))
      do jj=2,L-1
       do ii=2,K-1
        do j=2,M-1
         do i=2,N-1
          A(i,j,ii,jj)=
     *   A(i+1,j,ii,jj)+A(i,j+1,ii,jj)+
     *   A(i,j,ii+1,jj)+A(i,j,ii,jj+1)+
     *   A(i-1,j,ii,jj)+A(i,j-1,ii,jj)+
     *   A(i,j,ii-1,jj)+A(i,j,ii,jj-1)
         enddo 
        enddo
       enddo 
      enddo
  
!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
      do jj=2,L-1
       do ii=2,K-1
        do j=2,M-1
         do i=2,N-1         
          if (A(i,j,ii,jj).ne.C(i,j,ii,jj)) then
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
!dvm$ get_actual (nloopi)

      if (nloopi .eq.NL) then
       call ansyes(tname)
      else
       call ansno(tname)
      endif
      deallocate (A, B, C)
      end
C ---------------------------------------------ACR4202     
      subroutine ACR4202    
      integer, parameter :: N = 16,M=8,K=8,L=8, NL=1000
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,*,*,*)   
!dvm$ shadow(2:2,2:2,2:2,2:2) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4202'     
      allocate (B(N,M,K,L), A(N,M,K,L), C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      do i=3,N-2
       do j=3,M-2
        do ii=3,K-2
         do jj=3,L-2
          C(i,j,ii,jj) =
     *   C(i+2,j,ii,jj)+ C(i,j+2,ii,jj)+
     *   C(i,j,ii+1,jj)+ C(i,j,ii,jj+2)+
     *   C(i-1,j,ii,jj)+ C(i,j-2,ii,jj)+
     *   C(i,j,ii-2,jj)+ C(i,j,ii,jj-1)+
     *   C(i+1,j,ii,jj)+ C(i,j+1,ii,jj)+
     *   C(i,j,ii,jj+1)+ C(i,j-1,ii,jj)+
     *   C(i,j,ii,jj-1)
         enddo
        enddo
       enddo 
      enddo 
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual (nloopi,nloopj,nloopii,nloopjj,C)
!dvm$ region in (C),out (A) 

!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
       do j=1,M
        do ii=1,K
         do jj=1,L
          A(i,j,ii,jj) = NL+i+j+ii+jj
         enddo
        enddo
       enddo
      enddo 
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),across(A(1:2,2:2,2:1,1:2))
      do i=3,N-2
       do j=3,M-2
        do ii=3,K-2
         do jj=3,L-2
          A(i,j,ii,jj) =
     *   A(i+2,j,ii,jj)+ A(i,j+2,ii,jj)+
     *   A(i,j,ii+1,jj)+ A(i,j,ii,jj+2)+
     *   A(i-1,j,ii,jj)+ A(i,j-2,ii,jj)+
     *   A(i,j,ii-2,jj)+ A(i,j,ii,jj-1)+
     *   A(i+1,j,ii,jj)+ A(i,j+1,ii,jj)+
     *   A(i,j,ii,jj+1)+ A(i,j-1,ii,jj)+
     *   A(i,j,ii,jj-1)
         enddo
        enddo 
       enddo 
      enddo
  
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
      do i=3,N-2
       do j=3,M-2
        do ii=3,K-2
         do jj=3,L-2
          if (A(i,j,ii,jj).ne.C(i,j,ii,jj)) then
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
!dvm$ get_actual (nloopi)

      if (nloopi .eq.NL) then
       call ansyes(tname)
      else
       call ansno(tname)
      endif
      deallocate (A, B, C)
      end
C -----------------------------------------ACR4203      
      subroutine ACR4203     
      integer, parameter :: N = 16,M=8,K=8,L=8, NL=1000
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,*,*,*)   
!dvm$ shadow(2:2,2:2,2:2,2:2) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4203'     
      allocate (B(N,M,K,L), A(N,M,K,L), C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      do i=3,N-2
       do j=3,M-2
        do ii=3,K-2
         do jj=3,L-2
          C(i,j,ii,jj) =  C(i-2,j,ii,jj)+
     *   C(i,j-2,ii,jj)+ C(i,j,ii-2,jj)+
     *   C(i,j,ii,jj-2)+ C(i-1,j,ii,jj)+
     *   C(i,j-1,ii,jj)+ C(i,j,ii-1,jj)+
     *   C(i,j,ii,jj-1)
         enddo
        enddo 
       enddo 
      enddo  
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual (nloopi,nloopj,nloopii,nloopjj,C)
!dvm$ region in (C),out( A) 

!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj)
      do jj=1,L
       do ii=1,K
        do j=1,M
         do i=1,N
          A(i,j,ii,jj) = NL+i+j+ii+jj
         enddo
        enddo
       enddo
      enddo
 
!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj),
!dvm$*across(A(2:0,2:2,2:0,2:0))
      do jj=3,L-2
       do ii=3,K-2
        do j=3,M-2
         do i=3,N-2
          A(i,j,ii,jj) =  A(i-2,j,ii,jj)+
     *   A(i,j-2,ii,jj)+ A(i,j,ii-2,jj)+
     *   A(i,j,ii,jj-2)+ A(i-1,j,ii,jj)+
     *   A(i,j-1,ii,jj)+ A(i,j,ii-1,jj)+
     *   A(i,j,ii,jj-1)
         enddo
        enddo 
       enddo 
      enddo
  
!dvm$ parallel (jj,ii,j,i) on B(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
      do jj=3,L-2
       do ii=3,K-2
        do j=3,M-2
         do i=3,N-2
          if (A(i,j,ii,jj).ne.C(i,j,ii,jj)) then         
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
!dvm$ get_actual (nloopi)

      if (nloopi .eq.NL) then
       call ansyes(tname)
      else
       call ansno(tname)
      endif
      deallocate (A, B, C)
      end 
C ------------------------------------------ACR4204   
      subroutine ACR4204     
      integer, parameter :: N = 16,M=8,K=8,L=8, NL=1000            
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,*,*,*)   
!dvm$ shadow(2:2,2:2,2:2,2:2) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4204'     
      allocate (B(N,M,K,L), A(N,M,K,L), C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      do i=3,N-2
       do j=3,M-2
        do ii=3,K-2
         do jj=3,L-2
          C(i,j,ii,jj) =  C(i+2,j,ii,jj)+
     *   C(i,j,ii,jj+2)+ C(i-2,j,ii,jj)+
     *   C(i,j-2,ii,jj)+ C(i,j,ii-2,jj)+
     *   C(i+1,j,ii,jj)+ C(i,j,ii,jj+1)+
     *   C(i-1,j,ii,jj)+ C(i,j-1,ii,jj)+
     *   C(i,j,ii-1,jj)
         enddo
        enddo
       enddo 
      enddo
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual (nloopi,nloopj,nloopii,nloopjj,C)
!dvm$ region in (C),out (A) 

!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj)
      do jj=1,L
       do ii=1,K
        do j=1,M
         do i=1,N
          A(i,j,ii,jj) = NL+i+j+ii+jj
         enddo
        enddo
       enddo
      enddo
 
!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj),
!dvm$*across(A(2:2,2:0,2:0,0:2))
      do jj=3,L-2
       do ii=3,K-2
        do j=3,M-2
         do i=3,N-2
          A(i,j,ii,jj) =  A(i+2,j,ii,jj)+
     *   A(i,j,ii,jj+2)+ A(i-2,j,ii,jj)+
     *   A(i,j-2,ii,jj)+ A(i,j,ii-2,jj)+
     *   A(i+1,j,ii,jj)+ A(i,j,ii,jj+1)+
     *   A(i-1,j,ii,jj)+ A(i,j-1,ii,jj)+
     *   A(i,j,ii-1,jj)
         enddo
        enddo 
       enddo 
      enddo
  
!dvm$ parallel (jj,ii,j,i) on B(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
      do jj=3,L-2
       do ii=3,K-2
        do j=3,M-2
         do i=3,N-2
          if (A(i,j,ii,jj).ne.C(i,j,ii,jj)) then
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
!dvm$ get_actual (nloopi)

      if (nloopi .eq.NL) then
       call ansyes(tname)
      else
       call ansno(tname)
      endif
      deallocate (A, B, C)
      end  
C ------------------------------------------ACR4205
      subroutine ACR4205
      integer, parameter :: N = 16,M=16,K=16,L=16, NL=1000
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,*,*,*)   
!dvm$ shadow(2:2,2:0,0:2,2:2) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4205'     
      allocate (B(N,M,K,L), A(N,M,K,L), C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      do i=3,N-2
       do j=3,M-2
        do ii=3,K-2
         do jj=3,L-2
          C(i,j,ii,jj)=
     *   C(i+2,j,ii,jj)+ C(i,j,ii+2,jj)+
     *   C(i,j,ii,jj+2)+ C(i-2,j,ii,jj)+
     *   C(i,j-2,ii,jj)+ C(i,j,ii,jj-2)+
     *   C(i+1,j,ii,jj)+ C(i,j,ii+1,jj)+
     *   C(i,j,ii,jj+1)+ C(i-1,j,ii,jj)+
     *   C(i,j-1,ii,jj)+ C(i,j,ii,jj-1)         
         enddo
        enddo
       enddo
      enddo
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual (nloopi,nloopj,nloopii,nloopjj,C)
!dvm$ region in (C) 

!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj)
      do jj=1,L
       do ii=1,K
        do j=1,M
         do i=1,N
          A(i,j,ii,jj) = NL+i+j+ii+jj
         enddo
        enddo
       enddo
      enddo
 
!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj),
!dvm$*across(A(2:2,2:0,0:2,2:2))
      do jj=3,L-2
       do ii=3,K-2
        do j=3,M-2
         do i=3,N-2
          A(i,j,ii,jj)=
     *   A(i+2,j,ii,jj)+ A(i,j,ii+2,jj)+
     *   A(i,j,ii,jj+2)+ A(i-2,j,ii,jj)+
     *   A(i,j-2,ii,jj)+ A(i,j,ii,jj-2)+
     *   A(i+1,j,ii,jj)+ A(i,j,ii+1,jj)+
     *   A(i,j,ii,jj+1)+ A(i-1,j,ii,jj)+
     *   A(i,j-1,ii,jj)+ A(i,j,ii,jj-1)
         enddo
        enddo 
       enddo 
      enddo
  
!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
      do jj=3,L-2
       do ii=3,K-2
        do j=3,M-2
         do i=3,N-2
          if (A(i,j,ii,jj).ne.C(i,j,ii,jj)) then
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
!dvm$ get_actual (nloopi)

      if (nloopi .eq.NL) then
       call ansyes(tname)
      else
       call ansno(tname)
      endif
      deallocate (A, B, C)
      end  
C --------------------------------------------ACR4206  
      subroutine ACR4206
      integer, parameter :: N = 32,M=16,K=16,L=16, NL=1000
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,*,*,*)   
!dvm$ shadow(3:3,3:3,3:3,3:3) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4206'     
      allocate (B(N,M,K,L), A(N,M,K,L), C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      do i=4,N-3
       do j=4,M-3
        do ii=4,K-3
         do jj=4,L-3
          C(i,j,ii,jj) =
     *   C(i+3,j,ii,jj)+ C(i,j+3,ii,jj)+
     *   C(i,j,ii+3,jj)+ C(i,j,ii,jj+3)+
     *   C(i-3,j,ii,jj)+ C(i,j-3,ii,jj)+
     *   C(i,j,ii-3,jj)+ C(i,j,ii,jj-3)+
     *   C(i+2,j,ii,jj)+ C(i,j+2,ii,jj)+
     *   C(i,j,ii+2,jj)+ C(i,j,ii,jj+2)+
     *   C(i-2,j,ii,jj)+ C(i,j-2,ii,jj)+
     *   C(i,j,ii-2,jj)+ C(i,j,ii,jj-2)+
     *   C(i+1,j,ii,jj)+ C(i,j+1,ii,jj)+
     *   C(i,j,ii+1,jj)+ C(i,j,ii,jj+1)+
     *   C(i-1,j,ii,jj)+ C(i,j-1,ii,jj)+
     *   C(i,j,ii-1,jj)+ C(i,j,ii,jj-1)
         enddo
        enddo 
       enddo
      enddo
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual (nloopi,nloopj,nloopii,nloopjj,C)
!dvm$ region in (C),out (A) 

!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj)
      do jj=1,L
       do ii=1,K
        do j=1,M
         do i=1,N
          A(i,j,ii,jj) = NL+i+j+ii+jj
         enddo
        enddo
       enddo
      enddo
 
!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj),
!dvm$*across(A(3:3,3:3,3:3,3:3))
      do jj=4,L-3
       do ii=4,K-3
        do j=4,M-3
         do i=4,N-3
          A(i,j,ii,jj) =
     *   A(i+3,j,ii,jj)+ A(i,j+3,ii,jj)+
     *   A(i,j,ii+3,jj)+ A(i,j,ii,jj+3)+
     *   A(i-3,j,ii,jj)+ A(i,j-3,ii,jj)+
     *   A(i,j,ii-3,jj)+ A(i,j,ii,jj-3)+
     *   A(i+2,j,ii,jj)+ A(i,j+2,ii,jj)+
     *   A(i,j,ii+2,jj)+ A(i,j,ii,jj+2)+
     *   A(i-2,j,ii,jj)+ A(i,j-2,ii,jj)+
     *   A(i,j,ii-2,jj)+ A(i,j,ii,jj-2)+
     *   A(i+1,j,ii,jj)+ A(i,j+1,ii,jj)+
     *   A(i,j,ii+1,jj)+ A(i,j,ii,jj+1)+
     *   A(i-1,j,ii,jj)+ A(i,j-1,ii,jj)+
     *   A(i,j,ii-1,jj)+ A(i,j,ii,jj-1)
         enddo
        enddo 
       enddo 
      enddo
  
!dvm$ parallel (jj,ii,j,i) on B(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
      do jj=4,L-3
       do ii=4,K-3
        do j=4,M-3
         do i=4,N-3
          if (A(i,j,ii,jj).ne.C(i,j,ii,jj)) then
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
!dvm$ get_actual (nloopi)

      if (nloopi .eq.NL) then
       call ansyes(tname)
      else
       call ansno(tname)
      endif
      deallocate (A, B, C)
      end 
C -------------------------------------------ACR4207   
      subroutine ACR4207
      integer, parameter :: N = 16,M=16,K=16,L=16, NL=1000
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,*,*,*)   
!dvm$ shadow(0:3,3:3,0:3,0:3) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4207'     
      allocate (B(N,M,K,L), A(N,M,K,L), C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      do i=4,N-3
       do j=4,M-3
        do ii=4,K-3
         do jj=4,L-3
          C(i,j,ii,jj) =
     *   C(i+3,j,ii,jj)+ C(i,j+3,ii,jj)+
     *   C(i,j,ii+3,jj)+ C(i,j,ii,jj+3)+ 
     *   C(i,j-3,ii,jj)+ C(i+2,j,ii,jj)+
     *   C(i,j+2,ii,jj)+ C(i,j,ii+2,jj)+
     *   C(i,j,ii,jj+2)+ C(i,j-2,ii,jj)+
     *   C(i+1,j,ii,jj)+ C(i,j+1,ii,jj)+
     *   C(i,j,ii+1,jj)+ C(i,j,ii,jj+1)+ 
     *   C(i,j-1,ii,jj)
         enddo
        enddo 
       enddo
      enddo
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual (nloopi,nloopj,nloopii,nloopjj,C)
!dvm$ region in (C) 

!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj)
      do jj=1,L
       do ii=1,K
        do j=1,M
         do i=1,N
          A(i,j,ii,jj) = NL+i+j+ii+jj
         enddo
        enddo
       enddo
      enddo
 
!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj),
!dvm$*across(A(0:3,3:3,0:3,0:3))
      do jj=4,L-3
       do ii=4,K-3
        do j=4,M-3
         do i=4,N-3
          A(i,j,ii,jj) =
     *   A(i+3,j,ii,jj)+ A(i,j+3,ii,jj)+
     *   A(i,j,ii+3,jj)+ A(i,j,ii,jj+3)+ 
     *   A(i,j-3,ii,jj)+ A(i+2,j,ii,jj)+
     *   A(i,j+2,ii,jj)+ A(i,j,ii+2,jj)+
     *   A(i,j,ii,jj+2)+ A(i,j-2,ii,jj)+
     *   A(i+1,j,ii,jj)+ A(i,j+1,ii,jj)+
     *   A(i,j,ii+1,jj)+ A(i,j,ii,jj+1)+ 
     *   A(i,j-1,ii,jj)
         enddo
        enddo 
       enddo 
      enddo
  
!dvm$ parallel (jj,ii,j,i) on B(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
      do jj=4,L-3
       do ii=4,K-3
        do j=4,M-3
         do i=4,N-3
          if (A(i,j,ii,jj).ne.C(i,j,ii,jj)) then
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
!dvm$ get_actual (nloopi)

      if (nloopi .eq.NL) then
       call ansyes(tname)
      else
       call ansno(tname)
      endif
      deallocate (A, B, C)
      end   
C -------------------------------------------ACR4208   
      subroutine ACR4208
      integer, parameter :: N = 16,M=16,K=16,L=16, NL=1000
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,*,*,*)   
!dvm$ shadow(0:3,3:3,0:3,3:0) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4208'     
      allocate (B(N,M,K,L), A(N,M,K,L), C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      do i=4,N-3
       do j=4,M-3
        do ii=4,K-3
         do jj=4,L-3
          C(i,j,ii,jj) = 
     *   C(i+3,j,ii,jj)+ C(i,j+3,ii,jj)+
     *   C(i,j,ii+3,jj)+ C(i,j,ii,jj-3)+
     *   C(i+2,j,ii,jj)+ C(i,j+2,ii,jj)+
     *   C(i,j,ii+2,jj)+ C(i,j,ii,jj-2)+
     *   C(i+1,j,ii,jj)+ C(i,j+1,ii,jj)+
     *   C(i,j,ii+1,jj)+ C(i,j,ii,jj-1)
         enddo
        enddo
       enddo 
      enddo
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual (nloopi,nloopj,nloopii,nloopjj,C)
!dvm$ region in (C),out (A) 

!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj)
      do jj=1,L
       do ii=1,K
        do j=1,M
         do i=1,N
          A(i,j,ii,jj) = NL+i+j+ii+jj
         enddo
        enddo
       enddo
      enddo
 
!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj),
!dvm$*across(A(0:3,0:3,0:3,3:0))
      do jj=4,L-3
       do ii=4,K-3
        do j=4,M-3
         do i=4,N-3
          A(i,j,ii,jj) = 
     *   A(i+3,j,ii,jj)+ A(i,j+3,ii,jj)+
     *   A(i,j,ii+3,jj)+ A(i,j,ii,jj-3)+
     *   A(i+2,j,ii,jj)+ A(i,j+2,ii,jj)+
     *   A(i,j,ii+2,jj)+ A(i,j,ii,jj-2)+
     *   A(i+1,j,ii,jj)+ A(i,j+1,ii,jj)+
     *   A(i,j,ii+1,jj)+ A(i,j,ii,jj-1)
         enddo
        enddo 
       enddo 
      enddo
  
!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
      do jj=4,L-3
       do ii=4,K-3
        do j=4,M-3
         do i=4,N-3
          if (A(i,j,ii,jj).ne.C(i,j,ii,jj)) then
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
!dvm$ get_actual (nloopi)

      if (nloopi .eq.NL) then
       call ansyes(tname)
      else
       call ansno(tname)
      endif
      deallocate (A, B, C)
      end   
C -------------------------------------------ACR4209   
      subroutine ACR4209
      integer, parameter :: N = 48,M=48,K=48,L=48, NL=1000
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,*,*,*)   
!dvm$ shadow(11:11,11:11,11:11,11:11) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4209'     
      allocate (B(N,M,K,L), A(N,M,K,L), C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      do i=12,N-11
       do j=12,M-11
        do ii=12,K-11
         do jj=12,L-11
          C(i,j,ii,jj) =
     *   C(i+11,j,ii,jj)+ C(i,j+11,ii,jj)+
     *   C(i,j,ii+11,jj)+ C(i,j,ii,jj+11)+
     *   C(i-11,j,ii,jj)+ C(i,j-11,ii,jj)+
     *   C(i,j,ii-11,jj)+ C(i,j,ii,jj-11)
         enddo
        enddo
       enddo 
      enddo
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ actual (nloopi,nloopj,nloopii,nloopjj,C)
!dvm$ region  

!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj)
      do jj=1,L
       do ii=1,K
        do j=1,M
         do i=1,N
          A(i,j,ii,jj) = NL+i+j+ii+jj
         enddo
        enddo
       enddo
      enddo
 
!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj),
!dvm$*across(A(11:11,11:11,11:11,11:11))
      do jj=12,L-11
       do ii=12,K-11
        do j=12,M-11
         do i=12,N-11
          A(i,j,ii,jj) =
     *   A(i+11,j,ii,jj)+ A(i,j+11,ii,jj)+
     *   A(i,j,ii+11,jj)+ A(i,j,ii,jj+11)+
     *   A(i-11,j,ii,jj)+ A(i,j-11,ii,jj)+
     *   A(i,j,ii-11,jj)+ A(i,j,ii,jj-11)
         enddo
        enddo 
       enddo 
      enddo
  
!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
      do jj=12,L-11
       do ii=12,K-11
        do j=12,M-11
         do i=12,N-11
          if (A(i,j,ii,jj).ne.C(i,j,ii,jj)) then
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
!dvm$ get_actual (nloopi)

      if (nloopi .eq.NL) then
       call ansyes(tname)
      else
       call ansno(tname)
      endif
      deallocate (A, B, C)
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
      character*7 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*7 name
      print *,name,'  -  ***error'
      end