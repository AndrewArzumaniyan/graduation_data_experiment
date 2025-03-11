      program ACR43

c    TESTING OF THE ACROSS CLAUSE'.       
c    DISTRIBUTED ARRAY A(N,M,K,L) IS TO HAVE DIFFERENT 
c    FLOW-DEP-LENGTH ON BOTH SIDES      

      print *,'===START OF ACR43========================'
C --------------------------------------------------
      call acr4301
C --------------------------------------------------
      call acr4302
C --------------------------------------------------
      call acr4303
C -------------------------------------------------
      call acr4304
C -------------------------------------------------
      call acr4305
C -------------------------------------------------
      call acr4306
C --------------------------------------------------
      call acr4307
C --------------------------------------------------
      call acr4308
C----------------------------------------------------
       call acr4309
C----------------------------------------------------

C
C
      print *,'=== END OF ACR43 ========================= '    
      end
C ---------------------------------------------ACR4301 
      subroutine ACR4301    
      integer, parameter :: N = 16,M=8,K=8,L=8, NL=1000
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,BLOCK,BLOCK,BLOCK)      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4301'     
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
!dvm$ get_actual(nloopi)

      if (nloopi .eq.NL) then
       call ansyes(tname)
      else
       call ansno(tname)
      endif
      deallocate (A, B, C)
      end
C ---------------------------------------------ACR4302     
      subroutine ACR4302    
      integer, parameter :: N = 16,M=10,K=10,L=10, NL=1000
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,BLOCK,BLOCK,BLOCK)
!dvm$ shadow(2:2,2:2,2:2,2:2) :: A 
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4302'
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
!dvm$ region in( C),out( A) 

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
!dvm$ parallel (jj,ii,j,i) on A(i,j,ii,jj),across(A(1:2,2:2,2:1,1:2))
      do jj=3,L-2
       do ii=3,K-2
        do j=3,M-2
         do i=3,N-2
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
C -----------------------------------------ACR4303      
      subroutine ACR4303     
      integer, parameter :: N = 16,M=10,K=10,L=10, NL=1000
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,BLOCK,BLOCK,BLOCK)
!dvm$ shadow(2:2,2:2,2:2,2:2) :: A
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4303'
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
C ------------------------------------------ACR4304   
      subroutine ACR4304     
      integer, parameter :: N = 16,M=10,K=10,L=10, NL=1000            
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2,2:2,2:2) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4304'     
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
C ------------------------------------------ACR4305
      subroutine ACR4305
      integer, parameter :: N = 16,M=16,K=16,L=16, NL=1000
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:0,0:2,2:2) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4305'     
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
C --------------------------------------------ACR4306  
      subroutine ACR4306
      integer, parameter :: N = 32,M=16,K=16,L=16, NL=1000
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(3:3,3:3,3:3,3:3) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4306'     
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
C -------------------------------------------ACR4307   
      subroutine ACR4307
      integer, parameter :: N = 16,M=16,K=16,L=16, NL=1000
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(0:3,3:3,0:3,0:3) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4307'     
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
C -------------------------------------------ACR4308   
      subroutine ACR4308
      integer, parameter :: N = 16,M=16,K=16,L=16, NL=1000
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(0:3,3:3,0:3,3:0) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4308'     
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
C -------------------------------------------ACR4309   
      subroutine ACR4309
      integer, parameter :: N = 58,M=58,K=58,L=58, NL=1000
      integer, allocatable :: A(:,:,:,:), B(:,:,:,:), C(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj
      character*7 tname 
!dvm$ distribute B(*,BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(11:11,11:11,11:11,11:11) :: A      
!dvm$ align (i,j,ii,jj) with B(i,j,ii,jj) ::A
      tname='ACR4309'     
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
      