      program SC32
     
c    TESTING OF THE SHADOW DIRECTIVE AND THE SHADOW_COMPUTE CLAUSE       
c    DISTRIBUTED ARRAY A(N,M,K) IS TO HAVE DIFFERENT SHADOW WIDTH
c    ON BOTH SIDES 

      print *,'===START OF SC32========================'
C --------------------------------------------------
      call sc3201
C --------------------------------------------------
      call sc3202
C --------------------------------------------------
      call sc3203
C -------------------------------------------------
      call sc3204
C -------------------------------------------------
      call sc3205
C -------------------------------------------------
      call sc3206
C --------------------------------------------------
      call sc3207
C --------------------------------------------------
      call sc3208
C----------------------------------------------------
      call sc3209
C----------------------------------------------------

C
C
      print *,'=== END OF SC32 ========================= '    
      end
C ---------------------------------------------SC3201
      subroutine sc3201     
      integer, parameter :: N = 16,M=8,K=8,NL=1000     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3201'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii) on A(i,j,ii),shadow_compute
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
 
!dvm$ parallel (i,j,ii) on B(i,j,ii)
      do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i+1,j,ii)+A(i,j+1,ii)+A(i,j,ii+1)+A(i-1,j,ii)+
     *A(i,j-1,ii)+ A(i,j,ii-1)+A(i-1,j-1,ii-1)+A(i+1,j+1,ii+1)+
     *A(i-1,j+1,ii)+A(i+1,j-1,ii)+A(i-1,j+1,ii-1)+A(i-1,j+1,ii+1)+
     *A(i+1,j-1,ii-1)+A(i+1,j-1,ii+1)
         enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
!dvm$*,private(isum)
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          isum = C(i+1,j,ii)+C(i,j+1,ii)+C(i,j,ii+1)+C(i-1,j,ii)+
     *C(i,j-1,ii)+ C(i,j,ii-1)+C(i-1,j-1,ii-1)+C(i+1,j+1,ii+1)+
     *C(i-1,j+1,ii)+C(i+1,j-1,ii)+C(i-1,j+1,ii-1)+C(i-1,j+1,ii+1)+
     *C(i+1,j-1,ii-1)+C(i+1,j-1,ii+1)       
          if (B(i,j,ii).ne.isum) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii) 

      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end
C ---------------------------------------------SC3202     
      subroutine SC3202
      integer, parameter :: N = 16,M=10,K=10,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,*,BLOCK)   
!dvm$ shadow(2:2,2:2,2:2) :: A     
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3202'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii) on A(i,j,ii),shadow_compute(A(1:2,2:2,1:2))
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
 
!dvm$ parallel (i,j,ii) on B(i,j,ii)
      do i=3,N-2
        do j=3,M-2
          do ii=3,K-2
          B(i,j,ii) = A(i-1,j-2,ii+2)+A(i-1,j+2,ii-1)+A(i-1,j+2,ii+2)+
     *A(i+2,j+2,ii+2)+ A(i+2,j+2,ii-1)+A(i+2,j-2,ii+2)+A(i+2,j-2,ii-1)+
     *A(i-1,j-2,ii-1)
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
!dvm$*,private(isum)
      do i=3,N-2
        do j=3,M-2
          do ii=3,K-2
            isum = C(i-1,j-2,ii+2)+C(i-1,j+2,ii-1)+C(i-1,j+2,ii+2)+
     *C(i+2,j+2,ii+2)+ C(i+2,j+2,ii-1)+C(i+2,j-2,ii+2)+C(i+2,j-2,ii-1)+
     *C(i-1,j-2,ii-1)      
           if (B(i,j,ii).ne.isum) then
             nloopi=min(nloopi,i)
             nloopj=min(nloopj,j)
             nloopii=min(nloopii,ii)         
           endif
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii) 

      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end
C -----------------------------------------SC3203      
      subroutine SC3203
      integer, parameter :: N = 16,M=10,K=10,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(*,BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2,2:2) :: A     
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3203'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii) on A(i,j,ii),shadow_compute(A(0:2,2:2,0:2))
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
 
!dvm$ parallel (i,j,ii) on B(i,j,ii)
      do i=3,N-2
        do j=3,M-2
          do ii=3,K-2
            B(i,j,ii) = A(i+2,j+2,ii+2)+A(i,j-2,ii)+
     *A(i+2,j-2,ii)+A(i,j+2,ii)+ A(i,j+2,ii+2)+A(i+2,j-2,ii+2)+
     * A(i+2,j+2,ii)
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
!dvm$*,private(isum)
      do i=3,N-2
        do j=3,M-2
          do ii=3,K-2
            isum = C(i+2,j+2,ii+2)+C(i,j-2,ii)+
     *C(i+2,j-2,ii)+C(i,j+2,ii)+ C(i,j+2,ii+2)+C(i+2,j-2,ii+2)+
     *C(i+2,j+2,ii)
            if (B(i,j,ii).ne.isum) then         
              nloopi=min(nloopi,i)
              nloopj=min(nloopj,j)
              nloopii=min(nloopii,ii)
            endif
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)

      end
      
C ------------------------------------------SC3204   
      subroutine SC3204     
      integer, parameter :: N = 16,M=10,K=10,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,*)   
!dvm$ shadow(2:2,2:2,2:2) :: A     
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3204'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii) on A(i,j,ii),shadow_compute(A(2:2,2:0,2:0))
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
 
!dvm$ parallel (i,j,ii) on B(i,j,ii)
      do i=3,N-2
        do j=3,M-2
          do ii=3,K-2
           B(i,j,ii) = A(i+2,j,ii)+A(i-2,j-2,ii-2)+
     *A(i+2,j-2,ii-2)+ A(i-2,j,ii-2)+A(i-2,j-2,ii)+
     *A(i-2,j,ii)+A(i+2,j-2,ii)+A(i+2,j,ii-2)
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
!dvm$*,private(isum)
      do i=3,N-2
        do j=3,M-2
          do ii=3,K-2
           isum = C(i+2,j,ii)+C(i-2,j-2,ii-2)+
     *C(i+2,j-2,ii-2)+ C(i-2,j,ii-2)+C(i-2,j-2,ii)+
     *C(i-2,j,ii)+C(i+2,j-2,ii)+C(i+2,j,ii-2)
           if (B(i,j,ii).ne.isum) then         
             nloopi=min(nloopi,i)
             nloopj=min(nloopj,j)
             nloopii=min(nloopii,ii)
           endif
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)

      end
      
C ------------------------------------------SC3205
      subroutine SC3205   
      integer, parameter :: N = 16,M=16,K=16,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,*,BLOCK)   
!dvm$ shadow(0:2,2:2,0:2) :: A     
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3205'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii) on A(i,j,ii),shadow_compute 

      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
 
!dvm$ parallel (i,j,ii) on B(i,j,ii)
      do i=3,N-2
        do j=3,M-2
          do ii=3,K-2
            B(i,j,ii) = A(i+2,j+2,ii+2)+A(i,j-2,ii)+
     *   A(i+2,j-2,ii)+ A(i,j+2,ii)+A(i,j+2,ii+2)+
     *   A(i+2,j-2,ii+2)+A(i+2,j+2,ii) 
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
!dvm$*,private(isum)
      do i=3,N-2
        do j=3,M-2
          do ii=3,K-2
           isum = C(i+2,j+2,ii+2)+C(i,j-2,ii)+
     *   C(i+2,j-2,ii)+ C(i,j+2,ii)+C(i,j+2,ii+2)+
     *   C(i+2,j-2,ii+2)+C(i+2,j+2,ii)
           if (B(i,j,ii).ne.isum) then         
             nloopi=min(nloopi,i)
             nloopj=min(nloopj,j)
             nloopii=min(nloopii,ii)
           endif
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)

      end
           

C --------------------------------------------SC3206  
      subroutine SC3206   
      integer, parameter :: N = 16,M=16,K=16,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(*,BLOCK,BLOCK)   
!dvm$ shadow(3:3,3:3,3:3) :: A     
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3206'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii) on A(i,j,ii),shadow_compute
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
 
!dvm$ parallel (i,j,ii) on B(i,j,ii)
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            B(i,j,ii) = A(i-3,j-3,ii+3)+A(i+3,j+3,ii-3)+
     *   A(i+3,j-3,ii+3)+ A(i-3,j+3,ii+3)+A(i-3,j+3,ii-3)+
     *   A(i+3,j-3,ii-3)+A(i+3,j+3,ii+3)+A(i-3,j-3,ii-3) 
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
!dvm$*,private(isum)
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            isum = C(i-3,j-3,ii+3)+C(i+3,j+3,ii-3)+
     *   C(i+3,j-3,ii+3)+ C(i-3,j+3,ii+3)+C(i-3,j+3,ii-3)+
     *   C(i+3,j-3,ii-3)+ C(i+3,j+3,ii+3)+ C(i-3,j-3,ii-3)    
            if (B(i,j,ii).ne.isum) then         
              nloopi=min(nloopi,i)
              nloopj=min(nloopj,j)
              nloopii=min(nloopii,ii)
            endif
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii) 

      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end
           
C -------------------------------------------SC3207   
       subroutine SC3207   
       integer, parameter :: N = 16,M=16,K=16,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,*)   
!dvm$ shadow(3:3,0:3,3:0) :: A     
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3207'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii) on A(i,j,ii),shadow_compute
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
 
!dvm$ parallel (i,j,ii) on B(i,j,ii)
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
             B(i,j,ii) = A(i+3,j+3,ii)+A(i-3,j,ii-3)+
     *   A(i+3,j,ii-3)+ A(i-3,j+3,ii-3)+ A(i-3,j,ii)+
     *   A(i-3,j+3,ii)+ A(i+3,j,ii)+ A(i+3,j+3,ii-3)
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
!dvm$*,private(isum)
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            isum = C(i+3,j+3,ii)+C(i-3,j,ii-3)+
     *   C(i+3,j,ii-3)+ C(i-3,j+3,ii-3)+C(i-3,j,ii)+
     *   C(i-3,j+3,ii)+ C(i+3,j,ii)+ C(i+3,j+3,ii-3)    
            if (B(i,j,ii).ne.isum) then        
               nloopi=min(nloopi,i)
               nloopj=min(nloopj,j)
               nloopii=min(nloopii,ii)
            endif
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)

      end
                    
     
C -------------------------------------------SC3208   
      subroutine SC3208   
      integer, parameter :: N = 16,M=16,K=16,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,*,BLOCK)   
!dvm$ shadow(0:3,0:3,0:3) :: A     
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3208'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii) on A(i,j,ii),shadow_compute(A(0:3,0:3,0:3))
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
 
!dvm$ parallel (i,j,ii) on B(i,j,ii)
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
             B(i,j,ii) = A(i+3,j+3,ii+3)+A(i+3,j,ii)+
     *   A(i,j+3,ii)+ A(i,j,ii+3)+ A(i,j+3,ii+3)+
     *   A(i+3,j,ii+3)+ A(i+3,j+3,ii)
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
!dvm$*,private(isum)
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
           isum = C(i+3,j+3,ii+3)+C(i+3,j,ii)+
     *   C(i,j+3,ii)+ C(i,j,ii+3)+ C(i,j+3,ii+3)+
     *   C(i+3,j,ii+3)+ C(i+3,j+3,ii)
           if (B(i,j,ii).ne.isum) then         
             nloopi=min(nloopi,i)
             nloopj=min(nloopj,j)
             nloopii=min(nloopii,ii)
           endif
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)

      end
                    
C -------------------------------------------SC3209   
      subroutine SC3209   
      integer, parameter :: N = 120,M=120,K=120,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(*,BLOCK,BLOCK)   
!dvm$ shadow(11:11,11:11,11:11) :: A     
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3209'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii) on A(i,j,ii),shadow_compute
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
 
!dvm$ parallel (i,j,ii) on B(i,j,ii)
      do i=12,N-11
        do j=12,M-11
          do ii=12,K-11
             B(i,j,ii) = A(i+11,j+11,ii+11)+A(i-11,j-11,ii-11)+
     *   A(i+11,j-11,ii-11)+ A(i-11,j+11,ii-11)+ A(i-11,j-11,ii+11)+
     *   A(i-11,j+11,ii+11)+ A(i+11,j-11,ii+11)+A(i+11,j+11,ii-11)
          enddo 
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
!dvm$*,private(isum)
      do i=12,N-11
        do j=12,M-11
          do ii=12,K-11
            isum = C(i+11,j+11,ii+11)+C(i-11,j-11,ii-11)+
     *   C(i+11,j-11,ii-11)+ C(i-11,j+11,ii-11)+ C(i-11,j-11,ii+11)+
     *   C(i-11,j+11,ii+11)+ C(i+11,j-11,ii+11)+C(i+11,j+11,ii-11)
            if (B(i,j,ii).ne.isum) then
              nloopi=min(nloopi,i)
              nloopj=min(nloopj,j)
              nloopii=min(nloopii,ii)         
            endif
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj,nloopii) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)

      end

C -----------------------------------------------         
      subroutine serial3(AR,N,M,K,NL)
      integer AR(N,M,K)
      integer NL 
      do i=1,N
        do j=1,M
          do ii=1,K
            AR(i,j,ii) = NL+i+j+ii
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
