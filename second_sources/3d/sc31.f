      program SC31
     
c    TESTING OF THE SHADOW DIRECTIVE AND THE SHADOW_COMPUTE CLAUSE       
c    DISTRIBUTED ARRAY A(N,M,K) IS TO HAVE DIFFERENT SHADOW WIDTH
c    ON BOTH SIDES 

      print *,'===START OF SC31========================'
C --------------------------------------------------
      call sc3101
C --------------------------------------------------
      call sc3102
C --------------------------------------------------
      call sc3103
C -------------------------------------------------
      call sc3104
C -------------------------------------------------
      call sc3105
C -------------------------------------------------
      call sc3106
C --------------------------------------------------
      call sc3107
C --------------------------------------------------
      call sc3108
C----------------------------------------------------
      call sc3109
C----------------------------------------------------

C
C
      print *,'=== END OF SC31 ========================= '    
      end
C ---------------------------------------------SC3101
      subroutine sc3101     
      integer, parameter :: N = 16,M=8,K=8,NL=1000     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3101'     
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
C ---------------------------------------------SC3102     
      subroutine SC3102
      integer, parameter :: N = 16,M=10,K=10,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2,2:2) :: A     
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3102'     
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
C -----------------------------------------SC3103      
      subroutine SC3103
      integer, parameter :: N = 16,M=10,K=10,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2,2:2) :: A     
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3103'     
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
      
C ------------------------------------------SC3104   
      subroutine SC3104     
      integer, parameter :: N = 16,M=10,K=10,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2,2:2) :: A     
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3104'     
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
      
C ------------------------------------------SC3105
      subroutine SC3105   
      integer, parameter :: N = 16,M=16,K=16,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(0:2,2:2,0:2) :: A     
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3105'     
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
           

C --------------------------------------------SC3106  
      subroutine SC3106   
      integer, parameter :: N = 16,M=16,K=16,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(3:3,3:3,3:3) :: A     
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3106'     
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
           
C -------------------------------------------SC3107   
       subroutine SC3107   
       integer, parameter :: N = 16,M=16,K=16,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(3:3,0:3,3:0) :: A     
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3107'     
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
                    
     
C -------------------------------------------SC3108   
      subroutine SC3108   
      integer, parameter :: N = 16,M=16,K=16,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(0:3,0:3,0:3) :: A     
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3108'     
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
                    
C -------------------------------------------SC3109   
      subroutine SC3109   
      integer, parameter :: N = 120,M=120,K=120,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,isum
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(11:11,11:11,11:11) :: A     
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='SC3109'     
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
