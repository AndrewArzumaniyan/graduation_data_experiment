      program SC22

c    TESTING OF THE SHADOW DIRECTIVE AND THE SHADOW_COMPUTE CLAUSE'.            
c    DISTRIBUTED ARRAY A(N,M) IS TO HAVE DIFFERENT SHADOW WIDTH
c    ON BOTH SIDES 

      print *,'===START OF SC22========================'
C --------------------------------------------------
      call sc2201
C --------------------------------------------------
      call sc2202
C --------------------------------------------------
      call sc2203
C -------------------------------------------------
      call sc2204
C -------------------------------------------------
      call sc2205
C -------------------------------------------------
      call sc2206
C -------------------------------------------------
      call sc2207
C --------------------------------------------------
      call sc2208
C----------------------------------------------------
      call sc2209
C -------------------------------------------------
      call sc2210
C -------------------------------------------------
      call sc2211
C -------------------------------------------------
      call sc2212
C ------------------------------------------------- 
      call sc2213
C --------------------------------------------------
      call sc2214
C --------------------------------------------------
      call sc2215
C -------------------------------------------------
      call sc2216
C -------------------------------------------------
      call sc2217
C -------------------------------------------------
      call sc2218
C -------------------------------------------------
      call sc2219
C -------------------------------------------------
      call sc2220
C -------------------------------------------------
   
C -------------------------------------------------

C
C
      print *,'=== END OF SC22 ========================= '    
      end
C ---------------------------------------------SC2201
      subroutine SC2201
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum
      character*6 tname 
!dvm$ distribute B(BLOCK,*)       
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2201'     
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                               
 
!dvm$ parallel (i,j) on B(i,j)
      do i=2,N-1
        do j=2,M-1
          B(i,j) = A(i+1,j)+A(i,j+1)+A(i-1,j)+A(i,j-1)+
     *A(i-1,j-1)+ A(i+1,j+1)+A(i-1,j+1)+A(i+1,j-1)          
         enddo
      enddo
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=2,N-1
        do j=2,M-1
          isum = C(i+1,j)+C(i,j+1)+C(i-1,j)+C(i,j-1)+
     *C(i-1,j-1)+ C(i+1,j+1)+C(i-1,j+1)+C(i+1,j-1)       
          if (B(i,j).ne.isum) then         
             nloopi=min(nloopi,i)
             nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end
C ---------------------------------------------SC2202     
      subroutine SC2202
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)       
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2202'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute(A(0:1,0:1))
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                

!dvm$ parallel (i,J) on B(i,j)
      do i=2,N-1
        do j=2,M-1
          B(i,j) = A(i+1,j)+A(i,j+1)+A(i+1,j+1)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=2,N-1
        do j=2,M-1
          isum = C(i+1,j)+C(i,j+1)+C(i+1,j+1)
          if (B(i,j).ne.isum) then
            nloopi=min(nloopi,i)
            nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 
      
      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)
         
      end
C -----------------------------------------SC2203      
      subroutine SC2203
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)      
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2203'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute(A(1:0,0:1))
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j)
      do i=2,N-1
        do j=2,M-1
          B(i,j) = A(i-1,j)+A(i,j+1)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=2,N-1
        do j=2,M-1
          isum = C(i-1,j)+C(i,j+1)
          if (B(i,j).ne.isum) then
            nloopi=min(nloopi,i)
            nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)
    
      end
      
C ------------------------------------------SC2204   
      subroutine SC2204
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(1:1,0:1) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2204'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute(A(0:1,0:1))
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j)
      do i=2,N-1
        do j=2,M-1
          B(i,j) = A(i+1,j)+A(i,j+1)+A(i+1,j+1)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=2,N-1
        do j=2,M-1
          isum = C(i+1,j)+C(i,j+1)+C(i+1,j+1)
          if (B(i,j).ne.isum) then
            nloopi=min(nloopi,i)
            nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)        
 
      end
  
C ------------------------------------------SC2205   
      subroutine SC2205
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)   
!dvm$ shadow(0:1,1:1) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2205'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute(A(0:1,1:0))
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j)
      do i=2,N-1
        do j=2,M-1
          B(i,j) = A(i,j-1)+A(i+1,j)+A(i+1,j-1)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=2,N-1
        do j=2,M-1
          isum = C(i,j-1)+C(i+1,j)+C(i+1,j-1)
          if (B(i,j).ne.isum) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)      
 
      end
      
C --------------------------------------------SC2206  
         
      subroutine SC2206
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(0:1,0:1) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2206'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute(A(0:1,0:0))
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j)
      do i=2,N-1
        do j=2,M-1
          B(i,j) =A(i+1,j)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=2,N-1
        do j=2,M-1
          isum = C(i+1,j)
          if (B(i,j).ne.isum) then
            nloopi=min(nloopi,i)
            nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)          
  
      end
C -------------------------------------------SC2207   
         
      subroutine SC2207
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)   
!dvm$ shadow(1:0,1:0) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2207'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute(A(0:0,1:0))
      do i=1,N
        do j=1,M
         A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j)
      do i=2,N-1
        do j=2,M-1
          B(i,j) =A(i,j-1)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=2,N-1
        do j=2,M-1
          isum = C(i,j-1)
          if (B(i,j).ne.isum) then
            nloopi=min(nloopi,i)
            nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A,B,C)
     
      end      
C -------------------------------------------SC2208   
         
      subroutine SC2208
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(2:2,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2208'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                

!dvm$ parallel (i,J) on B(i,j)
      do i=3,N-2
        do j=3,M-2
          B(i,j) = A(i+2,j)+A(i,j+2)+A(i+2,j)+A(i+2,j+2)+
     * A(i-2,j+2)+A(i-2,j)+A(i,j-2)+A(i-2,j-2)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=3,N-2
        do j=3,M-2
          isum = C(i+2,j)+C(i,j+2)+C(i+2,j)+C(i+2,j+2)+
     *C(i-2,j+2)+C(i-2,j)+C(i,j-2)+C(i-2,j-2)       
          if (B(i,j).ne.isum) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,C)
      
      end   
      
C -------------------------------------------SC2209   
         
      subroutine SC2209
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)   
!dvm$ shadow(2:2,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2209'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute(A(0:2,2:2))
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j)
      do i=3,N-2
        do j=3,M-2
         B(i,j) = A(i+2,j+2)+ A(i+1,j+1)+A(i,j+2)+A(i,j-2)+A(i+2,j-2)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=3,N-2
        do j=3,M-2
          isum = C(i+2,j+2)+ C(i+1,j+1)+C(i,j+2)+C(i,j-2)+A(i+2,j-2)
          if (B(i,j).ne.isum) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,C)      
 
      end   
C -------------------------------------------SC2210  
         
      subroutine sc2210
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(2:2,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2210'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute(A(2:2,2:0))
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j)
      do i=3,N-2
        do j=3,M-2
         B(i,j) = A(i-2,j-2)+ A(i-1,j-1)+A(i-2,j)+A(i+2,j)+A(i+2,j-2)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=3,N-2
         do j=3,M-2
           isum = C(i-2,j-2)+C(i-1,j-1)+C(i-2,j)+C(i+2,j)+C(i+2,j-2)
           if (B(i,j).ne.isum) then
            nloopi=min(nloopi,i)
            nloopj=min(nloopj,j)
           endif
         enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,C)
   
      end 
C -------------------------------------------SC2211   
         
      subroutine SC2211
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)   
!dvm$ shadow(2:2,0:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2211'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute(A(2:2,0:2))
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j)
      do i=3,N-2
        do j=3,M-2
          B(i,j) = A(i+2,j+2)+ A(i+1,j+1)+A(i-2,j+2)+A(i+2,j)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=3,N-2
        do j=3,M-2
          isum = C(i+2,j+2)+ C(i+1,j+1)+C(i-2,j+2)+C(i+2,j)
          if (B(i,j).ne.isum) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif       
      deallocate (A,B,C) 
 
      end  
C -------------------------------------------SC2212   
         
      subroutine SC2212
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(2:0,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2212'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute(A(1:0,0:1))
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j)
      do i=3,N-2
        do j=3,M-2
          B(i,j) = A(i-1,j+1)+ A(i,j+1)+A(i-1,j)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=3,N-2
        do j=3,M-2
          isum = C(i-1,j+1)+ C(i,j+1)+C(i-1,j)
          if (B(i,j).ne.isum) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,C)        

      end
C -------------------------------------------SC2213  
         
      subroutine SC2213
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)   
!dvm$ shadow(2:2,2:0) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2213'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute(A(0:1,0:0))
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo          

!dvm$ parallel (i,J) on B(i,j)
      do i=3,N-2
        do j=3,M-2
          B(i,j) = A(i+1,j)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=3,N-2
        do j=3,M-2
          isum = C(i+1,j)
          if (B(i,j).ne.isum) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,C)

      end
C -------------------------------------------SC2214   
         
      subroutine SC2214
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(2:0,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2214'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute(A(0:0,0:2))
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                

!dvm$ parallel (i,J) on B(i,j)
      do i=3,N-2
        do j=3,M-2
          B(i,j) = A(i,j+2)+A(i,j+1)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=3,N-2
        do j=3,M-2
          isum = C(i,j+2)+C(i,j+1)
          if (B(i,j).ne.isum) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,C) 
   
      end 
C -------------------------------------------SC2215   
         
      subroutine SC2215
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)   
!dvm$ shadow(3:3,3:3) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2215'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                

!dvm$ parallel (i,J) on B(i,j)
      do i=4,N-3
        do j=4,M-3
          B(i,j) = A(i+1,j+1)+A(i+2,j+2)+A(i+3,j+3)+A(i-3,j-3)+
     * A(i-2,j-2)+A(i-1,j-1)+A(i-3,j+3)+A(i+3,j-3)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=4,N-3
        do j=4,M-3
          isum = C(i+1,j+1)+C(i+2,j+2)+C(i+3,j+3)+C(i-3,j-3)+
     * C(i-2,j-2)+C(i-1,j-1)+C(i-3,j+3)+C(i+3,j-3)       
          if (B(i,j).ne.isum) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif       
      deallocate (A,B,C)             

      end 
C -------------------------------------------SC2216   
         
      subroutine SC2216
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(3:3,0:3) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2216'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute(A(0:0,0:1))
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j)
      do i=3,N-2
        do j=3,M-2
          B(i,j) = A(i,j+1)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=3,N-2
        do j=3,M-2
          isum = C(i,j+1)
          if (B(i,j).ne.isum) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,C)         
     
      end 
C --------------------------------------------SC2217  
         
      subroutine SC2217
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)   
!dvm$ shadow(0:3,3:3) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2217'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute(A(0:1,0:0))
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                

!dvm$ parallel (i,J) on B(i,j)
      do i=3,N-2
        do j=3,M-2
          B(i,j) = A(i+1,j)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=3,N-2
        do j=3,M-2
          isum = C(i+1,j)
          if (B(i,j).ne.isum) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,C)

      end
C --------------------------------------------SC2218  
         
      subroutine SC2218
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(3:3,3:0) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2218'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j)
      do i=4,N-3
        do j=4,M-3
          B(i,j) = A(i-3,j-3)+A(i+3,j)+A(i-3,j)+A(i-3,j-3)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=4,N-3
        do j=4,M-3
          isum = C(i-3,j-3)+C(i+3,j)+C(i-3,j)+C(i-3,j-3)
          if (B(i,j).ne.isum) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,C)

      end
C --------------------------------------------SC2219  
         
      subroutine SC2219
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)   
!dvm$ shadow(3:0,3:3) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2219'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute(A(3:0,3:3))
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j)
      do i=4,N-3
        do j=4,M-3
          B(i,j) = A(i-3,j-3)+A(i,j+3)+A(i-3,j+3)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=4,N-3
        do j=4,M-3
          isum = C(i-3,j-3)+C(i,j+3)+C(i-3,j+3)
          if (B(i,j).ne.isum) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,C)

      end
C --------------------------------------------SC2220  
         
      subroutine SC2220
      integer, parameter :: N = 480,M=480,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(11:11,11:11) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2220'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j),shadow_compute
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j)
      do i=12,N-11
        do j=12,M-11
          B(i,j) = A(i+11,j+11)+A(i+10,j+10)+A(i+9,j+9)+
     *A(i-11,j-11)+A(i-10,j-10)+A(i-9,j-9)+A(i+11,j-11)+
     *A(i-11,j+11)
        enddo
      enddo 
  
!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
!dvm$*,private(isum) 
      do i=12,N-11
        do j=12,M-11
          isum = C(i+11,j+11)+C(i+10,j+10)+C(i+9,j+9)+
     *C(i-11,j-11)+C(i-10,j-10)+C(i-9,j-9)+C(i+11,j-11)+
     *C(i-11,j+11)
          if (B(i,j).ne.isum) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(nloopi,nloopj) 

      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,C)

      end
C -----------------------------------------------         
      subroutine serial2(AR,N,M,NL)
      integer AR(N,M)
      integer NL 
      do i=1,N
        do j=1,M
          AR(i,j) = NL+i+j
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
