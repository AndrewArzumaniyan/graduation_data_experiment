      program SC21

c    TESTING OF THE SHADOW DIRECTIVE AND THE SHADOW_COMPUTE CLAUSE'.            
c    DISTRIBUTED ARRAY A(N,M) IS TO HAVE DIFFERENT SHADOW WIDTH
c    ON BOTH SIDES 

      print *,'===START OF SC21========================'
C --------------------------------------------------
      call sc2101
C --------------------------------------------------
      call sc2102
C --------------------------------------------------
      call sc2103
C -------------------------------------------------
      call sc2104
C -------------------------------------------------
      call sc2105
C -------------------------------------------------
      call sc2106
C -------------------------------------------------
      call sc2107
C --------------------------------------------------
      call sc2108
C----------------------------------------------------
      call sc2109
C -------------------------------------------------
      call sc2110
C -------------------------------------------------
      call sc2111
C -------------------------------------------------
      call sc2112
C ------------------------------------------------- 
      call sc2113
C --------------------------------------------------
      call sc2114
C --------------------------------------------------
      call sc2115
C -------------------------------------------------
      call sc2116
C -------------------------------------------------
      call sc2117
C -------------------------------------------------
      call sc2118
C -------------------------------------------------
      call sc2119
C -------------------------------------------------
      call sc2120
C -------------------------------------------------
   
C -------------------------------------------------

C
C
      print *,'=== END OF SC21 ========================= '    
      end
C ---------------------------------------------SC2101
      subroutine sc2101
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum
      character*6 tname 
!dvm$ distribute B(BLOCK,BLOCK)       
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2101'     
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
C ---------------------------------------------SC2102     
      subroutine sc2102
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)       
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2102'
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
C -----------------------------------------SC2103      
      subroutine sc2103
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)      
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2103'
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
      
C ------------------------------------------SC2104   
      subroutine sc2104
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(1:1,0:1) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2104'
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
  
C ------------------------------------------SC2105   
      subroutine sc2105
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(0:1,1:1) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2105'
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
      
C --------------------------------------------SC2106  
         
      subroutine sc2106
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(0:1,0:1) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2106'
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
C -------------------------------------------SC2107   
         
      subroutine sc2107
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(1:0,1:0) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2107'
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
C -------------------------------------------SC2108   
         
      subroutine sc2108
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2108'
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
      
C -------------------------------------------SC2109   
         
      subroutine sc2109
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2109'
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
C -------------------------------------------SC2110  
         
      subroutine sc2110
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2110'
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
C -------------------------------------------SC2111   
         
      subroutine sc2111
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(2:2,0:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2111'
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
C -------------------------------------------SC2112   
         
      subroutine sc2112
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(2:0,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2112'
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
C -------------------------------------------SC2113  
         
      subroutine sc2113
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:0) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2113'
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
C -------------------------------------------SC2114   
         
      subroutine sc2114
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(2:0,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2114'
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
C -------------------------------------------SC2115   
         
      subroutine sc2115
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(3:3,3:3) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2115'
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
C -------------------------------------------SC2116   
         
      subroutine sc2116
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(3:3,0:3) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2116'
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
C --------------------------------------------SC2117  
         
      subroutine sc2117
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(0:3,3:3) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2117'
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
C --------------------------------------------SC2118  
         
      subroutine sc2118
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(3:3,3:0) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2118'
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
C --------------------------------------------SC2119  
         
      subroutine sc2119
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(3:0,3:3) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2119'
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
C --------------------------------------------SC2120  
         
      subroutine sc2120
      integer, parameter :: N = 480,M=480,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(11:11,11:11) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SC2120'
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
