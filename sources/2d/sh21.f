      program SH21
     
c    TESTING OF THE SHADOW DIRECTIVE AND THE SHADOW_RENEW CLAUSE'.       
c    DISTRIBUTED ARRAY A(N,M) IS TO HAVE DIFFERENT SHADOW WIDTH
c    ON BOTH SIDES 

      print *,'===START OF SH21========================'
C --------------------------------------------------
      call sh2101
C --------------------------------------------------
      call sh2102
C --------------------------------------------------
      call sh2103
C -------------------------------------------------
      call sh2104
C -------------------------------------------------
      call sh2105
C -------------------------------------------------
      call sh2106
C --------------------------------------------------
      call sh2107
C --------------------------------------------------
      call sh2108
C----------------------------------------------------
      call sh2109
C -------------------------------------------------
      call sh2110
C -------------------------------------------------
      call sh2111
C -------------------------------------------------
      call sh2112
C ------------------------------------------------- 
      call sh2113
C --------------------------------------------------
      call sh2114
C --------------------------------------------------
      call sh2115
C -------------------------------------------------
      call sh2116
C -------------------------------------------------
      call sh2117
C -------------------------------------------------
      call sh2118
C -------------------------------------------------
      call sh2119
C -------------------------------------------------
      call sh2120
C -------------------------------------------------
   
C -------------------------------------------------

C
C
      print *,'=== END OF SH21 ========================= '    
      end
C ---------------------------------------------SH2101
      subroutine SH2101
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum
      character*6 tname 
!dvm$ distribute B(BLOCK,BLOCK)       
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2101'     
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                               
 
!dvm$ parallel (i,j) on B(i,j),shadow_renew(A(CORNER))
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
C ---------------------------------------------SH2102     
      subroutine sh2102
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)       
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2102'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                

!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(0:1,0:1)(CORNER))
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
C -----------------------------------------SH2103      
      subroutine sh2103
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)      
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2103'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(1:0,0:1))
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
      
C ------------------------------------------SH2104   
      subroutine sh2104
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(1:1,0:1) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2104'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(0:1,0:1)(CORNER))
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
  
C ------------------------------------------SH2105   
      subroutine sh2105
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(0:1,1:1) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2105'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(0:1,1:0)(CORNER))
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
      
C --------------------------------------------SH2106  
         
      subroutine sh2106
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(0:1,0:1) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2106'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(0:1,0:0))
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
C -------------------------------------------SH2107   
         
      subroutine sh2107
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(1:0,1:0) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2107'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
         A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(0:0,1:0))
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
C -------------------------------------------SH2108   
         
      subroutine sh2108
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2108'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                

!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(CORNER))
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
      
C -------------------------------------------SH2109   
         
      subroutine sh2109
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2109'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(0:2,2:2)(CORNER))
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
C -------------------------------------------SH2110  
         
      subroutine sh2110
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2110'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(2:2,2:0)(CORNER))
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
C -------------------------------------------SH2111   
         
      subroutine sh2111
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(2:2,0:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2111'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(2:2,0:2)(CORNER))
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
C -------------------------------------------SH2112   
         
      subroutine sh2112
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(2:0,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2112'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(1:0,0:1)(CORNER))
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
C -------------------------------------------SH2113  
         
      subroutine sh2113
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:0) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2113'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo          

!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(0:1,0:0))
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
C -------------------------------------------SH2114   
         
      subroutine sh2114
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(2:0,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2114'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                

!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(0:0,0:2))
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
C -------------------------------------------SH2115   
         
      subroutine sh2115
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(3:3,3:3) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2115'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                

!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(CORNER))
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
C -------------------------------------------SH2116   
         
      subroutine sh2116
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(3:3,0:3) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2116'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(0:0,0:1))
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
C --------------------------------------------SH2117  
         
      subroutine sh2117
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(0:3,3:3) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2117'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                

!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(0:1,0:0))
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
C --------------------------------------------SH2118  
         
      subroutine sh2118
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(3:3,3:0) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2118'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(CORNER))
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
C --------------------------------------------SH2119  
         
      subroutine sh2119
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(3:0,3:3) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2119'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(3:0,3:3)(CORNER))
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
C --------------------------------------------SH2120  
         
      subroutine sh2120
      integer, parameter :: N = 480,M=480,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,BLOCK)   
!dvm$ shadow(11:11,11:11) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2120'
      allocate (B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ actual(nloopi,nloopj)
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                                
          
!dvm$ parallel (i,J) on B(i,j),shadow_renew(A(CORNER))
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
