      program SH22
     
c    TESTING OF THE SHADOW DIRECTIVE AND THE SHADOW_RENEW CLAUSE'.       
c    DISTRIBUTED ARRAY A(N,M) IS TO HAVE DIFFERENT SHADOW WIDTH
c    ON BOTH SIDES 

      print *,'===START OF SH22========================'
C --------------------------------------------------
      call sh2201
C --------------------------------------------------
      call sh2202
C --------------------------------------------------
      call sh2203
C -------------------------------------------------
      call sh2204
C -------------------------------------------------
      call sh2205
C -------------------------------------------------
      call sh2206
C --------------------------------------------------
      call sh2207
C --------------------------------------------------
      call sh2208
C----------------------------------------------------
      call sh2209
C -------------------------------------------------
      call sh2210
C -------------------------------------------------
      call sh2211
C -------------------------------------------------
      call sh2212
C ------------------------------------------------- 
      call sh2213
C --------------------------------------------------
      call sh2214
C --------------------------------------------------
      call sh2215
C -------------------------------------------------
      call sh2216
C -------------------------------------------------
      call sh2217
C -------------------------------------------------
      call sh2218
C -------------------------------------------------
      call sh2219
C -------------------------------------------------
      call sh2220
C -------------------------------------------------
   
C -------------------------------------------------

C
C
      print *,'=== END OF SH22 ========================= '    
      end
C ---------------------------------------------SH2201
      subroutine SH2201
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum
      character*6 tname 
!dvm$ distribute B(BLOCK,*)       
!dvm$ align (I,J) with B(I,J) ::A


      tname='SH2201'     
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
C ---------------------------------------------SH2202     
      subroutine sh2202
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)       
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2202'
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
C -----------------------------------------SH2203      
      subroutine sh2203
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)      
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2203'
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
      
C ------------------------------------------SH2204   
      subroutine sh2204
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(1:1,0:1) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2204'
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
  
C ------------------------------------------SH2205   
      subroutine sh2205
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)   
!dvm$ shadow(0:1,1:1) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2205'
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
      
C --------------------------------------------SH2206  
         
      subroutine sh2206
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(0:1,0:1) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2206'
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
C -------------------------------------------SH2207   
         
      subroutine sh2207
      integer, parameter :: N = 16,M=8,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)   
!dvm$ shadow(1:0,1:0) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2207'
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
C -------------------------------------------SH2208   
         
      subroutine sh2208
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(2:2,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2208'
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
      
C -------------------------------------------SH2209   
         
      subroutine sh2209
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)   
!dvm$ shadow(2:2,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2209'
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
C -------------------------------------------SH2210  
         
      subroutine sh2210
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(2:2,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2210'
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
C -------------------------------------------SH2211   
         
      subroutine sh2211
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)   
!dvm$ shadow(2:2,0:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2211'
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
C -------------------------------------------SH2212   
         
      subroutine sh2212
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(2:0,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2212'
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
C -------------------------------------------SH2213  
         
      subroutine sh2213
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)   
!dvm$ shadow(2:2,2:0) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2213'
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
C -------------------------------------------SH2214   
         
      subroutine sh2214
      integer, parameter :: N = 16,M=17,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(2:0,2:2) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2214'
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
C -------------------------------------------SH2215   
         
      subroutine sh2215
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)   
!dvm$ shadow(3:3,3:3) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2215'
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
C -------------------------------------------SH2216   
         
      subroutine sh2216
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(3:3,0:3) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2216'
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
C --------------------------------------------SH2217  
         
      subroutine sh2217
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)   
!dvm$ shadow(0:3,3:3) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2217'
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
C --------------------------------------------SH2218  
         
      subroutine sh2218
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(3:3,3:0) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2218'
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
C --------------------------------------------SH2219  
         
      subroutine sh2219
      integer, parameter :: N = 32,M=32,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(BLOCK,*)   
!dvm$ shadow(3:0,3:3) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2219'
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
C --------------------------------------------SH2220  
         
      subroutine sh2220
      integer, parameter :: N = 480,M=480,NL=1000
      character*6 tname 
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj,isum 

!dvm$ distribute B(*,BLOCK)   
!dvm$ shadow(11:11,11:11) :: A 
!dvm$ align (I,J) with B(I,J) ::A

      tname='SH2220'
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
