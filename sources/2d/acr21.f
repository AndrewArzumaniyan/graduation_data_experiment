      program ACR21
     
c    TESTING OF THE ACROSS CLAUSE.       
c    DISTRIBUTED ARRAY A(N,M) IS TO HAVE DIFFERENT 
c    FLOW-DEP-LENGTH ON BOTH SIDES 

      print *,'===START OF ACR21========================'
C --------------------------------------------------
      call acr2101
C --------------------------------------------------
      call acr2102
C --------------------------------------------------
      call acr2103
C -------------------------------------------------
      call acr2104
C -------------------------------------------------
      call acr2105
C -------------------------------------------------
      call acr2106
C --------------------------------------------------
      call acr2107
C --------------------------------------------------
      call acr2108
C --------------------------------------------------
      call acr2109
C -------------------------------------------------
      call acr2110
C -------------------------------------------------
      call acr2111
C -------------------------------------------------
      call acr2112
C -------------------------------------------------
      call acr2113
C -------------------------------------------------
      call acr2114
C -------------------------------------------------
      call acr2115
C -------------------------------------------------
      print *,'=== END OF ACR21 ========================= '    
      end
C ---------------------------------------------ACR2101
      subroutine ACR2101
     
      integer,parameter :: N = 16, M=16, NL=1000
      character*7 tname 
      integer,allocatable::  A(:,:), C(:,:)
      integer nloopi,nloopj
                      
!dvm$ distribute A(BLOCK,BLOCK)    

      tname='ACR2101'
      allocate (A(N,M), C(N,M))
      
      do iloop=0,2	  
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL
      do i=2,N-1
        do j=2,M-1
         C(i,j) = C(i+1,j)+C(i,j+1)+C(i-1,j)+C(i,j-1)
        enddo
      enddo
!dvm$ actual (nloopi,nloopj,C)
!dvm$ region

!dvm$ parallel (j,i) on A(i,j)
      do j=1,M
        do i=1,N
         A(i,j) = NL+i+j
        enddo
      enddo

!dvm$ parallel (j,i) on A(i,j),across(A(1:1,1:1)),stage(iloop)
      do j=2,M-1
         do i=2,N-1
            A(i,j) = A(i+1,j)+A(i,j+1)+A(i-1,j)+A(i,j-1)
        enddo
      enddo

!dvm$ parallel (j,i) on A(i,j), reduction( min( nloopi),min(nloopj))
      do j=2,M-1
       do i=2,N-1
          if (A(i,j).ne.C(i,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual (nloopi,nloopj)
      enddo
	  
      if (nloopi .eq.NL) then
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (A, C)
      
      end
C ---------------------------------------------ACR2102     
      subroutine ACR2102
      integer,parameter :: N = 16,M=16, NL=1000

      character*7 tname 
      integer,allocatable::  A(:,:), C(:,:)
      integer nloopi,nloopj 
                      
!dvm$ distribute A(BLOCK,BLOCK)    

      tname='ACR2102'
      allocate (A(N,M), C(N,M))
      
      do iloop=0,2	  
      NNL=NL    
      call serial2(C,N,M,NNL)
      do i=2,N-1
        do j=2,M-1
         C(i,j) = C(i+1,j)
        enddo
      enddo
      nloopi=NL
      nloopj=NL
!dvm$ actual(nloopi,nloopj)
!dvm$ region
!dvm$ parallel (j,i) on A(i,j)
      do j=1,M
        do i=1,N
         A(i,j) = NL+i+j
        enddo
      enddo
!dvm$ parallel (j,i) on A(i,j),across(A(0:1,0:0)),stage(iloop)
      do j=2,M-1
       do i=2,N-1
         A(i,j) = A(i+1,j)
        enddo
      enddo
  
!dvm$ parallel (j,i) on A(i,j), reduction( min( nloopi),min(nloopj))
       do j=2,M-1
        do i=2,N-1
          if (A(i,j).ne.C(i,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual (nloopi)
       enddo
	       
      if (nloopi .eq.NL) then
        call ansyes(tname)
      else
        call ansno(tname)
      endif                                                 
      deallocate (A, C)
               
      end

C -----------------------------------------ACR2103      
      subroutine acr2103
      integer,parameter :: N = 16,M=16, NL=1000
     
      character*7 tname 
      integer,allocatable::  A(:,:), C(:,:)

      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,BLOCK)   

      tname='ACR2103'
      allocate (A(N,M), C(N,M))  
	        
      do iloop=0,2
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL
      do i=2,N-1
        do j=2,M-1
         C(i,j) = C(i-1,j)+C(i,j+1)
        enddo
      enddo
!dvm$ actual (nloopi,nloopj,C(:,:))
!dvm$ region

!dvm$ parallel (j,i) on A(i,j)
      do j=1,M
        do i=1,N
         A(i,j) = NL+i+j
        enddo
      enddo

!dvm$ parallel (j,i) on A(i,j),across(A(1:0,0:1)),stage(iloop)
      do j=2,M-1
       do i=2,N-1
         A(i,j) = A(i-1,j)+A(i,j+1)
        enddo
      enddo
  
!dvm$ parallel (j,i) on A(i,j), reduction( min( nloopi),min(nloopj))
       do j=2,M-1
        do i=2,N-1
          if (A(i,j).ne.C(i,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual (nloopi)
      enddo
	  
      if (nloopi .eq.NL) then
          call ansyes(tname)
          else
          call ansno(tname)
      endif
      deallocate (A, C)
    
      end

C ------------------------------------------ACR2104   
      subroutine acr2104
      integer,parameter :: N = 16,M=16, NL=1000
      character*7 tname 
      integer,allocatable::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,BLOCK)   
!dvm$ shadow A(1:1,0:1)
      tname='ACR2104'
      allocate (A(N,M), C(N,M))  
	        
      do iloop=0,2
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL
      do i=2,N-1
        do j=2,M-1
         C(i,j) = C(i+1,j)+C(i,j+1)
        enddo
      enddo
!dvm$ actual (nloopi,nloopj,C)
!dvm$ region

!dvm$ parallel (j,i) on A(i,j)
      do j=1,M
        do i=1,N
         A(i,j) = NL+i+j
        enddo
      enddo

!dvm$ parallel (j,i) on A(i,j),across(A(0:1,0:1)),stage(iloop)
      do j=2,M-1
       do i=2,N-1
         A(i,j) = A(i+1,j)+A(i,j+1)
        enddo
      enddo
  
!dvm$ parallel (j,i) on A(i,j), reduction( min( nloopi),min(nloopj))
       do j=2,M-1
        do i=2,N-1
          if (A(i,j).ne.C(i,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual (nloopi)
      enddo
	  
      if (nloopi .eq.NL) then
          call ansyes(tname)
          else
          call ansno(tname)
      endif
      deallocate (A, C)
 
      end
    
C ------------------------------------------ACR2105   
      subroutine acr2105 
      integer,parameter :: N = 16,M=16, NL=1000
      integer,allocatable::  A(:,:), C(:,:)
      character*7 tname 
      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,BLOCK)   
!dvm$ shadow A(0:1,1:1)

      tname='ACR2105'
      allocate (A(N,M), C(N,M))  
      
      do iloop=0,2	  
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL
      do i=2,N-1
        do j=2,M-1
         C(i,j) = C(i,j-1)+C(i+1,j)
        enddo
      enddo
!dvm$ actual (nloopi,nloopj)
!dvm$ region

!dvm$ parallel (j,i) on A(i,j)
      do j=1,M
        do i=1,N
         A(i,j) = NL+i+j
        enddo
      enddo
!dvm$ parallel (j,i) on A(i,j),across(A(0:1,1:0)),stage(iloop)
      do j=2,M-1
       do i=2,N-1
         A(i,j) = A(i,j-1)+A(i+1,j)
        enddo
      enddo
  
!dvm$ parallel (j,i) on A(i,j), reduction( min( nloopi),min(nloopj))
      do j=2,M-1
       do i=2,N-1         
          if (A(i,j).ne.C(i,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual (nloopi)
      enddo
	  
      if (nloopi .eq.NL) then
          call ansyes(tname)
          else
          call ansno(tname)
      endif      
      deallocate (A, C) 

      end
       
C -------------------------------------------ACR2106   
         
      subroutine acr2106
      integer,parameter :: N = 16,M=16, NL=1000
      character*7 tname 
      integer,allocatable::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2) :: A 

      tname='ACR2106'
      allocate (A(N,M), C(N,M))  
	        
      do iloop=0,2
      NNL=NL    
      call serial2(C,N,M,NNL)
       do i=3,N-2
        do j=3,M-2
         C(i,j) =C(i+2,j)+C(i,j+2)+C(i+2,j)+C(i-2,j)+C(i,j-2) 
        enddo
      enddo
      nloopi=NL
      nloopj=NL
!dvm$ actual (nloopi,nloopj)
!dvm$ region

!dvm$ parallel (j,i) on A(i,j)
      do j=1,M
        do i=1,N
         A(i,j) = NL+i+j
        enddo
      enddo
          
!dvm$ parallel (j,i) on A(i,j),across(A(2:2,2:2)),stage(iloop)
       do j=3,M-2
        do i=3,N-2
          A(i,j) = A(i+2,j)+A(i,j+2)+A(i+2,j)+A(i-2,j)+A(i,j-2)
        enddo
      enddo
  
!dvm$ parallel (j,i) on A(i,j), reduction( min( nloopi),min(nloopj))
       do j=3,M-2
        do i=3,N-2
          if (A(i,j).ne.c(i,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual (nloopi)
      enddo
	  
      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A, C)
      
      end   
C -------------------------------------------ACR2107   
         
      subroutine acr2107

      integer,parameter :: N = 16,M=16, NL=1000
      integer,allocatable::  A(:,:), C(:,:)
      character*7 tname 
      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2) :: A 

      tname='ACR2107'
      allocate (A(N,M), C(N,M))  
	        
      do iloop=0,2
      NNL=NL    
      call serial2(C,N,M,NNL)
      do i=3,N-2
        do j=3,M-2
         C(i,j) =C(i+2,j)+C(i,j+2)+C(i,j-2)
        enddo
      enddo
      nloopi=NL
      nloopj=NL
!dvm$ actual (nloopi,nloopj)
!dvm$ region

!dvm$ parallel (j,i) on A(i,j)
      do j=1,M
        do i=1,N
         A(i,j) = NL+i+j
        enddo
      enddo

!dvm$ parallel (j,i) on A(i,j),across(A(0:2,2:2)),stage(iloop)
       do j=3,M-2
        do i=3,N-2
         A(i,j) =A(i+2,j)+A(i,j+2)+A(i,j-2)
        enddo
      enddo

!dvm$ parallel (j,i) on A(i,j), reduction( min( nloopi),min(nloopj))
       do j=3,M-2
        do i=3,N-2
          if (A(i,j).ne.C(i,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual (nloopi)
      enddo
	  
      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A, C)      
 
      end   
C -------------------------------------------ACR2108  
         
      subroutine acr2108
      integer,parameter :: N = 16,M=16, NL=1000
      character*7 tname 
      integer,allocatable::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2) :: A 

      tname='ACR2108'
      allocate (A(N,M), C(N,M))  
	        
      do iloop=0,2
      NNL=NL    
      call serial2(C,N,M,NNL)
      do i=3,N-2
       do j=3,M-2
         C(i,j) =C(i-1,j)+C(i,j-1)+C(i-2,j)+C(i+2,j)
        enddo
      enddo
      nloopi=NL
      nloopj=NL
!dvm$ actual (nloopi,nloopj)
!dvm$ region

!dvm$ parallel (j,i) on A(i,j)
      do j=1,M
        do i=1,N
         A(i,j) = NL+i+j
        enddo
      enddo
          
!dvm$ parallel (j,i) on A(i,j),across(A(2:2,2:0)),stage(iloop)
       do j=3,M-2
        do i=3,N-2
         A(i,j) = A(i-1,j)+A(i,j-1)+A(i-2,j)+A(i+2,j)
        enddo
      enddo
  
!dvm$ parallel (j,i) on A(i,j), reduction( min( nloopi),min(nloopj))
       do j=3,M-2
        do i=3,N-2   
          if (A(i,j).ne.c(i,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual (nloopi)
      enddo
	  
      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A, C)

      end          
C -------------------------------------------ACR2109   
         
      subroutine acr2109
      integer,parameter :: N = 16,M=16, NL=1000
      integer,allocatable::  A(:,:), C(:,:)
      character*7 tname 
      integer nloopi,nloopj
!dvm$ distribute A(BLOCK,BLOCK)   
!dvm$ shadow(2:2,0:2) :: A 

      tname='ACR2109'
      allocate (A(N,M), C(N,M))     
      
      do iloop=0,2	  
      NNL=NL    
      call serial2(C,N,M,NNL)
      do i=3,N-2
        do j=3,M-2
         C(i,j) =C(i,j+2)+ C(i+1,j)+C(i+2,j)
        enddo
      enddo

      nloopi=NL
      nloopj=NL
!dvm$ actual (nloopi,nloopj)
!dvm$ region
!dvm$ parallel (j,i) on A(i,j)
      do j=1,M
        do i=1,N
         A(i,j) = NL+i+j
        enddo
      enddo
          
!dvm$ parallel (j,i) on A(i,j),across(A(2:2,0:2)),stage(iloop)
      do j=3,M-2
        do i=3,N-2
         A(i,j) = A(i,j+2)+ A(i+1,j)+A(i+2,j)
        enddo
      enddo
  
!dvm$ parallel (j,i) on A(i,j), reduction( min( nloopi),min(nloopj))
      do j=3,M-2
       do i=3,N-2      
          if (A(i,j).ne.c(i,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual (nloopi)
      enddo
	  
      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A, C)
      end  
C -------------------------------------------ACR2110   
         
      subroutine acr2110
      integer,parameter :: N = 16,M=16, NL=1000
      integer,allocatable::  A(:,:), C(:,:)
      character*7 tname 
      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,BLOCK)   
!dvm$ shadow(3:3,3:3) :: A 

      tname='ACR2110'
      allocate (A(N,M), C(N,M)) 
      
      do iloop=0,2	  
      NNL=NL    
      call serial2(C,N,M,NNL)
      do i=4,N-3
        do j=4,M-3
         C(i,j) =C(i+1,j)+C(i,j+2)+C(i+3,j)+C(i,j-3)+
     * C(i-2,j)+C(i,j-1)
        enddo
      enddo
      nloopi=NL
      nloopj=NL
!dvm$ actual (nloopi,nloopj)
!dvm$ region

!dvm$ parallel (j,i) on A(i,j)
      do j=1,M
        do i=1,N
         A(i,j) = NL+i+j
        enddo
      enddo

!dvm$ parallel (j,i) on A(i,j),across(A(3:3,3:3)),stage(iloop)
      do j=4,M-3
        do i=4,N-3
         A(i,j) = A(i+1,j)+A(i,j+2)+A(i+3,j)+A(i,j-3)+
     * A(i-2,j)+A(i,j-1)
        enddo
      enddo
  
!dvm$ parallel (j,i) on A(i,j), reduction( min( nloopi),min(nloopj))
      do j=4,M-3
        do i=4,N-3
          if (A(i,j).ne.c(i,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual (nloopi)
      enddo
	  
      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A, C)      
      end 
C -------------------------------------------ACR2111   
         
      subroutine ACR2111
      integer,parameter :: N = 16,M=16, NL=1000
      integer,allocatable::  A(:,:), C(:,:)
      character*7 tname 
      integer nloopi,nloopj
!dvm$ distribute A(BLOCK,BLOCK)   
!dvm$ shadow(3:3,0:3) :: A 

      tname='ACR2111'
      allocate (A(N,M), C(N,M)) 
	        
      do iloop=0,2
      NNL=NL    
      call serial2(C,N,M,NNL)
      do i=3,N-2
        do j=3,M-2
         C(i,j) =C(i,j)+C(i,j+1)
        enddo
      enddo
      nloopi=NL
      nloopj=NL
!dvm$ actual (nloopi,nloopj)
!dvm$ region

!dvm$ parallel (j,i) on A(i,j)
      do j=1,M
        do i=1,N
         A(i,j) = NL+i+j
        enddo
      enddo

!dvm$ parallel (j,i) on A(i,j),across(A(0:0,0:1)),stage(iloop)
      do j=3,M-2
        do i=3,N-2
          A(i,j) = A(i,j)+A(i,j+1)
        enddo
      enddo
  
!dvm$ parallel (j,i) on A(i,j), reduction( min( nloopi),min(nloopj))
      do j=3,M-2
        do i=3,N-2
          if (A(i,j).ne.c(i,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual (nloopi)
      enddo
	  
      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A, C)      
      end 
C --------------------------------------------ACR2112  
         
      subroutine acr2112
      integer,parameter :: N = 16,M=16, NL=1000
      character*7 tname 
      integer,allocatable::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,BLOCK)   
!dvm$ shadow(0:3,3:3) :: A 

      tname='ACR2112'
      allocate (A(N,M), C(N,M))  
	        
      do iloop=0,2
      NNL=NL    
      call serial2(C,N,M,NNL)
      do i=3,N-2
        do j=3,M-2
         C(i,j) =C(i,j)+C(i+1,j)
        enddo
      enddo
      nloopi=NL
      nloopj=NL
!dvm$ actual (nloopi,nloopj)
!dvm$ region

!dvm$ parallel (j,i) on A(i,j)
      do j=1,M
        do i=1,N
         A(i,j) = NL+i+j
        enddo
      enddo

!dvm$ parallel (j,i) on A(i,j),across(A(0:1,0:0)),stage(iloop)
      do j=3,M-2
        do i=3,N-2
          A(i,j) = A(i,j)+A(i+1,j)
        enddo
      enddo

!dvm$ parallel (j,i) on A(i,j), reduction( min( nloopi),min(nloopj))
       do j=3,M-2
        do i=3,N-2       
          if (A(i,j).ne.c(i,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual (nloopi)
      enddo
	  
      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A, C)

      end
C --------------------------------------------ACR2113  
         
      subroutine acr2113
      integer,parameter :: N = 16,M=16, NL=1000
      integer,allocatable::  A(:,:), C(:,:)
      character*7 tname 
      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,BLOCK)   
!dvm$ shadow(3:3,3:0) :: A 

      tname='ACR2113'
      allocate (A(N,M), C(N,M))  
      
      do iloop=0,2	  
      NNL=NL    
      call serial2(C,N,M,NNL)
      do i=4,N-3
        do j=4,M-3
         C(i,j) =C(i,j-3)+C(i+3,j)+C(i-3,j)
        enddo
      enddo
      nloopi=NL
      nloopj=NL
!dvm$ actual (nloopi,nloopj)
!dvm$ region

!dvm$ parallel (j,i) on A(i,j)
      do j=1,M
        do i=1,N
         A(i,j) = NL+i+j
        enddo
      enddo

!dvm$ parallel (j,i) on A(i,j),across(A(3:3,3:0)),stage(iloop)
      do j=4,M-3
        do i=4,N-3
         A(i,j) = A(i,j-3)+A(i+3,j)+A(i-3,j)
        enddo
      enddo

!dvm$ parallel (j,i) on A(i,j), reduction( min( nloopi),min(nloopj))
      do j=4,M-3
       do i=4,N-3    
          if (A(i,j).ne.C(i,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual (nloopi)
      enddo
	  
      if (nloopi .eq.NL) then
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (A, C)

      end
C --------------------------------------------ACR2114  
         
      subroutine acr2114
      integer,parameter :: N = 16,M=16, NL=1000
      integer,allocatable::  A(:,:), C(:,:)
      character*7 tname 
      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,BLOCK)   
!dvm$ shadow(3:0,3:3) :: A 

      tname='ACR2114'
      allocate (A(N,M), C(N,M)) 
      
      do iloop=0,2	  
      NNL=NL    
      call serial2(C,N,M,NNL)
      do i=4,N-3
        do j=4,M-3
         C(i,j) =C(i-3,j)+C(i,j+3)
        enddo
      enddo
      nloopi=NL
      nloopj=NL
!dvm$ actual (nloopi,nloopj)
!dvm$ region

!dvm$ parallel (j,i) on A(i,j)
      do j=1,M
        do i=1,N
         A(i,j) = NL+i+j
        enddo
      enddo

!dvm$ parallel (j,i) on A(i,j),across(A(3:0,3:3)),stage(iloop)
      do j=4,M-3
       do i=4,N-3
         A(i,j) = A(i-3,j)+A(i,j+3)
        enddo
      enddo
  
!dvm$ parallel (j,i) on A(i,j), reduction( min( nloopi),min(nloopj))
      do j=4,M-3
       do i=4,N-3       
          if (A(i,j).ne.c(i,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual (nloopi)
      enddo
	  
      if (nloopi .eq.NL) then
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (A, C)

      end
C --------------------------------------------ACR2115  
         
      subroutine acr2115
      integer,parameter :: N = 59,M=59, NL=1000
      character*7 tname 
      integer,allocatable::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,BLOCK)   
!dvm$ shadow(11:11,11:11) :: A 

      tname='ACR2115'
      allocate (A(N,M), C(N,M))   
      
      do iloop=0,2	  
      NNL=NL    
      call serial2(C,N,M,NNL)
      do i=12,N-11
        do j=12,M-11
         C(i,j) =C(i+11,j)+C(i,j+10)+C(i+9,j)+
     *C(i,j-11)+C(i-10,j)+C(i,j-9)
        enddo
      enddo
      nloopi=NL
      nloopj=NL
!dvm$ actual (nloopi,nloopj)
!dvm$ region

!dvm$ parallel (j,i) on A(i,j)
      do j=1,M
        do i=1,N
         A(i,j) = NL+i+j
        enddo
      enddo

!dvm$ parallel (j,i) on A(i,j),across(A(11:11,11:11)),stage(iloop)
      do j=12,M-11
        do i=12,N-11
         A(i,j) = A(i+11,j)+A(i,j+10)+A(i+9,j)+
     *A(i,j-11)+A(i-10,j)+A(i,j-9)
        enddo
      enddo
  
!dvm$ parallel (j,i) on A(i,j), reduction( min( nloopi),min(nloopj))
      do j=12,M-11
        do i=12,N-11    
          if (A(i,j).ne.C(i,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual (nloopi)
      enddo
	  
      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A, C)

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
      character*7 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*7 name
      print *,name,'  -  ***error'
      end