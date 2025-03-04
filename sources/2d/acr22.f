       program ACR22
     
c    TESTING OF THE ACROSS CLAUSE'.       
c    DISTRIBUTED ARRAY A(N,M) IS TO HAVE DIFFERENT 
c    FLOW-DEP-LENGTH ON BOTH SIDES 

      print *,'===START OF ACR22========================'
C --------------------------------------------------
      call acr2201
C --------------------------------------------------
      call acr2202
C --------------------------------------------------
      call acr2203
C -------------------------------------------------
      call acr2204
C -------------------------------------------------
      call acr2205
C -------------------------------------------------
      call acr2206
C --------------------------------------------------
      call acr2207
C --------------------------------------------------
      call acr2208
C --------------------------------------------------
      call acr2209
C -------------------------------------------------
      call acr2210
C -------------------------------------------------
      call acr2211
C -------------------------------------------------
      call acr2212
C -------------------------------------------------
      call acr2213
C -------------------------------------------------
      call acr2214
C -------------------------------------------------
      call acr2215
C -------------------------------------------------
      print *,'=== END OF ACR22 ========================= '    
      end
C ---------------------------------------------ACR2201
      subroutine ACR2201
     
      integer, parameter :: N = 16,M=16, NL=1000

     
      character*7 tname 
      integer, allocatable ::  A(:,:), C(:,:)
      integer nloopi,nloopj
                      
!dvm$ distribute A(*,BLOCK)    
      tname='ACR2201'
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
!dvm$ region inout (C),out (A) 

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
!dvm$ get_actual (nloopi)
      enddo
	   
      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A, C)
      
      end
C ---------------------------------------------ACR2202     
      subroutine ACR2202
        integer, parameter :: N = 16,M=16, NL=1000
      character*7 tname 
      integer, allocatable ::  A(:,:), C(:,:)
      integer nloopi,nloopj                      
!dvm$ distribute A(BLOCK,*)    
      tname='ACR2202'
	  
      do iloop=0,2	  
      allocate (A(N,M), C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      do i=2,N-1
        do j=2,M-1
         C(i,j) = C(i+1,j)
        enddo
      enddo
      nloopi=NL
      nloopj=NL
!dvm$ actual (nloopi,nloopj,C)
!dvm$ region inout (C),out (A) 


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
      deallocate (A, C)
      enddo
	  
      if (nloopi .eq.NL) then
       call ansyes(tname)
      else
       call ansno(tname)
      endif                                                 
               
      end

C -----------------------------------------ACR2203      
      subroutine acr2203
      integer, parameter :: N = 16,M=16, NL=1000
      character*7 tname 
      integer, allocatable ::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(*,BLOCK)   
      tname='ACR2203'
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
!dvm$ actual (nloopi,nloopj,C)
!dvm$ region in (C),out (A) 

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
!dvm$ get_actual (nloopi,nloopj)
      enddo
	  
      if (nloopi .eq.NL) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      deallocate (A, C)
    
      end

C ------------------------------------------ACR2204   
      subroutine acr2204
      integer, parameter :: N = 16,M=16, NL=1000
      character*7 tname 
      integer, allocatable ::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,*)   
!dvm$ shadow A(1:1,0:1)
      tname='ACR2204'
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
!dvm$ region in (C) 


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
    
C ------------------------------------------ACR2205   
      subroutine acr2205
   
      integer, parameter :: N = 16,M=16, NL=1000

      
      character*7 tname 
      integer, allocatable ::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(*,BLOCK)   
!dvm$ shadow A(0:1,1:1)

      tname='ACR2205'
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
!dvm$ actual (nloopi,nloopj,C)
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
       
C -------------------------------------------ACR2206   
         
      subroutine acr2206
      integer, parameter :: N = 16,M=16, NL=1000     
      character*7 tname 
      integer, allocatable ::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,*)   
!dvm$ shadow(2:2,2:2) :: A 
      tname='ACR2206'
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
!dvm$ actual (nloopi,nloopj,C)
!dvm$ region inout (C),out (A) 


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
C -------------------------------------------ACR2207   
         
      subroutine acr2207
      integer, parameter :: N = 16,M=16, NL=1000
      character*7 tname 
      integer, allocatable ::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(*,BLOCK)   
!dvm$ shadow(2:2,2:2) :: A 
      tname='ACR2207'
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
!dvm$ actual (nloopi)
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
C -------------------------------------------ACR2208  
         
      subroutine acr2208
      integer, parameter :: N = 16,M=16, NL=1000
      character*7 tname 
      integer, allocatable ::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,*)   
!dvm$ shadow(2:2,2:2) :: A 
      tname='ACR2208'
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
!dvm$ actual (nloopi,nloopj,C)
!dvm$ region inout (C),out (A) 

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
C -------------------------------------------ACR2209   
         
      subroutine acr2209
      integer, parameter :: N = 16,M=16, NL=1000
      character*7 tname 
      integer, allocatable ::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(*,BLOCK)   
!dvm$ shadow(2:2,0:2) :: A 

      tname='ACR2209'
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
!dvm$ actual (nloopi)
!dvm$ region inout (C) 


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
C -------------------------------------------ACR2210   
         
      subroutine acr2210
      integer, parameter :: N = 16,M=16, NL=1000
      
      character*7 tname 
      integer, allocatable ::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,*)   
!dvm$ shadow(3:3,3:3) :: A 

      tname='ACR2210'
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

!dvm$ actual (nloopi,nloopj,C)
!dvm$ region inout (C),out (A) 


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
      do j = 4,M-3
       do i= 4,N-3
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
C -------------------------------------------ACR2211   
         
      subroutine ACR2211
      integer, parameter :: N = 16,M=16, NL=1000
      
      character*7 tname 
      integer, allocatable ::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(*,BLOCK)   
!dvm$ shadow(3:3,0:3) :: A 

      tname='ACR2211'
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

!dvm$ actual (nloopi)
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
C --------------------------------------------ACR2212  
         
      subroutine acr2212
      integer, parameter :: N = 16,M=16, NL=1000
      character*7 tname 
      integer, allocatable ::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,*)   
!dvm$ shadow(0:3,3:3) :: A 

      tname='ACR2212'
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

!dvm$ actual (nloopi,nloopj,C)
!dvm$ region inout (C) 


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
C --------------------------------------------ACR2213  
         
      subroutine acr2213
      integer, parameter :: N = 16,M=16, NL=1000
      
      character*7 tname 
      integer, allocatable ::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(*,BLOCK)   
!dvm$ shadow(3:3,3:0) :: A 

      tname='ACR2213'
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

!dvm$ actual (nloopi,nloopj,C)
!dvm$ region inout (C),out (A) 


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
C --------------------------------------------ACR2214  
         
      subroutine acr2214
      integer, parameter :: N = 16,M=16, NL=1000      
      character*7 tname 
      integer, allocatable ::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(BLOCK,*)   
!dvm$ shadow(3:0,3:3) :: A 

      tname='ACR2214'
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
C --------------------------------------------ACR2215  
         
      subroutine acr2215
      integer, parameter :: N = 58,M=58, NL=1000
      
      character*7 tname 
      integer, allocatable ::  A(:,:), C(:,:)
      integer nloopi,nloopj 
!dvm$ distribute A(*,BLOCK)   
!dvm$ shadow(11:11,11:11) :: A 
      tname='ACR2215'
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

!dvm$ actual (nloopi,nloopj,C)
!dvm$ region inout (C),out (A) 


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