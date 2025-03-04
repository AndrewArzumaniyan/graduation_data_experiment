      program ACR31

c    TESTING OF THE ACROSS CLAUSE'.       
c    DISTRIBUTED ARRAY A(N,M,K) IS TO HAVE DIFFERENT 
c    FLOW-DEP-LENGTH ON BOTH SIDES      

      print *,'===START OF ACR31========================'
C --------------------------------------------------
      call acr3101
C --------------------------------------------------
      call acr3102
C --------------------------------------------------
      call acr3103
C -------------------------------------------------
      call acr3104
C -------------------------------------------------
      call acr3105
C -------------------------------------------------
      call acr3106
C --------------------------------------------------
      call acr3107
C --------------------------------------------------
      call acr3108
C----------------------------------------------------
      call acr3109
C----------------------------------------------------

C
C
      print *,'=== END OF ACR31 ========================= '    
      end
C ---------------------------------------------ACR3101
      subroutine acr3101
     
      integer, parameter :: N = 16,M=8,K=8, NL=1000
      integer,allocatable :: A(:,:,:), C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*7 tname 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK)       
      tname='ACR3101'     
      allocate (A(N,M,K), C(N,M,K))
      
      do iloop=0,2	 	  
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      do i=2,N-1
       do j=2,M-1
         do ii=2,K-1
         C(i,j,ii) = C(i+1,j,ii)+C(i,j+1,ii)+C(i,j,ii+1)+C(i-1,j,ii)+
     *C(i,j-1,ii)+ C(i,j,ii-1)
         enddo
       enddo
      enddo
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual (nloopi,nloopj,nloopii,C)
!dvm$ region inout (C),out (A) 


!dvm$ parallel (ii,j,i) on A(i,j,ii)
      do ii=1,K
        do j=1,M
         do i=1,N
          A(i,j,ii) = NL+i+j+ii
         enddo
        enddo
      enddo                                              
!dvm$ parallel (ii,j,i) on A(i,j,ii),across(A(1:1,1:1,1:1)),
!dvm$*stage(iloop)
      do ii=2,K-1
       do j=2,M-1
         do i=2,N-1
          A(i,j,ii) = A(i+1,j,ii)+A(i,j+1,ii)+A(i,j,ii+1)+A(i-1,j,ii)+
     *A(i,j-1,ii)+ A(i,j,ii-1)
         enddo
       enddo
      enddo
  
!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do ii=2,K-1
       do j=2,M-1
        do i=2,N-1       
          if (A(i,j,ii).ne.C(i,j,ii)) then         
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
        enddo
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
C ---------------------------------------------ACR3102     
      subroutine acr3102
     
      integer, parameter :: N = 16,M=10,K=10, NL=1000
      integer,allocatable :: A(:,:,:), C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*7 tname 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2,2:2) :: A     
      tname='ACR3102'     
      allocate (A(N,M,K), C(N,M,K))
      
      do iloop=0,2	 	  
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      do i=3,N-2
       do j=3,M-2
        do ii=3,K-2
         C(i,j,ii)=C(i+2,j,ii)    +C(i,j-2,ii)  +
     *            C(i,j,ii-1)      +C(i-1,j,ii) +
     *            C(i+1,j,ii)      +C(i,j-1,ii) +
     *            C(i,j+2,ii)      +C(i,j,ii+2)  

        enddo
       enddo
      enddo
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual (nloopi,nloopj,nloopii,C)
!dvm$ region in (C) 

!dvm$ parallel (ii,j,i) on A(i,j,ii)
      do ii=1,K
        do j=1,M
         do i=1,N
          A(i,j,ii) = NL+i+j+ii
         enddo
        enddo
      enddo                                             
 
!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*across(A(1:2,2:2,1:2)),
!dvm$*stage(iloop)
      do ii=3,K-2
       do j=3,M-2
         do i=3,N-2
          A(i,j,ii)=A(i+2,j,ii)    +A(i,j-2,ii)  +
     *            A(i,j,ii-1)      +A(i-1,j,ii) +
     *            A(i+1,j,ii)      +A(i,j-1,ii) +
     *            A(i,j+2,ii)      +A(i,j,ii+2)  
         enddo
       enddo
      enddo
  
!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do ii=3,K-2
       do j=3,M-2
        do i=3,N-2       
          if (A(i,j,ii).ne.C(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)         
          endif
        enddo
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
C ---------------------------------------------ACR3103     
      subroutine acr3103     
      integer, parameter :: N = 16,M=10,K=10, NL=1000      
      integer,allocatable :: A(:,:,:), C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*7 tname 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2,2:2) :: A     
      tname='ACR3103'     
      allocate (A(N,M,K), C(N,M,K))
      
      do iloop=0,2	 	  
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      do i=3,N-2
       do j=3,M-2
        do ii=3,K-2
         C(i,j,ii) =C(i+2,j,ii)+C(i,j+2,ii)+C(i,j,ii+2)+
     *              C(i,j-2,ii)+ C(i,j-1,ii)+C(i+1,j,ii)+
     *              C(i,j+1,ii)+C(i,j,ii+1)
        enddo
       enddo
      enddo
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual (nloopi,nloopj,nloopii,C)
!dvm$ region inout (C),out (A) 


!dvm$ parallel (ii,j,i) on A(i,j,ii)
      do ii=1,K
        do j=1,M
         do i=1,N
          A(i,j,ii) = NL+i+j+ii
         enddo
        enddo
      enddo                                              
 
!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*across(A(0:2,2:2,0:2)),
!dvm$*stage(iloop)
      do ii=3,K-2
       do j=3,M-2
         do i=3,N-2
          A(i,j,ii) =A(i+2,j,ii)+A(i,j+2,ii)+A(i,j,ii+2)+
     *              A(i,j-2,ii)+ A(i,j-1,ii)+A(i+1,j,ii)+
     *              A(i,j+1,ii)+A(i,j,ii+1)
         enddo 
       enddo
      enddo
  
!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do ii=3,K-2
       do j=3,M-2
        do i=3,N-2       
          if (A(i,j,ii).ne.C(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)         
          endif
        enddo
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
C ---------------------------------------------ACR3104     
      subroutine acr3104     
      integer, parameter :: N = 16,M=10,K=10, NL=1000
      integer,allocatable :: A(:,:,:), C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*7 tname 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(2:2,2:2,2:2) :: A     
      tname='ACR3104'     
      allocate (A(N,M,K), C(N,M,K))
	        
      do iloop=0,2	 
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      do i=3,N-2
       do j=3,M-2
        do ii=3,K-2
         C(i,j,ii) =C(i+2,j,ii)+C(i,j,ii-2)+
     *C(i-2,j,ii)+ C(i,j-2,ii)+C(i-1,j,ii)+C(i,j-1,ii)+
     *C(i,j,ii-1)+C(i+1,j,ii)
        enddo
       enddo
      enddo
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual (nloopi,nloopj,nloopii)
!dvm$ region  


!dvm$ parallel (ii,j,i) on A(i,j,ii)
      do ii=1,K
        do j=1,M
         do i=1,N
          A(i,j,ii) = NL+i+j+ii
         enddo
        enddo
      enddo                                              
 
!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*across(A(2:2,2:0,2:0)),
!dvm$*stage(iloop)
      do ii=3,K-2
       do j=3,M-2
         do i=3,N-2
          A(i,j,ii) =A(i+2,j,ii)+A(i,j,ii-2)+
     *A(i-2,j,ii)+ A(i,j-2,ii)+A(i-1,j,ii)+A(i,j-1,ii)+
     *A(i,j,ii-1)+A(i+1,j,ii)
         enddo 
       enddo
      enddo
  
!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do ii=3,K-2
       do j=3,M-2
        do i=3,N-2       
          if (A(i,j,ii).ne.C(i,j,ii)) then         
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
        enddo
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
C ---------------------------------------------ACR3105     
      subroutine acr3105
      integer, parameter :: N = 16,M=10,K=10, NL=1000
      integer,allocatable :: A(:,:,:), C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*7 tname 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(0:2,2:2,0:2) :: A     
      tname='ACR3105'     
      allocate (A(N,M,K), C(N,M,K))
      
      do iloop=0,2	 	  
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      do i=3,N-2
       do j=3,M-2
        do ii=3,K-2
         C(i,j,ii) =C(i+2,j,ii)+C(i,j+2,ii)+C(i,j,ii+2)+
     *   C(i,j-2,ii)+C(i,j-1,ii)+C(i+1,j,ii)+C(i,j+1,ii)+
     *   C(i,j,ii+1)
        enddo
       enddo
      enddo
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual (nloopi,nloopj,nloopii,C)
!dvm$ region in (C) 


!dvm$ parallel (ii,j,i) on A(i,j,ii)
      do ii=1,K
        do j=1,M
         do i=1,N
          A(i,j,ii) = NL+i+j+ii
         enddo
        enddo
      enddo                                              
 
!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*across(A(0:2,2:2,0:2)),
!dvm$*stage(iloop)
      do ii=3,K-2
       do j=3,M-2
         do i=3,N-2
         A(i,j,ii) =A(i+2,j,ii)+A(i,j+2,ii)+A(i,j,ii+2)+
     *   A(i,j-2,ii)+A(i,j-1,ii)+A(i+1,j,ii)+A(i,j+1,ii)+
     *   A(i,j,ii+1)
         enddo 
       enddo
      enddo
  
!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do ii=3,K-2
       do j=3,M-2
        do i=3,N-2
          if (A(i,j,ii).ne.C(i,j,ii)) then         
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
        enddo
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

C --------------------------------------------ACR3106  
      subroutine acr3106   
      integer, parameter :: N = 16,M=16,K=16, NL=1000
      integer,allocatable :: A(:,:,:), C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*7 tname 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(3:3,3:3,3:3) :: A     
      tname='ACR3106'     
      allocate (A(N,M,K), C(N,M,K))
      
      do iloop=0,2	 	  
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      do i=4,N-3
       do j=4,M-3
         do ii=4,K-3
          C(i,j,ii) = C(i+3,j,ii)+C(i,j+3,ii)+C(i,j,ii+3)+
     *               C(i-3,j,ii)+C(i,j-3,ii)+C(i,j,ii-3)+
     *               C(i+2,j,ii)+C(i,j+2,ii)+C(i,j,ii+2)+
     *               C(i-2,j,ii)+C(i,j-2,ii)+C(i,j,ii-2)+
     *               C(i+1,j,ii)+C(i,j+1,ii)+C(i,j,ii+1)+
     *               C(i-1,j,ii)+C(i,j-1,ii)+C(i,j,ii-1)
         enddo 
       enddo
      enddo
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual (nloopi,nloopj,nloopii,C)
!dvm$ region inout (C),out (A) 


!dvm$ parallel (ii,j,i) on A(i,j,ii)
       do ii=1,K
        do j=1,M
         do i=1,N
          A(i,j,ii) = NL+i+j+ii
         enddo
        enddo
      enddo                                              
 
!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*across(A(3:3,3:3,3:3)),
!dvm$*stage(iloop)
      do ii=4,K-3
       do j=4,M-3
         do i=4,N-3
          A(i,j,ii) = A(i+3,j,ii)+A(i,j+3,ii)+A(i,j,ii+3)+
     *               A(i-3,j,ii)+A(i,j-3,ii)+A(i,j,ii-3)+
     *               A(i+2,j,ii)+A(i,j+2,ii)+A(i,j,ii+2)+
     *               A(i-2,j,ii)+A(i,j-2,ii)+A(i,j,ii-2)+
     *               A(i+1,j,ii)+A(i,j+1,ii)+A(i,j,ii+1)+
     *               A(i-1,j,ii)+A(i,j-1,ii)+A(i,j,ii-1)
         enddo 
       enddo
      enddo
  
!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do ii=4,K-3
       do j=4,M-3
        do i=4,N-3    
          if (A(i,j,ii).ne.C(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)         
          endif
        enddo
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
      deallocate(A,C)
      end
           
         
C --------------------------------------------ACR3107  
      subroutine acr3107   
      integer, parameter :: N = 16,M=16,K=16, NL=1000
      integer,allocatable :: A(:,:,:), C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*7 tname 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(3:3,0:3,3:0) :: A     
      tname='ACR3107'     
      allocate (A(N,M,K), C(N,M,K))
      
      do iloop=0,2	 	  
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      do i=4,N-3
       do j=4,M-3
         do ii=4,K-3
          C(i,j,ii) = C(i+3,j,ii)+C(i,j+3,ii)+C(i-3,j,ii)+
     *               C(i,j,ii-3)+C(i+2,j,ii)+C(i,j+2,ii)+
     *               C(i-2,j,ii)+C(i,j,ii-2)+
     *               C(i+1,j,ii)+C(i,j+1,ii)+C(i+1,j,ii)+
     *               C(i,j+1,ii)+C(i-1,j,ii)+C(i,j,ii-1) 
         enddo 
       enddo
      enddo

      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual (nloopi,nloopj,nloopii,C)
!dvm$ region inout (C),out (A) 


!dvm$ parallel (ii,j,i) on A(i,j,ii)
      do ii=1,K
        do j=1,M
         do i=1,N
          A(i,j,ii) = NL+i+j+ii
         enddo
        enddo
      enddo                                              
 
!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*across(A(3:3,0:3,3:0)),
!dvm$*stage(iloop)
      do ii=4,K-3
       do j=4,M-3
         do i=4,N-3
         A(i,j,ii) = A(i+3,j,ii)+A(i,j+3,ii)+A(i-3,j,ii)+
     *               A(i,j,ii-3)+A(i+2,j,ii)+A(i,j+2,ii)+
     *               a(i-2,j,ii)+A(i,j,ii-2)+
     *               A(i+1,j,ii)+A(i,j+1,ii)+A(i+1,j,ii)+
     *               A(i,j+1,ii)+A(i-1,j,ii)+A(i,j,ii-1)   
        enddo
       enddo
      enddo
!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do ii=4,K-3
       do j=4,M-3
        do i=4,N-3    
          if (A(i,j,ii).ne.C(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)         
          endif
        enddo
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
                    
C --------------------------------------------ACR3108  
      subroutine acr3108   
      integer, parameter :: N = 16,M=16,K=16, NL=1000
      integer,allocatable :: A(:,:,:), C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*7 tname 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(0:3,0:3,0:3) :: A     
      tname='ACR3108'     
      allocate (A(N,M,K), C(N,M,K))
      
      do iloop=0,2	 	  
      NNL=NL    
      call serial3(C,N,M,K,NNL)

      do i=1,N-3
       do j=1,M-3
         do ii=1,K-3
          C(i,j,ii) = C(i+3,j,ii)+C(i,j+3,ii)+C(i,j,ii+3)+
     *               C(i+2,j,ii)+C(i,j+2,ii)+C(i,j,ii+2)+
     *               C(i+1,j,ii)+C(i,j+1,ii)+C(i,j,ii+1) 
         enddo 
       enddo
      enddo

      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual (nloopi,nloopj,nloopii,C)
!dvm$ region  


!dvm$ parallel (ii,j,i) on A(i,j,ii)
      do ii=1,K
        do j=1,M
         do i=1,N
          A(i,j,ii) = NL+i+j+ii
         enddo
        enddo
      enddo                                              
 
!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*across(A(0:3,0:3,0:3)),
!dvm$*stage(iloop)
      do ii=1,K-3
       do j=1,M-3
         do i=1,N-3
          A(i,j,ii) = A(i+3,j,ii)+A(i,j+3,ii)+A(i,j,ii+3)+
     *               A(i+2,j,ii)+A(i,j+2,ii)+A(i,j,ii+2)+
     *               A(i+1,j,ii)+A(i,j+1,ii)+A(i,j,ii+1)    
         enddo 
       enddo
      enddo
  
!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do ii=4,K-3
       do j=4,M-3
        do i=4,N-3    
          if (A(i,j,ii).ne.C(i,j,ii)) then         
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
        enddo
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
           
C --------------------------------------------ACR3109  
      subroutine acr3109   
      integer, parameter :: N = 58,M=58,K=58, NL=1000      
      integer,allocatable :: A(:,:,:), C(:,:,:)
      integer nloopi,nloopj
      character*7 tname 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK)   
!dvm$ shadow(11:11,11:11,11:11) :: A     
      tname='ACR3109'     
      allocate (A(N,M,K), C(N,M,K))
      
      do iloop=0,2	 	  
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      do i=12,N-11
       do j=12,M-11
         do ii=12,K-11
          C(i,j,ii) = C(i+11,j,ii)+C(i,j+11,ii)+C(i,j,ii+11)+
     *               C(i-11,j,ii)+C(i,j-11,ii)+C(i,j,ii-11)+
     *               C(i+10,j,ii)+C(i,j+10,ii)+C(i,j,ii+10)+
     *               C(i-10,j,ii)+C(i,j-10,ii)+C(i,j,ii-10)+
     *               C(i-9,j,ii) +C(i,j-9,ii) +C(i,j,ii-9)+
     *               C(i+9,j,ii) +C(i,j+9,ii) +C(i,j,ii+9)  
         enddo 
        enddo
      enddo

      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ actual (nloopi,nloopj,nloopii,C)
!dvm$ region inout (C),out (A) 


!dvm$ parallel (ii,j,i) on A(i,j,ii)
      do ii=1,K
        do j=1,M
         do i=1,N
          A(i,j,ii) = NL+i+j+ii
         enddo
        enddo
      enddo

!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*across(A(11:11,11:11,11:11)),
!dvm$*stage(iloop)
      do ii=12,K-11
       do j=12,M-11
         do i=12,N-11
          A(i,j,ii) = A(i+11,j,ii)+A(i,j+11,ii)+A(i,j,ii+11)+
     *               A(i-11,j,ii)+A(i,j-11,ii)+A(i,j,ii-11)+
     *               A(i+10,j,ii)+A(i,j+10,ii)+A(i,j,ii+10)+
     *               A(i-10,j,ii)+A(i,j-10,ii)+A(i,j,ii-10)+
     *               A(i-9,j,ii)+A(i,j-9,ii)+A(i,j,ii-9)+
     *               A(i+9,j,ii)+A(i,j+9,ii)+A(i,j,ii+9)  
         enddo 
       enddo
      enddo
  
!dvm$ parallel (ii,j,i) on A(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do ii=4,K-3
       do j=4,M-3
        do i=4,N-3    
          if (A(i,j,ii).ne.C(i,j,ii)) then         
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
        enddo
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
      character*7 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*7 name
      print *,name,'  -  ***error'
      end