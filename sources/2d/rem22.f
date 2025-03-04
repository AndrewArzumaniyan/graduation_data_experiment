      program REM22
     
c    TESTING OF THE REMOTE_ACCESS DIRECTIVE AND THE REMOTE_ACCESS CLAUSE'.       
c    DISTRIBUTED ARRAY A(N,M) OR ELEMENTS OF THIS ARRAY ARE REPLICATED
c    ON ALL PROCESSORS. 

      print *,'===START OF REM22========================'
C --------------------------------------------------
      call rem2201
C --------------------------------------------------
      call rem2202
C --------------------------------------------------
      call rem2203
C -------------------------------------------------
      call rem2204
C -------------------------------------------------
      call rem2205
C -------------------------------------------------
      call rem2206
C --------------------------------------------------
      call rem2207
C --------------------------------------------------
      call rem2208
C --------------------------------------------------
      call rem2209
C -------------------------------------------------
      call rem2210
C -------------------------------------------------
      call rem2211
C -------------------------------------------------
      call rem2212
C ------------------------------------------------- 
      call rem2213
C ------------------------------------------------- 
      call rem2214
C ------------------------------------------------- 
      call rem2215
C ----------------------------------------------- 
      call rem2216
C ----------------------------------------------- 
      call rem2217
C ------------------------------------------------- 
      call rem2218
C ------------------------------------------------ 
      call rem2219
C ------------------------------------------------- 
      call rem2220
C ------------------------------------------------- 

C
      print *,'=== END OF REM22 ========================= '    
      end
C ---------------------------------------------------------REM2201

      subroutine REM2201     
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute B(*,BLOCK)    
!dvm$ align(:,:) with B(:,:) :: A 

      tname='REM2201'
      allocate(B(N,M),A(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ region out(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                       
!dvm$ end region
!dvm$ get_actual(A(1,1))
!dvm$ remote_access (A(1,1))
      ib=A(1,1)               

      if (ib .eq.C(1,1)) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate(A,B,C)

      end

C ------------------------------------------------------REM2202
      subroutine REM2202
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*)     
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2202'
      allocate(A(N,M),B(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ region out(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                       
!dvm$ end region
!dvm$ get_actual(A(N,M))
!dvm$ remote_access (A(N,M))
      ib=A(N,M)               
      if (ib .eq.C(N,M)) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate(B,A,C)

      end

C ------------------------------------------------------REM2203
      subroutine REM2203
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(*,BLOCK)    
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2203'
      allocate(A(N,M),B(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ region out(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                       
!dvm$ end region
!dvm$ get_actual(A(1,M))
!dvm$ remote_access (A(1,M))
      ib=A(1,M)               

      if (ib .eq.C(1,M)) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate(B,A,C)

      end

C ------------------------------------------------------REM2204
      subroutine REM2204
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*)     
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2204'
      allocate(A(N,M),B(N,M),C(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL
!dvm$ region out(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                       
!dvm$ end region
!dvm$ get_actual(A(N,1))
!dvm$ remote_access (A(N,1))
      ib=A(N,1)               
      if (ib .eq.C(N,1)) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate(B,A,C)

      end

C ------------------------------------------------------REM2205
      subroutine REM2205
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(*,BLOCK)     
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2205'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      isumc=0
      isuma=0
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ region out(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                       
!dvm$ end region
!dvm$ get_actual(A)
      do i=1,N         
        do j=i,M
!dvm$     remote_access (A(:,:))
          D(i,j)=A(i,j)
          isumc=isumc+C(i,j)
          isuma=isuma+D(i,j)
        enddo
      enddo           
      if (isumc .eq.isuma) then     
         call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate(B,A,C,D)
   
      end

C ------------------------------------------------------REM2206
      subroutine REM2206     
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*)     
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2206'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      isumc=0
      isuma=0
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ region out(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                       
!dvm$ end region
!dvm$ get_actual(A(:,1))
      do i=1,N         
!dvm$   remote_access (A(:,1))
        D(i,1)=A(i,1)
        isumc=isumc+C(i,1)
        isuma=isuma+D(i,1)
      enddo           

      if (isumc .eq.isuma) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate(B,A,C,D)
        
      end

C ------------------------------------------------------REM2207
      subroutine REM2207
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(*,BLOCK)    
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2207'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      isumc=0
      isuma=0
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ region out(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                       
!dvm$ end region
!dvm$ get_actual(A(1,:))
      do j=1,M               
!dvm$   remote_access (A(1,:))
        D(1,j)=A(1,j)
        isumc=isumc+C(1,j)
        isuma=isuma+D(1,j)
      enddo           

      if (isumc .eq.isuma) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate(B,A,C,D)   
     
      end
C -----------------------------------------------------REM2208
      subroutine REM2208
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*)    
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2208'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      isumc=0
      isuma=0
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ region out(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                       
!dvm$ end region
!dvm$ get_actual(A(:,M))
      do i=1,N         
!dvm$   remote_access (A(:,M))
        D(i,M)=A(i,M)
        isumc=isumc+C(i,M)
        isuma=isuma+D(i,M)
      enddo           
      if (isumc .eq.isuma) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate(B,A,C,D)
        
      end
C ------------------------------------------------------REM2209
      subroutine REM2209
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(*,BLOCK)    
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2209'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      isumc=0
      isuma=0
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ region out(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                       
!dvm$ end region
!dvm$ get_actual(A(N,:))
      do j=1,M               
!dvm$   remote_access (A(N,:))
        D(N,j)=A(N,j)
        isumc=isumc+C(N,j)
        isuma=isuma+D(N,j)
      enddo

      if (isumc .eq.isuma) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate(B,A,C,D)   
     
      end
C ------------------------------------------------------REM2210
      subroutine REM2210
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*)   
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2210'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      isumc=0
      isuma=0
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ region out(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                       
!dvm$ end region
!dvm$ get_actual(A)
      do i=1,N         
        do j=i,M
!dvm$     remote_access (A(i,j))
          D(i,j)=A(i,j)
          isumc=isumc+C(i,j)
          isuma=isuma+D(i,j)
        enddo
      enddo

      if (isumc .eq.isuma) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif   
      deallocate(B,A,C,D)     

      end

C ------------------------------------------------------REM2211
      subroutine REM2211
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj,isumc,isuma 
      character*7 tname
      integer ki           
!dvm$ distribute A(*,BLOCK)    
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2211'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      isumc=0
      isuma=0
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL
!dvm$ region out(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo
      enddo                                       
!dvm$ end region
!dvm$ get_actual(A)
      ki=2
      ki1=3
      kj=2
      kj1=3        
      do i=1,N/ki-ki1         
        do j=i,M/kj-kj1
!dvm$     remote_access (A(ki*i+ki1,kj*j+kj1))
          D(i,j)=A(ki*i+ki1,kj*j+kj1)
          isumc=isumc+C(ki*i+ki1,kj*j+kj1 )
          isuma=isuma+D(i,j)
        enddo
      enddo

      if (isumc .eq.isuma) then     
         call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate(B,A,C,D)   
     
      end
C ------------------------------------------------------REM2212
      subroutine REM2212
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*)    
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2212'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo 
      enddo 
               
!dvm$ parallel (i,J) on B(i,j),remote_access(A(1,1))
      do i=1,N
        do j=1,M
          B(i,j) = A(1,1)
        enddo
      enddo

!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
      do i=1,N
        do j=1,M
          if (B(i,j).ne.C(1,1)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(nloopi)
     
      if (nloopi .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate(B,A,C,D)   
     
      end
C ------------------------------------------------------REM2213
      subroutine REM2213
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(*,BLOCK)    
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2213'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
           A(i,j) = NL+i+j
        enddo 
      enddo 
               
!dvm$ parallel (i,J) on B(i,j),remote_access(A(N,M))
      do i=1,N
        do j=1,M
          B(i,j) = A(N,M)
        enddo
      enddo

!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
      do i=1,N
        do j=1,M
          if (B(i,j).ne.C(N,M)) then
            nloopi=min(nloopi,i)
            nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(nloopi)
      if (nloopi .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate(B,A,C,D)   
     
      end

C -----------------------------------------------------REM2214
      subroutine REM2214
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*)    
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2214'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo 
      enddo 
               
!dvm$ parallel (i,J) on B(i,j),remote_access(A(1,M))
      do i=1,N
        do j=1,M
          B(i,j) = A(1,M)
        enddo
      enddo 

!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
      do i=1,N
        do j=1,M
          if (B(i,j).ne.C(1,M)) then
            nloopi=min(nloopi,i)
            nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(nloopi)
      if (nloopi .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate(B,A,C,D)   
     
      end

C -----------------------------------------------------REM2215
      subroutine REM2215
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(*,BLOCK)    
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2215'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo 
      enddo 
               
!dvm$ parallel (i,J) on B(i,j),remote_access(A(N,1))
      do i=1,N
        do j=1,M
          B(i,j) = A(N,1)
        enddo
      enddo 

!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
      do i=1,N
        do j=1,M
          if (B(i,j).ne.C(N,1)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(nloopi)
      if (nloopi .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate(B,A,C,D)   
     
      end

C -----------------------------------------------------REM2216
      subroutine REM2216
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*)    
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2216'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo 
      enddo 
               
c !dvm$ parallel (i,J) on A(i,j),remote_access(A)
!dvm$ parallel (i,J) on B(i,j),remote_access(A(:,:))
      do i=1,N
        do j=1,M
          B(i,j) = A(i,j)
        enddo
      enddo 

!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
      do i=1,N
        do j=1,M
          if (B(i,j).ne.C(i,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(nloopi)
      if (nloopi .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate(B,A,C,D)   
     
      end

C -----------------------------------------------------REM2217
      subroutine REM2217
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(*,BLOCK)    
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2217'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo 
      enddo 
               
!dvm$ parallel (i) on B(i,1),remote_access(A(:,1))
      do i=1,N      
        B(i,1) = A(i,1)
      enddo
      
!dvm$ parallel (i) on B(i,1), reduction( min( nloopi),min(nloopj))
      do i=1,N
          if (B(i,1).ne.C(i,1)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
      enddo
!dvm$ end region
!dvm$ get_actual(nloopi)
      if (nloopi .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate(B,A,C,D)   
     
      end
C -----------------------------------------------------REM2218
      subroutine REM2218
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*)    
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2218'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL
!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo 
      enddo 

!dvm$ parallel (i,J) on A(i,j),remote_access(A(1,:))
      do i=1,N
        do j=1,M
          B(i,j) = A(1,j)
        enddo
      enddo 

!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
      do i=1,N
        do j=1,M
          if (B(i,j).ne.C(1,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(nloopi)
      if (nloopi .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate(B,A,C,D)   
     
      end

C ----------------------------------------------------REM2219
      subroutine REM2219
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(*,BLOCK)     
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2219'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo 
      enddo 

!dvm$ parallel (i,J) on B(i,j),remote_access(A(:,M))
      do i=1,N
        do j=1,M
          B(i,j) = A(i,M)
        enddo
      enddo 

!dvm$ parallel (i,j) on B(i,j), reduction( min( nloopi),min(nloopj))
      do i=1,N
        do j=1,M
          if (B(i,j).ne.C(i,M)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(nloopi)
      if (nloopi .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate(B,A,C,D)   
     
      end
C ------------------------------------------------------REM2220
      subroutine REM2220
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*)     
!dvm$ align(:,:) with A(:,:) :: B 

      tname='REM2220'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ region local(A,B)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
        enddo 
      enddo 
               
!dvm$ parallel (i,J) on A(i,j),remote_access(A(N,:))
      do i=1,N
        do j=1,M
          B(i,j) = A(N,j)
        enddo
      enddo 

!dvm$ parallel (i,j) on A(i,j), reduction( min( nloopi),min(nloopj))
      do i=1,N
        do j=1,M
          if (B(i,j).ne.C(N,j)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
          endif
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(nloopi)
      if (nloopi .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif   
      deallocate(B,A,C,D)     

      end

C ---------------------------------------------------------         
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
