      program REM21
     
c    TESTING OF THE REMOTE_ACCESS DIRECTIVE AND THE REMOTE_ACCESS CLAUSE'.       
c    DISTRIBUTED ARRAY A(N,M) OR ELEMENTS OF THIS ARRAY ARE REPLICATED
c    ON ALL PROCESSORS. 

      print *,'===START OF REM21========================'
C --------------------------------------------------
      call rem2101
C --------------------------------------------------
      call rem2102
C --------------------------------------------------
      call rem2103
C -------------------------------------------------
      call rem2104
C -------------------------------------------------
      call rem2105
C -------------------------------------------------
      call rem2106
C --------------------------------------------------
      call rem2107
C --------------------------------------------------
      call rem2108
C --------------------------------------------------
      call rem2109
C -------------------------------------------------
      call rem2110
C -------------------------------------------------
      call rem2111
C -------------------------------------------------
      call rem2112
C ------------------------------------------------- 
      call rem2113
C ------------------------------------------------- 
      call rem2114
C ------------------------------------------------- 
      call rem2115
C ----------------------------------------------- 
      call rem2116
C ----------------------------------------------- 
      call rem2117
C ------------------------------------------------- 
      call rem2118
C ------------------------------------------------ 
      call rem2119
C ------------------------------------------------- 
      call rem2120
C ------------------------------------------------- 

C
      print *,'=== END OF REM21 ========================= '    
      end
C ---------------------------------------------------------REM2101

      subroutine REM2101     
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute B(BLOCK,BLOCK)    
!dvm$ align :: A 

      tname='REM2101'
      allocate(B(N,M),A(N,M),C(N,M))
!dvm$ realign A(i,j) with B(i,j)
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

C ------------------------------------------------------REM2102
      subroutine REM2102
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)     
!dvm$ align :: B 

      tname='REM2102'
      allocate(A(N,M),B(N,M),C(N,M))
!dvm$ realign B(i,j) with A(i,j)
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

C ------------------------------------------------------REM2103
      subroutine REM2103
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)    
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2103'
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

C ------------------------------------------------------REM2104
      subroutine REM2104
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)     
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2104'
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

C ------------------------------------------------------REM2105
      subroutine REM2105
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)     
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2105'
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

C ------------------------------------------------------REM2106
      subroutine REM2106     
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)     
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2106'
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

C ------------------------------------------------------REM2107
      subroutine REM2107
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)    
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2107'
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
C -----------------------------------------------------REM2108
      subroutine REM2108
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)    
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2108'
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
C ------------------------------------------------------REM2109
      subroutine REM2109
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)    
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2109'
      allocate(A(N,M),B(N,M),C(N,M),D(N,M))
      isumc=0
      isuma=0
      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopi=NL
      nloopj=NL

!dvm$ region out (A)
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
C ------------------------------------------------------REM2110
      subroutine REM2110
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)   
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2110'
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

C ------------------------------------------------------REM2111
      subroutine REM2111
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj,isumc,isuma 
      character*7 tname
      integer ki           
!dvm$ distribute A(BLOCK,BLOCK)    
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2111'
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
C ------------------------------------------------------REM2112
      subroutine REM2112
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)    
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2112'
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
C ------------------------------------------------------REM2113
      subroutine REM2113
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)    
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2113'
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

C -----------------------------------------------------REM2114
      subroutine REM2114
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)    
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2114'
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

C -----------------------------------------------------REM2115
      subroutine REM2115
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)    
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2115'
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

C -----------------------------------------------------REM2116
      subroutine REM2116
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)    
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2116'
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

C -----------------------------------------------------REM2117
      subroutine REM2117
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)    
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2117'
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
C -----------------------------------------------------REM2118
      subroutine REM2118
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)    
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2118'
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

C ----------------------------------------------------REM2119
      subroutine REM2119
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)     
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2119'
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
C ------------------------------------------------------REM2120
      subroutine REM2120
      integer, parameter :: N = 16,M=8,NL=1000
      integer, allocatable :: A(:,:),B(:,:),C(:,:),D(:,:)
      integer nloopi,nloopj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK)     
!dvm$ align(i,j) with A(i,j) :: B 

      tname='REM2120'
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
