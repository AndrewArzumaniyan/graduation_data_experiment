      program REM41
     
c    TESTING OF THE REMOTE_ACCESS DIRECTIVE AND THE REMOTE_ACCESS CLAUSE'.       
c    DISTRIBUTED ARRAY A(N,M,K,L) OR ELEMENTS OF THIS ARRAY ARE REPLICATED
c    ON ALL PROCESSORS. 

      print *,'===START OF REM41========================'
C --------------------------------------------------
      call rem4101
C --------------------------------------------------
      call rem4102
C --------------------------------------------------
      call rem4103
C -------------------------------------------------
      call rem4104
C -------------------------------------------------
      call rem4105
C -------------------------------------------------
      call rem4106
C --------------------------------------------------
      call rem4107
C --------------------------------------------------
      call rem4108
C --------------------------------------------------
      call rem4109
C -------------------------------------------------
      call rem4110
C -------------------------------------------------
      call rem4111
C -------------------------------------------------
      call rem4112
C ------------------------------------------------- 
      call rem4113
C ------------------------------------------------- 
      call rem4114
C ------------------------------------------------- 
C
      print *,'=== END OF REM41 ========================= '    
      end
C ---------------------------------------------------------REM3101
      subroutine REM4101
      integer, parameter :: N = 16,M=8,K=8,L=16,NL=1000
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:),C(:,:,:,:)
      character*7 tname
                 
!dvm$ distribute B(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ align(:,:,:,:) with B(:,:,:,:) :: A 

      tname='REM4101'
      allocate (B(N,M,K,L),A(N,M,K,L),C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)

!dvm$ region out(A)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo 
          enddo
        enddo     
      enddo    
!dvm$ end region
!dvm$ get_actual(A(1,1,1,1))
               
!dvm$ remote_access (A(1,1,1,1))
      ib=A(1,1,1,1)

      if (ib .eq.C(1,1,1,1)) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate (A,B,C)

      end

C ------------------------------------------------------REM4102
      subroutine REM4102     
      integer, parameter :: N = 16,M=8,K=8,L=16,NL=1000
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:),C(:,:,:,:)
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4102'
      allocate (A(N,M,K,L),B(N,M,K,L),C(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)

!dvm$ region out(A)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo     
        enddo    
      enddo 
!dvm$ end region
!dvm$ get_actual(A(N,M,K,L))

!dvm$ remote_access (A(N,M,K,L))
      ib=A(N,M,K,L)               

      if (ib .eq.C(N,M,K,L)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B,A,C)

      end

C ------------------------------------------------------REM4103
      subroutine REM4103
      integer, parameter :: N = 4,M=4,K=4,L=4,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)   
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4103'
      allocate (A(N,M,K,L),B(N,M,K,L),C(N,M,K,L),D(N,M,K,L))
      isumc=0
      isuma=0
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)

!dvm$ region out(A)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo     
        enddo
      enddo   
!dvm$ end region
!dvm$ get_actual(A)
      do i=1,N         
        do j=i,M
          do ii=1,K
            do jj=1,L
!dvm$         remote_access (A(:,:,:,:))
              D(i,j,ii,jj)=A(i,j,ii,jj)
              isumc=isumc+C(i,j,ii,jj)
              isuma=isuma+D(i,j,ii,jj)
            enddo
          enddo
        enddo
      enddo 

      if (isumc .eq.isuma) then     
         call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B,A,C)   
     
      end

C ------------------------------------------------------REM4104
      subroutine REM4104
      integer, parameter :: N = 6,M=8,K=8,L=4,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4104'
      allocate (A(N,M,K,L),B(N,M,K,L),C(N,M,K,L),D(N,M,K,L))
      isumc=0
      isuma=0
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)

!dvm$ region out(A)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo 
          enddo
        enddo                                          
      enddo
!dvm$ end region
!dvm$ get_actual(A(1,:,:,:))
      
      do j=1,M         
        do ii=1,K
          do jj=1,L
!dvm$       remote_access (A(1,:,:,:))
            D(1,j,ii,jj)=A(1,j,ii,jj)
            isumc=isumc+C(1,j,ii,jj)
            isuma=isuma+D(1,j,ii,jj)
          enddo
        enddo
      enddo
        
      if (isumc .eq.isuma) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate (B,A,C,D)   
     
      end
C -----------------------------------------------------REM4105
      subroutine REM4105
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)   
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4105'
      allocate (A(N,M,K,L),B(N,M,K,L),C(N,M,K,L),D(N,M,K,L))
      isumc=0
      isuma=0
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)

!dvm$ region out(A)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo     
        enddo
      enddo   
!dvm$ end region
!dvm$ get_actual(A(:,M,:,:))

      do i=1,N         
        do ii=1,K
          do jj=1,L
!dvm$       remote_access (A(:,M,:,:))
            D(i,M,ii,jj)=A(i,M,ii,jj)
            isumc=isumc+C(i,M,ii,jj)
            isuma=isuma+D(i,M,ii,jj)
          enddo
        enddo
      enddo 

      if (isumc .eq.isuma) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate (B,A,C,D)   
     
      end
C ------------------------------------------------------REM4106
      subroutine REM4106
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)  
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4106'
      allocate (A(N,M,K,L),B(N,M,K,L),C(N,M,K,L),D(N,M,K,L))
      isumc=0
      isuma=0
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)

!dvm$ region out(A)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo     
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(A(:,:,K,:))

      do i=1,N
        do j=1,M         
          do jj=1,L
!dvm$       remote_access (A(:,:,K,:))
            D(i,j,K,jj)=A(i,j,K,jj)
            isumc=isumc+C(i,j,K,jj)
            isuma=isuma+D(i,j,K,jj)
          enddo
        enddo 
      enddo

      if (isumc .eq.isuma) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (B,A,C,D)   
     
      end
C ------------------------------------------------------REM4107
      subroutine REM4107
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4107'
      allocate (A(N,M,K,L),B(N,M,K,L),C(N,M,K,L),D(N,M,K,L))
      isumc=0
      isuma=0
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)

!dvm$ region out(A)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo     
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(A(:,:,:,L))

      do i=1,N
        do j=1,M         
          do ii=1,K
!dvm$       remote_access (A(:,:,:,L))
            D(i,j,ii,L)=A(i,j,ii,L)
            isumc=isumc+C(i,j,ii,L)
            isuma=isuma+D(i,j,ii,L)
          enddo
        enddo 
      enddo

      if (isumc .eq.isuma) then     
         call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B,A,C,D)   
     
      end
C ------------------------------------------------------REM4108
      subroutine REM4108
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)   
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4108'
      allocate (A(N,M,K,L),B(N,M,K,L),C(N,M,K,L),D(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo 
          enddo
        enddo 
      enddo 
      
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
!dvm$*remote_access(A(1,1,1,1))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              B(i,j,ii,jj) = A(1,1,1,1)
            enddo
          enddo
        enddo
      enddo

!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
!dvm$* reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if (B(i,j,ii,jj).ne.C(1,1,1,1)) then
                nloopi=min(nloopi,i)
                nloopj=min(nloopj,j)
                nloopii=min(nloopii,ii)
                nloopjj=min(nloopjj,jj)
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(nloopi)

      if (nloopi .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B,A,C,D)   
     
      end
C ------------------------------------------------------REM4109
      subroutine REM4109
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)     
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4109'
      allocate (A(N,M,K,L),B(N,M,K,L),C(N,M,K,L),D(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo 
      enddo 
     
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),remote_access(A(N,M,K,L))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              B(i,j,ii,jj) = A(N,M,K,L)
            enddo
          enddo
        enddo
      enddo 

!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if (B(i,j,ii,jj).ne.C(N,M,K,L)) then
                nloopi=min(nloopi,i)
                nloopj=min(nloopj,j)
                nloopii=min(nloopii,ii)
                nloopjj=min(nloopjj,jj)
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(nloopi)

      if (nloopi .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B,A,C,D)   
     
      end

C -----------------------------------------------------REM4110
      subroutine REM4110
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4110'
      allocate (A(N,M,K,L),B(N,M,K,L),C(N,M,K,L),D(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo 
      enddo 

!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),remote_access(A)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              B(i,j,ii,jj) = A(i,j,ii,jj)
            enddo
          enddo
        enddo
      enddo
 
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
!dvm$* reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if (B(i,j,ii,jj).ne.C(i,j,ii,jj)) then
                nloopi=min(nloopi,i)
                nloopj=min(nloopj,j)
                nloopii=min(nloopii,ii)
                nloopjj=min(nloopjj,jj)
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(nloopi)

      if (nloopi .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B,A,C,D)   
     
      end

C -----------------------------------------------------REM4111
      subroutine REM4111
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4111'
      allocate (A(N,M,K,L),B(N,M,K,L),C(N,M,K,L),D(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo 
      enddo 
                    
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),remote_access(A(1,:,:,:))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              B(i,j,ii,jj) = A(1,j,ii,jj)
            enddo
          enddo
        enddo
      enddo 

!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if (B(i,j,ii,jj).ne.C(1,j,ii,jj)) then
                nloopi=min(nloopi,i)
                nloopj=min(nloopj,j)
                nloopii=min(nloopii,ii)
                nloopjj=min(nloopjj,jj)
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(nloopi)

      if (nloopi .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B,A,C,D)   
     
      end

C ----------------------------------------------------REM4112
      subroutine REM4112
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4112'
      allocate (A(N,M,K,L),B(N,M,K,L),C(N,M,K,L),D(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo 
      enddo 
              
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
!dvm$*remote_access(A(:,M,:,:))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              B(i,j,ii,jj) = A(i,M,ii,jj)
            enddo
          enddo
        enddo
      enddo
 
!dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
!dvm$* reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if (B(i,j,ii,jj).ne.C(i,M,ii,jj)) then
                nloopi=min(nloopi,i)
                nloopj=min(nloopj,j)
                nloopii=min(nloopii,ii)
                nloopjj=min(nloopjj,jj)
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(nloopi)

      if (nloopi .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B,A,C,D)   
     
      end
C ------------------------------------------------------REM4113
      subroutine REM4113
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)   
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4113'
      allocate (A(N,M,K,L),B(N,M,K,L),C(N,M,K,L),D(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L        
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo 
      enddo 
              
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$*remote_access(A(:,:,K,:))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              B(i,j,ii,jj) = A(i,j,K,jj)
            enddo
          enddo
        enddo
      enddo

!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if (B(i,j,ii,jj).ne.C(i,j,K,jj)) then
                nloopi=min(nloopi,i)
                nloopj=min(nloopj,j)
                nloopii=min(nloopii,ii)
                nloopjj=min(nloopjj,jj)
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(nloopi)

      if (nloopi .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B,A,C,D)   
     
      end
C ------------------------------------------------------REM4114
      subroutine REM4114
      integer, parameter :: N = 16,M=8,K=8,L=16,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4114'
      allocate (A(N,M,K,L),B(N,M,K,L),C(N,M,K,L),D(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
      nloopjj=NL

!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L        
              A(i,j,ii,jj) = NL+i+j+ii+jj
             enddo
          enddo
        enddo 
      enddo 
              
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$*remote_access(A(:,:,:,L))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              B(i,j,ii,jj) = A(i,j,ii,L)
            enddo
          enddo
        enddo
      enddo

!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii),min(nloopjj))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if (B(i,j,ii,jj).ne.C(i,j,ii,L)) then
                nloopi=min(nloopi,i)
                nloopj=min(nloopj,j)
                nloopii=min(nloopii,ii)
                nloopjj=min(nloopjj,jj)
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(nloopi)

      if (nloopi .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B,A,C,D)   
     
      end

C ---------------------------------------------------------         
      subroutine serial4(AR,N,M,K,L,NL)
      integer AR(N,M,K,L)
      integer NL 
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              AR(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
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
