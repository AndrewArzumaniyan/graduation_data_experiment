      program REM42
     
c    TESTING OF THE REMOTE_ACCESS DIRECTIVE AND THE REMOTE_ACCESS CLAUSE'.       
c    DISTRIBUTED ARRAY A(N,M,K,L) OR ELEMENTS OF THIS ARRAY ARE REPLICATED
c    ON ALL PROCESSORS. 

      print *,'===START OF REM42========================'
C --------------------------------------------------
      call rem4201
C --------------------------------------------------
      call rem4202
C --------------------------------------------------
      call rem4203
C -------------------------------------------------
      call rem4204
C -------------------------------------------------
      call rem4205
C -------------------------------------------------
      call rem4206
C --------------------------------------------------
      call rem4207
C --------------------------------------------------
      call rem4208
C --------------------------------------------------
      call rem4209
C -------------------------------------------------
      call rem4210
C -------------------------------------------------
      call rem4211
C -------------------------------------------------
      call rem4212
C ------------------------------------------------- 
      call rem4213
C ------------------------------------------------- 
      call rem4214
C ------------------------------------------------- 
C
      print *,'=== END OF REM42 ========================= '    
      end
C ---------------------------------------------------------REM3101
      subroutine REM4201
      integer, parameter :: N = 16,M=8,K=8,L=16,NL=1000
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:),C(:,:,:,:)
      character*7 tname
                 
!dvm$ distribute B(*,*,*,*)    
!dvm$ align(:,:,:,:) with B(:,:,:,:) :: A 

      tname='REM4201'
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

C ------------------------------------------------------REM4202
      subroutine REM4202     
      integer, parameter :: N = 16,M=8,K=8,L=16,NL=1000
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:),C(:,:,:,:)
      character*7 tname
                 
!dvm$ distribute A(*,*,*,*)    
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4202'
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

C ------------------------------------------------------REM4203
      subroutine REM4203
      integer, parameter :: N = 4,M=4,K=4,L=4,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(*,*,*,*)   
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4203'
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

C ------------------------------------------------------REM4204
      subroutine REM4204
      integer, parameter :: N = 6,M=8,K=8,L=4,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(*,*,*,*)    
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4204'
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
C -----------------------------------------------------REM4205
      subroutine REM4205
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(*,*,*,*)   
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4205'
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
C ------------------------------------------------------REM4206
      subroutine REM4206
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(*,*,*,*)  
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4206'
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
C ------------------------------------------------------REM4207
      subroutine REM4207
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(*,*,*,*)    
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4207'
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
C ------------------------------------------------------REM4208
      subroutine REM4208
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(*,*,*,*)   
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4208'
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
C ------------------------------------------------------REM4209
      subroutine REM4209
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(*,*,*,*)     
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4209'
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

C -----------------------------------------------------REM4210
      subroutine REM4210
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(*,*,*,*)    
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4210'
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

C -----------------------------------------------------REM4211
      subroutine REM4211
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(*,*,*,*)    
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4211'
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

C ----------------------------------------------------REM4212
      subroutine REM4212
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(*,*,*,*)    
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4212'
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
C ------------------------------------------------------REM4213
      subroutine REM4213
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(*,*,*,*)   
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4213'
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
C ------------------------------------------------------REM4214
      subroutine REM4214
      integer, parameter :: N = 16,M=8,K=8,L=16,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(*,*,*,*)    
!dvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

      tname='REM4214'
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
