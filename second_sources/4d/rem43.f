      program REM43
     
c    TESTING OF THE REMOTE_ACCESS DIRECTIVE AND THE REMOTE_ACCESS CLAUSE'.       
c    DISTRIBUTED ARRAY A(N,M,K,L) OR ELEMENTS OF THIS ARRAY ARE REPLICATED
c    ON ALL PROCESSORS. 

      print *,'===START OF REM43========================'
C --------------------------------------------------
      call rem4301
C --------------------------------------------------
      call rem4302
C --------------------------------------------------
      call rem4303
C -------------------------------------------------
      call rem4304
C -------------------------------------------------
      call rem4305
C -------------------------------------------------
      call rem4306
C --------------------------------------------------
      call rem4307
C --------------------------------------------------
      call rem4308
C --------------------------------------------------
      call rem4309
C -------------------------------------------------
      call rem4310
C -------------------------------------------------
      call rem4311
C -------------------------------------------------
      call rem4312
C ------------------------------------------------- 
      call rem4313
C ------------------------------------------------- 
      call rem4314
C ------------------------------------------------- 
C
      print *,'=== END OF REM43 ========================= '    
      end
C ---------------------------------------------------------REM3101
      subroutine REM4301
      integer, parameter :: N = 16,M=8,K=8,L=16,NL=1000
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:),C(:,:,:,:)
      character*7 tname
      integer :: i,j,ii,jj
                 
!dvm$ distribute B(BLOCK,BLOCK,BLOCK,*)    
!dvm$ align(i,j,ii,jj) with B(i,j,ii,jj) :: A 

      tname='REM4301'
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

C ------------------------------------------------------REM4302
      subroutine REM4302     
      integer, parameter :: N = 16,M=8,K=8,L=16,NL=1000
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:),C(:,:,:,:)
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,*,BLOCK)    
!dvm$ align(i,j,ii,jj) with A(i,j,ii,jj) :: B 

      tname='REM4302'
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

C ------------------------------------------------------REM4303
      subroutine REM4303
      integer, parameter :: N = 4,M=4,K=4,L=4,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*,BLOCK,BLOCK)   
!dvm$ align(i,j,ii,jj) with A(i,j,ii,jj) :: B 

      tname='REM4303'
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

C ------------------------------------------------------REM4304
      subroutine REM4304
      integer, parameter :: N = 6,M=8,K=8,L=4,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(*,BLOCK,BLOCK,BLOCK)    
!dvm$ align(i1,i2,i3,i4) with A(i1,i2,i3,i4) :: B 

      tname='REM4304'
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
C -----------------------------------------------------REM4305
      subroutine REM4305
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,*)   
!dvm$ align(i,j,ii,jj) with A(i,j,ii,jj) :: B 

      tname='REM4305'
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
C ------------------------------------------------------REM4306
      subroutine REM4306
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,*,BLOCK)  
!dvm$ align(i,j,ii,jj) with A(i,j,ii,jj) :: B 

      tname='REM4306'
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
C ------------------------------------------------------REM4307
      subroutine REM4307
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*,BLOCK,BLOCK)    
!dvm$ align(i,j,ii,jj) with A(i,j,ii,jj) :: B 

      tname='REM4307'
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
C ------------------------------------------------------REM4308
      subroutine REM4308
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(*,BLOCK,BLOCK,BLOCK)   
!dvm$ align(i,j,ii,jj) with A(i,j,ii,jj) :: B 

      tname='REM4308'
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
C ------------------------------------------------------REM4309
      subroutine REM4309
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,*)     
!dvm$ align(i,j,ii,jj) with A(i,j,ii,jj) :: B 

      tname='REM4309'
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

C -----------------------------------------------------REM4310
      subroutine REM4310
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,*,BLOCK)    
!dvm$ align(i,j,ii,jj) with A(i,j,ii,jj) :: B 

      tname='REM4310'
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

C -----------------------------------------------------REM4311
      subroutine REM4311
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*,BLOCK,BLOCK)    
!dvm$ align(i,j,ii,jj) with A(i,j,ii,jj) :: B 

      tname='REM4311'
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

C ----------------------------------------------------REM4312
      subroutine REM4312
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(*,BLOCK,BLOCK,BLOCK)    
!dvm$ align(i,j,ii,jj) with A(i,j,ii,jj) :: B 

      tname='REM4312'
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
C ------------------------------------------------------REM4313
      subroutine REM4313
      integer, parameter :: N = 6,M=8,K=8,L=6,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,*)   
!dvm$ align(i,j,ii,jj) with A(i,j,ii,jj) :: B 

      tname='REM4313'
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
C ------------------------------------------------------REM4314
      subroutine REM4314
      integer, parameter :: N = 16,M=8,K=8,L=16,NL=1000
      integer,allocatable::A(:,:,:,:),B(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer nloopi,nloopj,nloopii,nloopjj 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,*,BLOCK)    
!dvm$ align(i,j,ii,jj) with A(i,j,ii,jj) :: B 

      tname='REM4314'
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
