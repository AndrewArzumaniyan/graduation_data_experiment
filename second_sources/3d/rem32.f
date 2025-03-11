      program REM32
     
c    TESTING OF THE REMOTE_ACCESS DIRECTIVE AND THE REMOTE_ACCESS CLAUSE'.       
c    DISTRIBUTED ARRAY A(N,M,K) OR ELEMENTS OF THIS ARRAY ARE REPLICATED
c    ON ALL PROCESSORS. 

      print *,'===START OF REM32========================'
C --------------------------------------------------
      call rem3201
C --------------------------------------------------
      call rem3202
C --------------------------------------------------
      call rem3203
C -------------------------------------------------
      call rem3204
C -------------------------------------------------
      call rem3205
C -------------------------------------------------
      call rem3206
C --------------------------------------------------
      call rem3207
C --------------------------------------------------
      call rem3208
C --------------------------------------------------
      call rem3209
C -------------------------------------------------
      call rem3210
C -------------------------------------------------
      call rem3211
C -------------------------------------------------
      call rem3212
C ------------------------------------------------- 
      call rem3213
C ------------------------------------------------- 
C
      print *,'=== END OF REM32 ========================= '    
      end
C ---------------------------------------------------------REM3201
      subroutine REM3201     
      integer, parameter ::  N=8,M=4,K=4,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii 
      character*7 tname
                 
!dvm$ distribute B(*,BLOCK,BLOCK)
!dvm$ align(:,:,:) with B(:,:,:) :: A 

      tname='REM3201'
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ region out(A)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo      
!dvm$ end region
!dvm$ get_actual(A(1,1,1))
!dvm$ remote_access (A(1,1,1))
      ib=A(1,1,1)               

      if (ib .eq.C(1,1,1)) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate (A,B,C)

      end

C ------------------------------------------------------REM3202
      subroutine REM3202
      integer, parameter ::  N=8,M=4,K=4,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*,BLOCK)
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3202'
      allocate (A(N,M,K),B(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ region out(A)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo      
!dvm$ end region
!dvm$ get_actual(A(N,M,K))
!dvm$ remote_access (A(N,M,K))
      ib=A(N,M,K)               
      if (ib .eq.C(N,M,K)) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate (B,A,C)

      end

C ------------------------------------------------------REM3203
      subroutine REM3203
      integer, parameter ::  N=8,M=4,K=4,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,*)     
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3203'
      allocate (A(N,M,K),B(N,M,K),C(N,M,K),D(N,M,K))
      isumc=0
      isuma=0
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ region out(A)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(A)
      do i=1,N         
        do j=i,M
          do ii=1,K
!dvm$        remote_access (A(:,:,:))
             D(i,j,ii)=A(i,j,ii)
             isumc=isumc+C(i,j,ii)
             isuma=isuma+D(i,j,ii)
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

C ------------------------------------------------------REM3204
      subroutine REM3204
      integer, parameter ::  N=8,M=4,K=4,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(*,BLOCK,BLOCK)     
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3204'
      allocate (A(N,M,K),B(N,M,K),C(N,M,K),D(N,M,K))
      isumc=0
      isuma=0
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=Nl

!dvm$ region out(A)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(A(1,:,:))

      do j=1,M         
        do ii=1,K
!dvm$     remote_access (A(1,:,:))
          D(1,j,ii)=A(1,j,ii)
          isumc=isumc+C(1,j,ii)
          isuma=isuma+D(1,j,ii)
        enddo
      enddo
              
      if (isumc .eq.isuma) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate (B,A,C,D)   
        
      end
C -----------------------------------------------------REM3205
      subroutine REM3205
      integer, parameter ::  N=8,M=4,K=4,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*,BLOCK)
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3205'
      allocate (A(N,M,K),B(N,M,K),C(N,M,K),D(N,M,K))
      isumc=0
      isuma=0
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ region out(A)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(A(:,M,:))

      do i=1,N         
        do ii=1,K
!dvm$     remote_access (A(:,M,:))
          D(i,M,ii)=A(i,M,ii)
          isumc=isumc+C(i,M,ii)
          isuma=isuma+D(i,M,ii)
        enddo
      enddo

      if (isumc .eq.isuma) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate (B,A,C,D)   
   
      end
C ------------------------------------------------------REM3206
      subroutine REM3206
      integer, parameter ::  N=8,M=4,K=4,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,*) 
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3206'
      allocate (A(N,M,K),B(N,M,K),C(N,M,K),D(N,M,K))
      isumc=0
      isuma=0
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ region out(A)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(A(:,:,K))
    
      do i=1,N
        do j=1,M         
!dvm$     remote_access (A(:,:,K))
          D(i,j,K)=A(i,j,K)
          isumc=isumc+C(i,j,K)
          isuma=isuma+D(i,j,K)
        enddo
      enddo

      if (isumc .eq.isuma) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate (B,A,C,D)      
     
      end

C ------------------------------------------------------REM3207
      subroutine REM3207
      integer, parameter ::  N=8,M=4,K=4,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(*,BLOCK,BLOCK) 
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3207'
      allocate (A(N,M,K),B(N,M,K),C(N,M,K),D(N,M,K))
      isumc=0
      isuma=0
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ region out(A)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo
!dvm$ end region
!dvm$ get_actual(A)
                                          
      ki=2
      ki1=3
      kj=2
      kj1=3
      kii=2
      kii1=3        
      do i=1,N/ki-ki1         
        do j=1,M/kj-kj1
          do ii=1,K/kii-kii1
!dvm$       remote_access (A(ki*i+ki1,kj*j+kj1,kii*ii+kii1))
            D(i,j,ii)=A(ki*i+ki1,kj*j+kj1,kii*ii+kii1)
            isumc=isumc+C(ki*i+ki1,kj*j+kj1,kii*ii+kii1)
            isuma=isuma+D(i,j,ii)
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
C ------------------------------------------------------REM3208
      subroutine REM3208
      integer, parameter ::  N=8,M=4,K=4,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*,BLOCK) 
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3208'
      allocate (A(N,M,K),B(N,M,K),C(N,M,K),D(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo 
      enddo 

!dvm$ parallel (i,j,ii) on B(i,j,ii),remote_access(A(1,1,1))
      do i=1,N
        do j=1,M
          do ii=1,K
            B(i,j,ii) = A(1,1,1)
          enddo
        enddo
      enddo

!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$* reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=1,N
        do j=1,M
          do ii=1,K
            if (B(i,j,ii).ne.C(1,1,1)) then
              nloopi=min(nloopi,i)
              nloopj=min(nloopj,j)
              nloopii=min(nloopii,ii)
            endif
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
C ------------------------------------------------------REM3209
      subroutine REM3209
      integer, parameter ::  N=8,M=4,K=4,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,*) 
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3209'
      allocate (A(N,M,K),B(N,M,K),C(N,M,K),D(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo 
      enddo 

!dvm$ parallel (i,j,ii) on B(i,j,ii),remote_access(A(N,M,K))
      do i=1,N
        do j=1,M
          do ii=1,K
            B(i,j,ii) = A(N,M,K)
          enddo
        enddo
      enddo 

!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=1,N
        do j=1,M
          do ii=1,K
            if (B(i,j,ii).ne.C(N,M,K)) then
              nloopi=min(nloopi,i)
              nloopj=min(nloopj,j)
              nloopii=min(nloopii,ii)
            endif
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

C -----------------------------------------------------REM3210
      subroutine REM3210
      integer, parameter ::  N=8,M=4,K=4,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii 
      character*7 tname
                 
!dvm$ distribute A(*,BLOCK,BLOCK) 
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3210'
      allocate (A(N,M,K),B(N,M,K),C(N,M,K),D(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo 
      enddo 

!dvm$ parallel (i,j,ii) on B(i,j,ii),remote_access(A)
      do i=1,N
        do j=1,M
          do ii=1,K
            B(i,j,ii) = A(i,j,ii)
          enddo
        enddo
      enddo 

!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$* reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=1,N
        do j=1,M
          do ii=1,K
            if (B(i,j,ii).ne.C(i,j,ii)) then
              nloopi=min(nloopi,i)
              nloopj=min(nloopj,j)
              nloopii=min(nloopii,ii)
            endif
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

C -----------------------------------------------------REM3211
      subroutine REM3211
      integer, parameter ::  N=8,M=4,K=4,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,*,BLOCK)
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3211'
      allocate (A(N,M,K),B(N,M,K),C(N,M,K),D(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo 
      enddo 
                     
!dvm$ parallel (i,j,ii) on A(i,j,ii),remote_access(A(1,:,:))
      do i=1,N
        do j=1,M
          do ii=1,K
            B(i,j,ii) = A(1,j,ii)
          enddo
        enddo
      enddo 

!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=1,N
        do j=1,M
          do ii=1,K
            if (B(i,j,ii).ne.C(1,j,ii)) then
              nloopi=min(nloopi,i)
              nloopj=min(nloopj,j)
              nloopii=min(nloopii,ii)
            endif
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

C ----------------------------------------------------REM3212
      subroutine REM3212
      integer, parameter ::  N=8,M=4,K=4,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,*)
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3212'
      allocate (A(N,M,K),B(N,M,K),C(N,M,K),D(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL

!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo 
      enddo 

!dvm$ parallel (i,j,ii) on B(i,j,ii),remote_access(A(:,M,:))
      do i=1,N
        do j=1,M
           do ii=1,K
             B(i,j,ii) = A(i,M,ii)
           enddo
        enddo
      enddo
 
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$* reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=1,N
        do j=1,M
          do ii=1,K
            if (B(i,j,ii).ne.C(i,M,ii)) then
              nloopi=min(nloopi,i)
              nloopj=min(nloopj,j)
              nloopii=min(nloopii,ii)
            endif
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
C ------------------------------------------------------REM3213
      subroutine REM3213
      integer, parameter ::  N=8,M=4,K=4,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii 
      character*7 tname
                 
!dvm$ distribute A(*,BLOCK,BLOCK)
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3213'
      allocate (A(N,M,K),B(N,M,K),C(N,M,K),D(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ region local(A,B)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K        
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo 
      enddo 
               
!dvm$ parallel (i,j,ii) on A(i,j,ii),
!dvm$*remote_access(A(:,:,K))
      do i=1,N
        do j=1,M
          do ii=1,K
            B(i,j,ii) = A(i,j,K)
          enddo
        enddo
      enddo
 
!dvm$ parallel (i,j,ii) on A(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=1,N
        do j=1,M
          do ii=1,K
            if (B(i,j,ii).ne.C(i,j,K)) then
              nloopi=min(nloopi,i)
              nloopj=min(nloopj,j)
              nloopii=min(nloopii,ii)
            endif
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
