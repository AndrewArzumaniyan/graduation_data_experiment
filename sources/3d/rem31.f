      program REM31
     
c    TESTING OF THE REMOTE_ACCESS DIRECTIVE AND THE REMOTE_ACCESS CLAUSE'.       
c    DISTRIBUTED ARRAY A(N,M,K) OR ELEMENTS OF THIS ARRAY ARE REPLICATED
c    ON ALL PROCESSORS. 

      print *,'===START OF REM31========================'
C --------------------------------------------------
      call rem3101
C --------------------------------------------------
      call rem3102
C --------------------------------------------------
      call rem3103
C -------------------------------------------------
      call rem3104
C -------------------------------------------------
      call rem3105
C -------------------------------------------------
      call rem3106
C --------------------------------------------------
      call rem3107
C --------------------------------------------------
      call rem3108
C --------------------------------------------------
      call rem3109
C -------------------------------------------------
      call rem3110
C -------------------------------------------------
      call rem3111
C -------------------------------------------------
      call rem3112
C ------------------------------------------------- 
      call rem3113
C ------------------------------------------------- 
C
      print *,'=== END OF REM31 ========================= '    
      end
C ---------------------------------------------------------REM3101
      subroutine REM3101     
      integer, parameter ::  N = 16,M=8,K=8,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii 
      character*7 tname
                 
!dvm$ distribute B(BLOCK,BLOCK,BLOCK)
!dvm$ align(:,:,:) with B(:,:,:) :: A 

      tname='REM3101'
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

C ------------------------------------------------------REM3102
      subroutine REM3102
      integer, parameter ::  N = 16,M=8,K=8,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK)
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3102'
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

C ------------------------------------------------------REM3103
      subroutine REM3103
      integer, parameter ::  N = 16,M=8,K=8,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK)     
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3103'
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

C ------------------------------------------------------REM3104
      subroutine REM3104
      integer, parameter ::  N = 16,M=8,K=8,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK)     
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3104'
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
C -----------------------------------------------------REM3105
      subroutine REM3105
      integer, parameter ::  N = 16,M=8,K=8,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK)
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3105'
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
C ------------------------------------------------------REM3106
      subroutine REM3106
      integer, parameter ::  N = 16,M=8,K=8,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK) 
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3106'
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

C ------------------------------------------------------REM3107
      subroutine REM3107
      integer, parameter ::  N = 16,M=8,K=8,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii,isumc,isuma 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK) 
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3107'
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
C ------------------------------------------------------REM3108
      subroutine REM3108
      integer, parameter ::  N = 16,M=8,K=8,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK) 
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3108'
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
C ------------------------------------------------------REM3109
      subroutine REM3109
      integer, parameter ::  N = 16,M=8,K=8,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK) 
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3109'
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

C -----------------------------------------------------REM3110
      subroutine REM3110
      integer, parameter ::  N = 16,M=8,K=8,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK) 
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3110'
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

C -----------------------------------------------------REM3111
      subroutine REM3111
      integer, parameter ::  N = 16,M=8,K=8,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK)
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3111'
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

C ----------------------------------------------------REM3112
      subroutine REM3112
      integer, parameter ::  N = 16,M=8,K=8,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK)
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3112'
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
C ------------------------------------------------------REM3113
      subroutine REM3113
      integer, parameter ::  N = 16,M=8,K=8,NL=1000
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:),D(:,:,:)
      integer nloopi,nloopj,nloopii 
      character*7 tname
                 
!dvm$ distribute A(BLOCK,BLOCK,BLOCK)
!dvm$ align(:,:,:) with A(:,:,:) :: B 

      tname='REM3113'
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
