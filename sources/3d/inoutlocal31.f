      program INOUTLOCAL31
     
c    TESTING OF INOULOCAL CLAUSE'.       

      print *,'===START OF INOUTLOCAL31========================'
C --------------------------------------------------
      call inoutlocal3101
      call inoutlocal3102
      call inoutlocal3103
      call inoutlocal3104
      call inoutlocal3105
      call inoutlocal3106
      call inoutlocal3107
      call inoutlocal3108
      call inoutlocal3109
      call inoutlocal3110
      call inoutlocal3111
      call inoutlocal3112
      call inoutlocal3113
      call inoutlocal3114
      call inoutlocal3115
      call inoutlocal3116

C --------------------------------------------------
C
      print *,'=== END OF inoutlocal31 ========================= '    
      end
C ---------------------------------------------IN3101
      subroutine INOUTLOCAL3101     
      integer, parameter :: N = 16,M=8,K=8,NL=1000     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3101'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
! dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ region 
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
 
!dvm$ parallel (i,j,ii) on B(i,j,ii)    
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
cdvm$ get_actual(B) 
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          if (B(i,j,ii).ne.c(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo

      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)
      end
c------------------------------------------------IN3102
      subroutine INOUTLOCAL3102     
      integer, parameter :: N = 16,M=8,K=8,NL=1100     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3102'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ region OUT(B(2:N-1,2:M-1,2:K-1)),IN(NL),LOCAL(A)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
 
!dvm$ parallel (i,j,ii) on B(i,j,ii)    
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
cdvm$ get_actual(B) 
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          if (B(i,j,ii).ne.c(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo

      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end


C ---------------------------------------------IN3103
      subroutine INOUTLOCAL3103     
      integer, parameter :: N = 16,M=8,K=8,NL=1200     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3103'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
! dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ region OUT(B(2:N-1,2:M-1,2:K-1)),IN(NL)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
 
!dvm$ parallel (i,j,ii) on B(i,j,ii)    
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
cdvm$ get_actual(B) 
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          if (B(i,j,ii).ne.c(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo

      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end

C ---------------------------------------------IN3104
      subroutine INOUTLOCAL3104     
      integer, parameter :: N = 16,M=8,K=8,NL=1300     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3104'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ region
!dvm$*LOCAL(B(1,1,1)
!dvm$*,B(N,M,K),B(1,M,K),B(N,1,K),B(N,M,1),
!dvm$*B(1,1,K),B(N,1,1),B(1,M,1))
!dvm$*,OUT(B(2:N-1,2:M-1,3:K-1),B(2:N-1,2:M-1,2))

!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
 
!dvm$ parallel (i,j,ii) on B(i,j,ii)    
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
cdvm$ get_actual(B) 
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          if (B(i,j,ii).ne.c(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo

      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end
C ---------------------------------------------IN3105
      subroutine INOUTLOCAL3105     
      integer, parameter :: N = 16,M=8,K=8,NL=1600     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3105'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
!dvm$ region  
!dvm$ parallel (i,j,ii) on B(i,j,ii)    
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
cdvm$ get_actual(B) 
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          if (B(i,j,ii).ne.c(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo

      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end


C ---------------------------------------------IN3106
      subroutine INOUTLOCAL3106     
      integer, parameter :: N = 16,M=8,K=8,NL=1700     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3106'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
!dvm$ region IN (A(2:N-1,2:M-1,2:K-1)),OUT (B(2:N-1,2:M-1,2:K-1)) 
!dvm$ parallel (i,j,ii) on B(i,j,ii)    
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
cdvm$ get_actual(B) 
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          if (B(i,j,ii).ne.c(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo

      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end

C ---------------------------------------------IN3107
      subroutine INOUTLOCAL3107     
      integer, parameter :: N = 16,M=8,K=8,NL=1800     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3107'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
!dvm$ region IN(A(2:N-1,2:M-1,2:K-1)),OUT (B(2:N-1,2:M-1,2:K-1)) 
!dvm$ parallel (i,j,ii) on B(i,j,ii)    
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
cdvm$ get_actual(B) 
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          if (B(i,j,ii).ne.c(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo

      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end

     
C ---------------------------------------------IN3108
      subroutine INOUTLOCAL3108     
      integer, parameter :: N = 16,M=8,K=8,NL=1900     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3108'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
!dvm$ region INLOCAL(A(2:N-1,2:M-1,2:K-1)),OUT (B(2:N-1,2:M-1,2:K-1)) 
!dvm$ parallel (i,j,ii) on B(i,j,ii)    
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
cdvm$ get_actual(B) 
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          if (B(i,j,ii).ne.c(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo

      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end

     
C ---------------------------------------------IN3109
      subroutine INOUTLOCAL3109     
      integer, parameter :: N = 16,M=8,K=8,NL=2000     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3109'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
!dvm$ region INOUT (A(2:N-1,2:M-1,2:K-1),B(2:N-1,2:M-1,2:K-1)) 
! dvm$ region IN (A(2:N-1,2:M-1,2:K-1)),OUT(B(2:N-1,2:M-1,2:K-1)) 
!dvm$ parallel (i,j,ii) on B(i,j,ii)    
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
cdvm$ get_actual(B) 
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          if (B(i,j,ii).ne.c(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo

      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end
C ---------------------------------------------IN3110
      subroutine INOUTLOCAL3110     
      integer, parameter :: N = 16,M=8,K=8,NL=2100     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3110'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
!dvm$ region IN (A(2:N-1,2:M-1,2:K-1)),IN(A)
!dvm$*,IN(A(2,2,2)),IN(A(2,M-1,K-1))
!dvm$*,OUT(B(2:N-1,2:M-1,2:K-1)),OUT(B(2,M-1,3)) 
!dvm$ parallel (i,j,ii) on B(i,j,ii)    
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
cdvm$ get_actual(B) 
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          if (B(i,j,ii).ne.c(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo

      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end


C ---------------------------------------------IN3111
      subroutine INOUTLOCAL3111     
      integer, parameter :: N = 16,M=8,K=8,NL=2200     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3111'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
!dvm$ region IN (A(2:N-1,2:M-1,2:K-1))
!dvm$ parallel (i,j,ii) on B(i,j,ii)    
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
cdvm$ get_actual(B)
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          if (B(i,j,ii).ne.c(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo

      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end

C ---------------------------------------------IN3112
      subroutine INOUTLOCAL3112     
      integer, parameter :: N = 16,M=8,K=8,NL=2300     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3112'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ actual(nloopi,nloopj,nloopii)
cdvm$ region
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo
cdvm$ end region 
!dvm$ region IN (A(2:N-1,2:M-1,2:K-1))
! dvm$*,OUT(B(3:N-1,2:M-2,4:K-1)),OUT(B(2,M-1,2:3)) 
!dvm$ parallel (i,j,ii) on B(i,j,ii)    
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
cdvm$ get_actual(B) 
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          if (B(i,j,ii).ne.c(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo

      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end
C ---------------------------------------------IN3113
      subroutine INOUTLOCAL3113     
      integer, parameter :: N = 16,M=8,K=8,NL=2400     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3113'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
!dvm$ region IN (A(2:N-1,2:M-1,2:K-1))
!dvm$*,OUT(B(3:N-1,2:M-2,4:K-1)),OUT(B(2,M-1,2:3),
!dvm$*B(2,2:M-2,2:K-1),B(2,2:M-2,2:K-1),
!dvm$*B(2:N-1,2:M-2,2:K-1),B(2:N-1,2:M-1,4:K-1),
!dvm$*B(2:N-1,2:M-1,2:3)) 
!dvm$ parallel (i,j,ii) on B(i,j,ii)    
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
cdvm$ get_actual(B) 
cdvm$ region
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          if (B(i,j,ii).ne.c(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo
cdvm$ end region
      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end

C  ---------------------------------------------IN3114
      subroutine INOUTLOCAL3114     
      integer, parameter :: N = 16,M=8,K=8,NL=2500     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3114'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ actual(nloopi,nloopj,nloopii)
Cdvm$ region
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
cdvm$ end region
!dvm$ region IN (A(2:N-1,2:M-1,2:K-1))
!dvm$*,OUT(B(3:N-1,2:M-2,4:K-1)),OUT(B(2,M-1,2:3),
!dvm$*B(2,2:M-2,2:K-1),B(2,2:M-2,2:K-1),
!dvm$*B(2:N-1,2:M-2,2:K-1),B(2:N-1,2:M-1,4:K-1),
!dvm$*B(2:N-1,2:M-1,2:3)) 
!dvm$ parallel (i,j,ii) on B(i,j,ii)
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
cdvm$ region 
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          if (B(i,j,ii).ne.c(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo
cdvm$ end region
      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end


C  ---------------------------------------------IN3115
      subroutine INOUTLOCAL3115     
      integer, parameter :: N = 16,M=8,K=8,NL=2600     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3115'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ actual(nloopi,nloopj,nloopii)
!dvm$ region
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
cdvm$ end region
!dvm$ region IN (A(2:N-1,2:M-1,2:K-1))
!dvm$*,OUT(B(2:N-1,2:M-1,2:K-1))
!dvm$ parallel (i,j,ii) on B(i,j,ii)
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
!dvm$ region IN (A(2:N-1,2:M-1,2:K-1))
!dvm$*,OUT(B(2:N-1,2:M-1,2:K-1))
!dvm$ parallel (i,j,ii) on B(i,j,ii)
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
!dvm$ region IN (A(2:N-1,2:M-1,2:K-1))
!dvm$*,OUT(B(2:N-1,2:M-1,2:K-1))
!dvm$ parallel (i,j,ii) on B(i,j,ii)
	do i=2,N-1
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
!dvm$ get_actual(B)

!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          if (B(i,j,ii).ne.c(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo
      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end

C  ---------------------------------------------IN3116
      subroutine INOUTLOCAL3116     
      integer, parameter :: N = 16,M=8,K=8,NL=2700     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3116'     
      allocate (B(N,M,K),A(N,M,K),C(N,M,K))
      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopi=NL
      nloopj=NL
      nloopii=NL
!dvm$ actual(nloopi,nloopj,nloopii)
Cdvm$ region
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
          enddo
        enddo
      enddo 
cdvm$ end region
!dvm$ region IN (A(2:N-1,2:M-1,2:K-1))
!dvm$*,OUT(B(2:2,2:M-1,2:K-1))
!dvm$ parallel (i,j,ii) on B(i,j,ii)
	do i=2,2
        do j=2,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
!dvm$ region IN (A(2:N-1,2:M-1,2:K-1)
!dvm$*, B(2:2,2:M-1,2:K-1))
!dvm$*,OUT(B(2:N-1,2:2,2:K-1))
!dvm$ parallel (i,j,ii) on B(i,j,ii)
	do i=3,N-1
        do j=2,2
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
!dvm$ region IN (A(2:N-1,2:M-1,2:K-1)
!dvm$*,B(2:N-1,2:2,2:K-1))
!dvm$*,OUT(B(2:N-1,2:M-1,2:K-1))
!dvm$ parallel (i,j,ii) on B(i,j,ii)
	do i=2,N-1
        do j=3,M-1
          do ii=2,K-1
          B(i,j,ii) = A(i,j,ii)
                   enddo 
        enddo 
      enddo
cdvm$ end region 
cdvm$ region
!dvm$ parallel (i,j,ii) on B(i,j,ii),
!dvm$*reduction( min( nloopi),min(nloopj),min(nloopii))
      do i=2,N-1
        do j=2,M-1
         do ii=2,K-1
          if (B(i,j,ii).ne.c(i,j,ii)) then
           nloopi=min(nloopi,i)
           nloopj=min(nloopj,j)
           nloopii=min(nloopii,ii)
          endif
         enddo
       enddo
      enddo
cdvm$ end region
      if (nloopi .eq.NL) then
         call ansyes(tname)
      else
         call ansno(tname)
      endif
      deallocate (A,B,C)

      end

 
    
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
      character*6 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*6 name
      print *,name,'  -  ***error'
      end
