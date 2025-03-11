      program INOUTLOCAL32
     
c    TESTING OF INOULOCAL CLAUSE'.       

      print *,'===START OF INOUTLOCAL32========================'
C --------------------------------------------------
      call inoutlocal3201
      call inoutlocal3202
      call inoutlocal3203
      call inoutlocal3204
      call inoutlocal3205
      call inoutlocal3206
      call inoutlocal3207
      call inoutlocal3208
      call inoutlocal3209
      call inoutlocal3210
      call inoutlocal3211
      call inoutlocal3212
      call inoutlocal3213
      call inoutlocal3214
      call inoutlocal3215
      call inoutlocal3216

C --------------------------------------------------
C
      print *,'=== END OF INOUTLOCAL32 ========================= '    
      end
C ---------------------------------------------IN3201
      subroutine INOUTLOCAL3201     
      integer, parameter :: N = 16,M=8,K=8,NL=1000     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3201'     
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
c------------------------------------------------IN3202
      subroutine INOUTLOCAL3202     
      integer, parameter :: N = 16,M=8,K=8,NL=1100     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,*,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3202'     
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


C ---------------------------------------------IN3203
      subroutine INOUTLOCAL3203     
      integer, parameter :: N = 16,M=8,K=8,NL=1200     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3203'     
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

C ---------------------------------------------IN3204
      subroutine INOUTLOCAL3204     
      integer, parameter :: N = 16,M=8,K=8,NL=1300     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3204'     
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
C ---------------------------------------------IN3205
      subroutine INOUTLOCAL3205     
      integer, parameter :: N = 16,M=8,K=8,NL=1600     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,*,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3205'     
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


C ---------------------------------------------IN3206
      subroutine INOUTLOCAL3206     
      integer, parameter :: N = 16,M=8,K=8,NL=1700     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3206'     
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

C ---------------------------------------------IN3207
      subroutine INOUTLOCAL3207     
      integer, parameter :: N = 16,M=8,K=8,NL=1800     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3207'     
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

     
C ---------------------------------------------IN3208
      subroutine INOUTLOCAL3208     
      integer, parameter :: N = 16,M=8,K=8,NL=1900     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,*,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3208'     
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

     
C ---------------------------------------------IN3209
      subroutine INOUTLOCAL3209     
      integer, parameter :: N = 16,M=8,K=8,NL=2000     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3209'     
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
C ---------------------------------------------IN3210
      subroutine INOUTLOCAL3210     
      integer, parameter :: N = 16,M=8,K=8,NL=2100     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3210'     
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


C ---------------------------------------------IN3211
      subroutine INOUTLOCAL3211     
      integer, parameter :: N = 16,M=8,K=8,NL=2200     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,*,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3211'     
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

C ---------------------------------------------IN3212
      subroutine INOUTLOCAL3212     
      integer, parameter :: N = 16,M=8,K=8,NL=2300     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3212'     
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
C ---------------------------------------------IN3213
      subroutine INOUTLOCAL3213     
      integer, parameter :: N = 16,M=8,K=8,NL=2400     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3213'     
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

C  ---------------------------------------------IN3214
      subroutine INOUTLOCAL3214     
      integer, parameter :: N = 16,M=8,K=8,NL=2500     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,*,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3214'     
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


C  ---------------------------------------------IN3215
      subroutine INOUTLOCAL3215     
      integer, parameter :: N = 16,M=8,K=8,NL=2600     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,BLOCK,BLOCK)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3215'     
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

C  ---------------------------------------------IN3216
      subroutine INOUTLOCAL3216     
      integer, parameter :: N = 16,M=8,K=8,NL=2700     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(BLOCK,BLOCK,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3216'     
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
