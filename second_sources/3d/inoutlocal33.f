      program INOUTLOCAL33
     
c    TESTING OF INOULOCAL CLAUSE'.       

      print *,'===START OF INOUTLOCAL33========================'
C --------------------------------------------------
      call inoutlocal3301
      call inoutlocal3302
      call inoutlocal3303
      call inoutlocal3304
      call inoutlocal3305
      call inoutlocal3306
      call inoutlocal3307
      call inoutlocal3308
      call inoutlocal3309
      call inoutlocal3310
      call inoutlocal3311
      call inoutlocal3312
      call inoutlocal3313
      call inoutlocal3314
      call inoutlocal3315
      call inoutlocal3316

C --------------------------------------------------
C
      print *,'=== END OF INOUTLOCAL33 ========================= '    
      end
C ---------------------------------------------IN3301
      subroutine INOUTLOCAL3301     
      integer, parameter :: N = 16,M=8,K=8,NL=1000     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,*,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3301'     
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
c------------------------------------------------IN3302
      subroutine INOUTLOCAL3302     
      integer, parameter :: N = 16,M=8,K=8,NL=1100     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,*,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3302'     
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


C ---------------------------------------------IN3303
      subroutine INOUTLOCAL3303     
      integer, parameter :: N = 16,M=8,K=8,NL=1200     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,*,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3303'     
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

C ---------------------------------------------IN3304
      subroutine INOUTLOCAL3304     
      integer, parameter :: N = 16,M=8,K=8,NL=1300     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,*,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3304'     
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
C ---------------------------------------------IN3305
      subroutine INOUTLOCAL3305     
      integer, parameter :: N = 16,M=8,K=8,NL=1600     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,*,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3305'     
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


C ---------------------------------------------IN3306
      subroutine INOUTLOCAL3306     
      integer, parameter :: N = 16,M=8,K=8,NL=1700     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,*,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3306'     
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

C ---------------------------------------------IN3307
      subroutine INOUTLOCAL3307     
      integer, parameter :: N = 16,M=8,K=8,NL=1800     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,*,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3307'     
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

     
C ---------------------------------------------IN3308
      subroutine INOUTLOCAL3308     
      integer, parameter :: N = 16,M=8,K=8,NL=1900     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,*,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3308'     
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

     
C ---------------------------------------------IN3309
      subroutine INOUTLOCAL3309     
      integer, parameter :: N = 16,M=8,K=8,NL=2000     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,*,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3309'     
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
C ---------------------------------------------IN3310
      subroutine INOUTLOCAL3310     
      integer, parameter :: N = 16,M=8,K=8,NL=2100     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,*,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3310'     
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


C ---------------------------------------------IN3311
      subroutine INOUTLOCAL3311     
      integer, parameter :: N = 16,M=8,K=8,NL=2200     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,*,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3311'     
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

C ---------------------------------------------IN3312
      subroutine INOUTLOCAL3312     
      integer, parameter :: N = 16,M=8,K=8,NL=2300     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,*,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3312'     
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
C ---------------------------------------------IN3313
      subroutine INOUTLOCAL3313     
      integer, parameter :: N = 16,M=8,K=8,NL=2400     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,*,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3313'     
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

C  ---------------------------------------------IN3314
      subroutine INOUTLOCAL3314     
      integer, parameter :: N = 16,M=8,K=8,NL=2500     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,*,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3314'     
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


C  ---------------------------------------------IN3315
      subroutine INOUTLOCAL3315     
      integer, parameter :: N = 16,M=8,K=8,NL=2600     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,*,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3315'     
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

C  ---------------------------------------------IN3316
      subroutine INOUTLOCAL3316     
      integer, parameter :: N = 16,M=8,K=8,NL=2700     
      integer, allocatable :: A(:,:,:),B(:,:,:),C(:,:,:)
      integer nloopi,nloopj,nloopii
      character*6 tname 

!dvm$ distribute B(*,*,*)       
!dvm$ align (I,J,II) with B(I,J,II) ::A

      tname='IN3316'     
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
