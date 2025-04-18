      program PARALLELNoOn3

c    TESTING parallel CLAUSE .       

      print *,'===START OF parallelNoOn3========================'
C --------------------------------------------------
c 11  PARALLEL , REDUCTION
      call parallelNoOn31
C --------------------------------------------------
c 12  PARALLEL, PRIVATE, REDUCTION
      call parallelNoOn32
C --------------------------------------------------
c 13  PARALLEL, ACROSS , TIE, REDUCTION
      call parallelNoOn33
C --------------------------------------------------
c 14  PARALLEL, ACROSS, TIE, REDUCTION
      call parallelNoOn34
C --------------------------------------------------
      print *,'=== END OF parallelNoOn3 ========================= '    
      end

C ---------------------------------------------parallelNoOn31
      subroutine parallelNoOn31
      integer, parameter :: N = 10,  ER=10000
      character*14:: tname='parallelNoOn31' 
      integer, allocatable :: A(:,:,:),B(:,:,:),AS(:,:,:),BS(:,:,:)
      integer:: erri=ER
                      
      allocate (B(N,N,N),A(N,N,N),BS(N,N,N),AS(N,N,N))
      do k=1,N
      do j=1,N
      do i=1,N
        if(i == N .or. i==1 .or. j==N .or. j==1 .or.k==N .or.k==1) then
           AS(i,j,k) = 0
           BS(i,j,k) = 0
        else
           AS(i,j,k) = i+j+k
        endif
      enddo
      enddo
      enddo
      do k=2,N-1
      do j=2,N-1
      do i=2,N-1
         BS(i,j,k) = AS(i-1,j,k)+AS(i+1,j,k)+AS(i,j+1,k)+AS(i,j-1,k)
     &             + AS(i,j,k-1) + AS(i,j,k+1)
      enddo
      enddo
      enddo
!dvm$ actual(erri)
!dvm$ region local(A,B)
!dvm$ parallel (k,j,i)
      do k=1,N
      do j=1,N
      do i=1,N
        if(i == N .or. i==1 .or. j==N .or. j==1 .or.k==N .or.k==1) then
           A(i,j,k) = 0
           B(i,j,k) = 0
        else
           A(i,j,k) = i+j+k
        endif
      enddo
      enddo
      enddo
!dvm$ parallel (k,j,i)
      do k=2,N-1
      do j=2,N-1
      do i=2,N-1
        B(i,j,k) =  A(i-1,j,k)+A(i+1,j,k)+A(i,j+1,k)+A(i,j-1,k)
     &          + A(i,j,k-1) + A(i,j,k+1)
      enddo
      enddo
      enddo
!dvm$ parallel (k,j,i), reduction( min( erri ) )
      do k=1,N
      do j=1,N
      do i=1,N
        if(B(i,j,k) .ne. BS(i,j,k)) 
     &      erri =  min(erri, ABS(B(i,j,k)-BS(i,j,k)))
      enddo
      enddo
      enddo
!dvm$ end region
   
!dvm$ get_actual(erri) 
      
      if (erri .eq. ER) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,AS,BS)
      
      end

C ---------------------------------------------parallelNoOn32
      subroutine parallelNoOn32
      integer, parameter :: N = 10,  ER=10000
      character*14:: tname='parallelNoOn32' 
      integer, allocatable :: A(:,:,:),B(:,:,:),AS(:,:,:),BS(:,:,:)
      integer:: erri=ER
                      
      allocate (B(N,N,N),A(N,N,N),BS(N,N,N),AS(N,N,N))
      do k=1,N
      do j=1,N
      do i=1,N
        if(i == N .or. i==1 .or. j==N .or. j==1 .or.k==N .or.k==1) then
           AS(i,j,k) = 0
           BS(i,j,k) = 0
        else
           AS(i,j,k) = i+j+k
        endif
      enddo
      enddo
      enddo
      do k=2,N-1
      do j=2,N-1
      do i=2,N-1
         BS(i,j,k) = AS(i-1,j,k)+AS(i+1,j,k)+AS(i,j+1,k)+AS(i,j-1,k)
     &             + AS(i,j,k-1) + AS(i,j,k+1)
      enddo
      enddo
      enddo
!dvm$ actual(erri)
!dvm$ region local(A,B)
!dvm$ parallel (k,j,i), private(i0) 
      do k=1,N
      do j=1,N
      do i=1,N
        if(i == N .or. i==1 .or. j==N .or. j==1 .or.k==N .or.k==1) then
           i0 = 0
           A(i,j,k) = i0
           B(i,j,k) = i0
        else
           A(i,j,k) = i+j+k
        endif
      enddo
      enddo
      enddo
!dvm$ parallel (k,j,i),private(ia1,ja1,ka1)
      do k=2,N-1
      do j=2,N-1
      do i=2,N-1
        ia1 = A(i-1,j,k)
        ja1 = A(i,j-1,k)
        ka1 = A(i,j,k-1)
        B(i,j,k) = ia1+A(i+1,j,k)+A(i,j+1,k)+ ja1
     &          + ka1 + A(i,j,k+1)
      enddo
      enddo
      enddo
!dvm$ parallel (k,j,i), reduction( min( erri ) )
      do k=1,N
      do j=1,N
      do i=1,N
        if(B(i,j,k) .ne. BS(i,j,k)) 
     &      erri =  min(erri, ABS(B(i,j,k)-BS(i,j,k)))
      enddo
      enddo
      enddo
!dvm$ end region
   
!dvm$ get_actual(erri) 
      
      if (erri .eq. ER) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,AS,BS)
      
      end

C ---------------------------------------------parallelNoOn33
      subroutine parallelNoOn33
      integer, parameter :: N = 10,  ER=10000
      character*14:: tname='parallelNoOn33' 
      integer, allocatable :: A(:,:,:),B(:,:,:),AS(:,:,:),BS(:,:,:)
      integer:: erri=ER
                      
      allocate (B(N,N,N),A(N,N,N),BS(N,N,N),AS(N,N,N))
      do k=1,N
      do j=1,N
      do i=1,N
        if(i == N .or. i==1 .or. j==N .or. j==1 .or.k==N .or.k==1) then
           AS(i,j,k) = 0
           BS(i,j,k) = 0
        else
           AS(i,j,k) = i+j+k
           BS(i,j,k) = i+j+k+1
        endif
      enddo
      enddo
      enddo
      do k=2,N-2
      do j=2,N-1
      do i=2,N-1
         AS(i,j,k) = AS(i-1,j,k)+AS(i+1,j,k)+AS(i,j+1,k)+AS(i,j-1,k)
     &             + AS(i,j,k-1) + AS(i,j,k+1)
         BS(i,j,k) = BS(i,j,k) + BS(i-1,j,k) + BS(i,j,k+2)
      enddo
      enddo
      enddo
!dvm$ actual(erri)
!dvm$ region local(A,B)
!dvm$ parallel (k,j,i)
      do k=1,N
      do j=1,N
      do i=1,N
        if(i == N .or. i==1 .or. j==N .or. j==1 .or.k==N .or.k==1) then
           A(i,j,k) = 0
           B(i,j,k) = 0
        else
           A(i,j,k) = i+j+k
           B(i,j,k) = i+j+k+1
        endif
      enddo
      enddo
      enddo
!dvm$ parallel (k,j,i), tie(A(i,j,k),B(i,j,k)), 
!dvm$&     across(A(1:1,1:1,1:1),B(1:0,0:0,0:2))
      do k=2,N-2
      do j=2,N-1
      do i=2,N-1

         A(i,j,k) =  A(i-1,j,k)+A(i+1,j,k)+A(i,j+1,k)+A(i,j-1,k)
     &          + A(i,j,k-1) + A(i,j,k+1)
         B(i,j,k) = B(i,j,k) + B(i-1,j,k) + B(i,j,k+2)
      enddo
      enddo
      enddo
!dvm$ parallel (k,j,i), reduction( min( erri ) )
      do k=1,N
      do j=1,N
      do i=1,N
        if(A(i,j,k) .ne. AS(i,j,k)) 
     &      erri =  min(erri, ABS(A(i,j,k)-AS(i,j,k)))
        if(B(i,j,k) .ne. BS(i,j,k)) 
     &      erri =  min(erri, ABS(B(i,j,k)-BS(i,j,k)))
      enddo
      enddo
      enddo
!dvm$ end region
   
!dvm$ get_actual(erri) 
      
      if (erri .eq. ER) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,AS,BS)
      
      end
C ---------------------------------------------parallelNoOn34
      subroutine parallelNoOn34
      integer, parameter :: N = 10,  ER=10000
      character*14:: tname='parallelNoOn34' 
      integer, allocatable :: A(:,:,:),B(:,:,:),AS(:,:,:),BS(:,:,:)
      integer:: erri=ER
                      
      allocate (B(N,N,N),A(N,N,N),BS(N,N,N),AS(N,N,N))
      do k=1,N
      do j=1,N
      do i=1,N
        if(i == N .or. i==1 .or. j==N .or. j==1 .or.k==N .or.k==1) then
           AS(i,j,k) = 0
           BS(i,j,k) = 0
        else
           AS(i,j,k) = i+j+k
           BS(i,j,k) = i+j+k+1
        endif
      enddo
      enddo
      enddo
      do k=2,N-1
      do j=2,N-1
      do i=2,N-1
         AS(i,j,k) = AS(i-1,j,k)+AS(i+1,j,k)+AS(i,j+1,k)+AS(i,j-1,k)
     &             + AS(i,j,k-1) + AS(i,j,k+1)
         BS(i+1,j,k-1) = BS(i+1,j,k) + BS(i-1,j,k-1) + BS(i+1,j,k+1)
      enddo
      enddo
      enddo
!dvm$ actual(erri)
!dvm$ region local(A,B)
!dvm$ parallel (k,j,i)
      do k=1,N
      do j=1,N
      do i=1,N
        if(i == N .or. i==1 .or. j==N .or. j==1 .or.k==N .or.k==1) then
           A(i,j,k) = 0
           B(i,j,k) = 0
        else
           A(i,j,k) = i+j+k
           B(i,j,k) = i+j+k+1
        endif
      enddo
      enddo
      enddo
!dvm$ parallel (k,j,i), tie(A(i,j,k),B(i+1,j,k-1)), 
!dvm$&     across(A(1:1,1:1,1:1),B(2:0,0:0,0:2))
      do k=2,N-1
      do j=2,N-1
      do i=2,N-1

         A(i,j,k) =  A(i-1,j,k)+A(i+1,j,k)+A(i,j+1,k)+A(i,j-1,k)
     &          + A(i,j,k-1) + A(i,j,k+1)
         B(i+1,j,k-1) = B(i+1,j,k) + B(i-1,j,k-1) + B(i+1,j,k+1)
      enddo
      enddo
      enddo
!dvm$ parallel (k,j,i), reduction( min( erri ) )
      do k=1,N
      do j=1,N
      do i=1,N
        if(A(i,j,k) .ne. AS(i,j,k)) 
     &      erri =  min(erri, ABS(A(i,j,k)-AS(i,j,k)))
        if(B(i,j,k) .ne. BS(i,j,k)) 
     &      erri =  min(erri, ABS(B(i,j,k)-BS(i,j,k)))
      enddo
      enddo
      enddo
!dvm$ end region
   
!dvm$ get_actual(erri) 
      
      if (erri .eq. ER) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,AS,BS)
      
      end

C -------------------------------------------------

      subroutine ansyes(name)
      character*14 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*14 name
      print *,name,'  -  ***error'
      end
