      program PARALLELNoOn2

c    TESTING parallel CLAUSE .       

      print *,'===START OF parallelNoOn2========================'
C --------------------------------------------------
c 11  PARALLEL , REDUCTION
      call parallelNoOn21
C --------------------------------------------------
c 12  PARALLEL, PRIVATE, REDUCTION
      call parallelNoOn22
C --------------------------------------------------
c 13  PARALLEL, ACROSS , TIE, REDUCTION
      call parallelNoOn23
C --------------------------------------------------
c 14  PARALLEL, ACROSS, TIE, REDUCTION
      call parallelNoOn24
C --------------------------------------------------
      print *,'=== END OF parallelNoOn2 ========================= '    
      end

C ---------------------------------------------parallelNoOn21
      subroutine parallelNoOn21
      integer, parameter :: N = 100,  ER=10000
      character*14:: tname='parallelNoOn21' 
      integer, allocatable :: A(:,:),B(:,:),AS(:,:),BS(:,:)
      integer:: erri=ER
                      
      allocate (B(N,N),A(N,N),BS(N,N),AS(N,N))

      do j=1,N
      do i=1,N
        if(i == N .or. i==1 .or. j==N .or. j==1) then
           AS(i,j) = 0
        else
           AS(i,j) = i+j
        endif
      enddo
      enddo

      do j=2,N-1
      do i=2,N-1
         BS(i,j) = AS(i-1,j)+AS(i+1,j)+AS(i,j+1)+AS(i,j-1)
      enddo
      enddo

!dvm$ actual(erri)
!dvm$ region local(A,B)
!dvm$ parallel (j,i)
      do j=1,N
      do i=1,N
        if(i == N .or. i==1 .or. j==N .or. j==1) then
           A(i,j) = 0
        else
           A(i,j) = i+j
        endif
      enddo
      enddo

!dvm$ parallel (j,i)
      do j=2,N-1
      do i=2,N-1
        B(i,j) = A(i-1,j)+A(i+1,j)+A(i,j+1)+A(i,j-1)
      enddo
      enddo
  
!dvm$ parallel (j,i), reduction( min( erri ) )
      do j=2,N-1
      do i=2,N-1
        if(B(i,j) .ne. BS(i,j)) erri =  min(erri, ABS(B(i,j)-BS(i,j)))
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

C ---------------------------------------------parallelNoOn22
      subroutine parallelNoOn22
      integer, parameter :: N = 100,  ER=10000
      character*14:: tname='parallelNoOn22' 
      integer, allocatable :: A(:,:),B(:,:),AS(:,:),BS(:,:)
      integer:: erri=ER
                      
      allocate (B(N,N),A(N,N),BS(N,N),AS(N,N))

      do j=1,N
      do i=1,N
        if(i == N .or. i==1 .or. j==N .or. j==1) then
           AS(i,j) = 0
        else
           AS(i,j) = i+j
        endif
      enddo
      enddo

      do j=2,N-1
      do i=2,N-1
         BS(i,j) = AS(i-1,j)+AS(i+1,j)+AS(i,j+1)+AS(i,j-1)
      enddo
      enddo

!dvm$ actual(erri)
!dvm$ region local(A,B)
!dvm$ parallel (j,i), private(ij)
      do j=1,N
      do i=1,N
        ij = i+j
        if(i == N .or. i==1 .or. j==N .or. j==1) then
           A(i,j) = 0
        else
           A(i,j) = ij
        endif

      enddo
      enddo

!dvm$ parallel (j,i), private(iai,iaj)
      do j=2,N-1
      do i=2,N-1
        iai = A(i-1,j)+A(i+1,j)
        iaj = A(i,j+1)+A(i,j-1)
        B(i,j) = iai+iaj
      enddo
      enddo
  
!dvm$ parallel (j,i), reduction( min( erri ) )
      do j=2,N-1
      do i=2,N-1
        if(B(i,j) .ne. BS(i,j)) erri =  min(erri, ABS(B(i,j)-BS(i,j)))
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

C ---------------------------------------------parallelNoOn23
      subroutine parallelNoOn23
      integer, parameter :: N = 100,  ER=10000
      character*14:: tname='parallelNoOn23' 
      integer, allocatable :: A(:,:),B(:,:),AS(:,:),BS(:,:)
      integer:: erri=ER
                      
      allocate (B(N,N),A(N,N),BS(N,N),AS(N,N))

      do j=1,N
      do i=1,N
        if(i == N .or. i==1 .or. j==N .or. j==1) then
           AS(i,j) = 0
           BS(i,j) = 0
        else
           AS(i,j) = i+j
           BS(i,j) = i+j+2
        endif
      enddo
      enddo

      do j=2,N-1
      do i=2,N-2
         AS(i,j) = AS(i-1,j)+AS(i+1,j)+AS(i,j+1)+AS(i,j-1)
         BS(i,j) = BS(i-1,j)+BS(i+2,j)+AS(i,j)
      enddo
      enddo

!dvm$ actual(erri)
!dvm$ region local(A,B)
!dvm$ parallel (j,i)
      do j=1,N
      do i=1,N
        if(i == N .or. i==1 .or. j==N .or. j==1) then
           A(i,j) = 0
           B(i,j) = 0
        else
           A(i,j) = i+j
           B(i,j) = i+j+2
        endif

      enddo
      enddo

!dvm$ parallel (j,i), across(A(1:1,1:1),B(1:2,0:0)),tie(A(i,j),B(i,j))
      do j=2,N-1
      do i=2,N-2
        A(i,j) = A(i-1,j)+A(i+1,j)+A(i,j+1)+A(i,j-1)
        B(i,j) = B(i-1,j)+B(i+2,j)+A(i,j)
      enddo
      enddo
  
!dvm$ parallel (j,i), reduction( min( erri ) )
      do j=1,N
      do i=1,N
        if(A(i,j) .ne. AS(i,j)) erri =  min(erri, ABS(A(i,j)-AS(i,j)))
        if(B(i,j) .ne. BS(i,j)) erri =  min(erri, ABS(B(i,j)-BS(i,j)))
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

C ---------------------------------------------parallelNoOn24
      subroutine parallelNoOn24
      integer, parameter :: N = 100,  ER=10000
      character*14:: tname='parallelNoOn24' 
      integer, allocatable :: A(:,:),B(:,:),AS(:,:),BS(:,:)
      integer:: erri=ER
                      
      allocate (B(N,N),A(N,N),BS(N,N),AS(N,N))

      do j=1,N
      do i=1,N
        if(i == N .or. i==1 .or. j==N .or. j==1) then
           AS(i,j) = 0
           BS(i,j) = 0
        else
           AS(i,j) = i+j
           BS(i,j) = i+j+2
        endif
      enddo
      enddo

      do j=2,N-1
      do i=2,N-1
         AS(i,j) = AS(i-1,j)+AS(i+1,j)+AS(i,j+1)+AS(i,j-1)
         BS(i+1,j) = BS(i-1,j)+BS(i+1,j)+AS(i,j)
      enddo
      enddo

!dvm$ actual(erri)
!dvm$ region local(A,B)
!dvm$ parallel (j,i)
      do j=1,N
      do i=1,N
        if(i == N .or. i==1 .or. j==N .or. j==1) then
           A(i,j) = 0
           B(i,j) = 0
        else
           A(i,j) = i+j
           B(i,j) = i+j+2
        endif

      enddo
      enddo

!dvm$ parallel (j,i),across(A(1:1,1:1),B(2:0,0:0)),tie(A(i,j),B(i+1,j))
      do j=2,N-1
      do i=2,N-1
        A(i,j) = A(i-1,j)+A(i+1,j)+A(i,j+1)+A(i,j-1)
        B(i+1,j) = B(i-1,j)+B(i+1,j)+A(i,j)
      enddo
      enddo
  
!dvm$ parallel (j,i), reduction( min( erri ) )
      do j=1,N
      do i=1,N
        if(A(i,j) .ne. AS(i,j)) erri =  min(erri, ABS(A(i,j)-AS(i,j)))
        if(B(i,j) .ne. BS(i,j)) erri =  min(erri, ABS(B(i,j)-BS(i,j)))
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
