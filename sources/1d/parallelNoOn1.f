      program PARALLELNoOn1

c    TESTING parallel CLAUSE .       

      print *,'===START OF parallelNoOn1========================'
C --------------------------------------------------
c 11  PARALLEL , REDUCTION
      call parallelNoOn11
C --------------------------------------------------
c 12  PARALLEL, PRIVATE, REDUCTION
      call parallelNoOn12
C --------------------------------------------------
c 13  PARALLEL, ACROSS , TIE, REDUCTION
      call parallelNoOn13
C --------------------------------------------------
c 14  PARALLEL, ACROSS, TIE, REDUCTION
      call parallelNoOn14
C --------------------------------------------------
      print *,'=== END OF parallelNoOn1 ========================= '    
      end

C ---------------------------------------------parallelNoOn11
      subroutine parallelNoOn11
      integer, parameter :: N = 100,  ER=10000
      character*14:: tname='parallelNoOn11' 
      integer, allocatable :: A(:),B(:),AS(:),BS(:)
      integer:: erri=ER
                      
      allocate (B(N),A(N),BS(N),AS(N))


      do i=1,N
        if(i == N .or. i==1) then
           AS(i) = 0
        else
           AS(i) = 1+i
        endif
      enddo

      do i=2,N-1
         BS(i) = AS(i-1)+AS(i+1)
      enddo

!dvm$ actual(erri)
!dvm$ region local(A,B)
!dvm$ parallel (i)
      do i=1,N
        if(i == N .or. i==1) then
           A(i) = 0
        else
           A(i) = 1+i
        endif

      enddo

!dvm$ parallel (i)
      do i=2,N-1
         B(i) = A(i-1)+A(i+1)
      enddo
  
!dvm$ parallel (i), reduction( min( erri ) )
      do i=2,N-1
        if(B(i) .ne. BS(i)) erri =  min(erri, ABS(B(i)-BS(i)))
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

C ---------------------------------------------parallelNoOn12
      subroutine parallelNoOn12
      integer, parameter :: N = 100,  ER=10000
      character*14:: tname='parallelNoOn12' 
      integer, allocatable :: A(:),B(:),AS(:),BS(:)
      integer:: erri=ER
                      
      allocate (B(N),A(N),BS(N),AS(N))


      do i=1,N
        if(i == N .or. i==1) then
           AS(i) = 0
        else
           AS(i) = 1+i
        endif
      enddo

      do i=2,N-1
         BS(i) = AS(i-1)+AS(i+1)
      enddo

!dvm$ actual(erri)
!dvm$ region local(A,B)
!dvm$ parallel (i)
      do i=1,N
        if(i == N .or. i==1) then
           A(i) = 0
        else
           A(i) = 1+i
        endif

      enddo

!dvm$ parallel (i), private(IA1,IA2)
      do i=2,N-1
         IA1 = A(i-1)
         IA2 = A(i+1)
         B(i) = IA1+IA2
      enddo
  
!dvm$ parallel (i), reduction( min( erri ) )
      do i=2,N-1
        if(B(i) .ne. BS(i)) erri =  min(erri, ABS(B(i)-BS(i)))
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


C ---------------------------------------------parallelNoOn13
      subroutine parallelNoOn13
      integer, parameter :: N = 100,  ER=10000
      character*14:: tname='parallelNoOn13' 
      integer, allocatable :: A(:),AS(:)
      integer:: erri=ER
                      
      allocate (A(N),AS(N))


      do i=1,N
        if(i == N .or. i==1) then
           AS(i) = 0
        else
           AS(i) = 1+i
        endif
      enddo

      do i=2,N-1
         AS(i) = AS(i-1)+AS(i+1)
      enddo

!dvm$ actual(erri)
!dvm$ region local(A)
!dvm$ parallel (i)
      do i=1,N
        if(i == N .or. i==1) then
           A(i) = 0
        else
           A(i) = 1+i
        endif

      enddo

!dvm$ parallel (i), across(A(1:1)), tie(A(i))
      do i=2,N-1
         A(i) = A(i-1)+A(i+1)
      enddo
  
!dvm$ parallel (i), reduction( min( erri ) )
      do i=2,N-1
        if(A(i) .ne. AS(i)) erri =  min(erri, ABS(A(i)-AS(i)))
      enddo
!dvm$ end region   
!dvm$ get_actual(erri) 
      
      if (erri .eq. ER) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,AS)

      end

C ---------------------------------------------parallelNoOn14
      subroutine parallelNoOn14
      integer, parameter :: N = 100,  ER=10000
      character*14:: tname='parallelNoOn14' 
      integer, allocatable :: A(:),B(:),AS(:),BS(:)
      integer:: erri=ER
                      
      allocate (B(N),A(N),BS(N),AS(N))


      do i=1,N
        if(i == N .or. i==1) then
           AS(i) = 0
           BS(i) = 0
        else
           AS(i) = 1+i
           BS(i) = i
        endif
      enddo

      do i=3,N-1
         AS(i) = AS(i-1)+AS(i+1)
         BS(i) = BS(i-2)
      enddo

!dvm$ actual(erri)
!dvm$ region local(A,B)
!dvm$ parallel (i)
      do i=1,N
        if(i == N .or. i==1) then
           A(i) = 0
           B(i) = 0
        else
           A(i) = 1+i
           B(i) = i
        endif

      enddo

!dvm$ parallel (i), across(A(1:1),B(2:0)), tie(A(i),B(i)) 
      do i=3,N-1
         A(i) = A(i-1)+A(i+1)
         B(i) = B(i-2)
      enddo
  
!dvm$ parallel (i), reduction( min( erri ) )
      do i=2,N-1
        if(A(i) .ne. AS(i)) erri =  min(erri, ABS(A(i)-AS(i)))
        if(B(i) .ne. BS(i)) erri =  min(erri, ABS(B(i)-BS(i)))
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
