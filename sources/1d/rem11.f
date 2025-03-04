      program REM11
     
c    TESTING OF THE REMOTE_ACCESS DIRECTIVE AND THE REMOTE_ACCESS CLAUSE'.       
c    DISTRIBUTED ARRAY A(N) OR ELEMENTS OF THIS ARRAY ARE REPLICATED
c    ON ALL PROCESSORS. 

      print *,'===START OF REM11========================'
C --------------------------------------------------
      call rem1101
C --------------------------------------------------
      call rem1102
C --------------------------------------------------
      call rem1103
C -------------------------------------------------
      call rem1104
C -------------------------------------------------
      call rem1105
C -------------------------------------------------
      call rem1106
C --------------------------------------------------
      call rem1107
C --------------------------------------------------
      call rem1108
C --------------------------------------------------
      call rem1109
C -------------------------------------------------
      call rem1110
C -------------------------------------------------
      call rem1111
C -------------------------------------------------
      call rem1112
C ------------------------------------------------- 

C
C
      print *,'=== END OF REM11 ========================= '    
      end
C ---------------------------------------------REM1101
      subroutine REM1101
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A

      tname='REM1101'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL
!dvm$ region out(A)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
!dvm$ end region
!dvm$ get_actual(A(1))
!dvm$ remote_access (A(1))
      ib=A(1)

      if (ib .eq.C(1)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,C)

      end
C ---------------------------------------------REM1102
      subroutine REM1102
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A

      tname='REM1102'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL
!dvm$ region out(A)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ end region
!dvm$ get_actual(A(N))
!dvm$ remote_access (A(N))
      ib=A(N)               
      if (ib .eq.C(N)) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate (A,B,C)

      end     
 
C ----------------------------------------REM1103
      subroutine REM1103
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A

      tname='REM1103'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ region out(A)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ end region
!dvm$ get_actual(A(N/2))
!dvm$ remote_access (A(N/2))
      ib=A(N/2)               
      if (ib .eq.C(N/2)) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate (A,B,C)

      end      
C ----------------------------------------REM1104
      subroutine REM1104
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:),D(:)
      integer nloop,isumc,isuma 
      character*7 tname
                
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A

      tname='REM1104'
      allocate (B(N),A(N),C(N),D(N))
      isumc=0
      isuma=0    
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ region out(A)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo

!dvm$ end region
      do i=1,N         
!dvm$ get_actual(A(i))
!dvm$   remote_access (A(i))
        D(i)=A(i)
        isumc=isumc+C(i)
        isuma=isuma+D(i)
      enddo
      if (isumc .eq.isuma) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate (A,B,C,D)

      end           
 
C ----------------------------------------REM1105
      subroutine REM1105
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:),D(:)
      integer nloop,isumc,isuma 
      character*7 tname
                
!dvm$ distribute B(BLOCK)     

!dvm$ align (I) with B(I) ::A

      tname='REM1105'
      allocate (B(N),A(N),C(N),D(N))
      isumc=0
      isuma=0    
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ region out(A)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo

!dvm$ end region
!dvm$ get_actual(A)
      do i=1,N         
!dvm$   remote_access (A(:))
        D(i)=A(i)
        isumc=isumc+C(i)
        isuma=isuma+D(i)
      enddo
      if (isumc .eq.isuma) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate (A,B,C,D)

      end           
 
C ----------------------------------------REM1106
      subroutine REM1106
      integer, parameter :: N = 16,NL=1000     
      integer, allocatable :: A(:),B(:),C(:),D(:)
      integer nloop,isumc,isuma 
      character*7 tname
                
!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A

      tname='REM1106'
      allocate (B(N),A(N),C(N),D(N))
      isumc=0
      isuma=0    
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ region out(A)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo

!dvm$ end region
      kk=2
      kk1=3                                               

      do i=1,N/kk-kk1
!dvm$ get_actual(A(kk*i+kk1))
!dvm$  remote_access (A(kk*i+kk1))
       D(i)=A(kk*i+kk1)
       isumc=isumc+C(kk*i+kk1)
       isuma=isuma+D(i)
      enddo
      if (isumc .eq.isuma) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,C,D)

      end           
C ---------------------------------------------REM1107
      subroutine REM1107
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A

      tname='REM1107'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ region local(A, B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),remote_access(A(1))
      do i=1,N
         B(i) = A(1)
      enddo

!dvm$ parallel (i) on A(i), reduction( min( nloop ) )
      do i=1,N
          if (B(i).ne.C(1)) nloop=min(nloop, i);
      enddo
!dvm$ end region
!dvm$ get_actual(nloop)

      if (nloop .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,C)

      end

C ---------------------------------------------REM1108
      subroutine REM1108
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A

      tname='REM1108'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),remote_access(A(N))
      do i=1,N
         B(i) = A(N)
      enddo

!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=1,N
          if (B(i).ne.C(N)) nloop=min(nloop,i)
      enddo
!dvm$ end region
!dvm$ get_actual(nloop)

      if (nloop .eq.NL) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,B,C)

      end
C ---------------------------------------------REM1109

      subroutine REM1109
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(BLOCK)   
!dvm$ align (I) with B(I) ::A

      tname='REM1109'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),remote_access(A(N/2))
      do i=1,N
         B(i) = A(N/2)
      enddo

!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=1,N
          if (B(i).ne.C(N/2)) nloop=min(nloop,i)
      enddo
!dvm$ end region
!dvm$ get_actual(nloop)

      if (nloop .eq.NL) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate (A,B,C)

      end
C ---------------------------------------------REM1110

      subroutine REM1110
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A

      tname='REM1110'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),remote_access(A)
      do i=1,N
         B(i) = A(i)
      enddo

!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=1,N
          if (B(i).ne.C(i)) nloop=min(nloop,i)
      enddo
!dvm$ end region
!dvm$ get_actual(nloop)

               
      if (nloop .eq.NL) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate (A,B,C)

      end

C ---------------------------------------------REM1111
      subroutine REM1111
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A

      tname='REM1111'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),remote_access(A(i))
      do i=1,N
         B(i) = A(i)
      enddo

!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=1,N
          if (B(i).ne.C(i)) nloop=min(nloop,i)
      enddo
!dvm$ end region
!dvm$ get_actual(nloop)
      if (nloop .eq.NL) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate (A,B,C)

      end


C ---------------------------------------------REM1112
      subroutine REM1112
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(BLOCK)   
!dvm$ align (I) with B(I) ::A

      tname='REM1112'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL
      kk=2
      kk1=3         
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo

!dvm$ parallel (i) on B(i),remote_access(A(kk*i+kk1))
      do i=1,N/kk-kk1
         B(i) = A(kk*i+kk1)
      enddo

!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=1,N/kk-kk1
          if (B(i).ne.C(kk*i+kk1)) nloop=min(nloop,i)
      enddo
!dvm$ end region
!dvm$ get_actual(nloop)
               
      if (nloop .eq.NL) then     
         call ansyes(tname)
      else
         call ansno(tname)
      endif 
      deallocate (A,B,C)

      end

C -----------------------------------------------         
      subroutine serial1(AR,N,NL)
      integer AR(N)
      integer NL 
      do i=1,N
        AR(i) = NL+i
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
