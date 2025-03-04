      program REM12
     
c    TESTING OF THE REMOTE_ACCESS DIRECTIVE AND THE REMOTE_ACCESS CLAUSE'.       
c    DISTRIBUTED ARRAY A(N) OR ELEMENTS OF THIS ARRAY ARE REPLICATED
c    ON ALL PROCESSORS. 

      print *,'===START OF REM12========================'
C --------------------------------------------------
      call rem1201
C --------------------------------------------------
      call rem1202
C --------------------------------------------------
      call rem1203
C -------------------------------------------------
      call rem1204
C -------------------------------------------------
      call rem1205
C -------------------------------------------------
      call rem1206
C --------------------------------------------------
      call rem1207
C --------------------------------------------------
      call rem1208
C --------------------------------------------------
      call rem1209
C -------------------------------------------------
      call rem1210
C -------------------------------------------------
      call rem1211
C -------------------------------------------------
      call rem1212
C ------------------------------------------------- 

C
C
      print *,'=== END OF REM12 ========================= '    
      end
C ---------------------------------------------REM1201
      subroutine REM1201
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(*)     
!dvm$ align (I) with B(I) ::A

      tname='REM1201'
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
C ---------------------------------------------REM1202
      subroutine REM1202
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(*)    
!dvm$ align (I) with B(I) ::A

      tname='REM1202'
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
 
C ----------------------------------------REM1203
      subroutine REM1203
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(*)     
!dvm$ align (I) with B(I) ::A

      tname='REM1203'
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
C ----------------------------------------REM1204
      subroutine REM1204
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:),D(:)
      integer nloop,isumc,isuma 
      character*7 tname
                
!dvm$ distribute B(*)    
!dvm$ align (I) with B(I) ::A

      tname='REM1204'
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
 
C ----------------------------------------REM1205
      subroutine REM1205
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:),D(:)
      integer nloop,isumc,isuma 
      character*7 tname
                
!dvm$ distribute B(*)     

!dvm$ align (I) with B(I) ::A

      tname='REM1205'
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
 
C ----------------------------------------REM1206
      subroutine REM1206
      integer, parameter :: N = 16,NL=1000     
      integer, allocatable :: A(:),B(:),C(:),D(:)
      integer nloop,isumc,isuma 
      character*7 tname
                
!dvm$ distribute B(*)     
!dvm$ align (I) with B(I) ::A

      tname='REM1206'
      allocate (B(N),A(N),C(N),D(N))
      isumc=0
      isuma=0    
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

      kk=2
      kk1=3                                               
!dvm$ region out(A)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo

!dvm$ end region
!dvm$ get_actual(A)

      do i=1,N/kk-kk1
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
C ---------------------------------------------REM1207
      subroutine REM1207
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(*)    
!dvm$ align (I) with B(I) ::A

      tname='REM1207'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ region local(A,B)
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
          if (B(i).ne.C(1)) nloop=min(nloop,i)
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

C ---------------------------------------------REM1208
      subroutine REM1208
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(*)    
!dvm$ align (I) with B(I) ::A

      tname='REM1208'
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
          if (B(i).ne.C(N)) nloop=min(nloop, i)
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
C ---------------------------------------------REM1209

      subroutine REM1209
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(*)   
!dvm$ align (I) with B(I) ::A

      tname='REM1209'
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
          if (B(i).ne.C(N/2)) nloop=min(nloop, i)
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
C ---------------------------------------------REM1210

      subroutine REM1210
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(*)    
!dvm$ align (I) with B(I) ::A

      tname='REM1210'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
c !dvm$ parallel (i) on B(i),remote_access(A)
!dvm$ parallel (i) on B(i),remote_access(A(:))
      do i=1,N
         B(i) = A(i)
      enddo

!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=1,N
          if (B(i).ne.C(i)) nloop=min(nloop, i)
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

C ---------------------------------------------REM1211
      subroutine REM1211
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(*)     
!dvm$ align (I) with B(I) ::A

      tname='REM1211'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),remote_access(A(:))
      do i=1,N
         B(i) = A(i)
      enddo

!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=1,N
          if (B(i).ne.C(i)) nloop=min(nloop, i)
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


C ---------------------------------------------REM1212
      subroutine REM1212
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
      character*7 tname
                
!dvm$ distribute B(*)   
!dvm$ align (I) with B(I) ::A

      tname='REM1212'
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

!dvm$ parallel (i) on B(i),remote_access(A(:))
      do i=1,N/kk-kk1
         B(i) = A(kk*i+kk1)
      enddo

!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=1,N/kk-kk1
          if (B(i).ne.C(kk*i+kk1)) nloop=min(nloop, i)
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
