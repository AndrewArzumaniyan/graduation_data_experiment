      program ACR12
     
c    TESTING OF THE ACROSS CLAUSE'.       
c    DISTRIBUTED ARRAY A(N) IS TO HAVE DIFFERENT 
c    FLOW-DEP-LENGTH ON BOTH SIDES 

      print *,'===START OF ACR12========================'
C --------------------------------------------------
      call acr1201
C --------------------------------------------------
      call acr1202
C --------------------------------------------------
      call acr1203
C -------------------------------------------------
      call acr1204
C -------------------------------------------------
      call acr1205
C -------------------------------------------------
      call acr1206
C --------------------------------------------------
      call acr1207
C --------------------------------------------------
      call acr1208
C --------------------------------------------------
      call acr1209
C -------------------------------------------------
      call acr1210
C -------------------------------------------------

C
      print *,'=== END OF ACR12 ========================= '    
      end
C ---------------------------------------------ACR1201
      subroutine acr1201
     
      integer, parameter :: N = 8, NL=1000
      character*7 tname 
      integer,allocatable::  A(:), C(:)
      integer nloop 
                      
!dvm$ distribute A(*)    

      tname='ACR1201'
      allocate (A(N), C(N))
      nloop=NL
      
      do iloop=0,2	 
      NNL=NL    
      call serial1(C,N,NNL)
      
      do i=2,N-1
         C(i) = C(i-1)+C(i+1)
      enddo
!dvm$ actual (nloop, C)
!dvm$ region

!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               

!dvm$ parallel (i) on A(i),across(A(1:1)),stage(iloop)
      do i=2,N-1
         A(i) = A(i-1)+A(i+1)
      enddo
  
!dvm$ parallel (i) on A(i), reduction( min( nloop ) )
      do i=2,N-1
          if (A(i).ne. C(i)) then
          nloop=min(nloop,i)
          endif
      enddo
!dvm$ end region
!dvm$ get_actual (nloop)
      enddo
	  
      if (nloop .eq.NL) then
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (A, C)
      
      end
C ---------------------------------------------ACR1202     
      subroutine acr1202
      integer, parameter :: N = 16, NL=1000
     
      character*7 tname 
      integer,allocatable:: A(:), C(:)
      integer nloop 
                      
!dvm$ distribute A(*)    

      tname='ACR1202'
      allocate (A(N), C(N))
	  nloop=NL
      
      do iloop=0,2
      NNL=NL    
      call serial1(C,N,NNL)
      do i=1,N-1
         C(i) = C(i)+C(i+1)
      enddo
      
!dvm$ actual (nloop, C)
!dvm$ region

!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo               

!dvm$ parallel (i) on A(i),across(A(0:1)),stage(iloop)
      do i=1,N-1
         A(i) = A(i)+A(i+1)
      enddo
  
!dvm$ parallel (i) on A(i), reduction( min( nloop ) )
      do i=2,N-1
          if (A(i).ne. C(i)) then
          nloop=min(nloop,i)
          endif          
      enddo
!dvm$ end region
!dvm$ get_actual (nloop)
      enddo
	  
      if (nloop .eq.NL) then
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (A, C)
      
      end
C -----------------------------------------ACR1203      
      subroutine acr1203
      integer, parameter :: N = 16, NL=1000
     
      character*7 tname 
      integer,allocatable::  A(:), C(:)
      integer nloop 
                     
!dvm$ distribute A(*)    

      tname='ACR1203'
      allocate (A(N), C(N))
      nloop=NL
  
      do iloop=0,2  
      NNL=NL    
      call serial1(C,N,NNL)
      do i=2,N
         C(i) =C(i)+ C(i-1)
      enddo
      
!dvm$ actual (nloop, C)
!dvm$ region

!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo               

!dvm$ parallel (i) on A(i),across(A(1:0)),stage(iloop)
      do i=2,N
         A(i) =A(i)+ A(i-1)
      enddo
  
!dvm$ parallel (i) on A(i), reduction( min( nloop ) )
      do i=2,N-1
          if (A(i).ne. C(i)) then
          nloop=min(nloop,i)
          endif          
      enddo
!dvm$ end region
!dvm$ get_actual (nloop)
      enddo
	  
      if (nloop .eq.NL) then
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (A, C)     

      end
      
C -------------------------------------------ACR1204   
         
      subroutine acr1204
      integer, parameter :: N = 16, NL=1000 
      character*7 tname 
      integer,allocatable:: A(:), C(:)
      integer nloop 
                    
!dvm$ distribute A(*)    
!dvm$ shadow A(2:2)
  
      tname='ACR1204'
      allocate (A(N), C(N))
	  nloop=NL
      
      do iloop=0,2
      NNL=NL
      call serial1(C,N,NNL)
      do i=3,N-2
         C(i) = C(i-1)+C(i+1)+C(i+2)+C(i-2)
      enddo
      
!dvm$ actual (nloop, C)
!dvm$ region

!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on A(i),across(A(2:2)),stage(iloop)
      do i=3,N-2
         A(i) = A(i-1)+A(i+1)+A(i+2)+A(i-2)
      enddo
  
!dvm$ parallel (i) on A(i), reduction( min( nloop ) )
      do i=3,N-2
          if (A(i).ne. C(i)) then
          nloop=min(nloop,i)
          endif
      enddo
!dvm$ end region
!dvm$ get_actual (nloop)
      enddo
	  
      if (nloop .eq.NL) then
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (A, C)       
      
      end      
C -------------------------------------------ACR1205   
         
      subroutine acr1205
      integer, parameter :: N = 16, NL=1000
     
      character*7 tname 
      integer,allocatable:: A(:),C(:)
      integer nloop 
                    
!dvm$ distribute A(*)   
!dvm$ shadow A(2:2)

      tname='ACR1205'      
      allocate (A(N), C(N))
	  nloop=NL
      
      do iloop=0,2
      NNL=NL    
      call serial1(C,N,NNL)
      do i=2,N-2
         C(i) = C(i+1)+C(i+2)
      enddo
      
!dvm$ actual (nloop, C)
!dvm$ region

!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on A(i),across(A(0:2)),stage(iloop)
      do i=2,N-2
         A(i) = A(i+1)+A(i+2)
      enddo
  
!dvm$ parallel (i) on A(i), reduction( min( nloop ) )
      do i=2,N-2
          if (A(i).ne. C(i)) then
          nloop=min(nloop,i)
          endif          
      enddo
!dvm$ end region
!dvm$ get_actual (nloop)
      enddo
	  
      if (nloop .eq.NL) then
        call ansyes(tname)    
      else
        call ansno(tname)
      endif
      deallocate (A, C)
  
      end   
      
C -------------------------------------------ACR1206   
         
      subroutine acr1206
      integer, parameter :: N = 16, NL=1000
     
      character*7 tname 
      integer,allocatable::  A(:), C(:)
      integer nloop                
!dvm$ distribute A(*)     
!dvm$ shadow A(2:2)

      tname='ACR1206'      
      allocate (A(N), C(N))
	  nloop=NL
      
      do iloop=0,2
      NNL=NL    
      call serial1(C,N,NNL)
      do i=3,N
         C(i) = C(i-1)+C(i-2)
      enddo
      
!dvm$ actual (nloop, C)
!dvm$ region

!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on A(i),across(A(2:0)),stage(iloop)
      do i=3,N
         A(i) = A(i-1)+A(i-2)
      enddo
  
!dvm$ parallel (i) on A(i), reduction( min( nloop ) )
      do i=3,N
          if (A(i).ne. C(i)) then
          nloop=min(nloop,i)
          endif          
      enddo
!dvm$ end region
!dvm$ get_actual (nloop)
      enddo
	  
      if (nloop .eq.NL) then
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (A, C)         
      end   

C -------------------------------------------ACR1207   
         
      subroutine acr1207
      integer, parameter :: N = 16, NL=1000
     
      character*7 tname 
      integer,allocatable::  A(:), C(:)
      integer nloop 
                     
!dvm$ distribute A(*)     
!dvm$ shadow A(3:3)

      tname='ACR1207'      
      allocate (A(N), C(N))
	  nloop=NL
      
      do iloop=0,2
      NNL=NL    
      call serial1(C,N,NNL)
       do i=4,N-3
         C(i) = C(i-1)+C(i+1)+C(i+2)+C(i-2)+C(i-3)+C(i+3)
      enddo
      
!dvm$ actual (nloop, C)
!dvm$ region

!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on A(i),across(A(3:3)),stage(iloop)
      do i=4,N-3
         A(i) = A(i-1)+A(i+1)+A(i+2)+A(i-2)+A(i-3)+A(i+3)
      enddo
  
!dvm$ parallel (i) on A(i), reduction( min( nloop ) )
      do i=4,N-3
          if (A(i).ne. C(i)) then
          nloop=min(nloop,i)
          endif
      enddo
!dvm$ end region
!dvm$ get_actual (nloop)
      enddo
	  
      if (nloop .eq.NL) then
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (A, C)
         
      end
C -------------------------------------------ACR1208  
         
      subroutine acr1208
      integer, parameter :: N = 24, NL=1000
     
      character*7 tname 
      integer,allocatable::  A(:), C(:)
      integer nloop 
                    
!dvm$ distribute A(*)     
!dvm$ shadow A(3:3)

      tname='ACR1208'
      allocate (A(N), C(N))
 	  nloop=NL
      
      do iloop=0,2   
      NNL=NL    
      call serial1(C,N,NNL)
       do i=2,N-3
         C(i) = C(i+1)+C(i+2)+C(i+3)
      enddo
      
!dvm$ actual (nloop, C)
!dvm$ region

!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on A(i),across(A(0:3)),stage(iloop)
      do i=2,N-3
         A(i) = A(i+1)+A(i+2)+A(i+3)
      enddo
  
!dvm$ parallel (i) on A(i), reduction( min( nloop ) )
      do i=2,N-3
          if (A(i).ne. C(i)) then
          nloop=min(nloop,i)
          endif          
      enddo
!dvm$ end region
!dvm$ get_actual (nloop)
      enddo
	  
       if (nloop .eq.NL) then
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (A, C)
       
      end
C -------------------------------------------ACR1209   
         
      subroutine acr1209
      integer, parameter :: N = 24, NL=1000

      character*7 tname 
      integer,allocatable::  A(:), C(:)
      integer nloop 
                    
!dvm$ distribute A(*)     
!dvm$ shadow A(3:3)

      tname='ACR1209'
      allocate (A(N), C(N))
	  nloop=NL
      
      do iloop=0,2	  
      NNL=NL
      call serial1(C,N,NNL)

      do i=4,N
         C(i) = C(i-1)+C(i-2)+C(i-3)
      enddo
      
!dvm$ actual (nloop, C)
!dvm$ region

!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
   
!dvm$ parallel (i) on A(i),across(A(3:0)),stage(iloop)
      do i=4,N
         A(i) = A(i-1)+A(i-2)+A(i-3)
      enddo
  
!dvm$ parallel (i) on A(i), reduction( min( nloop ) )
      do i=4,N
          if (A(i).ne. C(i)) then
          nloop=min(nloop,i)
          endif
        enddo
!dvm$ end region
!dvm$ get_actual (nloop)
      enddo
	  
      
       if (nloop .eq.NL) then
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (A, C)
        
      end 

C --------------------------------------------ACR1210  
         
      subroutine acr1210
      integer, parameter :: N = 50, NL=1000
    
      character*7 tname 
      integer,allocatable::  A(:), C(:)
      integer nloop 
                     
!dvm$ distribute A(*)     
!dvm$ shadow A(11:11)

      tname='ACR1210'      
      allocate (A(N), C(N))
 	  nloop=NL
      
      do iloop=0,2
      NNL=NL    
      call serial1(C,N,NNL)
      do i=12,N-11
         C(i) = C(i-9)+C(i+9)+C(i+10)+C(i-10)+C(i-11)+C(i+11)
      enddo
      
!dvm$ actual (nloop, C)
!dvm$ region

!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo

!dvm$ parallel (i) on A(i),across(A(11:11)),stage(iloop)
      do i=12,N-11
         A(i) = A(i-9)+A(i+9)+A(i+10)+A(i-10)+A(i-11)+A(i+11)
      enddo
  
!dvm$ parallel (i) on A(i), reduction( min( nloop ) )
      do i=12,N-11
          if (A(i).ne. C(i)) then
          nloop=min(nloop,i)
          endif          
      enddo
!dvm$ end region
!dvm$ get_actual (nloop)
      enddo
	  
       if (nloop .eq.NL) then
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (A, C)
     
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