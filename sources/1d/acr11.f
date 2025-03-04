      program ACR11
     
c    TESTING OF THE ACROSS CLAUSE'.       
c    DISTRIBUTED ARRAY A(N) IS TO HAVE DIFFERENT 
c    FLOW-DEP-LENGTH ON BOTH SIDES 

      print *,'===START OF ACR11========================'
C --------------------------------------------------
      call acr1101
C --------------------------------------------------
      call acr1102
C --------------------------------------------------
      call acr1103
C -------------------------------------------------
      call acr1104
C -------------------------------------------------
      call acr1105
C -------------------------------------------------
      call acr1106
C --------------------------------------------------
      call acr1107
C --------------------------------------------------
      call acr1108
C --------------------------------------------------
      call acr1109
C -------------------------------------------------
      call acr1110
C -------------------------------------------------

C
      print *,'=== END OF ACR11 ========================= '    
      end
C ---------------------------------------------ACR1101
      subroutine ACR1101   
      integer,parameter :: N = 8, NL=1000
      character*7 tname 
      integer,allocatable::  A(:), C(:)
      integer nloop                      
!dvm$ distribute A(BLOCK)    

      tname='ACR1101'
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
      deallocate (A,C)
      
      end
C ---------------------------------------------ACR1102     
      subroutine ACR1102
      integer,parameter :: N = 16, NL=1000

      character*7 tname 
      integer,allocatable::  A(:),C(:)
      integer nloop 
                      
!dvm$ distribute A(BLOCK)    

      tname='ACR1102'
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
C -----------------------------------------ACR1103      
      subroutine acr1103
      integer,parameter :: N = 16, NL=1000

      character*7 tname 
      integer,allocatable::  A(:), C(:)
      integer nloop 
                     
!dvm$ distribute A(BLOCK)    

      tname='ACR1103'
      allocate (A(N), C(N))
	  nloop=NL
      
      do iloop=0,2      
      NNL=NL    
      call serial1(C,N,NNL)
      do i=2,N
         C(i) = C(i)+ C(i-1)
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
      

  
 
C -------------------------------------------ACR1104   
         
      subroutine ACR1104
      integer,parameter :: N = 16, NL=1000

      character*7 tname 
      integer,allocatable::  A(:), C(:)
      integer nloop 
                    
!dvm$ distribute A(BLOCK)    
!dvm$ shadow A(2:2)
  
      tname='ACR1104'
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
C -------------------------------------------ACR1105   
         
      subroutine ACR1105
      integer,parameter :: N = 16, NL=1000

      character*7 tname 
      integer,allocatable::  A(:),C(:)
      integer nloop 
                    
!dvm$ distribute A(BLOCK)   
!dvm$ shadow A(2:2)

      tname='ACR1105'      
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
      
C -------------------------------------------ACR1106   
         
      subroutine ACR1106
      integer,parameter :: N = 16, NL=1000
      character*7 tname 
      integer,allocatable::  A(:), C(:)
      integer nloop                
!dvm$ distribute A(BLOCK)     
!dvm$ shadow A(2:2)

      tname='ACR1106'      
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

C -------------------------------------------ACR1107   
         
      subroutine acr1107
      integer,parameter :: N = 16, NL=1000

      character*7 tname 
      integer,allocatable::  A(:), C(:)
      integer nloop 
                     
!dvm$ distribute A(BLOCK)     
!dvm$ shadow A(3:3)

      tname='ACR1107'      
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
C -------------------------------------------ACR1108  
         
      subroutine acr1108
      integer,parameter :: N = 24, NL=1000

      character*7 tname 
      integer,allocatable::  A(:), C(:)
      integer nloop 
                    
!dvm$ distribute A(BLOCK)     
!dvm$ shadow A(3:3)

      tname='ACR1108'
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
C -------------------------------------------ACR1109   
         
      subroutine acr1109
      integer,parameter :: N = 24, NL=1000

      character*7 tname 
      integer,allocatable::  A(:), C(:)
      integer nloop 
                    
!dvm$ distribute A(BLOCK)     
!dvm$ shadow A(3:3)

      tname='ACR1109'
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

C --------------------------------------------acr1110  
         
      subroutine acr1110
      integer,parameter :: N = 60, NL=1000
      character*7 tname 
      integer,allocatable::  A(:), C(:)
      integer nloop 
                     
!dvm$ distribute A(BLOCK)     
!dvm$ shadow A(11:11)

      tname='ACR1110'      
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
      integer:: AR(N)
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