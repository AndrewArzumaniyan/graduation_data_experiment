      program SC11
     
c    TESTING OF THE SHADOW DIRECTIVE AND THE SHADOW_COMPUTE CLAUSE       
c    DISTRIBUTED ARRAY A(N) IS TO HAVE DIFFERENT SHADOW WIDTH
c    ON BOTH SIDES 

      print *,'===START OF SC11========================'
C --------------------------------------------------
      call sc1101
C --------------------------------------------------
      call sc1102
C --------------------------------------------------
      call sc1103
C -------------------------------------------------
      call sc1104
C -------------------------------------------------
      call sc1105
C -------------------------------------------------
      call sc1106
C --------------------------------------------------
      call sc1107
C --------------------------------------------------
      call sc1108
C --------------------------------------------------
      call sc1109
C -------------------------------------------------
      call sc1110
C -------------------------------------------------
      call sc1111
C -------------------------------------------------
      call sc1112
C ------------------------------------------------- 
      call sc1113
C --------------------------------------------------
      call sc1114
C --------------------------------------------------
      call sc1115
C -------------------------------------------------
      call sc1116
C -------------------------------------------------
      call sc1117
C -------------------------------------------------

C
C
      print *,'=== END OF SC11 ========================= '    
      end
C ---------------------------------------------SC1101
      subroutine SC1101
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                      
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A

      tname='SC1101'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i),shadow_compute    

      do i=1,N
        A(i) = NL+i
      enddo

!dvm$ parallel (i) on B(i)
      do i=2,N-1
         B(i) = A(i-1)+A(i+1)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N-1
          if (B(i).ne.(C(i-1)+C(i+1))) nloop=min(nloop,i)
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
C ---------------------------------------------SC1102     
      subroutine SC1102
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                      
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A

      tname='SC1102'
      allocate (B(N),A(N),C(N))     
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i),shadow_compute (A(1:1))
      do i=1,N
        A(i) = NL+i
      enddo

!dvm$ parallel (i) on B(i)
      do i=2,N-1
         B(i) = A(i-1)+A(i+1)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N-1
          if (B(i).ne.(C(i-1)+C(i+1))) nloop=min(nloop,i)
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
C -----------------------------------------SC1103      
      subroutine SC1103
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A

      tname='SC1103'
      allocate (B(N),A(N),C(N))            
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i),shadow_compute(A(0:1))
      do i=1,N
        A(i) = NL+i
      enddo

!dvm$ parallel (i) on B(i)
      do i=2,N-1
         B(i) = A(i+1)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N-1
          if (B(i).ne.(C(i+1))) nloop=min(nloop,i)
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
      
C ------------------------------------------SC1104   
      subroutine SC1104
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A

      tname='SC1104'
      allocate (B(N),A(N),C(N))      
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i),shadow_compute(A(1:0))
      do i=1,N
        A(i) = NL+i
      enddo

!dvm$ parallel (i) on B(i)
      do i=2,N
         B(i) = A(i-1)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N
          if (B(i).ne.C(i-1)) nloop=min(nloop,i)
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
      
C ------------------------------------------SC1105   
      subroutine SC1105
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(0:1)

      tname='SC1105'     
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i),shadow_compute(A(0:1))
      do i=1,N
        A(i) = NL+i
      enddo

!dvm$ parallel (i) on B(i)
      do i=2,N-1
         B(i) = A(i+1)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N-1
          if (B(i).ne.C(i+1)) nloop=min(nloop,i)
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
      
C --------------------------------------------SC1106  
         
      subroutine SC1106
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(1:0)

      tname='SC1106'
      allocate (B(N),A(N),C(N))          
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i),shadow_compute(A(1:0))
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i)
      do i=2,N
         B(i) = A(i-1)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N
          if (B(i).ne.C(i-1)) nloop=min(nloop,i)
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
C -------------------------------------------SC1107   
         
      subroutine SC1107
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(2:2)      

      tname='SC1107'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i) ,shadow_compute(A(2:2))
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i)
      do i=3,N-2
         B(i) = A(i-1)+A(i+1)+A(i+2)+A(i-2)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=3,N-2
          if (B(i).ne.(C(i-1)+C(i+1)+C(i-2)+C(i+2))) nloop=min(nloop,i)
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
C -------------------------------------------SC1108   
         
      subroutine SC1108
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(BLOCK)   
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(2:2)

      tname='SC1108'      
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i),shadow_compute(A(0:2))
      do i=1,N
        A(i) = NL+i
      enddo               

!dvm$ parallel (i) on B(i)
      do i=2,N-2
         B(i) = A(i+1)+A(i+2)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N-2
          if (B(i).ne.(C(i+1)+C(i+2))) nloop=min(nloop,i)          
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
      
C -------------------------------------------SC1109   
         
      subroutine SC1109
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop                

!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(2:2)

      tname='SC1109'      
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i),shadow_compute(A(2:0))
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i)
      do i=3,N
         B(i) = A(i-1)+A(i-2)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=3,N
          if (B(i).ne.(C(i-1)+C(i-2))) nloop=min(nloop,i)          
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
C -------------------------------------------SC1110   
         
      subroutine SC1110
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(0:2)

      tname='SC1110'      
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i),shadow_compute
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i)
      do i=2,N-2
         B(i) = A(i+1)+A(i+2)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N-2
          if (B(i).ne.(C(i+1)+C(i+2))) nloop=min(nloop,i)          
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
C -------------------------------------------SC1111   
         
      subroutine SC1111
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(2:0)

      tname='SC1111'      
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i),shadow_compute
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i)
      do i=3,N
         B(i) = A(i-1)+A(i-2)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=3,N
          if (B(i).ne.(C(i-1)+C(i-2))) nloop=min(nloop,i)          
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
C -------------------------------------------SC1112   
         
      subroutine SC1112
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(3:3)

      tname='SC1112'      
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i),shadow_compute
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i)
      do i=4,N-3
         B(i) = A(i-1)+A(i+1)+A(i+2)+A(i-2)+A(i-3)+A(i+3)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=4,N-3
         if (B(i).ne.(C(i-1)+C(i+1)+C(i-2)+C(i+2)+C(i-3)+C(i+3))) then
            nloop=min(nloop,i)
         endif    
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
C -------------------------------------------SC1113  
         
      subroutine SC1113
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(3:3)

      tname='SC1113'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i),shadow_compute(A(0:3))
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i)
      do i=2,N-3
         B(i) = A(i+1)+A(i+2)+A(i+3)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N-3
          if (B(i).ne.(C(i+1)+C(i+2)+C(i+3))) nloop=min(nloop,i)          
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
C -------------------------------------------SC1114   
         
      subroutine SC1114
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(3:3)

      tname='SC1114'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i),shadow_compute(A(3:0))
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i)
      do i=4,N
         B(i) = A(i-1)+A(i-2)+A(i-3)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=4,N
          if (B(i).ne.(C(i-1)+C(i-2)+C(i-3))) nloop=min(nloop,i)          
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
C -------------------------------------------SC1115   
         
      subroutine SC1115
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(3:0)

      tname='SC1115'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i),shadow_compute
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i)
      do i=4,N
         B(i) = A(i-1)+A(i-2)+A(i-3)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=4,N
          if (B(i).ne.(C(i-1)+C(i-2)+C(i-3))) nloop=min(nloop,i)          
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
C -------------------------------------------SC1116   
         
      subroutine SC1116
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(0:3)

      tname='SC1116'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i),shadow_compute
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i)
      do i=2,N-3
         B(i) = A(i+1)+A(i+2)+A(i+3)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N-3
          if (B(i).ne.(C(i+1)+C(i+2)+C(i+3))) nloop=min(nloop,i)          
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
C --------------------------------------------SC1117  
         
      subroutine SC1117
      integer, parameter :: N = 500,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(11:11)

      tname='SC1117'      
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i),shadow_compute
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i)
      do i=12,N-11
         B(i) = A(i-9)+A(i+9)+A(i+10)+A(i-10)+A(i-11)+A(i+11)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=12,N-11
        if (B(i).ne.(C(i-9)+C(i+9)+C(i-10)+C(i+10)+
     *C(i-11)+C(i+11))) nloop=min(nloop,i)          
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
      character*6 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*6 name
      print *,name,'  -  ***error'
      end
