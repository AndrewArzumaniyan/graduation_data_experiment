      program SH11
     
c    TESTING OF THE SHADOW DIRECTIVE AND THE SHADOW_RENEW CLAUSE'.       
c    DISTRIBUTED ARRAY A(N) IS TO HAVE DIFFERENT SHADOW WIDTH
c    ON BOTH SIDES 

      print *,'===START OF SH11========================'
C --------------------------------------------------
      call sh1101
C --------------------------------------------------
      call sh1102
C --------------------------------------------------
      call sh1103
C -------------------------------------------------
      call sh1104
C -------------------------------------------------
      call sh1105
C -------------------------------------------------
      call sh1106
C --------------------------------------------------
      call sh1107
C --------------------------------------------------
      call sh1108
C --------------------------------------------------
      call sh1109
C -------------------------------------------------
      call sh1110
C -------------------------------------------------
      call sh1111
C -------------------------------------------------
      call sh1112
C ------------------------------------------------- 
      call sh1113
C --------------------------------------------------
      call sh1114
C --------------------------------------------------
      call sh1115
C -------------------------------------------------
      call sh1116
C -------------------------------------------------
      call sh1117
C -------------------------------------------------
   
C -------------------------------------------------

C
C
      print *,'=== END OF SH11 ========================= '    
      end
C ---------------------------------------------SH1101
      subroutine SH1101
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                      
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A

      tname='SH1101'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo

!dvm$ parallel (i) on B(i),shadow_renew(A)
      do i=2,N-1
         B(i) = A(i-1)+A(i+1)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N-1
          if (B(i).ne.(C(i-1)+c(i+1))) nloop=min(nloop,i)
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
C ---------------------------------------------SH1102     
      subroutine sh1102
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                      
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A

      tname='SH1102'
      allocate (B(N),A(N),C(N))     
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo

!dvm$ parallel (i) on B(i),shadow_renew(A(1:1))
      do i=2,N-1
         B(i) = A(i-1)+A(i+1)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N-1
          if (B(i).ne.(C(i-1)+c(i+1))) nloop=min(nloop,i)
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
C -----------------------------------------SH1103      
      subroutine sh1103
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A

      tname='SH1103'
      allocate (B(N),A(N),C(N))            
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo

!dvm$ parallel (i) on B(i),shadow_renew(A(0:1))
      do i=2,N-1
         B(i) = A(i+1)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N-1
          if (B(i).ne.(c(i+1))) nloop=min(nloop,i)
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
      
C ------------------------------------------SH1104   
      subroutine sh1104
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A

      tname='SH1104'
      allocate (B(N),A(N),C(N))      
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo

!dvm$ parallel (i) on B(i),shadow_renew(A(1:0))
      do i=2,N
         B(i) = A(i-1)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N
          if (B(i).ne.(c(i-1))) nloop=min(nloop,i)
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
      
C ------------------------------------------SH1105   
      subroutine sh1105
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(0:1)

      tname='SH1105'     
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo

!dvm$ parallel (i) on B(i),shadow_renew(A(0:1))
      do i=2,N-1
         B(i) = A(i+1)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N-1
          if (B(i).ne.(c(i+1))) nloop=min(nloop,i)
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
      
C --------------------------------------------SH1106  
         
      subroutine sh1106
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(1:0)

      tname='SH1106'
      allocate (B(N),A(N),C(N))          
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),shadow_renew(A(1:0))
      do i=2,N
         B(i) = A(i-1)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N
          if (B(i).ne.(c(i-1))) nloop=min(nloop,i)
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
C -------------------------------------------SH1107   
         
      subroutine sh1107
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(2:2)      

      tname='SH1107'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),shadow_renew(A(2:2))
      do i=3,N-2
         B(i) = A(i-1)+A(i+1)+A(i+2)+A(i-2)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=3,N-2
          if (B(i).ne.(C(i-1)+c(i+1)+c(i-2)+c(i+2))) nloop=min(nloop,i)
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
C -------------------------------------------SH1108   
         
      subroutine sh1108
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(BLOCK)   
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(2:2)

      tname='SH1108'      
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo               

!dvm$ parallel (i) on B(i),shadow_renew(A(0:2))
      do i=2,N-2
         B(i) = A(i+1)+A(i+2)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N-2
          if (B(i).ne.(c(i+1)+c(i+2))) nloop=min(nloop,i)          
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
      
C -------------------------------------------SH1109   
         
      subroutine sh1109
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop                

!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(2:2)

      tname='SH1109'      
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),shadow_renew(A(2:0))
      do i=3,N
         B(i) = A(i-1)+A(i-2)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=3,N
          if (B(i).ne.(c(i-1)+c(i-2))) nloop=min(nloop,i)          
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
C -------------------------------------------SH1110   
         
      subroutine sh1110
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(0:2)

      tname='SH1110'      
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),shadow_renew(A)
      do i=2,N-2
         B(i) = A(i+1)+A(i+2)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N-2
          if (B(i).ne.(c(i+1)+c(i+2))) nloop=min(nloop,i)          
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
C -------------------------------------------SH1111   
         
      subroutine sh1111
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(2:0)

      tname='SH1111'      
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),shadow_renew(A)
      do i=3,N
         B(i) = A(i-1)+A(i-2)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=3,N
          if (B(i).ne.(c(i-1)+c(i-2))) nloop=min(nloop,i)          
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
C -------------------------------------------SH1112   
         
      subroutine sh1112
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(3:3)

      tname='SH1112'      
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),shadow_renew(A)
      do i=4,N-3
         B(i) = A(i-1)+A(i+1)+A(i+2)+A(i-2)+A(i-3)+A(i+3)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=4,N-3
         if (B(i).ne.(C(i-1)+c(i+1)+c(i-2)+c(i+2)+c(i-3)+c(i+3))) then
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
C -------------------------------------------SH1113  
         
      subroutine sh1113
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(3:3)

      tname='SH1113'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),shadow_renew(A(0:3))
      do i=2,N-3
         B(i) = A(i+1)+A(i+2)+A(i+3)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N-3
          if (B(i).ne.(c(i+1)+c(i+2)+c(i+3))) nloop=min(nloop,i)          
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
C -------------------------------------------SH1114   
         
      subroutine sh1114
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(3:3)

      tname='SH1114'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),shadow_renew(A(3:0))
      do i=4,N
         B(i) = A(i-1)+A(i-2)+A(i-3)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=4,N
          if (B(i).ne.(c(i-1)+c(i-2)+c(i-3))) nloop=min(nloop,i)          
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
C -------------------------------------------SH1115   
         
      subroutine sh1115
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(BLOCK)    
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(3:0)

      tname='SH1115'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),shadow_renew(A)
      do i=4,N
         B(i) = A(i-1)+A(i-2)+A(i-3)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=4,N
          if (B(i).ne.(c(i-1)+c(i-2)+c(i-3))) nloop=min(nloop,i)          
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
C -------------------------------------------SH1116   
         
      subroutine sh1116
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(0:3)

      tname='SH1116'
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),shadow_renew(A)
      do i=2,N-3
         B(i) = A(i+1)+A(i+2)+A(i+3)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=2,N-3
          if (B(i).ne.(c(i+1)+c(i+2)+c(i+3))) nloop=min(nloop,i)          
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
C --------------------------------------------SH1117  
         
      subroutine sh1117
      integer, parameter :: N = 500,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(BLOCK)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(11:11)

      tname='SH1117'      
      allocate (B(N),A(N),C(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

!dvm$ actual(nloop)
!dvm$ region local(A,B)
!dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
               
!dvm$ parallel (i) on B(i),shadow_renew(A)
      do i=12,N-11
         B(i) = A(i-9)+A(i+9)+A(i+10)+A(i-10)+A(i-11)+A(i+11)
      enddo
  
!dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=12,N-11
        if (B(i).ne.(C(i-9)+c(i+9)+c(i-10)+c(i+10)+
     *c(i-11)+c(i+11))) nloop=min(nloop,i)          
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
