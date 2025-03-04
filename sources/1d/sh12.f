      program SH12
     
c    TESTING OF THE SHADOW DIRECTIVE AND THE SHADOW_RENEW CLAUSE'.       
c    DISTRIBUTED ARRAY A(N) IS TO HAVE DIFFERENT SHADOW WIDTH
c    ON BOTH SIDES 

      print *,'===START OF SH12========================'
C --------------------------------------------------
      call sh1201
C --------------------------------------------------
      call sh1202
C --------------------------------------------------
      call sh1203
C -------------------------------------------------
      call sh1204
C -------------------------------------------------
      call sh1205
C -------------------------------------------------
      call sh1206
C --------------------------------------------------
      call sh1207
C --------------------------------------------------
      call sh1208
C --------------------------------------------------
      call sh1209
C -------------------------------------------------
      call sh1210
C -------------------------------------------------
      call sh1211
C -------------------------------------------------
      call sh1212
C ------------------------------------------------- 
      call sh1213
C --------------------------------------------------
      call sh1214
C --------------------------------------------------
      call sh1215
C -------------------------------------------------
      call sh1216
C -------------------------------------------------
      call sh1217
C -------------------------------------------------
   
C -------------------------------------------------

C
C
      print *,'=== END OF SH12 ========================= '    
      end
C ---------------------------------------------SH1201
      subroutine SH1201
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                      
!dvm$ distribute B(*)    
!dvm$ align (I) with B(I) ::A

      tname='SH1201'
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
C ---------------------------------------------SH1202     
      subroutine sh1202
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                      
!dvm$ distribute B(*)    
!dvm$ align (I) with B(I) ::A

      tname='SH1202'
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
C -----------------------------------------SH1203      
      subroutine sh1203
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(*)    
!dvm$ align (I) with B(I) ::A

      tname='SH1203'
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
      
C ------------------------------------------SH1204   
      subroutine sh1204
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(*)    
!dvm$ align (I) with B(I) ::A

      tname='SH1204'
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
      
C ------------------------------------------SH1205   
      subroutine sh1205
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(*)    
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(0:1)

      tname='SH1205'     
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
      
C --------------------------------------------SH1206  
         
      subroutine sh1206
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(*)    
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(1:0)

      tname='SH1206'
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
C -------------------------------------------SH1207   
         
      subroutine sh1207
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(*)    
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(2:2)      

      tname='SH1207'
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
C -------------------------------------------SH1208   
         
      subroutine sh1208
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(*)   
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(2:2)

      tname='SH1208'      
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
      
C -------------------------------------------SH1209   
         
      subroutine sh1209
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop                

!dvm$ distribute B(*)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(2:2)

      tname='SH1209'      
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
C -------------------------------------------SH1210   
         
      subroutine sh1210
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(*)    
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(0:2)

      tname='SH1210'      
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
C -------------------------------------------SH1211   
         
      subroutine sh1211
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(*)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(2:0)

      tname='SH1211'      
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
C -------------------------------------------SH1212   
         
      subroutine sh1212
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(*)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(3:3)

      tname='SH1212'      
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
C -------------------------------------------SH1213  
         
      subroutine sh1213
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(*)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(3:3)

      tname='SH1213'
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
C -------------------------------------------SH1214   
         
      subroutine sh1214
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(*)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(3:3)

      tname='SH1214'
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
C -------------------------------------------SH1215   
         
      subroutine sh1215
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(*)    
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(3:0)

      tname='SH1215'
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
C -------------------------------------------SH1216   
         
      subroutine sh1216
      integer, parameter :: N = 16,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                    
!dvm$ distribute B(*)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(0:3)

      tname='SH1216'
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
C --------------------------------------------SH1217  
         
      subroutine sh1217
      integer, parameter :: N = 50,NL=1000
      character*6 tname 
      integer, allocatable :: A(:),B(:),C(:)
      integer nloop 
                     
!dvm$ distribute B(*)     
!dvm$ align (I) with B(I) ::A
!dvm$ shadow A(11:11)

      tname='SH1217'      
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
