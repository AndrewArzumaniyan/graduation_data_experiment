      program SHA12
     
c    TESTING OF THE SHADOW_GROUP DIRECTIVE ,SHADOW_START DIRECRIVE AND
c    SHADOW_WAIT DIRECTIVE.       
c    DISTRIBUTED ARRAYES A(N),D(N),F(N) IS TO HAVE DIFFERENT 
c    SHADOW WIDTH ON BOTH SIDES 

      print *,'===START OF SHA12========================'
C --------------------------------------------------
      call sha1201
C --------------------------------------------------
      call sha1202
C --------------------------------------------------
      call sha1203
C -------------------------------------------------
      call sha1204
C -------------------------------------------------
 
C
C
      print *,'=== END OF SHA12 ========================= '    
      end
C ---------------------------------------------SHA1201
      subroutine SHA1201
     
      integer, parameter :: N = 32,NL=1000


      character*7 tname 
      integer, allocatable :: A(:),BA(:),BD(:),BF(:),C(:),D(:),F(:)
      integer nloop 
                      
cdvm$ distribute BA(*)     
cdvm$ shadow D(2:2)
cdvm$ shadow F(3:3)
cdvm$ align (I) with BA(I) ::A,D,F,BD,BF

      tname='SHA1201'
      allocate (BA(N),A(N),BD(N),BF(N),C(N),D(N),F(N))
cdvm$ shadow_group ADF(A(1:1),D(2:2),F(3:3))

      NNL=NL    
      call serial1(C,N,NNL)
      nloopa=NL
      nloopd=NL
      nloopf=NL
*dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
        D(i) =NL+i
        F(i) =NL+i
      enddo

cdvm$ shadow_start ADF
cdvm$ shadow_wait ADF               

*dvm$ parallel (i) on BA(i)
      do i=4,N-3
         BA(i) = A(i-1)+A(i+1)
         BD(i)=  D(i-2)+D(i+2)
         BF(i)=  F(i-3)+F(i+3)         
      enddo 
  
*dvm$ parallel (i) on BA(i), reduction( min( nloopa),
*dvm$* min(nloopd),min(nloopf) )
      do i=4,N-3
          if (BA(i).ne.(C(i-1)+c(i+1))) nloopa=min(nloopa,i)
          if (BD(i).ne.(C(i-2)+c(i+2))) nloopd=min(nloopd,i)
          if (BF(i).ne.(C(i-3)+c(i+3))) nloopf=min(nloopf,i)          
      enddo 
      
      if ((nloopa .eq.NL).and.(nloopd.eq.NL).and.(nloopf.eq.NL)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,BA,BD,BF,C,D,F)
      
      end

C ---------------------------------------------SHA1202
      subroutine SHA1202     
      integer, parameter :: N = 16,NL=1000
      character*7 tname 
      integer, allocatable :: A(:),BA(:),BD(:),BF(:),C(:),D(:),F(:)
      integer nloop 
                      
cdvm$ distribute BA(*)   
cdvm$ shadow D(2:2)
cdvm$ shadow F(3:3)
cdvm$ align (I) with BA(I) ::A,D,F,BD,BF

      tname='SHA1202'
      allocate (BA(N),A(N),BD(N),BF(N),C(N),D(N),F(N))
cdvm$ shadow_group ADF(A(1:1),D(2:2),F(3:3))

      NNL=NL    
      call serial1(C,N,NNL)
      nloopa=NL
      nloopd=NL
      nloopf=NL

*dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
        D(i) =NL+i
        F(i) =NL+i
      enddo

cdvm$ shadow_start ADF
     
*dvm$ parallel (i) on BA(i),shadow_wait ADF
      do i=4,N-3
         BA(i) = A(i-1)+A(i+1)
         BD(i)=  D(i-2)+D(i+2)
         BF(i)=  F(i-3)+F(i+3)         
      enddo 
    
*dvm$ parallel (i) on BA(i), reduction( min( nloopa),
*dvm$* min(nloopd),min(nloopf) )
      do i=4,N-3
          if (BA(i).ne.(C(i-1)+c(i+1))) nloopa=min(nloopa,i)
          if (BD(i).ne.(C(i-2)+c(i+2))) nloopd=min(nloopd,i)
          if (BF(i).ne.(C(i-3)+c(i+3))) nloopf=min(nloopf,i)          
      enddo 
     
      if ((nloopa .eq.NL).and.(nloopd.eq.NL).and.(nloopf.eq.NL)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,BA,BD,BF,C,D,F)
      
      end

C ---------------------------------------------SHA1203
   

      subroutine SHA1203
      integer, parameter :: N = 16,NL=1000
      character*7 tname 
      integer, allocatable :: A(:),BA(:),BD(:),BF(:),C(:),D(:),F(:)
      integer nloop 
                      
cdvm$ distribute BA(*)    
cdvm$ shadow D(2:2)
cdvm$ shadow F(3:3)
cdvm$ align (I) with BA(I) ::A,D,F,BD,BF

      tname='SHA1203'
      allocate (BA(N),A(N),BD(N),BF(N),C(N),D(N),F(N))
cdvm$ shadow_group ADF(A(1:1),D(2:2),F(3:3))

      NNL=NL    
      call serial1(C,N,NNL)
      nloopa=NL
      nloopd=NL
      nloopf=NL

*dvm$ parallel (i) on A(i),shadow_start ADF
      do i=1,N
        A(i) = NL+i
        D(i) =NL+i
        F(i) =NL+i
      enddo

cdvm$ shadow_wait ADF 
*dvm$ parallel (i) on BA(i)
      do i=4,N-3
         BA(i) = A(i-1)+A(i+1)
         BD(i)=  D(i-2)+D(i+2)
         BF(i)=  F(i-3)+F(i+3)
      enddo 
     
*dvm$ parallel (i) on BA(i), reduction( min( nloopa),
*dvm$* min(nloopd),min(nloopf) )
      do i=4,N-3
          if (BA(i).ne.(C(i-1)+c(i+1))) nloopa=min(nloopa,i)
          if (BD(i).ne.(C(i-2)+c(i+2))) nloopd=min(nloopd,i)
          if (BF(i).ne.(C(i-3)+c(i+3))) nloopf=min(nloopf,i)
      enddo 
      
      if ((nloopa .eq.NL).and.(nloopd.eq.NL).and.(nloopf.eq.NL)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,BA,BD,BF,C,D,F)
      
      end
C ---------------------------------------------SHA1204
      subroutine SHA1204
      integer, parameter :: N = 16,NL=1000
      character*7 tname 
      integer, allocatable :: A(:),BA(:),BD(:),BF(:),C(:),D(:),F(:)
      integer nloop 
                      
cdvm$ distribute BA(*)     
cdvm$ shadow D(2:2)
cdvm$ shadow F(3:3)
cdvm$ align (I) with BA(I) ::A,D,F,BD,BF

      tname='SHA1204'
      allocate (BA(N),A(N),BD(N),BF(N),C(N),D(N),F(N))
cdvm$ shadow_group ADF(A(1:1),D(2:2),F(3:3))

      NNL=NL    
      call serial1(C,N,NNL)
      nloopa=NL
      nloopd=NL
      nloopf=NL

*dvm$ parallel (i) on A(i),shadow_start ADF
      do i=1,N
        A(i) = NL+i
        D(i) =NL+i
        F(i) =NL+i
      enddo

*dvm$ parallel (i) on BA(i),shadow_wait ADF
      do i=4,N-3
         BA(i) = A(i-1)+A(i+1)
         BD(i)=  D(i-2)+D(i+2)
         BF(i)=  F(i-3)+F(i+3)
      enddo 
      
*dvm$ parallel (i) on BA(i), reduction( min( nloopa),
*dvm$* min(nloopd),min(nloopf) )
      do i=4,N-3
          if (BA(i).ne.(C(i-1)+c(i+1))) nloopa=min(nloopa,i)
          if (BD(i).ne.(C(i-2)+c(i+2))) nloopd=min(nloopd,i)
          if (BF(i).ne.(C(i-3)+c(i+3))) nloopf=min(nloopf,i)          
      enddo 
      
      if ((nloopa .eq.NL).and.(nloopd.eq.NL).and.(nloopf.eq.NL)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,BA,BD,BF,C,D,F)
      
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
