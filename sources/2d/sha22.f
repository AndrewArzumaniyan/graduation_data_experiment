      program SHA22
     
c    TESTING OF THE SHADOW_GROUP DIRECTIVE ,SHADOW_START DIRECRIVE AND
c    SHADOW_WAIT DIRECTIVE.       
c    DISTRIBUTED ARRAYES A(N,M),D(N,M),F(N,M) IS TO HAVE DIFFERENT 
c    SHADOW WIDTH ON BOTH SIDES 

      print *,'===START OF SHA22========================'
C --------------------------------------------------
      call sha2201
C --------------------------------------------------
      call sha2202
C --------------------------------------------------
      call sha2203
C -------------------------------------------------
      call sha2204
C -------------------------------------------------
 
C
C
      print *,'=== END OF SHA22 ========================= '    
      end
C ---------------------------------------------SHA2201
      subroutine SHA2201
      integer,parameter :: N = 16,M=16, PN = 16,NL=1000
      character*7 tname 
      integer, allocatable :: A(:,:),BA(:,:),BD(:,:)
      integer, allocatable :: BF(:,:),C(:,:),D(:,:),F(:,:)
      integer nloop 
                      
cdvm$ distribute BA(BLOCK,*)     
cdvm$ shadow D(2:2,2:2)
cdvm$ shadow F(3:3,3:3)
cdvm$ align (I,J) with BA(I,J) ::A,D,F,BD,BF

      tname='SHA2201'
      allocate (BA(N,M),A(N,M),BD(N,M))
      allocate (BF(N,M),C(N,M),D(N,M),F(N,M))
cdvm$ shadow_group ADF(A(CORNER),D(CORNER),F(CORNER))

      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopa=NL
      nloopd=NL
      nloopf=NL

*dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
          D(i,j) =NL+i+j
          F(i,j) =NL+i+j
        enddo
      enddo

cdvm$ shadow_start ADF
cdvm$ shadow_wait ADF               

c      print *,'C'
c      print *,C  
c      print *,'A'
c      print *,A
*dvm$ parallel (i,j) on BA(i,j),NEW(K)
      do i=4,N-3
        do j=4,M-3
          BA(i,j) = A(i-1,j-1)+A(i+1,j+1)
          BD(i,j)=  D(i-2,j-2)+D(i+2,j+2)
          BF(i,j)=  F(i-3,j-3)+F(i+3,j+3)         
        enddo
      enddo
  
c      print *,'BA'
c      print *,BA
*dvm$ parallel (i,j) on BA(i,j),NEW(K),reduction( min( nloopa),
*dvm$* min(nloopd),min(nloopf) )
      do i=4,N-3
        do j=4,M-3
          if (BA(i,j).ne.(C(i-1,j-1)+c(i+1,j+1))) nloopa=min(nloopa,i)
          if (BD(i,j).ne.(C(i-2,j-2)+c(i+2,j+2))) nloopd=min(nloopd,i)
          if (BF(i,j).ne.(C(i-3,j-3)+c(i+3,j+3))) nloopf=min(nloopf,i)        
        enddo
      enddo
      
      if ((nloopa .eq.NL).and.(nloopd.eq.NL).and.(nloopf.eq.NL)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,BA,BD,BF,C,D,F)
      
      end

C ---------------------------------------------SHA2202
      subroutine SHA2202
      integer,parameter :: N = 32,M=32,NL=1000
      character*7 tname 
      integer, allocatable :: A(:,:),BA(:,:),BD(:,:)
      integer, allocatable :: BF(:,:),C(:,:),D(:,:),F(:,:)
      integer nloop 
                      
cdvm$ distribute BA(*,BLOCK)     
cdvm$ shadow D(2:2,2:2)
cdvm$ shadow F(3:3,3:3)
cdvm$ align (I,J) with BA(I,J) ::A,D,F,BD,BF

      tname='SHA2202'
      allocate (BA(N,M),A(N,M),BD(N,M))
      allocate (BF(N,M),C(N,M),D(N,M),F(N,M))
cdvm$ shadow_group ADF(A(CORNER),D(CORNER),F(CORNER))

      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopa=NL
      nloopd=NL
      nloopf=NL

*dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
          D(i,j) =NL+i+j
          F(i,j) =NL+i+j
        enddo
      enddo                

cdvm$ shadow_start ADF

*dvm$ parallel (i,j) on BA(i,j),shadow_wait ADF
      do i=4,N-3
        do j=4,M-3
          BA(i,j) = A(i-1,j-1)+A(i+1,j+1)
          BD(i,j)=  D(i-2,j-2)+D(i+2,j+2)
          BF(i,j)=  F(i-3,j-3)+F(i+3,j+3)         
        enddo
      enddo
  
*dvm$ parallel (i,j) on BA(i,j), reduction( min( nloopa),
*dvm$* min(nloopd),min(nloopf) )
      do i=4,N-3
        do j=4,M-3
          if (BA(i,j).ne.(C(i-1,j-1)+c(i+1,j+1))) nloopa=min(nloopa,i)
          if (BD(i,j).ne.(C(i-2,j-2)+c(i+2,j+2))) nloopd=min(nloopd,i)
          if (BF(i,j).ne.(C(i-3,j-3)+c(i+3,j+3))) nloopf=min(nloopf,i)          
        enddo
      enddo
      
      if ((nloopa .eq.NL).and.(nloopd.eq.NL).and.(nloopf.eq.NL)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,BA,BD,BF,C,D,F)
      
      end

C ---------------------------------------------SHA2203
   

      subroutine SHA2203
      integer,parameter :: N = 32,M=32,NL=1000
      character*7 tname 
      integer, allocatable :: A(:,:),BA(:,:),BD(:,:)
      integer, allocatable :: BF(:,:),C(:,:),D(:,:),F(:,:)
      integer nloop 
                      
cdvm$ distribute BA(*,BLOCK)     
cdvm$ shadow D(2:2,2:2)
cdvm$ shadow F(3:3,3:3)
cdvm$ align (I,J) with BA(I,J) ::A,D,F,BD,BF

      tname='SHA2203'
      allocate (BA(N,M),A(N,M),BD(N,M))
      allocate (BF(N,M),C(N,M),D(N,M),F(N,M))
cdvm$ shadow_group ADF(A(CORNER),D(CORNER),F(CORNER))

      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopa=NL
      nloopd=NL
      nloopf=NL

*dvm$ parallel (i,j) on A(i,j),shadow_start ADF
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
          D(i,j) =NL+i+j
          F(i,j) =NL+i+j
        enddo
      enddo                
    
cdvm$ shadow_wait ADF 

*dvm$ parallel (i,j) on BA(i,j)
      do i=4,N-3
        do j=4,M-3
          BA(i,j) = A(i-1,j-1)+A(i+1,j+1)
          BD(i,j)=  D(i-2,j-2)+D(i+2,j+2)
          BF(i,j)=  F(i-3,j-3)+F(i+3,j+3)         
         enddo
      enddo
  
*dvm$ parallel (i,j) on BA(i,j), reduction( min( nloopa),
*dvm$* min(nloopd),min(nloopf) )
      do i=4,N-3
        do j=4,M-3
          if (BA(i,j).ne.(C(i-1,j-1)+c(i+1,j+1))) nloopa=min(nloopa,i)
          if (BD(i,j).ne.(C(i-2,j-2)+c(i+2,j+2))) nloopd=min(nloopd,i)
          if (BF(i,j).ne.(C(i-3,j-3)+c(i+3,j+3))) nloopf=min(nloopf,i)          
        enddo
      enddo     
      
      if ((nloopa .eq.NL).and.(nloopd.eq.NL).and.(nloopf.eq.NL)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,BA,BD,BF,C,D,F)
      
      end
C ---------------------------------------------SHA2204
      subroutine SHA2204
      integer,parameter :: N = 32,M=32,NL=1000
      character*7 tname 
      integer, allocatable :: A(:,:),BA(:,:),BD(:,:)
      integer, allocatable :: BF(:,:),C(:,:),D(:,:),F(:,:)
      integer nloop 
                      
cdvm$ distribute BA(*,BLOCK)     
cdvm$ shadow D(2:2,2:2)
cdvm$ shadow F(3:3,3:3)
cdvm$ align (I,J) with BA(I,J) ::A,D,F,BD,BF

      tname='SHA2204'
      allocate (BA(N,M),A(N,M),BD(N,M))
      allocate (BF(N,M),C(N,M),D(N,M),F(N,M))
cdvm$ shadow_group ADF(A(CORNER),D(CORNER),F(CORNER))

      NNL=NL    
      call serial2(C,N,M,NNL)
      nloopa=NL
      nloopd=NL
      nloopf=NL
*dvm$ parallel (i,j) on A(i,j),shadow_start ADF
      do i=1,N
        do j=1,M
          A(i,j) = NL+i+j
          D(i,j) =NL+i+j
          F(i,j) =NL+i+j
        enddo
      enddo                

*dvm$ parallel (i,j) on BA(i,j),shadow_wait ADF
      do i=4,N-3
        do j=4,M-3
          BA(i,j) = A(i-1,j-1)+A(i+1,j+1)
          BD(i,j)=  D(i-2,j-2)+D(i+2,j+2)
          BF(i,j)=  F(i-3,j-3)+F(i+3,j+3)         
        enddo
      enddo
  
*dvm$ parallel (i,j) on BA(i,j), reduction( min( nloopa),
*dvm$* min(nloopd),min(nloopf) )
      do i=4,N-3
        do j=4,M-3
          if (BA(i,j).ne.(C(i-1,j-1)+c(i+1,j+1))) nloopa=min(nloopa,i)
          if (BD(i,j).ne.(C(i-2,j-2)+c(i+2,j+2))) nloopd=min(nloopd,i)
          if (BF(i,j).ne.(C(i-3,j-3)+c(i+3,j+3))) nloopf=min(nloopf,i)          
        enddo
      enddo
      
      if ((nloopa .eq.NL).and.(nloopd.eq.NL).and.(nloopf.eq.NL)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,BA,BD,BF,C,D,F)
      
      end
C -----------------------------------------------         
      subroutine serial2(AR,N,M,NL)
      integer AR(N,M)
      integer NL 
      do i=1,N
        do j=1,M
          AR(i,j) = NL+i+j
        enddo
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
