      program SHA32
     
c    TESTING OF THE SHADOW_GROUP DIRECTIVE ,SHADOW_START DIRECRIVE AND
c    SHADOW_WAIT DIRECTIVE.       
c    DISTRIBUTED ARRAYES A(N,M,K),D(N,M,K),F(N,M,K) IS TO HAVE DIFFERENT 
c    SHADOW WIDTH ON BOTH SIDES 

      print *,'===START OF SHA32========================'
C --------------------------------------------------
      call sha3201
C --------------------------------------------------
      call sha3202
C --------------------------------------------------
      call sha3203
C -------------------------------------------------
      call sha3204
C -------------------------------------------------
 
C
C
      print *,'=== END OF SHA32 ========================= '    
      end
C ---------------------------------------------SHA3201
      subroutine SHA3201
      integer, parameter :: N = 16,M=16, K=16,NL=1000
      character*7 tname 
      integer, allocatable :: A(:,:,:),BA(:,:,:),BD(:,:,:)
      integer, allocatable :: BF(:,:,:),C(:,:,:),D(:,:,:),F(:,:,:)
      integer nloop 
                      
cdvm$ distribute BA(BLOCK,BLOCK,*)     
cdvm$ shadow D(2:2,2:2,2:2)
cdvm$ shadow F(3:3,3:3,3:3)
cdvm$ align (I,J,II) with BA(I,J,II) ::A,D,F,BD,BF

      tname='SHA3201'
      allocate (BA(N,M,K),A(N,M,K),BD(N,M,K))
      allocate (BF(N,M,K),C(N,M,K),D(N,M,K),F(N,M,K))
cdvm$ shadow_group ADF(A(CORNER),D(CORNER),F(CORNER))

      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopa=NL
      nloopd=NL
      nloopf=NL

*dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
            D(i,j,ii) =NL+i+j+ii
            F(i,j,ii) =NL+i+j+ii
          enddo
        enddo
      enddo                                                

cdvm$ shadow_start ADF
cdvm$ shadow_wait ADF               

c      print *,'C'
c      print *,C  
c      print *,'A'
c      print *,A

*dvm$ parallel (i,j,ii) on BA(i,j,ii)
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            BA(i,j,ii) = A(i-1,j-1,ii-1)+A(i+1,j+1,ii+1)
            BD(i,j,ii)=  D(i-2,j-2,ii-2)+D(i+2,j+2,ii+2)
            BF(i,j,ii)=  F(i-3,j-3,ii-3)+F(i+3,j+3,ii+3)
          enddo  
        enddo
      enddo 
c      print *,'BA'
c      print *,BA
*dvm$ parallel (i,j,ii) on BA(i,j,ii),reduction( min( nloopa),
*dvm$* min(nloopd),min(nloopf) )
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3 
            if (BA(i,j,ii).ne.(C(i-1,j-1,ii-1)+c(i+1,j+1,ii+1)))
     *    nloopa=min(nloopa,i)
            if (BD(i,j,ii).ne.(C(i-2,j-2,ii-2)+c(i+2,j+2,ii+2)))
     *    nloopd=min(nloopd,i)
            if (BF(i,j,ii).ne.(C(i-3,j-3,ii-3)+c(i+3,j+3,ii+3)))
     *    nloopf=min(nloopf,i)
          enddo
        enddo
      enddo 
      
      if ((nloopa .eq.NL).and.(nloopd.eq.NL).and.(nloopf.eq.NL)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,BA,BD,BF,C,D,F)
      
      end

C ---------------------------------------------SHA3202
      subroutine SHA3202
      integer, parameter :: N = 16,M=16, K=16,NL=1000
      character*7 tname 
      integer, allocatable :: A(:,:,:),BA(:,:,:),BD(:,:,:)
      integer, allocatable :: BF(:,:,:),C(:,:,:),D(:,:,:),F(:,:,:)
      integer nloop 
                      
cdvm$ distribute BA(BLOCK,*,BLOCK)     
cdvm$ shadow D(2:2,2:2,2:2)
cdvm$ shadow F(3:3,3:3,3:3)
cdvm$ align (I,J,II) with BA(I,J,II) ::A,D,F,BD,BF

      tname='SHA3202'
      allocate (BA(N,M,K),A(N,M,K),BD(N,M,K))
      allocate (BF(N,M,K),C(N,M,K),D(N,M,K),F(N,M,K))
cdvm$ shadow_group ADF(A(CORNER),D(CORNER),F(CORNER))

      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopa=NL
      nloopd=NL
      nloopf=NL

*dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
            D(i,j,ii) =NL+i+j+ii
            F(i,j,ii) =NL+i+j+ii
          enddo
        enddo
      enddo                                                

cdvm$ shadow_start ADF

*dvm$ parallel (i,j,ii) on BA(i,j,ii),shadow_wait ADF
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            BA(i,j,ii) = A(i-1,j-1,ii-1)+A(i+1,j+1,ii+1)
            BD(i,j,ii)=  D(i-2,j-2,ii-2)+D(i+2,j+2,ii+2)
            BF(i,j,ii)=  F(i-3,j-3,ii-3)+F(i+3,j+3,ii+3)
          enddo  
        enddo
      enddo 

*dvm$ parallel (i,j,ii) on BA(i,j,ii),reduction( min( nloopa),
*dvm$* min(nloopd),min(nloopf) )
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3 
            if (BA(i,j,ii).ne.(C(i-1,j-1,ii-1)+c(i+1,j+1,ii+1)))
     *    nloopa=min(nloopa,i)
            if (BD(i,j,ii).ne.(C(i-2,j-2,ii-2)+c(i+2,j+2,ii+2)))
     *    nloopd=min(nloopd,i)
            if (BF(i,j,ii).ne.(C(i-3,j-3,ii-3)+c(i+3,j+3,ii+3)))
     *    nloopf=min(nloopf,i)
          enddo   
        enddo
      enddo

      if ((nloopa .eq.NL).and.(nloopd.eq.NL).and.(nloopf.eq.NL)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,BA,BD,BF,C,D,F)
      
      end
     
C ---------------------------------------------SHA3203
      subroutine SHA3203
      integer, parameter :: N = 16,M=16, K=16,NL=1000
      character*7 tname 
      integer, allocatable :: A(:,:,:),BA(:,:,:),BD(:,:,:)
      integer, allocatable :: BF(:,:,:),C(:,:,:),D(:,:,:),F(:,:,:)
      integer nloop 
                      
cdvm$ distribute BA(*,BLOCK,BLOCK)     
cdvm$ shadow D(2:2,2:2,2:2)
cdvm$ shadow F(3:3,3:3,3:3)
cdvm$ align (I,J,II) with BA(I,J,II) ::A,D,F,BD,BF

      tname='SHA3203'
      allocate (BA(N,M,K),A(N,M,K),BD(N,M,K))
      allocate (BF(N,M,K),C(N,M,K),D(N,M,K),F(N,M,K))
cdvm$ shadow_group ADF(A(CORNER),D(CORNER),F(CORNER))

      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopa=NL
      nloopd=NL
      nloopf=NL

*dvm$ parallel (i,j,ii) on A(i,j,ii),shadow_start ADF
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
            D(i,j,ii) =NL+i+j+ii
            F(i,j,ii) =NL+i+j+ii
          enddo
        enddo
      enddo                                                

cdvm$ shadow_wait ADF 

*dvm$ parallel (i,j,ii) on BA(i,j,ii)
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            BA(i,j,ii) = A(i-1,j-1,ii-1)+A(i+1,j+1,ii+1)
            BD(i,j,ii)=  D(i-2,j-2,ii-2)+D(i+2,j+2,ii+2)
            BF(i,j,ii)=  F(i-3,j-3,ii-3)+F(i+3,j+3,ii+3)
          enddo  
        enddo
      enddo 

*dvm$ parallel (i,j,ii) on BA(i,j,ii),reduction( min( nloopa),
*dvm$* min(nloopd),min(nloopf) )
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3 
            if (BA(i,j,ii).ne.(C(i-1,j-1,ii-1)+c(i+1,j+1,ii+1)))
     *    nloopa=min(nloopa,i)
            if (BD(i,j,ii).ne.(C(i-2,j-2,ii-2)+c(i+2,j+2,ii+2)))
     *    nloopd=min(nloopd,i)
            if (BF(i,j,ii).ne.(C(i-3,j-3,ii-3)+c(i+3,j+3,ii+3)))
     *    nloopf=min(nloopf,i)
          enddo   
        enddo
      enddo
      
      if ((nloopa .eq.NL).and.(nloopd.eq.NL).and.(nloopf.eq.NL)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,BA,BD,BF,C,D,F)
      
      end
     
C ---------------------------------------------SHA3204
      subroutine SHA3204
      integer, parameter :: N = 16,M=16, K=16,NL=1000
      character*7 tname 
      integer, allocatable :: A(:,:,:),BA(:,:,:),BD(:,:,:)
      integer, allocatable :: BF(:,:,:),C(:,:,:),D(:,:,:),F(:,:,:)
      integer nloop 
                      
cdvm$ distribute BA(BLOCK,BLOCK,*)     
cdvm$ shadow D(2:2,2:2,2:2)
cdvm$ shadow F(3:3,3:3,3:3)
cdvm$ align (I,J,II) with BA(I,J,II) ::A,D,F,BD,BF

      tname='SHA3204'
      allocate (BA(N,M,K),A(N,M,K),BD(N,M,K))
      allocate (BF(N,M,K),C(N,M,K),D(N,M,K),F(N,M,K))
cdvm$ shadow_group ADF(A(CORNER),D(CORNER),F(CORNER))

      NNL=NL    
      call serial3(C,N,M,K,NNL)
      nloopa=NL
      nloopd=NL
      nloopf=NL

*dvm$ parallel (i,j,ii) on A(i,j,ii),shadow_start ADF
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = NL+i+j+ii
            D(i,j,ii) =NL+i+j+ii
            F(i,j,ii) =NL+i+j+ii
          enddo
        enddo
      enddo                                                

*dvm$ parallel (i,j,ii) on BA(i,j,ii),shadow_wait ADF
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            BA(i,j,ii) = A(i-1,j-1,ii-1)+A(i+1,j+1,ii+1)
            BD(i,j,ii)=  D(i-2,j-2,ii-2)+D(i+2,j+2,ii+2)
            BF(i,j,ii)=  F(i-3,j-3,ii-3)+F(i+3,j+3,ii+3)
          enddo  
        enddo
      enddo 

*dvm$ parallel (i,j,ii) on BA(i,j,ii),reduction( min( nloopa),
*dvm$* min(nloopd),min(nloopf) )
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3 
            if (BA(i,j,ii).ne.(C(i-1,j-1,ii-1)+c(i+1,j+1,ii+1)))
     *    nloopa=min(nloopa,i)
            if (BD(i,j,ii).ne.(C(i-2,j-2,ii-2)+c(i+2,j+2,ii+2)))
     *    nloopd=min(nloopd,i)
            if (BF(i,j,ii).ne.(C(i-3,j-3,ii-3)+c(i+3,j+3,ii+3)))
     *    nloopf=min(nloopf,i)
          enddo   
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
      subroutine serial3(AR,N,M,K,NL)
      integer AR(N,M,K)
      integer NL 
      do i=1,N
        do j=1,M
          do ii=1,K
            AR(i,j,ii) = NL+i+j+ii
          enddo
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
