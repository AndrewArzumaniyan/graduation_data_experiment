      program SHA43
     
c    TESTING OF THE SHADOW_GROUP DIRECTIVE ,SHADOW_START DIRECRIVE AND
c    SHADOW_WAIT DIRECTIVE.       
c    DISTRIBUTED ARRAYES A(N,M,K,L),D(N,M,K,L),F(N,M,K,L)
c    IS TO HAVE DIFFERENT SHADOW WIDTH ON BOTH SIDES 

      print *,'===START OF SHA43========================'
C --------------------------------------------------
      call sha4301
C --------------------------------------------------
      call sha4302
C --------------------------------------------------
      call sha4303
C -------------------------------------------------
      call sha4304
C -------------------------------------------------
 
C
C
      print *,'=== END OF SHA43 ========================= '    
      end
C ---------------------------------------------------------SHA4301
      subroutine SHA4301
      integer, parameter :: N = 16,M=16, K=16,L=16,NL=1000
      character*7 tname 
      integer, allocatable :: A(:,:,:,:),BA(:,:,:,:),BD(:,:,:,:)
      integer, allocatable :: BF(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer, allocatable :: F(:,:,:,:)
      integer nloop 
                      
cdvm$ distribute BA(BLOCK,BLOCK,BLOCK,*)     
cdvm$ shadow D(2:2,2:2,2:2,2:2)
cdvm$ shadow F(3:3,3:3,3:3,3:3)
cdvm$ align (I,J,II,JJ) with BA(I,J,II,JJ) ::A,D,F,BD,BF

      tname='SHA4301'
      allocate (BA(N,M,K,L),A(N,M,K,L),BD(N,M,K,L))
      allocate (BF(N,M,K,L),C(N,M,K,L),D(N,M,K,L),F(N,M,K,L))
cdvm$ shadow_group ADF(A(CORNER),D(CORNER),F(CORNER))

      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopa=NL
      nloopd=NL
      nloopf=NL
*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
              D(i,j,ii,jj) =NL+i+j+ii+jj
              F(i,j,ii,jj) =NL+i+j+ii+jj
            enddo
          enddo
        enddo
      enddo               

cdvm$ shadow_start ADF
cdvm$ shadow_wait ADF               
c      print *,'C'
c      print *,C  
c      print *,'A'
c      print *,A

*dvm$ parallel (i,j,ii,jj) on BA(i,j,ii,jj)
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            do jj=4,L-3
             BA(i,j,ii,jj)=A(i-1,j-1,ii-1,jj-1)+A(i+1,j+1,ii+1,jj+1)
             BD(i,j,ii,jj)= D(i-2,j-2,ii-2,jj-2)+D(i+2,j+2,ii+2,jj+2)
             BF(i,j,ii,jj)=  F(i-3,j-3,ii-3,jj-3)+F(i+3,j+3,ii+3,jj+3)
            enddo   
          enddo
        enddo  
      enddo
c      print *,'BA'
c      print *,BA

*dvm$ parallel (i,j,ii,jj) on BA(i,j,ii,jj),reduction( min( nloopa),
*dvm$* min(nloopd),min(nloopf) )
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3 
            do jj=4,L-3
      if (BA(i,j,ii,jj).ne.(C(i-1,j-1,ii-1,jj-1)+c(i+1,j+1,ii+1,jj+1)))
     *     nloopa=min(nloopa,i)
      if (BD(i,j,ii,jj).ne.(C(i-2,j-2,ii-2,jj-2)+c(i+2,j+2,ii+2,jj+2))) 
     *     nloopd=min(nloopd,i)
      if (BF(i,j,ii,jj).ne.(C(i-3,j-3,ii-3,jj-3)+c(i+3,j+3,ii+3,jj+3)))
     *     nloopf=min(nloopf,i)
            enddo
          enddo
        enddo
      enddo               
      
      if ((nloopa .eq.NL).and.(nloopd.eq.NL).and.(nloopf.eq.NL)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C ------------------------------------------------------------SHA4302
      subroutine SHA4302
      integer, parameter :: N = 16,M=16, K=16,L=16,NL=1000
      character*7 tname 
      integer, allocatable :: A(:,:,:,:),BA(:,:,:,:),BD(:,:,:,:)
      integer, allocatable :: BF(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer, allocatable :: F(:,:,:,:)
      integer nloop 
                      
cdvm$ distribute BA(BLOCK,BLOCK,*,BLOCK)     
cdvm$ shadow D(2:2,2:2,2:2,2:2)
cdvm$ shadow F(3:3,3:3,3:3,3:3)
cdvm$ align (I,J,II,JJ) with BA(I,J,II,JJ) ::A,D,F,BD,BF

      tname='SHA4302'
      allocate (BA(N,M,K,L),A(N,M,K,L),BD(N,M,K,L))
      allocate (BF(N,M,K,L),C(N,M,K,L),D(N,M,K,L),F(N,M,K,L))
cdvm$ shadow_group ADF(A(CORNER),D(CORNER),F(CORNER))

      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopa=NL
      nloopd=NL
      nloopf=NL

*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
              D(i,j,ii,jj) =NL+i+j+ii+jj
              F(i,j,ii,jj) =NL+i+j+ii+jj
            enddo
          enddo
        enddo
      enddo               

cdvm$ shadow_start ADF
*dvm$ parallel (i,j,ii,jj) on BA(i,j,ii,jj),shadow_wait ADF
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            do jj=4,L-3
            BA(i,j,ii,jj) = A(i-1,j-1,ii-1,jj-1)+A(i+1,j+1,ii+1,jj+1)
            BD(i,j,ii,jj)=  D(i-2,j-2,ii-2,jj-2)+D(i+2,j+2,ii+2,jj+2)
            BF(i,j,ii,jj)=  F(i-3,j-3,ii-3,jj-3)+F(i+3,j+3,ii+3,jj+3)
            enddo   
          enddo
        enddo  
      enddo
c      print *,'BA'
c      print *,BA
*dvm$ parallel (i,j,ii,jj) on BA(i,j,ii,jj),reduction( min( nloopa),
*dvm$* min(nloopd),min(nloopf) )
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3 
            do jj=4,L-3
      if (BA(i,j,ii,jj).ne.(C(i-1,j-1,ii-1,jj-1)+c(i+1,j+1,ii+1,jj+1)))
     *     nloopa=min(nloopa,i)
      if (BD(i,j,ii,jj).ne.(C(i-2,j-2,ii-2,jj-2)+c(i+2,j+2,ii+2,jj+2))) 
     *     nloopd=min(nloopd,i)
      if (BF(i,j,ii,jj).ne.(C(i-3,j-3,ii-3,jj-3)+c(i+3,j+3,ii+3,jj+3)))
     *     nloopf=min(nloopf,i)
            enddo
          enddo
        enddo
      enddo               

      if ((nloopa .eq.NL).and.(nloopd.eq.NL).and.(nloopf.eq.NL)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C --------------------------------------------------------------SHA4303
      subroutine SHA4303
      integer, parameter :: N = 16,M=16, K=16,L=16,NL=1000
      character*7 tname 
      integer, allocatable :: A(:,:,:,:),BA(:,:,:,:),BD(:,:,:,:)
      integer, allocatable :: BF(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer, allocatable :: F(:,:,:,:)
      integer nloop 
                      
cdvm$ distribute BA(BLOCK,*,BLOCK,BLOCK)     
cdvm$ shadow D(2:2,2:2,2:2,2:2)
cdvm$ shadow F(3:3,3:3,3:3,3:3)
cdvm$ align (I,J,II,JJ) with BA(I,J,II,JJ) ::A,D,F,BD,BF

      tname='SHA4303'
      allocate (BA(N,M,K,L),A(N,M,K,L),BD(N,M,K,L))
      allocate (BF(N,M,K,L),C(N,M,K,L),D(N,M,K,L),F(N,M,K,L))
cdvm$ shadow_group ADF(A(CORNER),D(CORNER),F(CORNER))

      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopa=NL
      nloopd=NL
      nloopf=NL
*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),shadow_start ADF
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
              D(i,j,ii,jj) =NL+i+j+ii+jj
              F(i,j,ii,jj) =NL+i+j+ii+jj
            enddo
          enddo
        enddo
      enddo               
                                        
cdvm$ shadow_wait ADF 

*dvm$ parallel (i,j,ii,jj) on BA(i,j,ii,jj)
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            do jj=4,L-3
            BA(i,j,ii,jj) = A(i-1,j-1,ii-1,jj-1)+A(i+1,j+1,ii+1,jj+1)
            BD(i,j,ii,jj)=  D(i-2,j-2,ii-2,jj-2)+D(i+2,j+2,ii+2,jj+2)
            BF(i,j,ii,jj)=  F(i-3,j-3,ii-3,jj-3)+F(i+3,j+3,ii+3,jj+3)
            enddo   
          enddo
        enddo  
      enddo
c      print *,'BA'
c      print *,BA

*dvm$ parallel (i,j,ii,jj) on BA(i,j,ii,jj),reduction( min( nloopa),
*dvm$* min(nloopd),min(nloopf) )
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3 
            do jj=4,L-3
      if (BA(i,j,ii,jj).ne.(C(i-1,j-1,ii-1,jj-1)+c(i+1,j+1,ii+1,jj+1)))
     *     nloopa=min(nloopa,i)
      if (BD(i,j,ii,jj).ne.(C(i-2,j-2,ii-2,jj-2)+c(i+2,j+2,ii+2,jj+2))) 
     *     nloopd=min(nloopd,i)
      if (BF(i,j,ii,jj).ne.(C(i-3,j-3,ii-3,jj-3)+c(i+3,j+3,ii+3,jj+3)))
     *     nloopf=min(nloopf,i)
            enddo
          enddo
        enddo
      enddo               
      
      if ((nloopa .eq.NL).and.(nloopd.eq.NL).and.(nloopf.eq.NL)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end
C --------------------------------------------------------------SHA4304
      subroutine SHA4304
      integer, parameter :: N = 16,M=16, K=16,L=16,NL=1000
      character*7 tname 
      integer, allocatable :: A(:,:,:,:),BA(:,:,:,:),BD(:,:,:,:)
      integer, allocatable :: BF(:,:,:,:),C(:,:,:,:),D(:,:,:,:)
      integer, allocatable :: F(:,:,:,:)
      integer nloop 
                      
cdvm$ distribute BA(*,BLOCK,BLOCK,BLOCK)     
cdvm$ shadow D(2:2,2:2,2:2,2:2)
cdvm$ shadow F(3:3,3:3,3:3,3:3)
cdvm$ align (I,J,II,JJ) with BA(I,J,II,JJ) ::A,D,F,BD,BF

      tname='SHA4304'
      allocate (BA(N,M,K,L),A(N,M,K,L),BD(N,M,K,L))
      allocate (BF(N,M,K,L),C(N,M,K,L),D(N,M,K,L),F(N,M,K,L))
cdvm$ shadow_group ADF(A(CORNER),D(CORNER),F(CORNER))

      NNL=NL    
      call serial4(C,N,M,K,L,NNL)
      nloopa=NL
      nloopd=NL
      nloopf=NL

*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),shadow_start ADF
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
              D(i,j,ii,jj) =NL+i+j+ii+jj
              F(i,j,ii,jj) =NL+i+j+ii+jj
            enddo
          enddo
        enddo
      enddo               

*dvm$ parallel (i,j,ii,jj) on BA(i,j,ii,jj),shadow_wait ADF
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3
            do jj=4,L-3
            BA(i,j,ii,jj) = A(i-1,j-1,ii-1,jj-1)+A(i+1,j+1,ii+1,jj+1)
            BD(i,j,ii,jj)=  D(i-2,j-2,ii-2,jj-2)+D(i+2,j+2,ii+2,jj+2)
            BF(i,j,ii,jj)=  F(i-3,j-3,ii-3,jj-3)+F(i+3,j+3,ii+3,jj+3)
            enddo   
          enddo
        enddo  
      enddo
c      print *,'BA'
c      print *,BA

*dvm$ parallel (i,j,ii,jj) on BA(i,j,ii,jj),reduction( min( nloopa),
*dvm$* min(nloopd),min(nloopf) )
      do i=4,N-3
        do j=4,M-3
          do ii=4,K-3 
            do jj=4,L-3
      if (BA(i,j,ii,jj).ne.(C(i-1,j-1,ii-1,jj-1)+c(i+1,j+1,ii+1,jj+1)))
     *     nloopa=min(nloopa,i)
      if (BD(i,j,ii,jj).ne.(C(i-2,j-2,ii-2,jj-2)+c(i+2,j+2,ii+2,jj+2))) 
     *     nloopd=min(nloopd,i)
      if (BF(i,j,ii,jj).ne.(C(i-3,j-3,ii-3,jj-3)+c(i+3,j+3,ii+3,jj+3)))
     *     nloopf=min(nloopf,i)
            enddo
          enddo
        enddo
      enddo               

      if ((nloopa .eq.NL).and.(nloopd.eq.NL).and.(nloopf.eq.NL)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end


C -----------------------------------------------         
      subroutine serial4(AR,N,M,K,L,NL)
      integer AR(N,M,K,L)
      integer NL 
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              AR(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
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
