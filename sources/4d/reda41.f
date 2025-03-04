      program REDA41

c    TESTING OF THE REDUCTION_GROUP DIRECTIVE,REDUCTION_START
c    DIRECTIVE,REDUCTION_WAIT DIRECTIVE.
c    REDUCTION GROUPE IS EXECUTED FOR DISTRIBUTED ARRAY A(N,M,K,L).  
c
      print *,'===START OF REDA41======================='
C --------------------------------------------------
      call reda4101
C --------------------------------------------------
      call reda4102
C --------------------------------------------------
      call reda4103
C -------------------------------------------------
      call reda4104
C -------------------------------------------------

C
C
      print *,'=== END OF REDA41 ========================= '    
      end

C ----------------------------------------------------REDA4101
      subroutine REDA4101
      integer, parameter :: N = 16,M=8,K=16,L=8,NL=1000
      character*8 tname
      integer, allocatable :: A(:,:,:,:),C(:,:,:,:)
      integer imin1,imint1 ,ni
      integer isum1,isumt1 
      integer imax1,imaxt1
                      
cdvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)
cdvm$ reduction_group smaxmin

      tname='REDA4101'
      allocate (A(N,M,K,L),C(N,M,K,L))      
      NNL=NL 
      NN=N
      MM=M
      KK=K
      LL=L
      call sersum4m(C,NN,MM,KK,LL,NNL,isum1)

*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L      
             A(i,j,ii,jj) = i+j+ii+jj+NL
            enddo
          enddo
        enddo
      enddo
      ni=N/2-1
      nj=M/2-1
      nii=K/2-1
      njj=L/2-1
      A(ni,nj,nii,njj)=N+M+K+L+1+NL
      imax1=N+M+K+L+1+NL

cdvm$ remote_access (A(1,1,1,1))
      imaxt1=A(1,1,1,1)  

      ni=N/2
      nj=M/2
      nii=K/2
      njj=L/2 
      A(ni,nj,nii,njj)=-(N+M+K+L+1+NL)
      imin1=-(N+M+K+L+1+NL)

cdvm$ remote_access (A(1,1,1,1))
      imint1=A(1,1,1,1)

      isumt1 = 0

*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
*dvm$*reduction(smaxmin:sum(isumt1),max(imaxt1),min(imint1))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
             isumt1 = isumt1+A(i,j,ii,jj)
             if (A(i,j,ii,jj).GT.imaxt1) imaxt1=A(i,j,ii,jj)
             if (A(i,j,ii,jj).LT.imint1) imint1=A(i,j,ii,jj)
            enddo
          enddo
        enddo
      enddo

cdvm$ reduction_start smaxmin
cdvm$ reduction_wait smaxmin
c      print *,isumt1,isum1
c      print *,imaxt1,imax1
c      print *,imint1,imin1
      if ((isum1 .eq.isumt1) .and.(imax1 .eq.imaxt1)
     *   .and.(imin1 .eq.imint1))   then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)

      end
C ----------------------------------------------------REDA4102
      subroutine REDA4102
      integer, parameter :: N = 16,M=8,K=16,L=8,NL=1000
      character*8 tname
      integer, allocatable :: A(:,:,:,:),C(:,:,:,:)
      integer iprod1,iprodt1 
      logical, allocatable :: B(:,:,:,:),CL(:,:,:,:)
      logical land1,landt1,lor1,leqv1,lneqv1
               
                 
cdvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)    
cdvm$ align B(I,J,II,JJ) with A(I,J,II,JJ)      
cdvm$ reduction_group prodand

      tname='REDA4102'
      allocate (A(N,M,K,L),C(N,M,K,L))
      allocate (B(N,M,K,L),CL(N,M,K,L))
      NNL=NL
      NN=N
      MM=M
      KK=K
      LL=L
      call serprod4(C,NN,MM,KK,LL,NNL,iprod1)
      call serlog4(CL,NN,MM,KK,LL,land1,lor1,leqv1,lneqv1)

*dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L,2
              B(i,j,ii,jj) = .true.
            enddo
          enddo 
        enddo         
      enddo
*dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K 
            do jj=2,L,2
              B(i,j,ii,jj)=.false.
            enddo
          enddo
        enddo
      enddo

cdvm$ remote_access (B(1,1,1,1))
      landt1 = B(1,1,1,1)                   

*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = i+j+ii+jj+NL
            enddo
          enddo
        enddo
      enddo
      iprodt1 = 1

*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
*dvm$*reduction(prodand:product( iprodt1 ),and(landt1))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L      
              iprodt1 = iprodt1*A(i,j,ii,jj)
              landt1 = landt1 .and.B(i,j,ii,jj)
            enddo
          enddo
        enddo
      enddo

cdvm$ reduction_start prodand

*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do  i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L 
              A(i,j,ii,jj) = i+j+ii+jj+NL
            enddo
          enddo
        enddo
      enddo

cdvm$ reduction_wait prodand 

      if ((iprod1 .eq.iprodt1)
     *.and. (land1 .eqv.landt1)) then       
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
      deallocate (B,CL)
 
      end   


C ----------------------------------------------------REDA4103
      subroutine REDA4103
      integer, parameter :: N = 8, M=4,K=16,L=8,NL=1000
      character*8 tname
      integer, allocatable :: A(:,:,:,:),C(:,:,:,:)
      integer imax1,imaxt1 ,ni,imin1,imint1
      integer imaxloct1,iminloct1,lcoor
      integer coor1(4),coor2(4)         
cdvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)    
cdvm$ reduction_group locmaxmin

      tname='REDA4103'
      allocate (A(N,M,K,L),C(N,M,K,L))
      
*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = i+j+ii+jj+NL
            enddo
          enddo
        enddo
      enddo

      ni=N/2
      nj=M/2
      nii=K/2
      njj=L/2
      A(ni,nj,nii,njj)=N+M+K+L+1+NL
      imax1=N+M+K+L+1+NL

cdvm$ remote_access (A(1,1,1,1))
      imaxt1=A(1,1,1,1)  

      imaxloct1=imaxt1
      ni1=N/2-1
      nj1=M/2-1
      nii1=K/2-1
      njj1=L/2-1
      A(ni1,nj1,nii1,njj1)=-(N+M+K+L+1+NL)
      imin1=-(N+M+K+L+1+NL)

cdvm$ remote_access (A(1,1,1,1))      
      imint1=A(1,1,1,1)

      iminloct1=imint1
      lcoor=4
      coor1(1)=0
      coor1(2)=0
      coor1(3)=0
      coor1(4)=0
      coor2(1)=0
      coor2(2)=0
      coor2(3)=0
      coor2(4)=0

*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
*dvm$*reduction(locmaxmin:max( imaxt1 ),
*dvm$*maxloc( imaxloct1,coor1,lcoor),
*dvm$*minloc( iminloct1,coor2,lcoor))
      do i=1,N
        do j=1,M
          do ii=1,K
           do jj=1,L
            if (A(i,j,ii,jj).GT.imaxt1) imaxt1 =A(i,j,ii,jj)
            if (A(i,j,ii,jj).GT.imaxloct1) then
              imaxloct1=A(i,j,ii,jj)
              coor1(1)=i
              coor1(2)=j 
              coor1(3)=ii
              coor1(4)=jj  
            endif
            if (A(i,j,ii,jj).LT.iminloct1) then
              iminloct1=A(i,j,ii,jj)
              coor2(1)=i
              coor2(2)=j 
              coor2(3)=ii 
              coor2(4)=jj
            endif
           enddo
          enddo
        enddo
      enddo

cdvm$ reduction_start locmaxmin
cdvm$ reduction_wait locmaxmin

c      print *,imax1,imaxt1,imaxloct1
c      print *,imin1,imint1,iminloct1
c      print *,it1,ni
c      print *,it2,ni1
c      print *,jt1,nj
c      print *,jt2,nj1
       if ((imaxloct1.eq.imax1).and.(iminloct1.eq.imin1).and.
     * (imaxt1.eq.imaxloct1).and.( coor1(1).eq.ni).and.
     * (coor1(2).eq.nj).and.(coor1(3).eq.nii)
     * .and.(coor1(4).eq.njj).and.
     * (coor2(1).eq.ni1) .and.(coor2(2).eq.nj1) 
     * .and.(coor2(3).eq.nii1).and.
     * (coor2(4).eq.njj1)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)

      end


C ----------------------------------------------------REDA4104
      subroutine REDA4104
      integer, parameter :: N = 8,M=6,K=16,L=8
      real, parameter :: NL=1000.
      character*8 tname
      real, allocatable :: A(:,:,:,:),C(:,:,:,:)
      real isum1,isumt1
      real imax1,imaxt1 ,imin1,imint1
      real imaxloct1,iminloct1,NNL
      integer ni,ni1,lcoor
      integer coor1(4),coor2(4)
   
cdvm$ distribute A(BLOCK,BLOCK,BLOCK,BLOCK)     
cdvm$ reduction_group locsum

      tname='REDA4104'
      allocate (A(N,M,K,L),C(N,M,K,L))

      NNL=NL 
      NN=N
      MM=M
      KK=K
      LL=L
      call sersum4mr(C,NN,MM,KK,LL,NNL,isum1)

*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = i+j+ii+jj+NL
            enddo
          enddo
        enddo
      enddo

      ni=N/2
      nj=M/2
      nii=K/2
      njj=L/2
      A(ni,nj,nii,njj)=N+M+K+L+1.+NL
      imax1=N+M+K+L+1.+NL

cdvm$ remote_access (A(1,1,1,1))
      imaxt1=A(1,1,1,1)  

      imaxloct1=imaxt1
      ni1=N/2-1
      nj1=M/2-1
      nii1=K/2-1
      njj1=L/2-1 
      A(ni1,nj1,nii1,njj1)=-(N+M+K+L+1.+NL)
      imin1=-(N+M+K+L+1.+NL)

cdvm$ remote_access (A(1,1,1,1))      
      imint1=A(1,1,1,1)

      iminloct1=imint1
      isumt1 = 0.
      lcoor=4
      coor1(1)=0
      coor1(2)=0
      coor1(3)=0
      coor1(4)=0
      coor2(1)=0
      coor2(2)=0
      coor2(3)=0
      coor2(4)=0

*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
*dvm$*reduction(locsum:sum( isumt1 ),
*dvm$*maxloc( imaxloct1,coor1,lcoor ),minloc( iminloct1,coor2,lcoor ))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L 
             isumt1 = isumt1+A(i,j,ii,jj)
             if (A(i,j,ii,jj).GT.imaxloct1) then
               imaxloct1=A(i,j,ii,jj)
               coor1(1)=i
               coor1(2)=j 
               coor1(3)=ii
               coor1(4)=jj  
             endif
             if (A(i,j,ii,jj).LT.iminloct1) then
               iminloct1=A(i,j,ii,jj)
               coor2(1)=i
               coor2(2)=j 
               coor2(3)=ii 
               coor2(4)=jj
             endif
            enddo
          enddo
        enddo
      enddo

cdvm$ reduction_start locsum
cdvm$ reduction_wait locsum
c      print *,A
c       print *,imax1,imaxt1,imaxloct1
c       print *,imin1,imint1,iminloct1
     
c       print *,isum1,isumt1
c      print *,it1,ni
c      print *,it2,ni1
c      print *,jt1,nj
c      print *,jt2,nj1
      if ((imaxloct1.eq.imax1).and.(iminloct1.eq.imin1)
     *.and.(isumt1.eq.isum1).and.(coor1(1).eq.ni)
     *.and.(coor2(1).eq.ni1).and.(coor1(2).eq.nj)
     * .and.(coor2(2).eq.nj1)
     *.and.(coor1(3).eq.nii).and.(coor2(3).eq.nii1).and.
     * (coor1(4).eq.njj).and.(coor2(4).eq.njj1))then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)

      end


C -----------------------------------------------------

      subroutine sersum4(AR,N,M,K,L,NL,S)
      integer AR(N,M,K,L)
      integer S,NL

      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              AR(i,j,ii,jj) = i+j+ii+jj+NL
            enddo          
          enddo
        enddo
      enddo
      S=0
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              s = s+ AR(i,j,ii,jj)
            enddo
          enddo
        enddo
      enddo
      end   
   
      subroutine sersum4m(AR,N,M,K,L,NL,S)
      integer AR(N,M,K,L)
      integer S,NL
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              AR(i,j,ii,jj) = i+j+ii+jj+NL
            enddo 
          enddo 
        enddo        
      enddo
      ni=N/2-1
      nj=M/2-1
      nii=K/2-1
      njj=L/2-1
      AR(ni,nj,nii,njj)=N+M+K+L+1+NL
      ni=N/2
      nj=M/2
      nii=K/2
      njj=L/2
      AR(ni,nj,nii,njj)=-(N+M+K+L+1+NL)      
      S=0
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              s = s+ AR(i,j,ii,jj)
            enddo
          enddo
        enddo
      enddo    
      end   

      subroutine sersum4mr(AR,N,M,K,L,NL,S)
      real AR(N,M,K,L)
      real S,NL
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              AR(i,j,ii,jj) = i+j+ii+jj+NL
            enddo 
          enddo
        enddo
      enddo 
      ni=N/2-1
      nj=M/2-1
      nii=K/2-1
      njj=L/2-1
      AR(ni,nj,nii,njj)=N+M+K+L+1.+NL
      ni=N/2
      nj=M/2
      nii=K/2
      njj=L/2
      AR(ni,nj,nii,njj)=-(N+M+K+L+1.+NL)
      S=0.
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              s = s+ AR(i,j,ii,jj)
            enddo
          enddo
        enddo
      enddo
      end  
  
      subroutine serprod4(AR,N,M,K,L,NL,P)
      integer AR(N,M,K,L)
      integer P,NL
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              AR(i,j,ii,jj) = i+j+ii+jj+NL
            enddo
          enddo
        enddo
      enddo   
      P=1
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              P = P* AR(i,j,ii,jj)
            enddo
          enddo
        enddo
      enddo
      end   
 
      subroutine serprodr4(AR,N,M,K,L,NL,P)
      real AR(N,M,K,L)
      real P,NL
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L 
              AR(i,j,ii,jj) = i+j+ii+jj+NL
            enddo
          enddo
        enddo
      enddo 
      P=1.
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              P = P* AR(i,j,ii,jj)
            enddo
          enddo
        enddo
      enddo
      end
     
      subroutine serlog4(AR,N,M,K,L,LAND,LOR,LEQV,LNEQV)
      logical AR(N,M,K,L)
      logical LAND,LOR,LEQV,LNEQV
      do  i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L,2
              AR(i,j,ii,jj) = .true.
            enddo         
          enddo
        enddo
      enddo
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=2,L,2     
              AR(i,j,ii,jj)=.false.
            enddo
          enddo
        enddo 
      enddo
      LAND=AR(1,1,1,1)
      LOR=AR(1,1,1,1)
      LEQV=AR(1,1,1,1)
      LNEQV=AR(1,1,1,1)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              LAND = LAND .and. AR(i,j,ii,jj)
              LOR = LOR .or.AR(i,j,ii,jj)
            enddo
          enddo
        enddo
      enddo
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              LEQV = LEQV .eqv. AR(i,j,ii,jj)
            enddo
          enddo
        enddo
      enddo
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L 
              LNEQV = LNEQV .neqv. AR(i,j,ii,jj)
            enddo
          enddo
        enddo
      enddo
      end   

      subroutine ansyes(name)
      character*8 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*8 name
      print *,name,'  -  ***error'
      end
   