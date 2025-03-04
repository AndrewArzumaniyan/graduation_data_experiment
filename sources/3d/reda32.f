      program REDA32

c    TESTING OF THE REDUCTION_GROUP DIRECTIVE,REDUCTION_START
c    DIRECTIVE,REDUCTION_WAIT DIRECTIVE.
c    REDUCTION GROUPE IS EXECUTED FOR DISTRIBUTED ARRAY A(N,M,K).  
c
      print *,'===START OF REDA32======================='
C --------------------------------------------------
      call reda3201
C --------------------------------------------------
      call reda3202
C --------------------------------------------------
      call reda3203
C -------------------------------------------------
      call reda3204
C -------------------------------------------------

C
C
      print *,'=== END OF REDA32 ========================= '    
      end


 

C ----------------------------------------------------REDA3201
      subroutine REDA3201
      integer, parameter :: N = 16,M=8,K=16,NL=1000
      character*8 tname
      integer, allocatable :: A(:,:,:),C(:,:,:)
      integer imin1,imint1 ,ni
      integer isum1,isumt1 
      integer imax1,imaxt1
                      
cdvm$ distribute A(BLOCK,BLOCK,*)
cdvm$ reduction_group smaxmin

      tname='REDA3201'
      allocate (A(N,M,K),C(N,M,K))
      NNL=NL 
      NN=N
      MM=M
      KK=K
      call sersum3m(C,NN,MM,KK,NNL,isum1)

*dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = i+j+ii+NL
          enddo
        enddo
      enddo
      ni=N/2-1
      nj=M/2-1
      nii=K/2-1
      A(ni,nj,nii)=N+M+K+1+NL
      imax1=N+M+K+1+NL

cdvm$ remote_access (A(1,1,1))
      imaxt1=A(1,1,1)  

      ni=N/2
      nj=M/2
      nii=K/2 
      A(ni,nj,nii)=-(N+M+K+1+NL)
      imin1=-(N+M+K+1+NL)

cdvm$ remote_access (A(1,1,1))
      imint1=A(1,1,1)

      isumt1 = 0

*dvm$ parallel (i,j,ii) on A(i,j,ii),
*dvm$*reduction(smaxmin:sum(isumt1),max(imaxt1),min(imint1))
      do i=1,N
        do j=1,M
          do ii=1,K
            isumt1 = isumt1+A(i,j,ii)
            if (A(i,j,ii).GT.imaxt1) imaxt1=A(i,j,ii)
            if (A(i,j,ii).LT.imint1) imint1=A(i,j,ii)
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
C ----------------------------------------------------REDA3202
      subroutine REDA3202
      integer, parameter :: N = 16,M=8,K=16,NL=1000
      character*8 tname
      integer, allocatable :: A(:,:,:),C(:,:,:)
      integer iprod1,iprodt1 
      logical, allocatable :: B(:,:,:),CL(:,:,:)
      logical land1,landt1,lor1,leqv1,lneqv1
                 
cdvm$ distribute A(BLOCK,*,BLOCK)    
cdvm$ align B(I,J,II) with A(I,J,II)      
cdvm$ reduction_group prodand

      tname='REDA3202'
      allocate (A(N,M,K),C(N,M,K))
      allocate (B(N,M,K),CL(N,M,K))
      NNL=NL
      NN=N
      MM=M
      KK=K
      call serprod3(C,NN,MM,KK,NNL,iprod1)
      call serlog3(CL,NN,MM,KK,land1,lor1,leqv1,lneqv1)

*dvm$ parallel (i,j,ii) on B(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K,2
            B(i,j,ii) = .true.
          enddo
        enddo         
      enddo

*dvm$ parallel (i,j,ii) on B(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=2,K,2 
           B(i,j,ii)=.false.
          enddo
        enddo
      enddo

cdvm$ remote_access (B(1,1,1))
      landt1 = B(1,1,1)    

*dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = i+j+ii+NL
          enddo
        enddo
      enddo
  
      iprodt1 = 1

*dvm$ parallel (i,j,ii) on A(i,j,ii),
*dvm$*reduction(prodand:product( iprodt1 ),and(landt1))
      do i=1,N
        do j=1,M
          do ii=1,K
           iprodt1 = iprodt1*A(i,j,ii)
           landt1 = landt1 .and.B(i,j,ii)
          enddo
        enddo
      enddo

cdvm$ reduction_start prodand

*dvm$ parallel (i,j,ii) on A(i,j,ii)
      do  i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = i+j+ii+NL
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


C ----------------------------------------------------REDA3203
      subroutine REDA3203
      integer, parameter :: N = 8, M=4,K=16,NL=1000
      character*8 tname
      integer, allocatable :: A(:,:,:),C(:,:,:)
      integer imax1,imaxt1 ,ni,imin1,imint1,it1,it2,jt1,jt2
      integer imaxloct1,iminloct1,lcoor
      integer coor1(3),coor2(3)
            
cdvm$ distribute A(*,BLOCK,BLOCK)    
cdvm$ reduction_group locmaxmin

      tname='REDA3203'
      allocate (A(N,M,K),C(N,M,K))
      
*dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = i+j+ii+NL
          enddo
        enddo
      enddo

      ni=N/2
      nj=M/2
      nii=K/2
      A(ni,nj,nii)=N+M+K+1+NL
      imax1=N+M+K+1+NL

cdvm$ remote_access (A(1,1,1))
      imaxt1=A(1,1,1)  

      imaxloct1=imaxt1
      ni1=N/2-1
      nj1=M/2-1
      nii1=K/2-1
      A(ni1,nj1,nii1)=-(N+M+K+1+NL)
      imin1=-(N+M+K+1+NL)

cdvm$ remote_access (A(1,1,1))      
      imint1=A(1,1,1)

      iminloct1=imint1
      lcoor=3
      coor1(1)=0
      coor1(2)=0
      coor1(3)=0
      coor2(1)=0
      coor2(2)=0
      coor2(3)=0

*dvm$ parallel (i,j,ii) on A(i,j,ii),
*dvm$*reduction(locmaxmin:max( imaxt1 ),
*dvm$*maxloc( imaxloct1,coor1,lcoor),
*dvm$*minloc( iminloct1,coor2,lcoor))
      do i=1,N
        do j=1,M
          do ii=1,K
           if (A(i,j,ii).GT.imaxt1) imaxt1 =A(i,j,ii)
           if (A(i,j,ii).GT.imaxloct1) then
             imaxloct1=A(i,j,ii)         
             coor1(1)=i
             coor1(2)=j 
             coor1(3)=ii 
           endif
           if (A(i,j,ii).LT.iminloct1) then
             iminloct1=A(i,j,ii)
             coor2(1)=i
             coor2(2)=j 
             coor2(3)=ii  
           endif
          enddo
        enddo
      enddo

cdvm$ reduction_startlocmaxmin
cdvm$ reduction_wait locmaxmin

c      print *,imax1,imaxt1,imaxloct1
c      print *,imin1,imint1,iminloct1
c      print *,it1,ni
c      print *,it2,ni1
c      print *,jt1,nj
c      print *,jt2,nj1
       if ((imaxloct1.eq.imax1).and.(iminloct1.eq.imin1).and.
     * (imaxt1.eq.imaxloct1).and.(coor1(1).eq.ni).and.
     * (coor1(2).eq.nj).and.(coor1(3).eq.nii).and.
     * (coor2(1).eq.ni1) .and.(coor2(2).eq.nj1) 
     * .and.(coor2(3).eq.nii1)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)

      end


C ----------------------------------------------------REDA3204
      subroutine REDA3204
      integer, parameter :: N = 8,M=6,K=16
      real, parameter :: NL=1000.
      character*8 tname
      real, allocatable :: A(:,:,:),C(:,:,:)
      real isum1,isumt1
      real imax1,imaxt1 ,imin1,imint1
      real imaxloct1,iminloct1,NNL
      integer ni,ni1,lcoor
      integer coor1(3),coor2(3)
          
cdvm$ distribute A(BLOCK,BLOCK,*)     
cdvm$ reduction_group locsum

      tname='REDA3204'
      allocate (A(N,M,K),C(N,M,K))
      NNL=NL 
      NN=N
      MM=M
      KK=K
      call sersum3mr(C,NN,MM,KK,NNL,isum1)

*dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = i+j+ii+NL
          enddo
        enddo
      enddo

      ni=N/2
      nj=M/2
      nii=K/2
      A(ni,nj,nii)=N+M+K+1.+NL
      imax1=N+M+K+1.+NL

cdvm$ remote_access (A(1,1,1))
      imaxt1=A(1,1,1)  

      imaxloct1=imaxt1
      ni1=N/2-1
      nj1=M/2-1
      nii1=K/2-1 
      A(ni1,nj1,nii1)=-(N+M+K+1.+NL)
      imin1=-(N+M+K+1.+NL)

cdvm$ remote_access (A(1,1,1))      
      imint1=A(1,1,1)

      iminloct1=imint1
      isumt1 = 0.
      lcoor=3
      coor1(1)=0
      coor1(2)=0
      coor1(3)=0
      coor2(1)=0
      coor2(2)=0
      coor2(3)=0

*dvm$ parallel (i,j,ii) on A(i,j,ii),
*dvm$*reduction(locsum:sum( isumt1 ),
*dvm$*maxloc( imaxloct1,coor1,lcoor),minloc( iminloct1,coor2,lcoor))
      do i=1,N
        do j=1,M
          do ii=1,K
           isumt1 = isumt1+A(i,j,ii)
           if (A(i,j,ii).GT.imaxloct1) then
             imaxloct1=A(i,j,ii)
             coor1(1)=i
             coor1(2)=j 
             coor1(3)=ii  
           endif
           if (A(i,j,ii).LT.iminloct1) then
             iminloct1=A(i,j,ii)
             coor2(1)=i
             coor2(2)=j 
             coor2(3)=ii  
           endif
          enddo
        enddo
      enddo

cdvm$ reduction_start locsum
cdvm$ reduction_wait locsum
c      print *,A
c      print *,imax1,imaxt1,imaxloct1
c      print *,imin1,imint1,iminloct1
     
c      print *,isum1,isumt1
c      print *,it1,ni
c      print *,it2,ni1
c      print *,jt1,nj
c      print *,jt2,nj1
      if ((imaxloct1.eq.imax1).and.(iminloct1.eq.imin1)
     *.and.(isumt1.eq.isum1).and.(coor1(1).eq.ni)
     *.and.(coor2(1).eq.ni1).and.(coor1(2).eq.nj) 
     *.and.(coor2(2).eq.nj1)
     *.and.(coor1(3).eq.nii).and.(coor2(3).eq.nii1))then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)

      end


C -----------------------------------------------------

      subroutine sersum3(AR,N,M,K,NL,S)
      integer AR(N,M,K)
      integer S,NL
      do i=1,N
        do j=1,M
          do ii=1,K
            AR(i,j,ii) = i+j+ii+NL
          enddo          
        enddo
      enddo
      S=0
      do i=1,N
        do j=1,M
          do ii=1,K
            s = s+ AR(i,j,ii)
          enddo
        enddo
      enddo
      end   
      
      subroutine sersum3m(AR,N,M,K,NL,S)
      integer AR(N,M,K)
      integer S,NL
      do i=1,N
        do j=1,M
          do ii=1,K
            AR(i,j,ii) = i+j+ii+NL
          enddo 
        enddo         
      enddo     
      ni=N/2-1
      nj=M/2-1
      nii=K/2-1
      AR(ni,nj,nii)=N+M+K+1+NL
      ni=N/2
      nj=M/2
      nii=K/2
      AR(ni,nj,nii)=-(N+M+K+1+NL)
      S=0
      do i=1,N
        do j=1,M
          do ii=1,K
            s = s+ AR(i,j,ii)
          enddo
        enddo
      enddo    
      end   

      subroutine sersum3mr(AR,N,M,K,NL,S)
      real AR(N,M,K)
      real S,NL
      do i=1,N
        do j=1,M
          do ii=1,K
            AR(i,j,ii) = i+j+ii+NL
          enddo 
        enddo
      enddo 
      ni=N/2-1
      nj=M/2-1
      nii=K/2-1
      AR(ni,nj,nii)=N+M+K+1+NL
      ni=N/2
      nj=M/2
      nii=K/2
      AR(ni,nj,nii)=-(N+M+K+1+NL)
      S=0.
      do i=1,N
        do j=1,M
          do ii=1,K
            s = s+ AR(i,j,ii)
          enddo
        enddo
      enddo
      end  
  
      subroutine serprod3(AR,N,M,K,NL,P)
      integer AR(N,M,K)
      integer P,NL
      do i=1,N
        do j=1,M
          do ii=1,K
           AR(i,j,ii) = i+j+ii+NL
          enddo
        enddo
      enddo   
      P=1
      do i=1,N
        do j=1,M
          do ii=1,K
            P = P* AR(i,j,ii)
          enddo
        enddo
      enddo
      end   
 
      subroutine serprodr3(AR,N,M,K,NL,P)
      real AR(N,M,K)
      real P,NL
      do i=1,N
        do j=1,M
          do ii=1,K 
            AR(i,j,ii) = i+j+ii+NL
          enddo
        enddo
      enddo 
      P=1.
      do i=1,N
        do j=1,M
          do ii=1,K
            P = P* AR(i,j,ii)
          enddo
        enddo
      enddo
      end
     
      subroutine serlog3(AR,N,M,K,LAND,LOR,LEQV,LNEQV)
      logical AR(N,M,K)
      logical LAND,LOR,LEQV,LNEQV
      do  i=1,N
        do j=1,M
          do ii=1,K,2
            AR(i,j,ii) = .true.
          enddo         
        enddo
      enddo
      do i=1,N
        do j=1,M
          do ii=2,K,2
            AR(i,j,ii)=.false.
          enddo
        enddo
      enddo 
      LAND=AR(1,1,1)
      LOR=AR(1,1,1)
      LEQV=AR(1,1,1)
      LNEQV=AR(1,1,1)
      do i=1,N
        do j=1,M
          do ii=1,K
            LAND = LAND .and. AR(i,j,ii)
            LOR = LOR .or.AR(i,j,ii)
          enddo
        enddo
      enddo
      do i=1,N
        do j=1,M
          do ii=1,K
            LEQV = LEQV .eqv. AR(i,j,ii)
          enddo
        enddo
      enddo
      do i=1,N
        do j=1,M
          do ii=1,K
            LNEQV = LNEQV .neqv. AR(i,j,ii)
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
