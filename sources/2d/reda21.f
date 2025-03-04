      program REDA21

c    TESTING OF THE REDUCTION_GROUP DIRECTIVE,REDUCTION_START
c    DIRECTIVE,REDUCTION_WAIT DIRECTIVE.
c    REDUCTION GROUPE IS EXECUTED FOR DISTRIBUTED ARRAY A(N,M).  
c
      print *,'===START OF REDA21======================='
C --------------------------------------------------
      call reda2101
C --------------------------------------------------
      call reda2102
C --------------------------------------------------
      call reda2103
C -------------------------------------------------
      call reda2104
C -------------------------------------------------

C
C
      print *,'=== END OF REDA21 ========================= '    
      end


 

C ----------------------------------------------------REDA2101
      subroutine REDA2101
      integer, parameter :: N = 16,M=8,NL=1000
      character*8 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer imin1,imint1 ,ni
      integer isum1,isumt1 
      integer imax1,imaxt1
                      
cdvm$ distribute A(BLOCK,BLOCK)
cdvm$ reduction_group smaxmin
      tname='REDA2101'
      allocate (A(N,M),C(N,M))
      NNL=NL 
      NN=N
      MM=M
      call sersum2m(C,NN,MM,NNL,isum1)

*dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
           A(i,j) = i+j+NL
         enddo
      enddo
      ni=N/2-1
      nj=M/2-1
      A(ni,nj)=N+M+1+NL
      imax1=N+M+1+NL

cdvm$ remote_access (A(1,1))
      imaxt1=A(1,1)  

      ni=N/2
      nj=M/2
      A(ni,nj)=-(N+M+1+NL)
      imin1=-(N+M+1+NL)

cdvm$ remote_access (A(1,1))
      imint1=A(1,1)

      isumt1 = 0

*dvm$ parallel (i,j) on A(i,j),
*dvm$*reduction(smaxmin:sum(isumt1),max(imaxt1),min(imint1))
      do i=1,N
         do j=1,M
           isumt1 = isumt1+A(i,j)
           if (A(i,j).GT.imaxt1) imaxt1=A(i,j)
           if (A(i,j).LT.imint1) imint1=A(i,j)
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
C ----------------------------------------------------REDA2102
      subroutine REDA2102
      integer, parameter :: N = 16,M=8,NL=1000
      character*8 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer iprod1,iprodt1 
      logical, allocatable :: B(:,:),CL(:,:)
      logical land1,landt1,lor1,leqv1,lneqv1
                 
cdvm$ distribute A(BLOCK,BLOCK)    
cdvm$ align B(I,J) with A(I,J)      
cdvm$ reduction_group prodand

      tname='REDA2102'
      allocate (A(N,M),C(N,M))
      allocate (B(N,M),CL(N,M))
      NNL=NL
      NN=N
      MM=M
      call serprod2(C,NN,MM,NNL,iprod1)
      call serlog2(CL,NN,MM,land1,lor1,leqv1,lneqv1)

*dvm$ parallel (i,j) on B(i,j)
      do i=1,N
         do j=1,M,2
            B(i,j) = .true.
         enddo         
      enddo

*dvm$ parallel (i,j) on B(i,j)
      do i=1,N
         do j=2,M,2
            B(i,j)=.false.
         enddo
      enddo
cdvm$ remote_access (B(1,1))
      landt1 = B(1,1)    

*dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
            A(i,j) = i+j+NL
         enddo
      enddo
  
      iprodt1 = 1

*dvm$ parallel (i,j) on A(i,j),
*dvm$*reduction(prodand:product( iprodt1 ),and(landt1))
      do i=1,N
         do j=1,M
           iprodt1 = iprodt1*A(i,j)
           landt1 = landt1 .and.B(i,j)
         enddo
      enddo
cdvm$ reduction_start prodand

*dvm$ parallel (i,j) on A(i,j)
      do  i=1,N
        do j=1,M
          A(i,j) = i+j+NL
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


C ----------------------------------------------------REDA2103
      subroutine REDA2103
      integer, parameter :: N = 8, M=4,PN = 2,NL=1000
      character*8 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer imax1,imaxt1 ,ni,imin1,imint1,it1,it2,jt1,jt2
      integer imaxloct1,iminloct1,lcoor
      integer coor1(2),coor2(2)          
cdvm$ distribute A(BLOCK,BLOCK)    
cdvm$ reduction_group locmaxmin
      tname='REDA2103'
      allocate (A(N,M),C(N,M))
      
*dvm$ parallel (i,j) on A(i,j)
      do i=1,N
        do j=1,M
           A(i,j) = i+j+NL
        enddo
      enddo

      ni=N/2
      nj=M/2
      A(ni,nj)=N+M+1+NL
      imax1=N+M+1+NL

cdvm$ remote_access (A(1,1))
      imaxt1=A(1,1)

      imaxloct1=imaxt1
      ni1=N/2-1
      nj1=M/2-1
      A(ni1,nj1)=-(N+M+1+NL)
      imin1=-(N+M+1+NL)

cdvm$ remote_access (A(1,1))      
      imint1=A(1,1)

      iminloct1=imint1
      lcoor=2
      coor1(1)=0
      coor1(2)=0
      coor2(1)=0
      coor2(2)=0

*dvm$ parallel (i,j) on A(i,j),
*dvm$*reduction(locmaxmin:max( imaxt1 ),
*dvm$*maxloc( imaxloct1,coor1,lcoor),
*dvm$*minloc( iminloct1,coor2,lcoor))
      do i=1,N
        do j=1,M
         if (A(i,j).GT.imaxt1) imaxt1 =A(i,j)
         if (A(i,j).GT.imaxloct1) then
           imaxloct1=A(i,j)
           coor1(1)=i
           coor1(2)=j 
         endif
         if (A(i,j).LT.iminloct1) then
           iminloct1=A(i,j)
           coor2(1)=i
           coor2(2)=j
         endif
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
      if ((imaxloct1.eq.imax1).and.(iminloct1.eq.imin1)
     *.and.(imaxt1.eq.imaxloct1).and.(coor1(1).eq.ni)
     *.and.(coor2(1).eq.ni1) 
     *.and.(coor1(2).eq.nj) .and.(coor2(2).eq.nj1))then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)

      end


C ----------------------------------------------------REDA2104
      subroutine REDA2104

      integer, parameter :: N = 8,M=6
      real, parameter :: NL=1000.
      character*8 tname
      real, allocatable :: A(:,:),C(:,:)
      real isum1,isumt1
      real imax1,imaxt1 ,imin1,imint1
      real imaxloct1,iminloct1,NNL
      integer it1,it2,ni,ni1,jt1,jt2
      integer coor1(2),coor2(2),lcoor          
cdvm$ distribute A(BLOCK,BLOCK)     
cdvm$ reduction_group locsum

      tname='REDA2104'
      allocate (A(N,M),C(N,M))
      NNL=NL 
      NN=N
      MM=M
      call sersum2mr(C,NN,MM,NNL,isum1)
*dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
           A(i,j) = i+j+NL
         enddo
      enddo

c      ni=N/2+1
c      nj=M/2+1
      ni=N/2
      nj=M/2
      A(ni,nj)=N+M+1.+NL
      imax1=N+M+1.+NL

cdvm$ remote_access (A(1,1))
      imaxt1=A(1,1)  

      imaxloct1=imaxt1
      ni1=N/2-1
      nj1=M/2-1
      A(ni1,nj1)=-(N+M+1.+NL)
      imin1=-(N+M+1.+NL)

cdvm$ remote_access (A(1,1))      
      imint1=A(1,1)

      iminloct1=imint1
      isumt1 = 0.
      lcoor=2
      coor1(1)=0
      coor1(2)=0
      coor2(1)=0
      coor2(2)=0

*dvm$ parallel (i,j) on A(i,j),
*dvm$*reduction(locsum:sum( isumt1 ),
*dvm$*maxloc( imaxloct1,coor1,lcoor ),minloc( iminloct1,coor2,lcoor ))
      do i=1,N
        do j=1,M
          isumt1 = isumt1+A(i,j)
          if (A(i,j).GT.imaxloct1) then
           imaxloct1=A(i,j)
           coor1(1)=i
           coor1(2)=j  
          endif
          if (A(i,j).LT.iminloct1) then
           iminloct1=A(i,j)
           coor2(1)=i
           coor2(2)=j  
          endif
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
     *.and.(coor2(1).eq.ni1)
     *.and.(coor1(2).eq.nj) .and.(coor2(2).eq.nj1))then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)

      end


C -----------------------------------------------------
      subroutine sersum2(AR,N,M,NL,S)
      integer AR(N,M)
      integer S,NL

      do i=1,N
         do j=1,M
           AR(i,j) = i+j+NL
         enddo          
      enddo
      S=0
      do i=1,N
         do j=1,M
           s = s+ AR(i,j)
         enddo
      enddo
      end   
   
      subroutine sersum2m(AR,N,M,NL,S)
      integer AR(N,M)
      integer S,NL

      do i=1,N
         do j=1,M
           AR(i,j) = i+j+NL
         enddo          
      enddo
     
      ni=N/2-1
      nj=M/2-1
      AR(ni,nj)=N+M+1+NL
      ni=N/2
      nj=M/2
      AR(ni,nj)=-(N+M+1+NL)
      S=0

      do i=1,N
         do j=1,M
           s = s+ AR(i,j)
         enddo
      enddo
    
      end   

      subroutine sersum2mr(AR,N,M,NL,S)
      real AR(N,M)
      real S,NL

      do i=1,N
         do j=1,M
           AR(i,j) = i+j+NL
         enddo 
         enddo 
      ni=N/2
      nj=M/2
      AR(ni,nj)=N+M+1.+NL
      ni1=N/2-1
      nj1=M/2-1
      AR(ni1,nj1)=-(N+M+1.+NL)
      S=0.
      do i=1,N
         do j=1,M
           s = s+ AR(i,j)
         enddo
      enddo
      end  
  
      subroutine serprod2(AR,N,M,NL,P)
      integer AR(N,M)
      integer P,NL
      do i=1,N
         do j=1,M
           AR(i,j) = i+j+NL
         enddo
      enddo   
      P=1
      do i=1,N
         do j=1,M
           P = P* AR(i,j)
         enddo
      enddo
      end   
 
      subroutine serprodr2(AR,N,M,NL,P)
      real AR(N,M)
      real P,NL

      do i=1,N
         do j=1,M
           AR(i,j) = i+j+NL
         enddo
      enddo 
      P=1.
      do i=1,N
         do j=1,M
           P = P* AR(i,j)
         enddo
      enddo
      end
     
      subroutine serlog2(AR,N,M,LAND,LOR,LEQV,LNEQV)
      logical AR(N,M)
      logical LAND,LOR,LEQV,LNEQV
      do  i=1,N
         do j=1,M,2
           AR(i,J) = .true.
         enddo         
      enddo
      do i=1,N
         do j=2,M,2
           AR(i,j)=.false.
         enddo
      enddo 
      LAND=AR(1,1)
      LOR=AR(1,1)
      LEQV=AR(1,1)
      LNEQV=AR(1,1)
      do i=1,N
         do j=1,M
           LAND = LAND .and. AR(i,j)
           LOR = LOR .or.AR(i,j)
         enddo
      enddo
       do i=1,N
         do j=1,M
           LEQV = LEQV .eqv. AR(i,j)
         enddo
      enddo
      do i=1,N
         do j=1,M
           LNEQV = LNEQV .neqv. AR(i,j)
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
