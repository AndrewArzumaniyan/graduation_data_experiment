      program REDA12

c    TESTING OF THE REDUCTION_GROUP DIRECTIVE,REDUCTION_START
c    DIRECTIVE,REDUCTION_WAIT DIRECTIVE.
c    REDUCTION GROUPE IS EXECUTED FOR DISTRIBUTED ARRAY A(N).  
c

      print *,'===START OF REDA12========================'
C --------------------------------------------------
      call reda1201
C --------------------------------------------------
      call reda1202
C --------------------------------------------------
       call reda1203
C --------------------------------------------------
       call reda1204
C --------------------------------------------------
C
      print *,'=== END OF REDA12 ========================= '    
      end


C ----------------------------------------------------REDA1201
      subroutine REDA1201
      integer, parameter :: N = 16,NL=1000
      character*8 tname
      integer, allocatable :: A(:),C(:)
      integer isum1,isumt1 
      integer imax1,imaxt1 ,ni,imin1,imint1
                 
cdvm$ distribute A(*)     
cdvm$ reduction_group smaxmin
      tname='REDA1201'
      allocate (A(N),C(N))
      NNL=NL 
      NN=N
      call sersum1m(C,NN,NNL,isum1)

*dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo

      ni=N/2-1
      A(ni)=N+1+NL
      imax1=N+1+NL

cdvm$ remote_access (A(1))
      imaxt1=A(1)  

      ni=N/2
      A(ni)=-(N+1+NL)
      imin1=-(N+1+NL)

cdvm$ remote_access (A(1))      
      imint1=A(1)
  
      isumt1 = 0

*dvm$ parallel (i) on A(i),
*dvm$*reduction(smaxmin:sum(isumt1),max(imaxt1),min(imint1))
      do i=1,N
         isumt1 = isumt1+A(i)
         if (A(i).GT.imaxt1) imaxt1=A(i)
         if (A(i).LT.imint1) imint1=A(i)
      enddo

cdvm$ reduction_start smaxmin
cdvm$ reduction_wait smaxmin

      if ((isum1 .eq.isumt1) .and.(imax1 .eq.imaxt1)
     *   .and.(imin1 .eq.imint1))   then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)

      end

C -----------------------------------------------------REDA1202
      subroutine REDA1202
      integer, parameter :: N = 16, NL=1000
      character*8 tname
      integer, allocatable :: A(:),C(:)
      integer iprod1,iprodt1 
      logical, allocatable :: B(:),CL(:)
      logical land1,landt1,lor1,leqv1,lneqv1
                 
cdvm$ distribute A(*)     
cdvm$ align B(I) with A(I)
cdvm$ reduction_group prodand      
      tname='REDA1202'
      allocate (A(N),C(N))
      allocate (B(N),CL(N))
      NNL=NL
      NN=N
      call serprod1(C,NN,NNL,iprod1)
      call serlog1(CL,NN,land1,lor1,leqv1,lneqv1)  
               
*dvm$ parallel (i) on B(i)
      do i=1,N,2
         B(i) = .true.
      enddo
*dvm$ parallel (i) on B(i+1)
      do i=1,N-1,2
         B(i+1)=.false.
      enddo

cdvm$ remote_access (B(1))
      landt1 = B(1)

*dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo
  
      iprodt1 = 1

*dvm$ parallel (i) on A(i),
*dvm$*reduction(prodand:product( iprodt1 ),and(landt1))
      do i=1,N
         iprodt1 = iprodt1*A(i)
         landt1 = landt1 .and.B(i)
      enddo

cdvm$ reduction_start prodand

*dvm$ parallel (i) on A(i)
      do  i=1,N
         A(i) = i+NL
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


C ----------------------------------------------------REDA1203
      subroutine REDA1203
      integer, parameter :: N = 8,NL=1000
      character*8 tname
      integer, allocatable :: A(:),C(:)
      integer imax1,imaxt1 ,ni,imin1,imint1,it1,it2,lit
      integer imaxloct1,iminloct1
cdvm$ distribute A(*)    
cdvm$ reduction_group locmaxmin
c dvm$ reduction_group maxminloc
      tname='REDA1203'
      allocate (A(N),C(N))
      
*dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo

      ni=N/2+2
      A(ni)=N+1+NL
      imax1=N+1+NL

cdvm$ remote_access (A(1))
      imaxt1=A(1)  

      imaxloct1=imaxt1
      ni1=N/2+1
      A(ni1)=-(N+1+NL)
      imin1=-(N+1+NL)

cdvm$ remote_access (A(1))      
      imint1=A(1)

      iminloct1=imint1
      lit=1
      it1=0
      it2=0

*dvm$ parallel (i) on A(i),
*dvm$*reduction(locmaxmin:max( imaxt1 ),
*dvm$*maxloc( imaxloct1,it1,lit),
*dvm$*minloc( iminloct1,it2,lit))
      do i=1,N
         if (A(i).GT.imaxt1) imaxt1 =A(i)
         if (A(i).GT.imaxloct1) then
	   imaxloct1=A(i)
           it1=i 
         endif
         if (A(i).LT.iminloct1) then
           iminloct1=A(i)
           it2=i
         endif
      enddo

cdvm$ reduction_start locmaxmin
cdvm$ reduction_wait locmaxmin

      if ((imaxloct1.eq.imax1).and.(iminloct1.eq.imin1)
     *.and.(imaxt1.eq.imaxloct1).and.(it1.eq.ni)
     *.and.(it2.eq.ni1) )   then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)

      end


C ----------------------------------------------------REDA1204
      subroutine REDA1204
      integer, parameter :: N = 16
      real, parameter :: NL=1000.
      character*8 tname
      real, allocatable :: A(:),C(:)
      real isum1,isumt1
      real imax1,imaxt1 ,imin1,imint1
      real imaxloct1,iminloct1,NNL
      integer it1,it2,ni,ni1,lit          

cdvm$ distribute A(*)     
cdvm$ reduction_group locsumloc

      tname='REDA1204'
      allocate (A(N),C(N))
      NNL=NL 
      NN=N
      call sersum1mr(C,NN,NNL,isum1)

*dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo

      ni=N/2+1
      A(ni)=N+1.+NL
      imax1=N+1.+NL

cdvm$ remote_access (A(1))
      imaxt1=A(1)  

      imaxloct1=imaxt1
      ni1=N/2+2
      A(ni1)=-(N+1.+NL)
      imin1=-(N+1.+NL)

cdvm$ remote_access (A(1))      
      imint1=A(1)

      iminloct1=imint1
      isumt1 = 0.
      lit=1
      it1=0
      it2=0

*dvm$ parallel (i) on A(i),
*dvm$*reduction(locsumloc:sum( isumt1 ),
*dvm$*maxloc( imaxloct1,it1,lit),minloc( iminloct1,it2,lit ))
      do i=1,N
         isumt1 = isumt1+A(i)
         if (A(i).GT.imaxloct1) then
           imaxloct1=A(i)
           it1=i
         endif         
         if (A(i).LT.iminloct1) then
           iminloct1=A(i)
           it2=i
         endif
      enddo

cdvm$ reduction_start locsumloc
cdvm$ reduction_wait locsumloc
      if ((imaxloct1.eq.imax1).and.(iminloct1.eq.imin1)
     *.and.(isumt1.eq.isum1).and.(it1.eq.ni)
     *.and.(it2.eq.ni1) )   then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)

      end

C -----------------------------------------------------

      subroutine sersum1(AR,N,NL,S)
      integer AR(N)
      integer S,NL
      do i=1,N
         AR(i) = i+NL
      enddo
      S=0
      do i=1,N
         s = s+ AR(i)
      enddo
      end   
   
      subroutine sersum1m(AR,N,NL,S)
      integer AR(N)
      integer S,NL
      do i=1,N
         AR(i) = i+NL
      enddo
      ni=N/2-1
      AR(ni)=N+1+NL
      ni=N/2
      AR(ni)=-(N+1+NL)
      S=0
      do i=1,N
         s = s+ AR(i)
      enddo
      end 
  
      subroutine sersum1mr(AR,N,NL,S)
      real AR(N)
      real S,NL
      do i=1,N
         AR(i) = i+NL
      enddo
      ni=N/2+1
      AR(ni)=N+1.+NL
      ni=N/2+2
      AR(ni)=-(N+1.+NL)
      S=0.
      do i=1,N
         S = S+ AR(i)
      enddo
      end  
  
      subroutine serprod1(AR,N,NL,P)
      integer AR(N)
      integer P,NL
      do i=1,N
         AR(i) = i+NL
      enddo
      P=1
      do i=1,N
         P = P* AR(i)
      enddo
      end   
 
      subroutine serprodr1(AR,N,NL,P)
      real AR(N)
      real P,NL
      do i=1,N
         AR(i) = i+NL
      enddo
      P=1.
      do i=1,N
         P = P* AR(i)
      enddo
      end
     
      subroutine serlog1(AR,N,LAND,LOR,LEQV,LNEQV)
      logical AR(N)
      logical LAND,LOR,LEQV,LNEQV
      do i=1,N,2
         AR(i) = .true.
         AR(i+1)=.false.          
      enddo
      LAND=AR(1)
      LOR=AR(1)
      LEQV=AR(1)
      LNEQV=AR(1)
      do i=2,N
         LAND = LAND .and. AR(i)
         LOR = LOR .or.AR(i)
      enddo
      do i=1,N,2
         LEQV = LEQV .eqv. AR(i)
      enddo
      do i=1,N
         LNEQV = LNEQV .neqv. AR(i)
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
   
