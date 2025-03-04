      program REDA11

c    TESTING OF THE REDUCTION_GROUP DIRECTIVE,REDUCTION_START
c    DIRECTIVE,REDUCTION_WAIT DIRECTIVE.
c    REDUCTION GROUP IS EXECUTED FOR DISTRIBUTED ARRAY A(N).  
c

      print *,'===START OF REDA11========================'
C --------------------------------------------------
      call reda1101
C --------------------------------------------------
      call reda1102
C --------------------------------------------------
      call reda1103
C --------------------------------------------------
      call reda1104
C --------------------------------------------------
C
      print *,'=== END OF REDA11 ========================= '    

      end

C ----------------------------------------------------REDA1101
      subroutine REDA1101
      integer, parameter :: N = 16,NL=1000
      integer, allocatable :: A(:),C(:)
      integer isum1,isumt1 
      integer imax1,imaxt1 ,ni,imin1,imint1
      character(8) :: tname='REDA1101'

!dvm$ distribute A(BLOCK)     
!dvm$ reduction_group smaxmin
      allocate (A(N),C(N))
      NNL=NL 
      NN=N
      call sersum1m(C,NN,NNL,isum1)

!dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo


      ni=N/2-1
      A(ni)=N+1+NL
      imax1=N+1+NL

!dvm$ remote_access (A(1))
      imaxt1=A(1)  

      ni=N/2
      A(ni)=-(N+1+NL)
      imin1=-(N+1+NL)

!dvm$ remote_access (A(1))      
      imint1=A(1)
  
      isumt1 = 0

!dvm$ parallel (i) on A(i),
!dvm$*reduction(smaxmin:sum(isumt1),max(imaxt1),min(imint1))
      do i=1,N
         isumt1 = isumt1+A(i)
         if (A(i).GT.imaxt1) imaxt1=A(i)
         if (A(i).LT.imint1) imint1=A(i)
      enddo

!dvm$ reduction_start smaxmin
!dvm$ reduction_wait smaxmin

      if ((isum1 .eq.isumt1) .and.(imax1 .eq.imaxt1)
     *   .and.(imin1 .eq.imint1))   then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A,C)

      end

C -----------------------------------------------------REDA1102
      subroutine REDA1102
      integer, parameter :: N = 16, NL=1000
      integer, allocatable :: A(:),C(:)
      integer iprod1,iprodt1 
      logical, allocatable :: B(:),CL(:)
      logical land1,landt1,lor1,leqv1,lneqv1
      character(8) :: tname='REDA1102'
                 
!dvm$ distribute A(BLOCK)     
!dvm$ align B(I) with A(I)
!dvm$ reduction_group prodand      
      allocate (A(N),C(N))
      allocate (B(N),CL(N))
      NNL=NL
      NN=N
      call serprod1(C,NN,NNL,iprod1)
      call serlog1(CL,NN,land1,lor1,leqv1,lneqv1)  
               
!dvm$ parallel (i) on B(i)
      do i=1,N,2
         B(i) = .true.
      enddo
!dvm$ parallel (i) on B(i+1)
      do i=1,N-1,2
         B(i+1)=.false.
      enddo

!dvm$ remote_access (B(1))
      landt1 = B(1)

!dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo
  
      iprodt1 = 1

!dvm$ parallel (i) on B(i),
!dvm$*reduction(prodand:product( iprodt1 ),and(landt1))
      do i=1,N
         iprodt1 = iprodt1*A(i)
         landt1 = landt1 .and.B(i)
      enddo

!dvm$ reduction_start prodand

!dvm$ parallel (i) on A(i)
      do  i=1,N
         A(i) = i+NL
      enddo

!dvm$ reduction_wait prodand

      if ((iprod1 .eq.iprodt1)
     *.and. (land1 .eqv.landt1)) then     
          call ansyes(tname)
       else
          call ansno(tname)
      endif 
 
      deallocate (A,C)
      deallocate (B,CL)
 
      end

C ----------------------------------------------------REDA1103
      subroutine REDA1103
      integer, parameter :: N = 8,NL=1000
      integer, allocatable :: A(:),C(:)
      integer imax1,imaxt1 ,ni,imin1,imint1,it1,it2,lit
      integer imaxloct1,iminloct1
      character(8) :: tname='REDA1103'

!dvm$ distribute A(BLOCK)    
!dvm$ reduction_group locmaxmin
c dvm$ reduction_group maxminloc

      allocate (A(N),C(N))
      
!dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo

      ni=N/2+2
      A(ni)=N+1+NL
      imax1=N+1+NL

!dvm$ remote_access (A(1))
      imaxt1=A(1)  

      imaxloct1=imaxt1
      ni1=N/2+1
      A(ni1)=-(N+1+NL)
      imin1=-(N+1+NL)

!dvm$ remote_access (A(1))      
      imint1=A(1)

      iminloct1=imint1
      lit=1
      it1=0
      it2=0

!dvm$ parallel (i) on A(i),
!dvm$*reduction(locmaxmin:max( imaxt1 ),
!dvm$*maxloc( imaxloct1,it1,lit),
!dvm$*minloc( iminloct1,it2,lit))

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

!dvm$ reduction_start locmaxmin
!dvm$ reduction_wait locmaxmin

      if ((imaxloct1.eq.imax1).and.(iminloct1.eq.imin1)
     *.and.(imaxt1.eq.imaxloct1).and.(it1.eq.ni)
     *.and.(it2.eq.ni1) )   then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A,C)

      end


C ----------------------------------------------------REDA1104
      subroutine REDA1104
      integer, parameter :: N = 16
      real, parameter :: NL=1000.
      real, allocatable :: A(:),C(:)
      real isum1,isumt1
      real imax1,imaxt1 ,imin1,imint1
      real imaxloct1,iminloct1,NNL
      integer it1,it2,ni,ni1,lit          
      character(8) :: tname='REDA1104'

!dvm$ distribute A(BLOCK)     
!dvm$ reduction_group locsumloc

      allocate (A(N),C(N))
      NNL=NL 
      NN=N
      call sersum1mr(C,NN,NNL,isum1)

!dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo

      ni=N/2+1
      A(ni)=N+1.+NL
      imax1=N+1.+NL

!dvm$ remote_access (A(1))
      imaxt1=A(1)  

      imaxloct1=imaxt1
      ni1=N/2+2
      A(ni1)=-(N+1.+NL)
      imin1=-(N+1.+NL)

!dvm$ remote_access (A(1))      
      imint1=A(1)

      iminloct1=imint1
      isumt1 = 0.
      lit=1
      it1=0
      it2=0

!dvm$ parallel (i) on A(i),
!dvm$*reduction(locsumloc:sum( isumt1 ),
!dvm$*maxloc( imaxloct1,it1,lit),minloc( iminloct1,it2,lit ))
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

!dvm$ reduction_start locsumloc
!dvm$ reduction_wait locsumloc

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
   
C ------
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
  
C ------
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
  
C ------
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
     
C ------
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

C -----------------------------------------------------
      subroutine ansyes(name)
      character*8 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*8 name
      print *,name,'  -  ***error'
      end
