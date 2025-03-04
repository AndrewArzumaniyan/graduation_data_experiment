      program RED11

c    TESTING OF THE REDUCTION CLAUSE .       
c    REDUCTION OPERATION : SUM.PRODUCT,MAX,MIN,AND,OR, EQV,
C    NEQV,MAXLOC,MINLOC AND THEIR COMBINATION ARE EXECUTED
c    FOR DISTRIBUTED ARRAY A(N). 

      print *,'===START OF RED11========================'
C --------------------------------------------------
      call red1101
C --------------------------------------------------
      call red1102
C --------------------------------------------------
      call red1103
C -------------------------------------------------
      call red1104
C -------------------------------------------------
      call red1105
C -------------------------------------------------
      call red1106
C --------------------------------------------------
      call red1107
C --------------------------------------------------
      call red1108
C --------------------------------------------------
       call red1109
C -------------------------------------------------
      call red1110
C -------------------------------------------------
      call red1111
C -------------------------------------------------
      call red1112
C ------------------------------------------------- 
      call red1113
C --------------------------------------------------
      call red1114
C --------------------------------------------------
      call red1115
C -------------------------------------------------
      call red1116
C -------------------------------------------------

C
C
      print *,'=== END OF RED11 ========================= '    
      end

C ----------------------------------------------------RED1101
      subroutine RED1101
      integer, parameter :: N = 16,NL=1000
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer isum1,isumt1 
                 
!dvm$ distribute A(BLOCK)    

      tname='RED1101'
      allocate (A(N),C(N))
      NNL=NL 
      NN=N
      call sersum1(C,NN,NNL,isum1)
      isumt1 = 0

!dvm$ actual(isumt1)
!dvm$ region local(A)
!dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo
  
!dvm$ parallel (i) on A(i), reduction( sum( isumt1 ) )
      do i=1,N
         isumt1 = isumt1+A(i)
      enddo
!dvm$ end region   
!dvm$ get_actual(isumt1) 
     
      if (isum1 .eq.isumt1) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)

      end
C -----------------------------------------------------RED1102
      subroutine RED1102
      integer, parameter :: N = 16 ,NL=1002
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer iprod1,iprodt1 
                       
!dvm$ distribute A(BLOCK)  
      
      tname='RED1102'
      allocate (A(N),C(N))
      NNL=NL
      NN=N
      call serprod1(C,NN,NNL,iprod1)
      iprodt1 = 1

!dvm$ actual(iprodt1)
!dvm$ region local(A)
!dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo
  
!dvm$ parallel (i) on A(i), reduction( product( iprodt1 ) )
      do i=1,N
         iprodt1 = iprodt1*A(i)
      enddo
!dvm$ end region   
!dvm$ get_actual(iprodt1) 
 
      if (iprod1 .eq.iprodt1) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
 
      end
C ----------------------------------------------------RED1103
      subroutine RED1103
      integer, parameter :: N = 16,NL=1003
      character*7 tname
      integer, allocatable :: A(:)
      integer imax1,imaxt1 ,ni,imin
                       
!dvm$ distribute A(BLOCK)  

      tname='RED1103'
      allocate (A(N))
     
!dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo
      ni=N/2
      A(ni)=N+1+NL
      imax1=N+1+NL

!dvm$ remote_access (A(1))
      imaxt1=A(1)  

!dvm$ actual(imaxt1,A)
!dvm$ region
!dvm$ parallel (i) on A(i), reduction( max( imaxt1 ) )
      do i=2,N
         if (A(i).GT.imaxt1) imaxt1=A(i)
      enddo
!dvm$ end region   
!dvm$ get_actual(imaxt1) 

      if (imax1 .eq.imaxt1) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A)
      
      end
    
C ----------------------------------------------------RED1104
      subroutine RED1104
      integer, parameter :: N = 16,NL=1004
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer imax1,imaxt1 ,ni,imin
                       
!dvm$ distribute A(BLOCK)    

      tname='RED1104'
      allocate (A(N),C(N))
 
!dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo
      ni=N/2
      A(ni)=-(N+1+NL)
      imin1=-(N+1+NL)

!dvm$ remote_access (A(1))      
      imint1=A(1)

!dvm$ actual(imint1)
!dvm$ region
!dvm$ parallel (i) on A(i), reduction( min( imint1 ) )
      do i=2,N
         if (A(i).LT.imint1) imint1=A(i)
      enddo
!dvm$ end region   
!dvm$ get_actual(imint1) 

      if (imin1 .eq.imint1) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif  
      deallocate (A,C)

      end
C ----------------------------------------------------RED1105
      subroutine RED1105
      integer, parameter :: N = 16
      real, parameter :: NL=1005
      character*7 tname
      real, allocatable :: A(:),C(:)
      integer ni
      real imax1,imaxt1                  
!dvm$ distribute A(BLOCK)    

      tname='RED1105'
      allocate (A(N),C(N))
     
!dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo
      ni=N/2
      A(ni)=N+1.+NL
      imax1=N+1.+NL

!dvm$ remote_access (A(1))
      imaxt1=A(1)  

!dvm$ actual(imaxt1)
!dvm$ region
!dvm$ parallel (i) on A(i), reduction( max( imaxt1 ) )
      do i=2,N
         if (A(i).GT.imaxt1) imaxt1=A(i)
      enddo
!dvm$ end region   
!dvm$ get_actual(imaxt1) 

      if (imax1 .eq.imaxt1) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
      
      end
                    
C -----------------------------------------------------RED1106
      subroutine RED1106
      integer, parameter :: N = 8 ,NL=1.
      character*7 tname
      real, allocatable :: A(:),C(:)
      real iprod1,iprodt1 
      real NNl
                 
!dvm$ distribute A(BLOCK)    
      
      tname='RED1106'
      allocate (A(N),C(N))
      NNL=NL
      NN=N 
      call serprodr1(C,NN,NNL,iprod1)
      iprodt1 = 1.

!dvm$ actual(iprodt1)
!dvm$ region local(A)
!dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo
  
!dvm$ parallel (i) on A(i), reduction( product( iprodt1 ) )
      do i=1,N
         iprodt1 = iprodt1*A(i)
      enddo
!dvm$ end region   
!dvm$ get_actual(iprodt1) 

      if (iprod1 .eq.iprodt1) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
 
      end
C -----------------------------------------------------RED1107
      subroutine RED1107
      integer, parameter :: N = 16
      character*7 tname
      logical, allocatable :: A(:),C(:)
      logical land1,landt1,leqv1,lneqv1,lor1

!dvm$ distribute A(BLOCK)   

      tname='RED1107'
      allocate (A(N),C(N))
      NN=N            
      call serlog1(C,NN,land1,lor1,leqv1,lneqv1)
                                    
!dvm$ parallel (i) on A(i)
      do i=1,N,2
         A(i) = .true.
      enddo

!dvm$ parallel (i) on A(i+1)
      do i=1,N-1,2        
         A(i+1)=.false.
      enddo

!dvm$ remote_access (A(1))
      landt1 = A(1)

!dvm$ actual(landt1)
!dvm$ region
!dvm$ parallel (i) on A(i), reduction( AND( landt1 ) )
      do i=2,N
         landt1 = landt1 .and.A(i)
      enddo
!dvm$ end region   
!dvm$ get_actual(landt1) 
      
      if (land1 .eqv.landt1) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
 
      end
C -----------------------------------------------------RED1108
      subroutine RED1108
      integer, parameter :: N = 16
      character*7 tname
      logical, allocatable :: A(:),C(:)
      logical land1,landt1,lor1,lort1,leqv1,lneqv1

!dvm$ distribute A(BLOCK) 

      tname='RED1108'
      allocate (A(N),C(N))
      NN=N
      call serlog1(C,NN,land1,lor1,leqv1,lneqv1)
               
!dvm$ parallel (i) on A(i)
      do i=1,N,2
         A(i) = .true.
      enddo

!dvm$ parallel (i) on A(i)
      do i=2,N,2
         A(i)=.false.
      enddo

!dvm$ remote_access (A(1))
      lort1 = A(1)

!dvm$ actual(lort1)
!dvm$ region
!dvm$ parallel (i) on A(i),reduction( OR( lort1 ) )
      do i=2,N
         lort1 = lort1 .or.A(i)
      enddo
!dvm$ end region   
!dvm$ get_actual(lort1) 

      if (lor1 .eqv.lort1) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
 
      end
C -----------------------------------------------------RED1109
      subroutine RED1109
      integer, parameter :: N = 16
      character*7 tname
      logical, allocatable :: A(:),C(:)
      logical land1,landt1,lor1,lort1,leqv1,leqvt1,lneqv1

!dvm$ distribute A(BLOCK)    

      tname='RED1109'
      allocate (A(N),C(N))
      NN=N
      call serlog1(C,NN,land1,lor1,leqv1,lneqv1)
               
!dvm$ parallel (i) on A(i)
      do i=1,N,2
         A(i) = .true.
      enddo

!dvm$ parallel (i) on A(i)
      do i=2,N,2
         A(i)=.false.
      enddo

!dvm$ remote_access (A(1))
       leqvt1 = A(1)

!dvm$ actual(leqvt1)
!dvm$ region
!dvm$ parallel (i) on A(i), reduction( EQV( leqvt1 ) )
      do i=2,N
         leqvt1 = leqvt1 .eqv.A(i)
      enddo
!dvm$ end region   
!dvm$ get_actual(leqvt1) 

      if (leqv1 .eqv.leqvt1) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
 
      end
C -----------------------------------------------------RED1110
      subroutine RED1110
      integer, parameter :: N = 8
      character*7 tname
      logical, allocatable :: A(:),C(:)
      logical land1,landt1,lor1,lort1,leqv1,lneqv1,lneqvt1

!dvm$ distribute A(BLOCK)    

      tname='RED1110'
      allocate (A(N),C(N))
      NN=N
      call serlog1(C,NN,land1,lor1,leqv1,lneqv1)
                                    
!dvm$ parallel (i) on A(i)
      do i=1,N,2
         A(i) = .true.
      enddo

!dvm$ parallel (i) on A(i)
      do i=2,N,2
         A(i)=.false.
      enddo

!dvm$ remote_access (A(1))
      lneqvt1 = A(1)

!dvm$ actual(lneqvt1)
!dvm$ region
!dvm$ parallel (i) on A(i), reduction( NEQV( lneqvt1 ) )
      do i=2,N
         lneqvt1 = lneqvt1 .neqv.A(i)
      enddo
!dvm$ end region   
!dvm$ get_actual(lneqvt1) 

      if (lneqv1 .eqv.lneqvt1) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
 
      end

C ----------------------------------------------------RED1111
      subroutine RED1111
      integer, parameter :: N = 16,NL=1000
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer imax1,imaxt1 ,ni,imin
                      
!dvm$ distribute A(BLOCK)    

      tname='RED1111'
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
      it1=0

!dvm$ actual(imaxt1,it1)
!dvm$ region
!dvm$ parallel (i) on A(i), reduction( maxloc( imaxt1,it1,1 ) )
      do i=2,N
         if (A(i).GT.imaxt1)then
         imaxt1=A(i)
         it1=i
         endif
      enddo
!dvm$ end region   
!dvm$ get_actual(imaxt1,it1) 

      if ((imax1 .eq.imaxt1) .and. (it1.eq.ni)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
      
      end
    
C ----------------------------------------------------RED1112
      subroutine RED1112
      integer, parameter :: N = 16,NL=1000
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer imax1,imaxt1 ,ni,imin,imint1
                      
!dvm$ distribute A(BLOCK)   

      tname='RED1112'
      allocate (A(N),C(N))
     
!dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo
      ni=N/2
      A(ni)=-(N+1+NL)
      imin1=-(N+1+NL)
!dvm$ remote_access (A(1))
      imint1=A(1)        
      it1=0

!dvm$ actual(imint1,it1)
!dvm$ region
!dvm$ parallel (i) on A(i), reduction( minloc( imint1,it1,1 ) )
      do i=2,N
         if (A(i).LT.imint1)then
         imint1=A(i)
         it1=i
         endif
      enddo
!dvm$ end region   
!dvm$ get_actual(imint1,it1) 

      if ((imin1 .eq.imint1) .and. (it1.eq.ni)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
      
      end

C ----------------------------------------------------RED1113
      subroutine RED1113
      integer, parameter :: N = 16,NL=1000
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer isum1,isumt1 
      integer imax1,imaxt1 ,ni,imin1,imint1
                 
!dvm$ distribute A(BLOCK)    

      tname='RED1113'
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

!dvm$ actual(imint1,imaxt1,isumt1)
!dvm$ region
!dvm$ parallel (i) on A(i), reduction( sum( isumt1 ),
!dvm$*max( imaxt1 ),min( imint1 ) )
      do i=1,N
         isumt1 = isumt1+A(i)
         if (A(i).GT.imaxt1) imaxt1=A(i)
         if (A(i).LT.imint1) imint1=A(i)
      enddo
!dvm$ end region   
!dvm$ get_actual(imint1,imaxt1,isumt1) 

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

C -----------------------------------------------------RED1114
      subroutine RED1114
      integer, parameter :: N = 16 ,NL=1
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer iprod1,iprodt1 
      logical, allocatable :: B(:),CL(:)
      logical land1,landt1,lor1,leqv1,lneqv1
                 
!dvm$ distribute A(BLOCK)   
!dvm$ align B(I) with A(I)
      
      tname='RED1114'
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
      iprodt1 = 1


!dvm$ actual(iprodt1,landt1)
!dvm$ region local(A)

!dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo
  
!dvm$ parallel (i) on A(i), reduction( product( iprodt1 ),
!dvm$* and(landt1))
      do i=1,N
         iprodt1 = iprodt1*A(i)
         if (i.eq.1) then 
!         landt1=B(1)
         else
         landt1 = landt1 .and.B(i)
         endif
      enddo
!dvm$ end region   
!dvm$ get_actual(iprodt1,landt1) 

      if ((iprod1 .eq.iprodt1)
     *.and. (land1 .eqv.landt1)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B,CL)
      deallocate (A,C)
 
      end


C ----------------------------------------------------RED1115
      subroutine RED1115
      integer, parameter :: N = 16,NL=1000
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer imax1,imaxt1 ,ni,imin1,imint1,it1,it2
      integer imaxloct1,iminloct1          

!dvm$ distribute A(BLOCK)    

      tname='RED1115'
      allocate (A(N),C(N))
      
!dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo

      ni=N/2-1
      A(ni)=N+1+NL
      imax1=N+1+NL
!dvm$ remote_access (A(1))
      imaxt1=A(1)  
      imaxloct1=imaxt1
      ni1=N/2
      A(ni1)=-(N+1+NL)
      imin1=-(N+1+NL)
!dvm$ remote_access (A(1))      
      imint1=A(1)
      iminloct1=imint1
      it1=0
      it2=0

!dvm$ actual(imaxloct1,it1,iminloct1,it2)
!dvm$ region
!dvm$ parallel (i) on A(i), reduction( max( imaxt1 ),
!dvm$*maxloc( imaxloct1,it1,1 ),minloc( iminloct1,it2,1 ) )
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
!dvm$ end region   
!dvm$ get_actual(imaxloct1,it1,iminloct1,it2) 

c      print *,imax1,imaxt1,imaxloct1
c      print *,imin1,imint1,iminloct1
c      print *,it1,it2,ni,ni1

      if ((imaxloct1.eq.imax1).and.(iminloct1.eq.imin1)
     *.and.(imaxt1.eq.imaxloct1).and.(it1.eq.ni)
     *.and.(it2.eq.ni1) )   then     
          call ansyes(tname)
          else
          call ansno(tname)
      endif 
      deallocate (A,C)

      end


C ----------------------------------------------------RED1116
      subroutine RED1116
      integer, parameter :: N = 16
      real, parameter :: NL=1000.
      character*7 tname
      real, allocatable :: A(:),C(:)
      real isum1,isumt1
      real imax1,imaxt1 ,imin1,imint1
      real imaxloct1,iminloct1,NNL
      integer it1,it2,ni,ni1          
!dvm$ distribute A(BLOCK)    

      tname='RED1116'
      allocate (A(N),C(N))
      NNL=NL
      NN=N 
      call sersum1mr(C,NN,NNL,isum1)

!dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo

      ni=N/2-1
      A(ni)=N+1+NL
      imax1=N+1+NL
!dvm$ remote_access (A(1))
      imaxt1=A(1)  

      imaxloct1=imaxt1
      ni1=N/2
      A(ni1)=-(N+1+NL)
      imin1=-(N+1+NL)

!dvm$ remote_access (A(1))      
      imint1=A(1)

      iminloct1=imint1
      isumt1 = 0.
      it1=0
      it2=0


!dvm$ actual(isumt1,imaxloct1,it1,iminloct1,it2)
!dvm$ region
!dvm$ parallel (i) on A(i), reduction( sum( isumt1 ),
!dvm$*maxloc( imaxloct1,it1,1 ),minloc( iminloct1,it2,1 ) )
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
!dvm$ end region   
!dvm$ get_actual(isumt1,imaxloct1,it1,iminloct1,it2) 

c      print *,A
c      print *,imax1,imaxt1,imaxloct1
c      print *,imin1,imint1,iminloct1
c      print *,it1,it2,ni,ni1
c      print *,isum1,isumt1
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
      ni=N/2-1
      AR(ni)=N+1+NL
      ni=N/2
      AR(ni)=-(N+1+NL)
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
c      LEQV=.true.
c      LNEQV=.false.
       LEQV=AR(1)
       LNEQV=AR(1)
      do i=2,N
         LAND = LAND .and. AR(i)
         LOR = LOR .or.AR(i)
      enddo
      do i=2,N
         LEQV = LEQV .eqv. AR(i)
      enddo
      do i=2,N
         LNEQV = LNEQV .neqv. AR(i)
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
