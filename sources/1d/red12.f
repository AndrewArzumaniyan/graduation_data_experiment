      program RED12

c    TESTING OF THE REDUCTION CLAUSE .       
c    REDUCTION OPERATION : SUM.PRODUCT,MAX,MIN,AND,OR, EQV,
C    NEQV,MAXLOC,MINLOC AND THEIR COMBINATION ARE EXECUTED
c    FOR DISTRIBUTED ARRAY A(N). 

      print *,'===START OF RED12========================'
C --------------------------------------------------
      call red1201
C --------------------------------------------------
      call red1202
C --------------------------------------------------
      call red1203
C -------------------------------------------------
      call red1204
C -------------------------------------------------
      call red1205
C -------------------------------------------------
      call red1206
C --------------------------------------------------
      call red1207
C --------------------------------------------------
      call red1208
C --------------------------------------------------
      call red1209
C -------------------------------------------------
      call red1210
C -------------------------------------------------
      call red1211
C -------------------------------------------------
      call red1212
C ------------------------------------------------- 
      call red1213
C --------------------------------------------------
      call red1214
C --------------------------------------------------
      call red1215
C -------------------------------------------------
      call red1216
C -------------------------------------------------

C
C
      print *,'=== END OF RED12 ========================= '    
      end

C ----------------------------------------------------RED1201
      subroutine RED1201
      integer, parameter :: N = 16,NL=1000
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer isum1,isumt1 
      
!dvm$ distribute A(*)   

      tname='RED1201'
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
C -----------------------------------------------------RED1202
      subroutine RED1202
      integer, parameter :: N = 16, NL=1000
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer iprod1,iprodt1 
                 
!dvm$ distribute A(*)    
      
      tname='RED1202'
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
C ----------------------------------------------------RED1203
      subroutine RED1203
      integer, parameter :: N = 16,NL=1000
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer imax1,imaxt1 ,ni,imin
                       
!dvm$ distribute A(*)     

      tname='RED1203'
      allocate (A(N),C(N))
     
!dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo

      ni=N/2
      A(ni)=N+1+NL
      imax1=N+1+NL

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
    
C ----------------------------------------------------RED1204
      subroutine RED1204
      integer, parameter :: N = 16,NL=1000
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer imax1,imaxt1 ,ni,imin
                       
!dvm$ distribute A(*)     

      tname='RED1204'
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
C ----------------------------------------------------RED1205
      subroutine RED1205
      integer, parameter :: N = 16
      real, parameter :: NL=1000.
      character*7 tname
      real, allocatable :: A(:),C(:)
      integer ni
      real imax1,imaxt1                  
!dvm$ distribute A(*)     

      tname='RED1205'
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
                    
C -----------------------------------------------------RED1206
      subroutine RED1206
      integer, parameter :: N = 8
      real, parameter :: NL=1.
      character*7 tname
      real, allocatable :: A(:),C(:)
      real iprod1,iprodt1 
      real NNl
                 
!dvm$ distribute A(*)     

      tname='RED1206'
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

C -----------------------------------------------------RED1207
      subroutine RED1207
      integer, parameter :: N = 16
      character*7 tname
      logical, allocatable :: A(:),C(:)
      logical land1,landt1,leqv1,lneqv1,lor1

!dvm$ distribute A(*)     

      tname='RED1207'
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
C -----------------------------------------------------RED1208
      subroutine RED1208
      integer, parameter :: N = 16
      character*7 tname
      logical, allocatable :: A(:),C(:)
      logical land1,landt1,lor1,lort1,leqv1,lneqv1

!dvm$ distribute A(*)     

      tname='RED1208'
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
      lort1 = A(1)

!dvm$ actual(lort1)
!dvm$ region
!dvm$ parallel (i) on A(i), reduction( OR( lort1 ) )
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
C -----------------------------------------------------RED1209
      subroutine RED1209
      integer, parameter :: N = 16     
      character*7 tname
      logical, allocatable :: A(:),C(:)
      logical land1,landt1,lor1,leqv1,leqvt1,lneqv1

!dvm$ distribute A(*)     

      tname='RED1209'
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
C -----------------------------------------------------RED1210
      subroutine RED1210
      integer, parameter :: N = 16
      character*7 tname
      logical, allocatable :: A(:),C(:)
      logical land1,landt1,lor1,leqv1,lneqv1,lneqvt1

!dvm$ distribute A(*)    

      tname='RED1210'
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

C ----------------------------------------------------RED1211

      subroutine RED1211
      integer, parameter :: N = 16,NL=1000
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer imax1,imaxt1 ,ni,imin,lit
                       
!dvm$ distribute A(*)     

      tname='RED1211'
      allocate (A(N),C(N))
     
!dvm$ parallel (i) on A(i)
      do i=1,N
         A(i) = i+NL
      enddo
      ni=N/2
      A(ni)=N+1+NL
      imax1=N+1+NL

!dvm$ remote_access (A(1))
      imaxt1=A(1)  

      lit=1
      it1=0

!dvm$ actual(imaxt1)
!dvm$ region
!dvm$ parallel (i) on A(i), reduction( maxloc( imaxt1,it1,1 ) )
      do i=2,N
         if (A(i).GT.imaxt1)then
         imaxt1=A(i)
         it1=i
         endif
      enddo
!dvm$ end region   
!dvm$ get_actual(imaxt1,it1,lit) 

      if ((imax1 .eq.imaxt1) .and. (it1.eq.ni)) then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)      

      end
    
 
C ----------------------------------------------------RED1212
      subroutine RED1212
      integer, parameter :: N = 16,NL=1000
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer imax1,imaxt1 ,ni,imin,lit
                       
!dvm$ distribute A(*)     

      tname='RED1212'
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

      lit=1
      it1=0

!dvm$ actual(imaxt1)
!dvm$ region
!dvm$ parallel (i) on A(i), reduction( minloc( imint1,it1,1 ) )
      do i=2,N
         if (A(i).LT.imint1)then
         imint1=A(i)
         it1=i
         endif
      enddo
!dvm$ end region   
!dvm$ get_actual(imint1,it1,lit) 

      if ((imin1 .eq.imint1) .and. (it1.eq.ni)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)      

      end


C ----------------------------------------------------RED1213
      subroutine RED1213
      integer, parameter :: N = 16,NL=1000
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer isum1,isumt1 
      integer imax1,imaxt1 ,ni,imin1,imint1
                 
!dvm$ distribute A(*)     

      tname='RED1213'
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

!dvm$ actual(isumt1,imaxt1, imint1)
!dvm$ region
!dvm$ parallel (i) on A(i), reduction( sum( isumt1 ),
!dvm$*max( imaxt1 ),min( imint1 ) )
      do i=1,N
         isumt1 = isumt1+A(i)
         if (A(i).GT.imaxt1) imaxt1=A(i)
         if (A(i).LT.imint1) imint1=A(i)
      enddo
!dvm$ end region   
!dvm$ get_actual(isumt1,imaxt1, imint1)

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

C -----------------------------------------------------RED1214
      subroutine RED1214
      integer, parameter :: N = 16 ,NL=1
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer iprod1,iprodt1 
      logical, allocatable :: B(:),CL(:)
      logical land1,landt1,lor1,leqv1,lneqv1
                 
!dvm$ distribute A(*)     
!dvm$ align B(I) with A(I)
      
      tname='RED1214'
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


C ----------------------------------------------------RED1215
      subroutine RED1215
      integer, parameter :: N = 16,NL=1000
      character*7 tname
      integer, allocatable :: A(:),C(:)
      integer imax1,imaxt1 ,ni,imin1,imint1,it1,it2
      integer imaxloct1,iminloct1,lit          
!dvm$ distribute A(*)     

      tname='RED1215'
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
      lit=1
      it1=0
      it2=0

!dvm$ actual(imaxt1,imaxloct1,it1,lit,iminloct1,it2,lit)
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
!dvm$ get_actual(imaxt1,imaxloct1,it1,iminloct1,it2) 

      if ((imaxloct1.eq.imax1).and.(iminloct1.eq.imin1)
     *.and.(imaxt1.eq.imaxloct1).and.(it1.eq.ni)
     *.and.(it2.eq.ni1) )   then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)

      end


C ----------------------------------------------------RED1216
      subroutine RED1216
      integer, parameter :: N = 16,NL=1000
      character*7 tname
      real, allocatable :: A(:),C(:)
      real isum1,isumt1
      real imax1,imaxt1 ,imin1,imint1
      real imaxloct1,iminloct1,NNL
      integer it1,it2,ni,ni1,lit
          
!dvm$ distribute A(*)     

      tname='RED1216'
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
      lit=1
      it1=0
      it2=0

!dvm$ actual(isumt1,imaxloct1,it1,lit,iminloct1,it2)
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
C      LEQV=.true.
C      LNEQV=.false.
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
