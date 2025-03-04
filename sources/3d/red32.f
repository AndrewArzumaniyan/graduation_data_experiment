      program RED32

c    TESTING OF THE REDUCTION CLAUSE .       
c    REDUCTION OPERATION : SUM,PRODUCT,MAX,MIN,AND,OR, EQV,
C    NEQV,MAXLOC,MINLOC AND THEIR COMBINATION ARE EXECUTED
c    FOR DISTRIBUTED ARRAY A(N,M,K). 

      print *,'===START OF RED32======================='
C --------------------------------------------------
      call red3201
C --------------------------------------------------
      call red3202
C --------------------------------------------------
      call red3203
C -------------------------------------------------
      call red3204
C -------------------------------------------------
      call red3205
C -------------------------------------------------
      call red3206
C --------------------------------------------------
      call red3207
C --------------------------------------------------
      call red3208
C --------------------------------------------------
      call red3209
C -------------------------------------------------
      call red3210
C -------------------------------------------------
       call red3211
C -------------------------------------------------
      call red3212
C ------------------------------------------------- 
      call red3213
C --------------------------------------------------
      call red3214
C -------------------------------------------------

C
C
      print *,'=== END OF RED32 ========================= '    
      end

C ----------------------------------------------------RED3201
      subroutine RED3201
      integer, parameter :: N = 16,M=8,K=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:,:),C(:,:,:)
      integer isum1,isumt1 
                 
!dvm$ distribute A(BLOCK,BLOCK,*)    


      tname='RED3201'
      allocate (A(N,M,K),C(N,M,K))
      NNL=NL 
      NN=N
      MM=M
      KK=K
      call sersum3(C,NN,MM,KK,NNL,isum1)
      isumt1 = 0

!dvm$ actual(isumt1)
!dvm$ region local(A)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = i+j+ii+NL
          enddo
        enddo
      enddo
  
!dvm$ parallel (i,j,ii) on A(i,j,ii), reduction( sum( isumt1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            isumt1 = isumt1+A(i,j,ii)
          enddo
        enddo
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
C -----------------------------------------------------RED3202
      subroutine RED3202
      integer, parameter :: N = 16,M=8,K=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:,:),C(:,:,:)
      integer iprod1,iprodt1 
                 
!dvm$ distribute A(BLOCK,*,BLOCK)    
      
      tname='RED3202'
      allocate (A(N,M,K),C(N,M,K))
      NNL=NL
      NN=N
      MM=M
      KK=K
      call serprod3(C,NN,MM,KK,NNL,iprod1)
      iprodt1 = 1

!dvm$ actual(iprodt1)
!dvm$ region local(A)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            if ((i.eq.j).and.(j.eq.ii))  then
              A(i,j,ii) = i
            else
              A(i,j,ii) =1
            endif
          enddo
        enddo 
      enddo
  
!dvm$ parallel (i,j,ii) on A(i,j,ii), reduction( product( iprodt1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            iprodt1 = iprodt1*A(i,j,ii)
          enddo
        enddo
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
C ----------------------------------------------------RED3203
      subroutine RED3203
      integer, parameter :: N = 16,M=8,K=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:,:),C(:,:,:)
      integer imax1,imaxt1 ,ni,imin
                       
!dvm$ distribute A(*,BLOCK,BLOCK) 

      tname='RED3203'
      allocate (A(N,M,K),C(N,M,K))
     
!dvm$ parallel (i,j,ii) on A(i,j,ii)
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

!dvm$ remote_access (A(1,1,1))
      imaxt1=A(1,1,1)  

!dvm$ actual(imaxt1)
!dvm$ region
!dvm$ parallel (i,j,ii) on A(i,j,ii), reduction( max( imaxt1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            if (A(i,j,ii).GT.imaxt1) imaxt1=A(i,j,ii)
          enddo
        enddo
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
    
C ----------------------------------------------------RED3204
      subroutine RED3204
      integer, parameter :: N = 16,M=8,K=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:,:),C(:,:,:)
      integer imax1,imaxt1 ,ni,imin
                       
!dvm$ distribute A(BLOCK,BLOCK,*)

      tname='RED3204'
      allocate (A(N,M,K),C(N,M,K))

!dvm$ parallel (i,j,ii) on A(i,j,ii)
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
      A(ni,nj,nii)=-(N+M+K+1+NL)
      imin1=-(N+M+K+1+NL)

!dvm$ remote_access (A(1,1,1))      
      imint1=A(1,1,1)

!dvm$ actual(imint1)
!dvm$ region
!dvm$ parallel (i,j,ii) on A(i,j,ii), reduction( min( imint1 ) )
      do i=2,N
        do j=1,M
          do ii=1,K
            if (A(i,j,ii).LT.imint1) imint1=A(i,j,ii)
          enddo
        enddo
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
C ----------------------------------------------------RED3205
      subroutine RED3205
      integer, parameter :: N = 16,M=8,K=8
      real, parameter :: NL=1000.
      character*7 tname
      real, allocatable :: A(:,:,:),C(:,:,:)
      integer ni
      real imax1,imaxt1                  
!dvm$ distribute A(BLOCK,*,BLOCK)

      tname='RED3205'
      allocate (A(N,M,K),C(N,M,K))
     
!dvm$ parallel (i,j,ii) on A(i,j,ii)
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

!dvm$ remote_access (A(1,1,1))
      imaxt1=A(1,1,1)  

!dvm$ actual(imaxt1)
!dvm$ region
!dvm$ parallel (i,j,ii) on A(i,j,ii), reduction( max( imaxt1 ) )
      do i=2,N
        do j=1,M
          do ii=1,K                  
            if (A(i,j,ii).GT.imaxt1) imaxt1=A(i,j,ii)
          enddo
        enddo
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
                    
C -----------------------------------------------------RED3206
      subroutine RED3206
      integer, parameter :: N =8,M=8,K=8
      real, parameter :: NL=1.
      character*7 tname
      real, allocatable :: A(:,:,:),C(:,:,:)
      real iprod1,iprodt1 
      real NNl
                 
!dvm$ distribute A(*,BLOCK,BLOCK)    

      tname='RED3206'
      allocate (A(N,M,K),C(N,M,K))
      NNL=NL
      NN=N
      MM=M
      KK=K
      call serprodr3(C,NN,MM,KK,NNL,iprod1)
      iprodt1 = 1.

!dvm$ actual(iprodt1)
!dvm$ region local(A)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            if ((i.eq.j).and.(j.eq.ii))  then
              A(i,j,ii) = i
            else
              A(i,j,ii) =1.
            endif
          enddo
        enddo
      enddo
  
!dvm$ parallel (i,j,ii) on A(i,j,ii), reduction( product( iprodt1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            iprodt1 = iprodt1*A(i,j,ii)
          enddo
        enddo
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
C -----------------------------------------------------RED3207
      subroutine RED3207
      integer, parameter :: N = 16,M=8,K=8
      character*7 tname
      logical, allocatable :: A(:,:,:),C(:,:,:)
      logical land1,landt1,leqv1,lneqv1,lor1

!dvm$ distribute A(BLOCK,BLOCK,*)     

      tname='RED3207'
      allocate (A(N,M,K),C(N,M,K))
      NN=N
      MM=M
      KK=K
          
      call serlog3(C,NN,MM,KK,land1,lor1,leqv1,lneqv1)
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K,2
            A(i,j,ii) = .true.
          enddo
        enddo         
      enddo

!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=2,K,2
            A(i,j,ii)=.false.
          enddo
        enddo
      enddo
                                  
!dvm$ remote_access (A(1,1,1))
      landt1 = A(1,1,1)

!dvm$ actual(landt1)
!dvm$ region
!dvm$ parallel (i,j,ii) on A(i,j,ii), reduction( AND( landt1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            if ((i.eq.1).and.(j.eq.1).and.(ii.eq.1))  then
!              landt1=A(i,j,ii)
            else
              landt1 = landt1 .and. A(i,j,ii)
            endif
          enddo
        enddo 
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
C -----------------------------------------------------RED3208
      subroutine RED3208
      integer, parameter :: N = 16,M=8,K=16      
      character*7 tname
      logical, allocatable :: A(:,:,:),C(:,:,:)
      logical land1,landt1,lor1,lort1,leqv1,lneqv1

!dvm$ distribute A(BLOCK,*,BLOCK) 

      tname='RED3208'
      allocate (A(N,M,K),C(N,M,K))
      NN=N
      MM=M
      KK=K
      call serlog3(C,NN,MM,KK,land1,lor1,leqv1,lneqv1)
                                    
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K,2
            A(i,j,ii) = .true.
          enddo
        enddo         
      enddo

!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=2,K,2
           A(i,j,ii)=.false.
          enddo
        enddo
      enddo
                                                 
!dvm$ remote_access (A(1,1,1))
      lort1 = A(1,1,1)

!dvm$ actual(lort1)
!dvm$ region
!dvm$ parallel (i,j,ii) on A(i,j,ii), reduction( OR( lort1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            if ((i.eq.1).and.(j.eq.1).and.(ii.eq.1))  then
!              lort1=A(i,j,ii)
            else
              lort1 = lort1 .or. A(i,j,ii)
            endif
          enddo
        enddo
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
C -----------------------------------------------------RED3209
      subroutine RED3209
      integer, parameter :: N = 16,M=8,K=8
      logical, allocatable :: A(:,:,:),C(:,:,:)      
      character*7 tname
      logical land1,landt1,lor1,leqv1,leqvt1,lneqv1

!dvm$ distribute A(*,BLOCK,BLOCK)

      tname='RED3209'
      allocate (A(N,M,K),C(N,M,K))
      NN=N
      MM=M
      KK=K

      call serlog3(C,NN,MM,KK,land1,lor1,leqv1,lneqv1)

!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K,2
            A(i,j,ii) = .true.
          enddo
        enddo         
      enddo

!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=2,K,2
            A(i,j,ii)=.false.
          enddo
        enddo
      enddo                                    

!dvm$ remote_access (A(1,1,1))
      leqvt1 = A(1,1,1)

!dvm$ actual(leqvt1)
!dvm$ region
!dvm$ parallel (i,j,ii) on A(i,j,ii), reduction( EQV( leqvt1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            if ((i.eq.1).and.(j.eq.1).and.(ii.eq.1))  then
!              leqvt1=A(i,j,ii)
            else
              leqvt1 = leqvt1 .eqv. A(i,j,ii)
            endif
          enddo
        enddo
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
C -----------------------------------------------------RED3210
      subroutine RED3210
      integer, parameter :: N = 16,M=8,K=8
      logical, allocatable :: A(:,:,:),C(:,:,:)      
      character*7 tname
      logical land1,landt1,lor1,leqv1,lneqv1,lneqvt1

!dvm$ distribute A(BLOCK,BLOCK,*)

      tname='RED3210'
      allocate (A(N,M,K),C(N,M,K))
      NN=N
      MM=M
      KK=K
      call serlog3(C,NN,MM,KK,land1,lor1,leqv1,lneqv1)

!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K,2
            A(i,j,ii) = .true.
          enddo
        enddo         
      enddo

!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=2,K,2
            A(i,j,ii)=.false.
          enddo
        enddo
      enddo
                                  
!dvm$ remote_access (A(1,1,1))
      lneqvt1 = A(1,1,1)

!dvm$ actual(lneqvt1)
!dvm$ region
!dvm$ parallel (i,j,ii) on A(i,j,ii), reduction( NEQV( lneqvt1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            if ((i.eq.1).and.(j.eq.1).and.(ii.eq.1))  then
             continue
!             lneqvt1=A(i,j,ii)
            else
             lneqvt1 = lneqvt1 .neqv. A(i,j,ii)
            endif
          enddo
        enddo
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
C ----------------------------------------------------RED3211
      subroutine RED3211
      integer, parameter :: N = 16,M=8,K=16,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:,:),C(:,:,:)
      integer imax1,imaxt1 ,ni,imin
      integer it1,jt1,it2,jt2,iit1   
      integer coor(3),lcoor              
!dvm$ distribute A(BLOCK,*,BLOCK)

      tname='RED3211'
      allocate (A(N,M,K),C(N,M,K))
     
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = i*NL*NL+j*NL+ii
          enddo
        enddo
      enddo
      ni=N/2
      nj=M/2
      nii=K/2
      A(ni,nj,nii)=N+M+K+1+NL*NL*NL
      imax1=N+M+K+1+NL*NL*NL

!dvm$ remote_access (A(1,1,1))
      imaxt1=A(1,1,1)  

      lcoor=3
      coor(1)=0
      coor(2)=0
      coor(3)=0

!dvm$ actual(imaxt1,coor,lcoor)
!dvm$ region
!dvm$ parallel (i,j,ii) on A(i,j,ii),
!dvm$* reduction( maxloc( imaxt1,coor,3))
      do i=2,N
        do j=1,M
          do ii=1,K
            if (A(i,j,ii).GT.imaxt1)then
              imaxt1=A(i,j,ii)
              coor(1)=i
              coor(2)=j
              coor(3)=ii
            endif
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(imaxt1,coor)

      if ((imax1 .eq.imaxt1) .and.(coor(1).eq.ni)
     *.and.(coor(2).eq.nj).and.(coor(3).eq.nii)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
      
      end
C ----------------------------------------------------RED3212
      subroutine RED3212
      integer, parameter :: N = 16,M=8,K=16,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:,:),C(:,:,:)
      integer imin1,imint1 ,ni
      integer it1,jt1,it2,jt2,iit1                 
      integer coor(3),lcoor
!dvm$ distribute A(*,BLOCK,BLOCK)

      tname='RED3212'
      allocate (A(N,M,K),C(N,M,K))
     
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            A(i,j,ii) = i*NL*NL+j*NL+ii
          enddo
        enddo
      enddo
      ni=N/2
      nj=M/2
      nii=K/2
      A(ni,nj,nii)=-(N+M+K+1+NL*NL*NL)
      imin1=-(N+M+K+1+NL*NL*NL)

!dvm$ remote_access (A(1,1,1))
      imint1=A(1,1,1)  

      lcoor=3
      coor(1)=0
      coor(2)=0
      coor(3)=0

!dvm$ actual(imint1,coor,lcoor)
!dvm$ region
!dvm$ parallel (i,j,ii) on A(i,j,ii),
!dvm$* reduction( minloc( imint1,coor,3))
      do i=2,N
        do j=1,M
          do ii=1,K
            if (A(i,j,ii).LT.imint1)then
              imint1=A(i,j,ii)
              coor(1)=i
              coor(2)=j
              coor(3)=ii
            endif
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(imint1,coor)

      if ((imin1 .eq.imint1) .and.(coor(1).eq.ni)
     *.and.(coor(2).eq.nj).and.(coor(3).eq.nii)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
      
      end
C ----------------------------------------------------RED3213
      subroutine RED3213
      integer, parameter :: N = 16,M=8,K=16,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:,:),C(:,:,:)
      integer imin1,imint1 ,ni
      integer isum1,isumt1 
      integer imax1,imaxt1
                      
!dvm$ distribute A(BLOCK,BLOCK,*)

      tname='RED3213'
      allocate (A(N,M,K),C(N,M,K))
      NNL=NL 
      NN=N
      MM=M
      KK=K
      call sersum3m(C,NN,MM,KK,NNL,isum1)

!dvm$ parallel (i,j,ii) on A(i,j,ii)
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

!dvm$ remote_access (A(1,1,1))
      imaxt1=A(1,1,1)  

      ni1=N/2
      nj1=M/2
      nii1=K/2
      A(ni1,nj1,nii1)=-(N+M+K+1+NL)
      imin1=-(N+M+K+1+NL)

!dvm$ remote_access (A(1,1,1))
      imint1=A(1,1,1)

      isumt1 = 0
!dvm$ actual(isumt1,imaxt1,imint1)
!dvm$ region
!dvm$ parallel (i,j,ii) on A(i,j,ii), reduction( sum( isumt1 ),
!dvm$*max( imaxt1 ),min( imint1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            isumt1 = isumt1+A(i,j,ii)
            if (A(i,j,ii).GT.imaxt1) imaxt1=A(i,j,ii)
            if (A(i,j,ii).LT.imint1) imint1=A(i,j,ii)
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(isumt1,imaxt1,imint1) 
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
C ----------------------------------------------------RED3214
      subroutine RED3214
      integer, parameter :: N = 16,M=8,K=16,NL=1
      character*7 tname
      integer, allocatable :: A(:,:,:),C(:,:,:)
      integer iprod1,iprodt1 
      logical, allocatable :: B(:,:,:),CL(:,:,:)
      logical land1,landt1,lor1,leqv1,lneqv1
               
!dvm$ distribute A(BLOCK,*,BLOCK)    
!dvm$ align B(I,J,II) with A(I,J,II)      

      tname='RED3214'
      allocate (A(N,M,K),C(N,M,K))
      allocate (B(N,M,K),CL(N,M,K))

      NNL=NL
      NN=N
      MM=M
      KK=K
      call serprod3(C,NN,MM,KK,NNL,iprod1)
      call serlog3(CL,NN,MM,KK,land1,lor1,leqv1,lneqv1)

!dvm$ parallel (i,j,ii) on B(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K,2
            B(i,j,ii) = .true.
          enddo
        enddo         
      enddo

!dvm$ parallel (i,j,ii) on B(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K,2
            B(i,j,ii)=.false.
          enddo
        enddo
      enddo

!dvm$ remote_access (B(1,1,1))
      landt1 = B(1,1,1)    
      iprodt1 = 1
               
!dvm$ parallel (i,j,ii) on A(i,j,ii)
      do i=1,N
        do j=1,M
          do ii=1,K
            if ((i.eq.j).and.(j.eq.ii))  then
              A(i,j,ii) = i
            else
              A(i,j,ii) =1
            endif
          enddo
        enddo
      enddo
  
!dvm$ actual(iprodt1,landt1)
!dvm$ region
!dvm$ parallel (i,j,ii) on A(i,j,ii), reduction( product( iprodt1 ),
!dvm$* and(landt1))
      do i=1,N
        do j=1,M
          do ii=1,K
            iprodt1 = iprodt1*A(i,j,ii)
            if ((i.eq.1).and.(j.eq.1).and.(ii.eq.1))  then
!              landt1=B(i,j,ii)
            else
              landt1 = landt1 .and. B(i,j,ii)
            endif             
          enddo
        enddo
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
            if ((i.eq.j).and.(j.eq.ii))  then
              AR(i,j,ii) = i
            else
              AR(i,j,ii) =1
            endif
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
           if ((i.eq.j).and.(j.eq.ii))  then
             AR(i,j,ii) = i
           else
             AR(i,j,ii) =1.
           endif
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
      do  i=1,N,1
        do j=1,M,1
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

      do i=1,N
        do j= 1,M
          do ii=1,K
           if ((i.eq.1).and.(j.eq.1).and.(ii.eq.1))  then
             LAND=AR(1,1,1)
             LOR=AR(1,1,1)
             LEQV=AR(1,1,1)
             LNEQV=AR(1,1,1)
           else
             LAND = LAND .and. AR(i,j,ii)
             LOR = LOR .or.AR(i,j,ii)
             LEQV = LEQV .eqv. AR(i,j,ii)
             LNEQV = LNEQV .neqv. AR(i,j,ii)
           endif
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
