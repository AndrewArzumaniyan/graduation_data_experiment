      program RED43

c    TESTING OF THE REDUCTION CLAUSE .       
c    REDUCTION OPERATION : SUM,PRODUCT,MAX,MIN,AND,OR, EQV,
C    NEQV,MAXLOC,MINLOC AND THEIR COMBINATION ARE EXECUTED
c    FOR DISTRIBUTED ARRAY A(N,M,K,L). 

      print *,'===START OF RED43======================='
C --------------------------------------------------
      call red4301
C --------------------------------------------------
      call red4302
C --------------------------------------------------
      call red4303
C -------------------------------------------------
      call red4304
C -------------------------------------------------
      call red4305
C -------------------------------------------------
      call red4306
C --------------------------------------------------
      call red4307
C --------------------------------------------------
      call red4308
C --------------------------------------------------
      call red4309
C -------------------------------------------------
      call red4310
C -------------------------------------------------
       call red4311
C -------------------------------------------------
      call red4312
C ------------------------------------------------- 
      call red4313
C --------------------------------------------------
      call red4314
C --------------------------------------------------

C
C
      print *,'=== END OF RED43 ========================= '    
      end

C ----------------------------------------------------RED4301
      subroutine RED4301
      integer, parameter :: N = 16,M=8,K=8,L=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:,:,:),C(:,:,:,:)
      integer isum1,isumt1 
      
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,*)     

      tname='RED4301'
      allocate (A(N,M,K,L),C(N,M,K,L))
      NNL=NL 
      NN=N
      MM=M
      KK=K
      LL=L
      call sersum4(C,NN,MM,KK,LL,NNL,isum1)
      isumt1 = 0

!dvm$ actual(isumt1)
!dvm$ region local(A)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = i+j+ii+jj+NL
            enddo
          enddo
        enddo
      enddo
  
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj), reduction( sum( isumt1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              isumt1 = isumt1+A(i,j,ii,jj)
            enddo
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
C -----------------------------------------------------RED4302
      subroutine RED4302
      integer, parameter :: N = 16,M=8,K=8,L=8,NL=10
      character*7 tname
      integer, allocatable :: A(:,:,:,:),C(:,:,:,:)
      integer iprod1,iprodt1       
                 
!dvm$ distribute A(BLOCK,BLOCK,*,BLOCK)    
      
      tname='RED4302'
      allocate (A(N,M,K,L),C(N,M,K,L))
      NNL=NL
      NN=N
      MM=M
      KK=K
      LL=L
      call serprod4(C,NN,MM,KK,LL,NNL,iprod1)
      iprodt1 = 1

!dvm$ actual(iprodt1)
!dvm$ region local(A)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if ((i.eq.j).and.(j.eq.ii).and.(ii.eq.jj)) then
                A(i,j,ii,jj) = i
              else
                A(i,j,ii,jj) =1
              endif
            enddo
          enddo
        enddo 
      enddo
    
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$* reduction( product( iprodt1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              iprodt1 = iprodt1*A(i,j,ii,jj)
            enddo
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
C ----------------------------------------------------RED4303
      subroutine RED4303
      integer, parameter :: N = 16,M=8,K=8,L=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:,:,:),C(:,:,:,:)
      integer imax1,imaxt1 ,ni,imin
                       
!dvm$ distribute A(BLOCK,*,BLOCK,BLOCK) 

      tname='RED4303'
      allocate (A(N,M,K,L),C(N,M,K,L))
     
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
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

!dvm$ remote_access (A(1,1,1,1))
      imaxt1=A(1,1,1,1)  

!dvm$ actual(imaxt1)
!dvm$ region
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$* reduction( max( imaxt1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if (A(i,j,ii,jj).GT.imaxt1) imaxt1=A(i,j,ii,jj)
            enddo
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
    
C ----------------------------------------------------RED4304
      subroutine RED4304
      integer, parameter :: N = 16,M=8,K=8,L=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:,:,:),C(:,:,:,:)
      integer imax1,imaxt1 ,ni,imin
                       
!dvm$ distribute A(*,BLOCK,BLOCK,BLOCK)

      tname='RED4304'
      allocate (A(N,M,K,L),C(N,M,K,L))

!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
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
      A(ni,nj,nii,njj)=-(N+M+K+L+1+NL)
      imin1=-(N+M+K+L+1+NL)

!dvm$ remote_access (A(1,1,1,1))      
      imint1=A(1,1,1,1)

!dvm$ actual(imint1)
!dvm$ region
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$* reduction( min( imint1 ) )
      do i=2,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if (A(i,j,ii,jj).LT.imint1) imint1=A(i,j,ii,jj)
            enddo
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
C ----------------------------------------------------RED4305
      subroutine RED4305
      integer, parameter :: N = 16,M=8,K=8,L=8
      real, parameter :: NL=1000.
      character*7 tname
      real, allocatable :: A(:,:,:,:),C(:,:,:,:)
      integer ni
      real imax1,imaxt1                  
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,*)

      tname='RED4305'
      allocate (A(N,M,K,L),C(N,M,K,L))
     
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
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

!dvm$ remote_access (A(1,1,1,1))
      imaxt1=A(1,1,1,1)  

!dvm$ actual(imaxt1)
!dvm$ region
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$* reduction( max( imaxt1 ) )
      do i=2,N
        do j=1,M
          do ii=1,K
            do jj=1,L      
              if (A(i,j,ii,jj).GT.imaxt1) imaxt1=A(i,j,ii,jj)
            enddo
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
                    
C -----------------------------------------------------RED4306
      subroutine RED4306
      integer, parameter :: N = 8,M=8,K=8,L=8
      real, parameter :: NL=1.
      character*7 tname
      real, allocatable :: A(:,:,:,:),C(:,:,:,:)
      real iprod1,iprodt1 
      real NNl
                 
!dvm$ distribute A(BLOCK,BLOCK,*,BLOCK)    
      
      tname='RED4306'
      allocate (A(N,M,K,L),C(N,M,K,L))
      NNL=NL
      NN=N
      MM=M
      KK=K
      LL=L
      call serprodr4(C,NN,MM,KK,LL,NNL,iprod1)
      iprodt1 = 1.

!dvm$ actual(iprodt1)
!dvm$ region local(A)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if ((i.eq.j).and.(j.eq.ii).and.(ii.eq.jj))  then
                A(i,j,ii,jj) = i
              else
                A(i,j,ii,jj) =1.
              endif
            enddo
          enddo
        enddo
      enddo
  
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$* reduction( product( iprodt1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              iprodt1 = iprodt1*A(i,j,ii,jj)
            enddo
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
C -----------------------------------------------------RED4307
      subroutine RED4307
      integer, parameter :: N = 16,M=8,K=8,L=8
      character*7 tname
      logical, allocatable :: A(:,:,:,:),C(:,:,:,:)
      logical land1,landt1,leqv1,lneqv1,lor1

!dvm$ distribute A(BLOCK,*,BLOCK,BLOCK) 

      tname='RED4307'
      allocate (A(N,M,K,L),C(N,M,K,L))
      NN=N
      MM=M
      KK=K
      LL=L           
      call serlog4(C,NN,MM,KK,LL,land1,lor1,leqv1,lneqv1)

!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L,2
              A(i,j,ii,jj) = .true. 
            enddo 
          enddo
        enddo        
      enddo

!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=2,L,2
              A(i,j,ii,jj)=.false.
            enddo
          enddo
        enddo
      enddo
 
!dvm$ remote_access (A(1,1,1,1))
      landt1 = A(1,1,1,1)

!dvm$ actual(landt1)
!dvm$ region
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$* reduction( AND( landt1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if ((i.eq.1).and.(j.eq.1)
     *.and.(ii.eq.1).and.(jj.eq.1)) then
!                landt1=A(i,j,ii,jj)
              else
                landt1 = landt1 .and. A(i,j,ii,jj)
              endif
            enddo
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
C -----------------------------------------------------RED4308
      subroutine RED4308
      integer, parameter :: N = 16,M=8,K=16,L=8
      character*7 tname
      logical, allocatable :: A(:,:,:,:),C(:,:,:,:)
      logical land1,landt1,lor1,lort1,leqv1,lneqv1

!dvm$ distribute A(*,BLOCK,BLOCK,BLOCK) 

      tname='RED4308'
      allocate (A(N,M,K,L),C(N,M,K,L))
      NN=N
      MM=M
      KK=K
      LL=L
      call serlog4(C,NN,MM,KK,LL,land1,lor1,leqv1,lneqv1)
                                    
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L,2
               A(i,j,ii,jj) = .true. 
            enddo 
          enddo
        enddo        
      enddo

!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=2,L,2
              A(i,j,ii,jj)=.false.
            enddo
          enddo
        enddo
      enddo
                
!dvm$ remote_access (A(1,1,1,1))
      lort1 = A(1,1,1,1)

!dvm$ actual(lort1)
!dvm$ region
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$* reduction( OR( lort1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if ((i.eq.1).and.(j.eq.1)
     *.and.(ii.eq.1).and.(jj.eq.1)) then
!                lORt1=A(i,j,ii,jj)
              else
                lort1 = lort1 .or. A(i,j,ii,jj)
              endif
            enddo
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
C -----------------------------------------------------RED4309
      subroutine RED4309
      integer, parameter :: N = 16,M=8,K=8,L=8
      character*7 tname
      logical, allocatable :: A(:,:,:,:),C(:,:,:,:)
      logical land1,landt1,lor1,leqv1,leqvt1,lneqv1

!dvm$ distribute A(BLOCK,BLOCK,BLOCK,*)

      tname='RED4309'
      allocate (A(N,M,K,L),C(N,M,K,L))
      NN=N
      MM=M
      KK=K
      LL=L
      call serlog4(C,NN,MM,KK,LL,land1,lor1,leqv1,lneqv1)

!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
         do j=1,M
           do ii=1,K
             do jj=1,L,2
               A(i,j,ii,jj) = .true. 
             enddo 
           enddo
         enddo        
      enddo

!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=2,L,2
              A(i,j,ii,jj)=.false.
            enddo
          enddo
        enddo
      enddo
                                     
!dvm$ remote_access (A(1,1,1,1))
      leqvt1 = A(1,1,1,1)

!dvm$ actual(leqvt1)
!dvm$ region
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$* reduction( EQV( leqvt1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if ((i.eq.1).and.(j.eq.1)
     *.and.(ii.eq.1).and.(jj.eq.1)) then
!                leqvt1=A(i,j,ii,jj)
              else
                leqvt1 = leqvt1 .eqv. A(i,j,ii,jj)
              endif
            enddo
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
C -----------------------------------------------------RED4310
      subroutine RED4310
      integer, parameter :: N = 16,M=8,K=8,L=8
      character*7 tname
      logical, allocatable :: A(:,:,:,:),C(:,:,:,:)
      logical land1,landt1,lor1,leqv1,lneqv1,lneqvt1

!dvm$ distribute A(BLOCK,BLOCK,*,BLOCK)

      tname='RED4310'
      allocate (A(N,M,K,L),C(N,M,K,L))

      NN=N
      MM=M
      KK=K
      LL=L
      call serlog4(C,NN,MM,KK,LL,land1,lor1,leqv1,lneqv1)
                                    
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L,2
              A(i,j,ii,jj) = .true. 
            enddo 
          enddo
        enddo        
      enddo

!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=2,L,2
              A(i,j,ii,jj)=.false.
            enddo
          enddo
        enddo
      enddo

!dvm$ remote_access (A(1,1,1,1))
      lneqvt1 = A(1,1,1,1)

!dvm$ actual(lneqvt1)
!dvm$ region
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$* reduction( NEQV( lneqvt1 ) )
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if ((i.eq.1).and.(j.eq.1)
     *.and.(ii.eq.1).and.(jj.eq.1)) then
!                lneqvt1=A(i,j,ii,jj)
              else
                lneqvt1 = lneqvt1 .neqv. A(i,j,ii,jj)
              endif
            enddo
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
C ----------------------------------------------------RED4311
      subroutine RED4311
      integer, parameter :: N = 16,M=8,K=16,L=8,NL=100
      character*7 tname
      integer, allocatable :: A(:,:,:,:),C(:,:,:,:)
      integer imax1,imaxt1 ,ni,imin
      integer it1,jt1,it2,jt2,iit1,jjt1                 
      integer coor(4),lcoor
!dvm$ distribute A(BLOCK,*,BLOCK,BLOCK)

      tname='RED4311'
      allocate (A(N,M,K,L),C(N,M,K,L))
     
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = i*NL*NL*NL+j*NL*NL+ii*NL+jj
            enddo
          enddo
        enddo
      enddo
      
      ni=N/2
      nj=M/2
      nii=K/2
      njj=L/2
      A(ni,nj,nii,njj)=N+M+K+L+1+NL*NL*NL*NL       
      imax1=N+M+K+L+1+NL*NL*NL*NL 

!dvm$ remote_access (A(1,1,1,1))
      imaxt1=A(1,1,1,1)  

      lcoor=4
      coor(1)=0
      coor(2)=0
      coor(3)=0
      coor(4)=0

!dvm$ actual(imaxt1,coor,lcoor)
!dvm$ region
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$* reduction( maxloc( imaxt1,coor,4))
      do i=2,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if (A(i,j,ii,jj).GT.imaxt1)then
                imaxt1=A(i,j,ii,jj)
                coor(1)=i
                coor(2)=j
                coor(3)=ii
                coor(4)=jj 
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(imaxt1,coor)

      if ((imax1 .eq.imaxt1) .and.(coor(1).eq.ni)
     *.and.(coor(2).eq.nj).and.(coor(3).eq.nii)
     *.and.(coor(4).eq.njj)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
      
      end
C ----------------------------------------------------RED4312
      subroutine RED4312
      integer, parameter :: N = 16,M=8,K=16,L=8,NL=100
      character*7 tname
      integer, allocatable :: A(:,:,:,:),C(:,:,:,:)
      integer imin1,imint1 ,ni
      integer it1,jt1,it2,jt2,iit1,jjt1
      integer coor(4),lcoor
                 
!dvm$ distribute A(*,BLOCK,BLOCK,BLOCK)

      tname='RED4312'
      allocate (A(N,M,K,L),C(N,M,K,L))
     
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = i*NL*NL*NL+j*NL*NL+ii*NL+jj
            enddo
          enddo
        enddo
      enddo
      ni=N/2
      nj=M/2
      nii=K/2
      njj=L/2
      A(ni,nj,nii,njj)=-(N+M+K+L+1+NL*NL*NL*NL )
      
      imin1=-(N+M+K+L+1+NL*NL*NL*NL )

!dvm$ remote_access (A(1,1,1,1))
      imint1=A(1,1,1,1)  

      lcoor=4
      coor(1)=0
      coor(2)=0
      coor(3)=0
      coor(4)=0

!dvm$ actual(imint1,coor,lcoor)
!dvm$ region
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$* reduction( minloc( imint1,coor,4))
      do i=2,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              if (A(i,j,ii,jj).LT.imint1)then
                imint1=A(i,j,ii,jj)
                coor(1)=i
                coor(2)=j
                coor(3)=ii
                coor(4)=jj 
              endif
            enddo
          enddo
        enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(imint1,coor)

      if ((imin1 .eq.imint1) .and.(coor(1).eq.ni)
     *.and.(coor(2).eq.nj).and.(coor(3).eq.nii)
     * .and.(coor(4).eq.njj)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
      
      end
C ----------------------------------------------------RED4313
      subroutine RED4313
      integer, parameter :: N = 16,M=8,K=16,L=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:,:,:),C(:,:,:,:)
      integer imin1,imint1 ,ni
      integer isum1,isumt1 
      integer imax1,imaxt1
                      
!dvm$ distribute A(BLOCK,BLOCK,BLOCK,*)

      tname='RED4313'
      allocate (A(N,M,K,L),C(N,M,K,L))
      NNL=NL 
      NN=N
      MM=M
      KK=K
      LL=L

      call sersum4m(C,NN,MM,KK,LL,NNL,isum1)

!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
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
c      print *,'before remote'

!dvm$ remote_access (A(1,1,1,1))
      imaxt1=A(1,1,1,1)

      ni=N/2
      nj=M/2
      nii=K/2
      njj=L/2
      A(ni,nj,nii,njj)=-(N+M+K+L+1+NL)              
      imin1=-(N+M+K+L+1+NL)

!dvm$ remote_access (A(1,1,1,1))
      imint1=A(1,1,1,1)

      isumt1 = 0
c      print *,'before cycle' 
!dvm$ actual(isumt1,imaxt1,imint1)
!dvm$ region
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$* reduction( sum( isumt1 ),
!dvm$*max( imaxt1 ),min( imint1 ) )
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
C ----------------------------------------------------RED4314
      subroutine RED4314
      integer, parameter :: N = 16,M=8,K=16,L=8,NL=1
      character*7 tname
      integer, allocatable :: A(:,:,:,:),C(:,:,:,:)
      integer iprod1,iprodt1 
      logical, allocatable :: B(:,:,:,:),CL(:,:,:,:)
      logical land1,landt1,lor1,leqv1,lneqv1               
                 
!dvm$ distribute A(BLOCK,BLOCK,*,BLOCK)    
!dvm$ align B(I,J,II,JJ) with A(I,J,II,JJ)      

      tname='RED4314'
      allocate (A(N,M,K,L),C(N,M,K,L))
      allocate (B(N,M,K,L),CL(N,M,K,L))
      NNL=NL
      NN=N
      MM=M
      KK=K
      LL=L
      call serprod4(C,NN,MM,KK,LL,NNL,iprod1)
      call serlog4(CL,NN,MM,KK,LL,land1,lor1,leqv1,lneqv1)

!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L,2
              B(i,j,ii,jj) = .true. 
            enddo 
          enddo
        enddo        
      enddo

!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=2,L,2
              B(i,j,ii,jj)=.false.
            enddo
          enddo
        enddo
      enddo
 
!dvm$ remote_access (B(1,1,1,1))
      landt1 = B(1,1,1,1)                   
      iprodt1 = 1

!dvm$ actual(iprodt1,landt1)
!dvm$ region local(A)
!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
             if ((i.eq.j).and.(j.eq.ii).and.(ii.eq.jj)) then
               A(i,j,ii,jj) = i
             else
               A(i,j,ii,jj) =1
             endif
            enddo
          enddo
        enddo
      enddo

!dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),
!dvm$* reduction( product( iprodt1 ), and(landt1))
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              iprodt1 = iprodt1*A(i,j,ii,jj)
              if ((i.eq.1).and.(j.eq.1)
     *.and.(ii.eq.1).and.(jj.eq.1)) then
!                 landt1=B(i,j,ii,jj)
              else
                 landt1 = landt1 .and. B(i,j,ii,jj)
              endif
!              landt1 = landt1 .and.B(i,j,ii,jj)
            enddo
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
             if ((i.eq.j).and.(j.eq.ii).and.(ii.eq.jj))  then
               AR(i,j,ii,jj) = i
             else
               AR(i,j,ii,jj) =1
             endif
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
              if ((i.eq.j).and.(j.eq.ii).and.(ii.eq.jj)) then
                AR(i,j,ii,jj) = i
              else
                AR(i,j,ii,jj) =1.
              endif
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
      do i=1,N
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

      do i=1,N
        do j= 1,M
          do ii=1,K
            do jj=1,L
             if ((i.eq.1).and.(j.eq.1).and.(ii.eq.1).and.(jj.eq.1)) then
               LAND=AR(1,1,1,1)
               LOR=AR(1,1,1,1)
               LEQV=AR(1,1,1,1)
               LNEQV=AR(1,1,1,1)
             else
               LAND = LAND .and. AR(i,j,ii,jj)
               LOR = LOR .or.AR(i,j,ii,jj)
               LEQV = LEQV .eqv. AR(i,j,ii,jj)
               LNEQV = LNEQV .neqv. AR(i,j,ii,jj)
             endif
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
