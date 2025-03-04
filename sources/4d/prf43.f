      program PRF43
     
c    TESTING OF THE PREFETCH DIRECTIVE. 

      print *,'===START OF PRF43========================'
C --------------------------------------------------
      call prf4301
      call prf4302
      call prf4303
C
      print *,'=== END OF PRF43 ========================= '    
      end
C ---------------------------------------------------------PRF4301
      subroutine PRF4301
      integer, parameter ::  N = 16,M=8,K=8,L=16,NL=1000,NIT=3
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:)
      integer, allocatable :: C(:,:,:,:),A1(:,:,:,:)
      character*7 tname
                 
cdvm$ distribute B(BLOCK,BLOCK,BLOCK,*)    
cdvm$ align(:,:,:,:) with B(:,:,:,:) :: A,A1 

cdvm$ remote_group GR1
cdvm$ remote_group GR2
cdvm$ remote_group GR3

      tname='PRF4301'
      allocate (B(N,M,K,L),A(N,M,K,L),C(N,M,K,L),A1(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)

*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
             A(i,j,ii,jj) = NL+i+j+ii+jj
             A1(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo                                         
      enddo 
                                        
      do it=1,NIT
cdvm$ prefetch GR1                                                  
cdvm$ prefetch GR2 
cdvm$ prefetch GR3

cdvm$ remote_access (GR1:A(1,1,1,1))
      ib1=A(1,1,1,1)               

cdvm$ remote_access (GR1:A(N,M,K,L))
      ib2=A(N,M,K,L)               

cdvm$ remote_access (GR2:A(1,M,K,L))
      ib3=A(1,M,K,L)               

cdvm$ remote_access (GR3:A(N,1,K,L))
      ib4=A(N,1,K,L)               

cdvm$ remote_access (GR3:A(N,M,1,L))
      ib5=A(N,M,1,L)               

cdvm$ remote_access (GR3:A1(N,M,K,1))
      ib6=A1(N,M,K,1)                

      if ((ib1 .eq.C(1,1,1,1)) .and.(ib2 .eq.C(N,M,K,L)) .and.
     * (ib3 .eq.C(1,M,K,L)) .and.(ib4 .eq.C(N,1,K,L)) .and.
     * (ib5 .eq.C(N,M,1,L)).and.(ib6 .eq.C(N,M,K,1))) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      if (it .eq. 2) cycle 
cdvm$ reset GR1
cdvm$ reset GR2
cdvm$ reset GR3

      enddo
      deallocate (A,B,C,A1)

      end

C ------------------------------------------------------PRF4302
      subroutine PRF4302     
      integer, parameter ::  N = 16,M=8,K=8,L=16,NL=1000,NIT=3
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:)
      integer, allocatable :: C(:,:,:,:),D(:,:,:,:)
      character*7 tname
                 
cdvm$ distribute A(BLOCK,BLOCK,*,BLOCK)    
cdvm$ align(:,:,:,:) with A(:,:,:,:) :: B 

cdvm$ remote_group GR1
cdvm$ remote_group GR2
cdvm$ remote_group GR3

      tname='PRF4302'
      allocate (A(N,M,K,L),B(N,M,K,L),C(N,M,K,L),D(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)

*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo                                         
      enddo 
      do it=1,NIT
cdvm$ prefetch GR1                                                  
cdvm$ prefetch GR2 
cdvm$ prefetch GR3 
               
      isumc1=0
      isuma1=0

cdvm$ remote_access (GR1:A(:,:,:,:))
      do i=1,N         
       do j=i,M
        do ii=1,K
         do jj=1,L
          D(i,j,ii,jj)=A(i,j,ii,jj)
          isumc1=isumc1+C(i,j,ii,jj)
          isuma1=isuma1+D(i,j,ii,jj)
         enddo
        enddo
       enddo
      enddo 

      isumc2=0
      isuma2=0

cdvm$ remote_access (GR1:A(1,:,:,:))
      do j=1,M         
       do ii=1,K
        do jj=1,L
         D(1,j,ii,jj)=A(1,j,ii,jj)
         isumc2=isumc2+C(1,j,ii,jj)
         isuma2=isuma2+D(1,j,ii,jj)
        enddo
       enddo
      enddo 

      isumc3=0
      isuma3=0

cdvm$ remote_access (GR2:A(:,M,:,:))
      do i=1,N         
       do ii=1,K
        do jj=1,L
         D(i,M,ii,jj)=A(i,M,ii,jj)
         isumc3=isumc3+C(i,M,ii,jj)
         isuma3=isuma3+D(i,M,ii,jj)
        enddo
       enddo
      enddo            

      isumc4=0
      isuma4=0

cdvm$ remote_access (GR3:A(:,:,K,:))
      do i=1,N
       do j=1,M         
        do jj=1,L
         D(i,j,K,jj)=A(i,j,K,jj)
         isumc4=isumc4+C(i,j,K,jj)
         isuma4=isuma4+D(i,j,K,jj)
        enddo
       enddo             
      enddo

      isumc5=0
      isuma5=0

cdvm$ remote_access (GR3:A(:,:,:,L))
      do i=1,N
       do j=1,M         
        do ii=1,K
         D(i,j,ii,L)=A(i,j,ii,L)
         isumc5=isumc5+C(i,j,ii,L)
         isuma5=isuma5+D(i,j,ii,L)
        enddo
       enddo             
      enddo
           
      if ((isumc1 .eq.isuma1).and.(isumc2 .eq.isuma2).and. 
     *      (isumc3 .eq.isuma3).and.(isumc4 .eq.isuma4).and.
     *      (isumc5 .eq.isuma5)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      if (it .eq. 2) cycle 
cdvm$ reset GR1
cdvm$ reset GR2
cdvm$ reset GR3

      enddo  
      deallocate (A,B,C,D)
     
      end

C ------------------------------------------------------PRF4303
      subroutine PRF4303
      integer, parameter ::  N = 16,M=8,K=8,L=16,NL=1000,NIT=3
      integer, allocatable :: A(:,:,:,:),B(:,:,:,:)
      integer, allocatable :: C(:,:,:,:),A1(:,:,:,:)
      character*7 tname
                 
cdvm$ distribute A(BLOCK,*,BLOCK,BLOCK)    
cdvm$ align(:,:,:,:) with A(:,:,:,:) :: B,A1 

cdvm$ remote_group GR1
cdvm$ remote_group GR2
cdvm$ remote_group GR3

      tname='PRF4303'
      allocate (A(N,M,K,L),B(N,M,K,L),C(N,M,K,L),A1(N,M,K,L))
      NNL=NL    
      call serial4(C,N,M,K,L,NNL)

*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj)
      do i=1,N
        do j=1,M
          do ii=1,K
            do jj=1,L
              A(i,j,ii,jj) = NL+i+j+ii+jj
              A1(i,j,ii,jj) = NL+i+j+ii+jj
            enddo
          enddo
        enddo                                         
      enddo 
 
      do it=1,NIT
cdvm$ prefetch GR1                                                  
cdvm$ prefetch GR2 
cdvm$ prefetch GR3 
               

      nloopi1=NL
      nloopj1=NL
      nloopii1=NL
      nloopjj1=NL

*dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
*dvm$*remote_access(GR1:A(1,1,1,1))
      do i=1,N
       do j=1,M
        do ii=1,K
         do jj=1,L
          B(i,j,ii,jj) = A(1,1,1,1)
         enddo
        enddo
       enddo
      enddo

*dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
*dvm$* reduction( min( nloopi1),min(nloopj1),min(nloopii1),
*dvm$*  min(nloopjj1))
      do i=1,N
       do j=1,M
        do ii=1,K
         do jj=1,L
          if (B(i,j,ii,jj).ne.C(1,1,1,1)) then
           nloopi1=min(nloopi1,i)
           nloopj1=min(nloopj1,j)
           nloopii1=min(nloopii1,ii)
           nloopjj1=min(nloopjj1,jj)
          endif
         enddo
        enddo
       enddo
      enddo

      nloopi2=NL
      nloopj2=NL
      nloopii2=NL
      nloopjj2=NL

*dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),remote_access(GR1:A(N,M,K,L))
      do i=1,N
       do j=1,M
        do ii=1,K
         do jj=1,L
          B(i,j,ii,jj) = A(N,M,K,L)      
         enddo
        enddo
       enddo
      enddo 

*dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
*dvm$*reduction( min( nloopi2),min(nloopj2),min(nloopii2),min(nloopjj2))
      do i=1,N
       do j=1,M
        do ii=1,K
         do jj=1,L
          if (B(i,j,ii,jj).ne.C(N,M,K,L)) then
           nloopi2=min(nloopi2,i)
           nloopj2=min(nloopj2,j)
           nloopii2=min(nloopii2,ii)
           nloopjj2=min(nloopjj2,jj)
          endif
         enddo
        enddo
       enddo
      enddo

      nloopi3=NL
      nloopj3=NL
      nloopii3=NL
      nloopjj3=NL

*dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),remote_access(GR2:A)
      do i=1,N
       do j=1,M
        do ii=1,K
         do jj=1,L
          B(i,j,ii,jj) = A(i,j,ii,jj)      
         enddo
        enddo
       enddo
      enddo
 
*dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
*dvm$* reduction( min( nloopi3),min(nloopj3),min(nloopii3),
*dvm$* min(nloopjj3))
      do i=1,N
       do j=1,M
        do ii=1,K
         do jj=1,L
          if (B(i,j,ii,jj).ne.C(i,j,ii,jj)) then
           nloopi3=min(nloopi3,i)
           nloopj3=min(nloopj3,j)
           nloopii3=min(nloopii3,ii)
           nloopjj3=min(nloopjj3,jj)
          endif
         enddo
        enddo
       enddo
      enddo

      nloopi4=NL
      nloopj4=NL
      nloopii4=NL
      nloopjj4=NL

*dvm$ parallel (i,j,ii,jj) on A(i,j,ii,jj),remote_access(GR2:A(1,:,:,:))
      do i=1,N
       do j=1,M
        do ii=1,K
         do jj=1,L
           B(i,j,ii,jj) = A(1,j,ii,jj)      
         enddo
        enddo
       enddo
      enddo 

*dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
*dvm$*reduction( min( nloopi4),min(nloopj4),min(nloopii4),
*dvm$*min(nlooopjj4))
      do i=1,N
       do j=1,M
        do ii=1,K
         do jj=1,L
          if (B(i,j,ii,jj).ne.C(1,j,ii,jj)) then
           nloopi4=min(nloopi4,i)
           nloopj4=min(nloopj4,j)
           nloopii4=min(nloopii4,ii)
           nloopjj4=min(nloopjj4,jj)
          endif
         enddo
        enddo
       enddo
      enddo

      nloopi5=NL
      nloopj5=NL
      nloopii5=NL
      nloopjj5=NL

*dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
*dvm$*remote_access(GR3:A(:,M,:,:))
      do i=1,N
       do j=1,M
        do ii=1,K
         do jj=1,L
          B(i,j,ii,jj) = A(i,M,ii,jj)      
         enddo
        enddo
       enddo
      enddo
 
*dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
*dvm$* reduction( min( nloopi5),min(nloopj5),min(nloopii5),
*dvm$* min(nloopjj5))
      do i=1,N
       do j=1,M
        do ii=1,K
         do jj=1,L
          if (B(i,j,ii,jj).ne.C(i,M,ii,jj)) then
           nloopi5=min(nloopi5,i)
           nloopj5=min(nloopj5,j)
           nloopii5=min(nloopii5,ii)
           nloopjj5=min(nloopjj5,jj)
          endif
         enddo
        enddo
       enddo
      enddo

      nloopi6=NL
      nloopj6=NL
      nloopii6=NL
      nloopjj6=NL

*dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
*dvm$*remote_access(GR3:A1(:,:,K,:))
      do i=1,N
       do j=1,M
        do ii=1,K
         do jj=1,L
           B(i,j,ii,jj) = A1(i,j,K,jj)      
         enddo
        enddo
       enddo
      enddo

*dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
*dvm$*reduction( min( nloopi6),min(nloopj6),min(nloopii6),min(nloopjj6))
      do i=1,N
       do j=1,M
        do ii=1,K
         do jj=1,L
          if (B(i,j,ii,jj).ne.C(i,j,K,jj)) then
           nloopi6=min(nloopi6,i)
           nloopj6=min(nloopj6,j)
           nloopii6=min(nloopii6,ii)
           nloopjj6=min(nloopjj6,jj)
          endif
         enddo
        enddo
       enddo
      enddo

      nloopi7=NL
      nloopj7=NL
      nloopii7=NL
      nloopjj7=NL

*dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
*dvm$*remote_access(GR3:A1(:,:,:,L))
      do i=1,N
       do j=1,M
        do ii=1,K
         do jj=1,L
          B(i,j,ii,jj) = A1(i,j,ii,L)      
         enddo
        enddo
       enddo
      enddo

*dvm$ parallel (i,j,ii,jj) on B(i,j,ii,jj),
*dvm$*reduction( min( nloopi7),min(nloopj7),min(nloopii7),min(nloopjj7))
      do i=1,N
       do j=1,M
        do ii=1,K
         do jj=1,L
          if (B(i,j,ii,jj).ne.C(i,j,ii,L)) then
           nloopi7=min(nloopi7,i)
           nloopj7=min(nloopj7,j)
           nloopii7=min(nloopii7,ii)
           nloopjj7=min(nloopjj7,jj)
          endif
         enddo
        enddo
       enddo
      enddo 

      if ((nloopi1 .eq.NL).and.(nloopi2 .eq.NL).and.
     *     (nloopi3 .eq.NL).and. (nloopi4 .eq.NL).and.
     *     (nloopi5 .eq.NL).and.(nloopi6 .eq.NL).and.
     *     (nloopi7 .eq.NL)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
   
      if (it .eq. 2) cycle 
cdvm$ reset GR1
cdvm$ reset GR2
cdvm$ reset GR3

      enddo    
      deallocate (A,B,C,A1)

      end

C ---------------------------------------------------------         
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
