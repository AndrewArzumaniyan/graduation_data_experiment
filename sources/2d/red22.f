      program RED22

c    TESTING OF THE REDUCTION CLAUSE .       
c    REDUCTION OPERATION : SUM,PRODUCT,MAX,MIN,AND,OR, EQV,
C    NEQV,MAXLOC,MINLOC AND THEIR COMBINATION ARE EXECUTED
c    FOR DISTRIBUTED ARRAY A(N,M). 

      print *,'===START OF RED22======================='
C --------------------------------------------------
      call red2201
C --------------------------------------------------
      call red2202
C --------------------------------------------------
      call red2203
C -------------------------------------------------
      call red2204
C -------------------------------------------------
      call red2205
C -------------------------------------------------
      call red2206
C --------------------------------------------------
      call red2207
C --------------------------------------------------
      call red2208
C --------------------------------------------------
      call red2209
C -------------------------------------------------
      call red2210
C -------------------------------------------------
      call red2211
C -------------------------------------------------
      call red2212
C ------------------------------------------------- 
      call red2213
C --------------------------------------------------
      call red2214
C --------------------------------------------------

C
C
      print *,'=== END OF RED22 ========================= '    
      end

C ----------------------------------------------------RED2201
      subroutine RED2201
      integer, parameter :: N = 16,M=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer isum1,isumt1 
                 
!dvm$ distribute A(BLOCK,*)   


      tname='RED2201'
      allocate (A(N,M),C(N,M))
      NNL=NL 
      NN=N
      MM=M
      call sersum2(C,NN,MM,NNL,isum1)
      isumt1 = 0

!dvm$ actual(isumt1)
!dvm$ region local(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
           A(i,j) = i+j+NL
         enddo
      enddo
  
!dvm$ parallel (i,j) on A(i,j), reduction( sum( isumt1 ) )
      do i=1,N
         do j=1,M
           isumt1 = isumt1+A(i,j)
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
C -----------------------------------------------------RED2202
      subroutine RED2202
      integer, parameter :: N = 16,M=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer iprod1,iprodt1       
                 
!dvm$ distribute A(*,BLOCK)    
     
      tname='RED2202'
      allocate (A(N,M),C(N,M))
      NNL=NL
      NN=N
      MM=M
      call serprod2(C,N,M,NNL,iprod1)
      iprodt1 = 1

!dvm$ actual(iprodt1)
!dvm$ region local(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
          if (i.eq.j)  then
           A(i,j) = i
          else
           A(i,j) =1
          endif
         enddo
      enddo
  

!dvm$ parallel (i,j) on A(i,j), reduction( product( iprodt1 ) )
      do i=1,N
         do j=1,M
          iprodt1 = iprodt1*A(i,j)
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
C ----------------------------------------------------RED2203
      subroutine RED2203
      integer, parameter :: N = 16,M=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer imax1,imaxt1 ,ni,imin
                       
!dvm$ distribute A(BLOCK,*) 

      tname='RED2203'
      allocate (A(N,M),C(N,M))
     
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
           A(i,j) = i+j+NL
         enddo
      enddo
      ni=N/2
      nj=M/2
      A(ni,nj)=N+M+1+NL
      imax1=N+M+1+NL

!dvm$ remote_access (A(1,1))
      imaxt1=A(1,1)  

!dvm$ actual(imaxt1)
!dvm$ region
!dvm$ parallel (i,j) on A(i,j), reduction( max( imaxt1 ) )
      do i=1,N
         do j=1,M
           if (A(i,j).GT.imaxt1) imaxt1=A(i,j)
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
    
C ----------------------------------------------------RED2204
      subroutine RED2204
      integer, parameter :: N = 16,M=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer imax1,imaxt1 ,ni,imin
                       
!dvm$ distribute A(*,BLOCK)

      tname='RED2204'
      allocate (A(N,M),C(N,M))
 
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
           A(i,j) = i+j+NL
         enddo
      enddo
  
      ni=N/2
      nj=M/2 
      A(ni,nj)=-(N+M+1+NL)
      imin1=-(N+M+1+NL)

!dvm$ remote_access (A(1,1))      
      imint1=A(1,1)

!dvm$ actual(imint1)
!dvm$ region
!dvm$ parallel (i,j) on A(i,j), reduction( min( imint1 ) )
      do i=2,N
         do j=1,M
           if (A(i,j).LT.imint1) imint1=A(i,j)
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
C ----------------------------------------------------RED2205
      subroutine RED2205
      integer, parameter :: N = 16,M=8
      real, parameter :: NL=1000.
      character*7 tname
      real, allocatable :: A(:,:),C(:,:)
      integer ni
      real imax1,imaxt1                  
!dvm$ distribute A(BLOCK,*)

      tname='RED2205'
      allocate (A(N,M),C(N,M))

!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
           A(i,j) = i+j+NL
         enddo
      enddo
      ni=N/2
      nj=M/2
      A(ni,nj)=N+M+1.+NL
      imax1=N+M+1.+NL

!dvm$ remote_access (A(1,1))
      imaxt1=A(1,1)  

!dvm$ actual(imaxt1)
!dvm$ region
!dvm$ parallel (i,j) on A(i,j), reduction( max( imaxt1 ) )
      do i=2,N
         do j=1,M
           if (A(i,j).GT.imaxt1) imaxt1=A(i,j)
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
                    
C -----------------------------------------------------RED2206
      subroutine RED2206
      integer, parameter :: N = 8,M=8
      real, parameter :: NL=1.
      character*7 tname
      real, allocatable :: A(:,:),C(:,:)
      real iprod1,iprodt1 
      real NNl
                 
!dvm$ distribute A(*,BLOCK)    
      
      tname='RED2206'
      allocate (A(N,M),C(N,M))

      NNL=NL
      NN=N
      MM=M
      call serprodr2(C,NN,MM,NNL,iprod1)
	  iprodt1 = 1.

!dvm$ actual(iprodt1)
!dvm$ region local(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
          if (i.eq.j)  then
           A(i,j) = i
          else
           A(i,j) =1.
          endif
         enddo
      enddo
      
!dvm$ parallel (i,j) on A(i,j), reduction( product( iprodt1 ) )
      do i=1,N
         do j=1,M
          iprodt1 = iprodt1*A(i,j)
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
C -----------------------------------------------------RED2207
      subroutine RED2207
      integer, parameter :: N = 16,M=8
      character*7 tname
      logical, allocatable :: A(:,:),C(:,:)
      logical land1,landt1,leqv1,lneqv1,lor1

!dvm$ distribute A(BLOCK,*)    

      tname='RED2207'
      allocate (A(N,M),C(N,M))
      NN=N
      MM=M            
      call serlog2(C,NN,MM,land1,lor1,leqv1,lneqv1)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M,2
           A(i,J) = .true. 
         enddo        
      enddo

!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=2,M,2
           A(i,j)=.false.
         enddo
      enddo
 
!dvm$ remote_access (A(1,1))
      landt1 = A(1,1)

!dvm$ actual(landt1)
!dvm$ region
!dvm$ parallel (i,j) on A(i,j), reduction( AND( landt1 ) )
      do i=1,N
         do j=1,M
          if ((i.eq.1).and.(j.eq.1))  then
!            landt1=A(i,j)
          else
            landt1 = landt1 .and. A(i,j)
          endif
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
C -----------------------------------------------------RED2208
      subroutine RED2208
      integer, parameter :: N = 16,M=8
      character*7 tname
      logical, allocatable :: A(:,:),C(:,:)
      logical land1,landt1,lor1,lort1,leqv1,lneqv1

!dvm$ distribute A(*,BLOCK)    

      tname='RED2208'
      allocate (A(N,M),C(N,M))
      NN=N
      MM=M
      call serlog2(C,NN,MM,land1,lor1,leqv1,lneqv1)
                                    
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M,2
          A(i,j) = .true.
         enddo
      enddo

!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=2,M,2
          A(i,j)=.false.
         enddo
      enddo

!dvm$ remote_access (A(1,1))
      lort1 = A(1,1)

!dvm$ actual(lort1)
!dvm$ region
!dvm$ parallel (i,j) on A(i,j), reduction( OR( lort1 ) )
      do i=1,N
         do j=1,M
          if ((i.eq.1).and.(j.eq.1))  then
!           lort1=A(i,j)
          else
           lort1 = lort1 .or. A(i,j)
          endif
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
C -----------------------------------------------------RED2209
      subroutine RED2209
      integer, parameter :: N = 16,M=8
      character*7 tname
      logical, allocatable :: A(:,:),C(:,:)
      logical land1,landt1,lor1,leqv1,leqvt1,lneqv1

!dvm$ distribute A(BLOCK,*)

      tname='RED2209'
      allocate (A(N,M),C(N,M))
      NN=N
      MM=M
      call serlog2(C,NN,MM,land1,lor1,leqv1,lneqv1)
                                    
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M,2
          A(i,J) = .true.
         enddo         
      enddo

!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=2,M,2
          A(i,j)=.false.
         enddo
      enddo

!dvm$ remote_access (A(1,1))
      leqvt1 = A(1,1)

!dvm$ actual(leqvt1)
!dvm$ region
!dvm$ parallel (i,j) on A(i,j), reduction( EQV( leqvt1 ) )
      do i=1,N
         do j=1,M
          if ((i.eq.1).and.(j.eq.1))  then
!           leqvt1=A(i,j)
          else
           leqvt1 = leqvt1 .eqv. A(i,j)
          endif
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
C -----------------------------------------------------RED2210
      subroutine RED2210
      integer, parameter :: N = 16,M=8
      character*7 tname
      logical, allocatable :: A(:,:),C(:,:)
      logical land1,landt1,lor1,leqv1,lneqv1,lneqvt1

!dvm$ distribute A(*,BLOCK)

      tname='RED2210'
      allocate (A(N,M),C(N,M))
      NN=N
      MM=M
      call serlog2(C,NN,MM,land1,lor1,leqv1,lneqv1)
               
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M,2
           A(i,J) = .true.
         enddo         
      enddo
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=2,M,2
          A(i,j)=.false.
         enddo
      enddo

!dvm$ remote_access (A(1,1))
      lneqvt1 = A(1,1)

!dvm$ actual(lneqvt1)
!dvm$ region
!dvm$ parallel (i,j) on A(i,j), reduction( NEQV( lneqvt1 ) )
      do i=1,N
         do j=1,M
          if ((i.eq.1).and.(j.eq.1))  then
           continue
!           lneqvt1=A(i,j)
          else
           lneqvt1 = lneqvt1 .neqv. A(i,j)
          endif
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
C ----------------------------------------------------RED2211
      subroutine RED2211
      integer, parameter :: N = 16,M=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer imax1,imaxt1 ,ni,imin
      integer it1,jt1,it2,jt2              
      integer coor(2),lcoor
   
!dvm$ distribute A(BLOCK,*)

      tname='RED2211'
      allocate (A(N,M),C(N,M))
     
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
          A(i,j) = i*NL+j
         enddo
      enddo
      ni=N/2
      nj=M/2
      A(ni,nj)=N+M+1+NL*NL
      imax1=N+M+1+NL*NL

!dvm$ remote_access (A(1,1))
      imaxt1=A(1,1)  

      lcoor=2
      coor(1)=0
      coor(2)=0

!dvm$ actual(imaxt1,coor,lcoor)
!dvm$ region
!dvm$ parallel (i,j) on A(i,j), reduction( maxloc( imaxt1,coor,2))
      do i=2,N
         do j=1,M
          if (A(i,j).GT.imaxt1)then
           imaxt1=A(i,j)
           coor(1)=i
           coor(2)=j
          endif
         enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(imaxt1,coor) 

      if ((imax1 .eq.imaxt1) .and.(coor(1).eq.ni)
     *.and.(coor(2).eq.nj)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
      
      end
C ----------------------------------------------------RED2212
      subroutine RED2212
      integer, parameter :: N = 16,M=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer imin1,imint1 ,ni
      integer it1,jt1,it2,jt2
      integer coor(2),lcoor
                 
!dvm$ distribute A(*,BLOCK)

      tname='RED2212'
      allocate (A(N,M),C(N,M))
     
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
           A(i,j) = i*NL+j
         enddo
      enddo
      ni=N/2
      nj=M/2 
      A(ni,nj)=-(N+M+1+NL*NL)
      imin1=-(N+M+1+NL*NL)

!dvm$ remote_access (A(1,1))
      imint1=A(1,1)  

      lcoor=2
      coor(1)=0
      coor(2)=0

!dvm$ actual(imint1,coor,lcoor)
!dvm$ region
!dvm$ parallel (i,j) on A(i,j), reduction( minloc( imint1,coor,2))
      do i=2,N
         do j=1,M
          if (A(i,j).LT.imint1)then
           imint1=A(i,j)
           coor(1)=i
           coor(2)=j
          endif
         enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(imint1,coor)

      if ((imin1 .eq.imint1) .and.(coor(1).eq.ni)
     *.and.(coor(2).eq.nj)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
      
      end
C ----------------------------------------------------RED2213
      subroutine RED2213
      integer, parameter :: N = 16,M=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer imin1,imint1 ,ni
      integer isum1,isumt1 
      integer imax1,imaxt1
                      
!dvm$ distribute A(BLOCK,*)

      tname='RED2213'
      allocate (A(N,M),C(N,M))
      NNL=NL 
      NN=N
      MM=M
      call sersum2m(C,NN,MM,NNL,isum1)

!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
           A(i,j) = i+j+NL
         enddo
      enddo
      ni=N/2-1
      nj=M/2-1
      A(ni,nj)=N+M+1+NL
      imax1=N+M+1+NL

!dvm$ remote_access (A(1,1))
      imaxt1=A(1,1)  

      ni1=N/2
      nj1=M/2
      A(ni1,nj1)=-(N+M+1+NL)
      imin1=-(N+M+1+NL)

!dvm$ remote_access (A(1,1))
      imint1=A(1,1)

      isumt1 = 0
!dvm$ actual(isumt1,imaxt1,imint1)
!dvm$ region
!dvm$ parallel (i,j) on A(i,j), reduction( sum( isumt1 ),
!dvm$*max( imaxt1 ),min( imint1 ) )
      do i=1,N
         do j=1,M
          isumt1 = isumt1+A(i,j)
          if (A(i,j).GT.imaxt1) imaxt1=A(i,j)
          if (A(i,j).LT.imint1) imint1=A(i,j)
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
C ----------------------------------------------------RED2214
      subroutine RED2214
      integer, parameter :: N = 16,M=8,NL=1
      character*7 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer iprod1,iprodt1 
      logical, allocatable :: B(:,:),CL(:,:)
      logical land1,landt1,lor1,leqv1,lneqv1
               
!dvm$ distribute A(BLOCK,*)    
!dvm$ align B(I,J) with A(I,J)      

      tname='RED2214'
      allocate (A(N,M),C(N,M))
      allocate (B(N,M),CL(N,M))

      NNL=NL
      NN=N
      MM=M
      call serprod2(C,NN,MM,NNL,iprod1)
      call serlog2(CL,NN,MM,land1,lor1,leqv1,lneqv1)

!dvm$ parallel (i,j) on B(i,j)
      do i=1,N
         do j=1,M,2
          B(i,J) = .true.
         enddo         
      enddo
!dvm$ parallel (i,j) on B(i,j)
      do i=1,N
         do j=2,M,2
          B(i,j)=.false.
         enddo
      enddo

!dvm$ remote_access (B(1,1))
      landt1 = B(1,1)    
      iprodt1 = 1
               
!dvm$ actual(iprodt1,landt1)
!dvm$ region local(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
          if (i.eq.j)  then
           A(i,j) = i
          else
           A(i,j) =1
          endif
         enddo
      enddo
  
!dvm$ parallel (i,j) on A(i,j), reduction( product( iprodt1 ),
!dvm$* and(landt1))
      do i=1,N
         do j=1,M
          iprodt1 = iprodt1*A(i,j)
          if ((i.eq.1).and.(j.eq.1))  then
!            landt1=B(i,j)
          else
            landt1 = landt1 .and. B(i,j)
          endif
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
      ni=N/2-1
      nj=M/2-1
      AR(ni,nj)=N+M+1.+NL
      ni=N/2
      nj=M/2
      AR(ni,ni)=-(N+M+1.+NL)
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
           if (i.eq.j)  then
            AR(i,j) = i
           else
            AR(i,j) =1
           endif
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
           if (i.eq.j)  then
            AR(i,j) = i
           else
            AR(i,j) =1.
           endif
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
      do  i=1,N,1
         do j=1,M,2
           AR(i,j) = .true.
         enddo         
      enddo
      do i=1,N,1
         do j=2,M,2
           AR(i,j)=.false.
         enddo
      enddo 
      do i=1,N
       do j= 1,M
        if ((i.eq.1).and.(j.eq.1))  then
         LAND=AR(1,1)
         LOR=AR(1,1)
         LEQV=AR(1,1)
         LNEQV=AR(1,1)
        else
         LAND = LAND .and. AR(i,j)
         LOR = LOR .or.AR(i,j)
         LEQV = LEQV .eqv. AR(i,j)
         LNEQV = LNEQV .neqv. AR(i,j)
        endif
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
