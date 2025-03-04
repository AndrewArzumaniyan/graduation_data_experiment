      program RED21

c    TESTING OF THE REDUCTION CLAUSE .       
c    REDUCTION OPERATION : SUM,PRODUCT,MAX,MIN,AND,OR, EQV,
C    NEQV,MAXLOC,MINLOC AND THEIR COMBINATION ARE EXECUTED
c    FOR DISTRIBUTED ARRAY A(N,M). 

      print *,'===START OF RED21======================='
C --------------------------------------------------
      call red2101
C --------------------------------------------------
      call red2102
C --------------------------------------------------
      call red2103
C -------------------------------------------------
      call red2104
C -------------------------------------------------
      call red2105
C -------------------------------------------------
      call red2106
C --------------------------------------------------
      call red2107
C --------------------------------------------------
      call red2108
C --------------------------------------------------
      call red2109
C -------------------------------------------------
      call red2110
C -------------------------------------------------
      call red2111
C -------------------------------------------------
      call red2112
C --------------------------------------------------
      call red2113
C --------------------------------------------------
      call red2114
C --------------------------------------------------
C
C
      print *,'=== END OF RED21 ========================= '    
      end

C ----------------------------------------------------RED2101
      subroutine RED2101
      integer, parameter :: N = 16,M=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer isum1,isumt1 
                 
!dvm$ distribute A(BLOCK,BLOCK)    

      tname='RED2101'
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
C -----------------------------------------------------RED2102
      subroutine RED2102
      integer, parameter :: N = 16,M=8,NL=1
      character*7 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer iprod1,iprodt1 
      
!dvm$ distribute A(BLOCK,BLOCK)    
      
      tname='RED2102'
      allocate (A(N,M),C(N,M))
      NNL=NL
      NN=N
      MM=M
      call serprod2(C,NN,MM,NNL,iprod1)
      iprodt1 = 1

!dvm$ actual(iprodt1)
!dvm$ region local(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
           A(i,j) = i+j+NL
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
C ----------------------------------------------------RED2103
      subroutine RED2103
      integer, parameter :: N = 16,M=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:)
      integer imax1,imaxt1 ,ni,imin,nj
                       
!dvm$ distribute A(BLOCK,BLOCK) 

      tname='RED2103'
      allocate (A(N,M))
     
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

!dvm$ actual(imaxt1,A)
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
      deallocate (A)
      
      end
    
C ----------------------------------------------------RED2104
      subroutine RED2104
      integer, parameter :: N = 16,M=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer imax1,imaxt1 ,ni,imin,nj
                       
!dvm$ distribute A(BLOCK,BLOCK)

      tname='RED2104'
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
C ----------------------------------------------------RED2105
      subroutine RED2105
      integer, parameter :: N = 16,M=8
      real, parameter :: NL=1000.
      character*7 tname
      real, allocatable :: A(:,:),C(:,:)
      integer ni
      real imax1,imaxt1                  
!dvm$ distribute A(BLOCK,BLOCK)

      tname='RED2105'
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
                    
C -----------------------------------------------------RED2106
      subroutine RED2106
      integer, parameter :: N = 8,M=6
      real, parameter :: NL=1.
      character*7 tname
      real, allocatable :: A(:,:),C(:,:)
      real iprod1,iprodt1 
      real NNl
      intrinsic INT           
!dvm$ distribute A(BLOCK,BLOCK)    

      tname='RED2106'
      allocate (A(N,M),C(N,M))
      NNL=NL
      NN=N
      MM=M
      call serprodr2(C,NN,Mm,NNL,iprod1)
      iprodt1 = 1.

!dvm$ actual(iprodt1)
!dvm$ region local(A)
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
            if(i.eq.j) then
               A(i,j) = I+NL
            else
               A(i,j) = 1.
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
       if(INT(iprod1) .eq. INT(iprodt1))  then
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
 
      end
C -----------------------------------------------------RED2107
      subroutine RED2107
      integer, parameter :: N = 16,M=8
      character*7 tname
      logical, allocatable :: A(:,:),C(:,:)
      logical land1,landt1,leqv1,lneqv1,lor1

!dvm$ distribute A(BLOCK,BLOCK)     

      tname='RED2107'
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
          continue
!           landt1=A(i,j)
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
C -----------------------------------------------------RED2108
      subroutine RED2108
      integer, parameter :: N = 16,M=8
      character*7 tname
      logical, allocatable :: A(:,:),C(:,:)
      logical land1,landt1,lor1,lort1,leqv1,lneqv1

!dvm$ distribute A(BLOCK,BLOCK)   

      tname='RED2108'
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
      lort1 = A(1,1)

!dvm$ actual(lort1)
!dvm$ region
!dvm$ parallel (i,j) on A(i,j), reduction( OR( lort1 ) )
      do i=1,N
         do j=1,M
          if ((i.eq.1).and.(j.eq.1))  then
          continue
!           lort1=A(i,j)
          else
           lort1 = lort1 .or. A(i,j)
          endif
         enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(lort1) 

      if (lort1 .eqv.lort1) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)

      end
C -----------------------------------------------------RED2109
      subroutine RED2109
      integer, parameter :: N = 16,M=8
      character*7 tname
      logical, allocatable :: A(:,:),C(:,:)
      logical land1,landt1,lor1,leqv1,leqvt1,lneqv1

!dvm$ distribute A(BLOCK,BLOCK)

      tname='RED2109'
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
          continue
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
C -----------------------------------------------------RED2110
      subroutine RED2110
      integer, parameter :: N = 16,M=8
      character*7 tname
      logical, allocatable :: A(:,:),C(:,:)
      logical land1,landt1,lor1,leqv1,lneqv1,lneqvt1

!dvm$ distribute A(BLOCK,BLOCK)

      tname='RED2110'
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
C ----------------------------------------------------RED2111
      subroutine RED2111
      integer, parameter :: N = 16,M=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer imax1,imaxt1 ,ni,imin,nj
      integer it1,jt1,it2,jt2
      integer coor(2),lcoor           
!dvm$ distribute A(BLOCK,BLOCK)

      tname='RED2111'
      allocate (A(N,M),C(N,M))
     
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
           A(i,j) = i * NL + j
         enddo
      enddo
      ni=N/2
      nj=M/2
      A(ni,nj)=N+M+1+NL * NL
      imax1=N+M+1+NL * NL

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
C ----------------------------------------------------RED2112
      subroutine RED2112
      integer, parameter :: N = 16,M=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer imin1,imint1 ,ni
c      integer it1,jt1,it2,jt2 
      integer coor(2),lcoor
                
!dvm$ distribute A(BLOCK,BLOCK)

      tname='RED2112'
      allocate (A(N,M),C(N,M))
     
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
           A(i,j) = i * NL + j
         enddo
      enddo
      ni=N/2
      nj=M/2 
      A(ni,nj)=-(N+M+1+NL * NL)
      imin1=-(N+M+1+NL * NL)

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

c      print *,imin1, imint1
c      print *,coor(1),ni
c      print *,coor(2),nj
      if ((imin1 .eq.imint1) .and.(coor(1).eq.ni)
     *.and.(coor(2).eq.nj)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (A,C)
      
      end
C ----------------------------------------------------RED2113
      subroutine RED2113
      integer, parameter :: N = 16,M=8,NL=1000
      character*7 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer imin1,imint1 ,ni
      integer isum1,isumt1 
      integer imax1,imaxt1
                      
!dvm$ distribute A(BLOCK,BLOCK)

      tname='RED2113'
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
C ----------------------------------------------------RED2114
      subroutine RED2114
      integer, parameter :: N = 16,M=8,NL=1
      character*7 tname
      integer, allocatable :: A(:,:),C(:,:)
      integer iprod1,iprodt1 
      logical, allocatable :: B(:,:),CL(:,:)
      logical land1,landt1,lor1,leqv1,lneqv1
                 
!dvm$ distribute A(BLOCK,BLOCK)    
!dvm$ align B(I,J) with A(I,J)      

      tname='RED2114'
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
               
!dvm$ parallel (i,j) on A(i,j)
      do i=1,N
         do j=1,M
           A(i,j) = i+j+NL
         enddo
      enddo
  
C      print *,A
      iprodt1 = 1

!dvm$ actual(iprodt1,landt1)
!dvm$ region
!dvm$ parallel (i,j) on A(i,j), reduction( product( iprodt1 ),
!dvm$* and(landt1))
      do i=1,N
         do j=1,M
          iprodt1 = iprodt1*A(i,j)
c         print *,i, j,iprodt1
          if ((i.eq.1).and.(j.eq.1))  then
          continue
!           landt1=B(i,j)
          else
           landt1 = landt1 .and. B(i,j)
          endif
         enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(iprodt1,landt1) 

c      print *,iprod1,iprodt1,land1,landt1
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
            if(i.eq.j)then
               AR(i,j) = I+NL
            else
               AR(i,j) = 1.  
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
      do i=1,N
       do j= 1,M
        if ((i.eq.1).and.(j.eq.1))  then
         LAND=AR(1,1)
         LOR=AR(1,1)
C         LEQV=.true.
C         LNEQV=.false.
          LNEQV=AR(1,1)
          LEQV=AR(1,1)
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
