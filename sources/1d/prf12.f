      program PRF12
     
c    TESTING OF THE PREFETCH DIRECTIVE . 

      print *,'===START OF PRF11========================'
C --------------------------------------------------
      call prf1201
      call prf1202
      call prf1203
C --------------------------------------------------

C
      print *,'=== END OF PRF12 ========================= '    
      end
C ---------------------------------------------PRF1201
      subroutine PRF1201
      integer, parameter ::  N = 16,NL=1000,NIT=3
      integer, allocatable :: A(:),B(:),C(:),D(:)
      integer nloop 
      character*7 tname
                
cdvm$ distribute B(*)     

cdvm$ align (I) with B(I) ::A,D
cdvm$ remote_group GR1
cdvm$ remote_group GR2
cdvm$ remote_group GR3

      tname='PRF1201'
      allocate (B(N),A(N),C(N),D(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL
*dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
        B(i)=NL+i
        D(i)=NL+i
      enddo
      it=0
      do it=1,NIT

cdvm$ prefetch GR1 
cdvm$ prefetch GR2 
cdvm$ prefetch GR3 
                                              
cdvm$ remote_access (GR1:A(1))
      ib1=A(1)
               
cdvm$ remote_access (GR2:A(N/2))
      ib2=A(N/2)  

cdvm$ remote_access (GR3:A(N))
      ib3=A(N)

cdvm$ remote_access (GR1:B(2))
      ib4=B(2)
               
cdvm$ remote_access (GR2:B(N/2-1))
      ib5=B(N/2-1)  

cdvm$ remote_access (GR3:B(N-1))
      ib6=B(N-1)

cdvm$ remote_access (GR1:D(3))
      ib7=D(3)
               
cdvm$ remote_access (GR2:D(N/2-2))
      ib8=D(N/2-2)  

cdvm$ remote_access (GR3:D(N-2))
      ib9=D(N-2)

      if ((ib1 .eq.C(1)).and.(ib2.eq.C(N/2)).and.(ib3.eq.C(N))
     * .and.(ib4 .eq.C(2)) .and.(ib5 .eq.C(N/2-1))
     * .and.(ib6 .eq.C(N-1))
     * .and.(ib7 .eq.C(3)).and.(ib8 .eq.C(N/2-2))
     * .and.(ib9 .eq.C(N-2))) then     
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
C ---------------------------------------------PRF1202
      subroutine PRF1202     
      integer, parameter ::  N = 16,NL=1000,NIT=3
      integer, allocatable :: A(:),B(:),C(:),D(:)
      integer nloop 
      character*7 tname
                
cdvm$ distribute B(*)     

cdvm$ align (I) with B(I) ::A
cdvm$ remote_group GR1
cdvm$ remote_group GR2
cdvm$ remote_group GR3

      tname='PRF1202'
      allocate (B(N),A(N),C(N),D(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL
*dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
      enddo
      it=0
      do it=1,NIT

cdvm$ prefetch GR1 
cdvm$ prefetch GR2 
cdvm$ prefetch GR3 
                                              
      isumc1=0
      isuma1=0  

cdvm$ remote_access (GR1:A(:))                                           
      do i=1,N         
       D(i)=A(i)
       isumc1=isumc1+C(i)
       isuma1=isuma1+D(i)
      enddo

      isumc2=0
      isuma2=0

      kk=2
      kk1=3
cdvm$ remote_access (GR2:A(:))                                               
      do i=1,N/kk-kk1
       D(i)=A(kk*i+kk1)
       isumc2=isumc2+C(kk*i+kk1)
       isuma2=isuma2+D(i)
      enddo
      if ((isumc1 .eq.isuma1) .and.(isumc2 .eq.isuma2)) then     
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
C ---------------------------------------------PRF1203
      subroutine PRF1203     
      integer, parameter ::  N = 16,NL=1000,NIT=3
      integer, allocatable :: A(:),B(:),C(:),A1(:)
      integer nloop 
      character*7 tname
                
cdvm$ distribute B(*)     

cdvm$ align (I) with B(I) ::A,A1
cdvm$ remote_group GR1
cdvm$ remote_group GR2
cdvm$ remote_group GR3

      tname='PRF1203'
      allocate (B(N),A(N),C(N),A1(N))
      NNL=NL    
      call serial1(C,N,NNL)
      nloop=NL

*dvm$ parallel (i) on A(i)
      do i=1,N
        A(i) = NL+i
        A1(i) = NL+i
      enddo

      it=0
      do it=1,NIT

cdvm$ prefetch GR1 
cdvm$ prefetch GR2 
cdvm$ prefetch GR3 

      nloop1=NL

*dvm$ parallel (i) on B(i),remote_access(GR1:A(1))
      do i=1,N
         B(i) = A(1)
      enddo
*dvm$ parallel (i) on A(i), reduction( min( nloop1 ) )
      do i=1,N
          if (B(i).ne.C(1)) nloop1=min(nloop1,i)
      enddo

      nloop2=NL

*dvm$ parallel (i) on B(i),remote_access(GR1:A(N))
      do i=1,N
         B(i) = A(N)
      enddo
*dvm$ parallel (i) on B(i), reduction( min( nloop2 ) )
      do i=1,N
          if (B(i).ne.C(N)) nloop2=min(nloop2,i)
      enddo
      nloop3=NL

*dvm$ parallel (i) on B(i),remote_access(GR2:A(N/2))
      do  i=1,N
         B(i) = A(N/2)
      enddo
*dvm$ parallel (i) on B(i), reduction( min( nloop3 ) )
      do i=1,N
          if (B(i).ne.C(N/2)) nloop3=min(nloop3,i)
      enddo
               
      nloop4=NL

*dvm$ parallel (i) on B(i),remote_access(GR2:A)
      do  i=1,N
         B(i) = A(i)
      enddo
*dvm$ parallel (i) on B(i), reduction( min( nloop4 ) )
      do  i=1,N
          if (B(i).ne.C(i)) nloop4=min(nloop4,i)
      enddo
      nloop5=NL

*dvm$ parallel (i) on B(i),remote_access(GR3:A1(i))
      do i=1,N
         B(i) = A1(i)
      enddo
*dvm$ parallel (i) on B(i), reduction( min( nloop ) )
      do i=1,N
          if (B(i).ne.C(i)) nloop=min(nloop,i)
      enddo

      nloop6=NL
      kk=2
      kk1=3         
*dvm$ parallel (i) on B(i),remote_access(GR3:A(kk*i+kk1))
      do i=1,N/kk-kk1
         B(i) = A(kk*i+kk1)
      enddo

*dvm$ parallel (i) on B(i), reduction( min( nloop6 ) )
      do i=1,N/kk-kk1
          if (B(i).ne.C(kk*i+kk1)) nloop6=min(nloop6,i)
      enddo
          
      if ((nloop1 .eq.NL) .and.(nloop2 .eq.NL).and.(nloop2 .eq.NL)
     * .and.(nloop3 .eq.NL).and.(nloop4 .eq.NL).and.(nloop5 .eq.NL)
     * .and.(nloop6 .eq.NL)  ) then     
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
C -----------------------------------------------         
      subroutine serial1(AR,N,NL)
      integer AR(N)
      integer NL 
      do i=1,N
        AR(i) = NL+i
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
