      program ALIGN12

c    TESTING align CLAUSE .       

      print *,'===START OF align12========================'
C --------------------------------------------------
c 121	arrA1[BLOCK]	arrB2[][]	ALIGN arrB[][i] WITH arrA[i]	matrix compression: 
c                                                              column on vector element
      call align121
C -------------------------------------------------
c 122			                    ALIGN arrB[i][ ] WITH arrA[2*i+1]	matrix compression: 
c                                                               line on vector element
      call align122
C -------------------------------------------------
c 123			                    ALIGN arrB[][ ] WITH arrA[]	 
      call align123
C -------------------------------------------------
C
C
      print *,'=== END OF align12 ========================= '    
      end

C ----------------------------------------------------align121
c 121	arrA1[BLOCK]	arrB2[][]	ALIGN arrB[][i] WITH arrA[i]	matrix compression: 
c                                                              column on vector element
      subroutine align121
      integer, parameter ::  AN1=8,BN1=4,BN2=4,NL=1000,ER=10000
c     parameters for ALIGN arrB(*,i) WITH arrA[k1i*i+li]                                                
      integer, parameter ::  k1i=1,k2i=0,li=0
      character*9 tname
      integer, allocatable :: A1(:),B2(:,:)
      integer s,cs,erri,i,j,ib,jb              
!dvm$ distribute A1(BLOCK)    
!dvm$ ALIGN B2(*,i) WITH A1(k1i*i+li)

      tname='align121'
      allocate (A1(AN1),B2(BN1,BN2))
      erri= ER
c      call stralign121 
      NNL=NL 
      s=0 

!dvm$ actual(erri, s)
!dvm$ region local(A1,B2)
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) =0     
          enddo
      enddo 

!dvm$ parallel (i) on A1(i), private(ib,jb,j)
      do i=1,AN1
          A1(i) = i
          do j=1,BN1
             if (  
     *          ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN2)  )then
                ib = j
                jb = (i-li)/k1i
                B2(ib,jb) = ib*NL+jb
             endif   
          enddo 
      enddo 
      
!dvm$ parallel (i,j) on B2(i,j), reduction( min( erri ),sum(s) )
      do i=1,BN1
          do j=1,BN2
            s = s + B2(i,j)
            if (B2(i,j) .eq.(i*NL+j)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
          enddo 
      enddo 
!dvm$ end region
!dvm$ get_actual(erri,s)
  
      cs = 0              
      do i=1,BN1
          do j=1,BN2
                cs = cs + i*NL+j
          enddo 
      enddo 
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
          else
          call ansno(tname)
          write (*,*) erri,s,cs
      endif 
      deallocate (B2,A1)

      end
C ----------------------------------------------------align122
c 122			                    ALIGN arrB[i][ ] WITH arrA[2*i+1]	matrix compression: 
c                                                               line on vector element
      subroutine align122
      integer, parameter ::  AN1=16,BN1=4,BN2=4,NL=1000,ER=10000
c     parameters for ALIGN arrB(i,*) WITH arrA[k1i*i+li]                                                
      integer, parameter ::  k1i=2,k2i=0,li=1
      character*9 tname
      integer, allocatable :: A1(:),B2(:,:)
      integer s,cs,erri,i,j,ib,jb
!dvm$ distribute A1(BLOCK)    
!dvm$ ALIGN B2(i,*) WITH A1(k1i*i+li)


      tname='align122'
      allocate (A1(AN1),B2(BN1,BN2))
      erri= ER
c      call stralign122 
      NNL=NL 
      s=0 

!dvm$ actual(erri, s)
!dvm$ region local(A1,B2)
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) =0     
          enddo
      enddo 

!dvm$ parallel (i) on A1(i), private(ib,jb,j)
      do i=1,AN1
          A1(i) = i
          do j=1,BN2
             if (  
     *          ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)  )then
                jb = j
                ib = (i-li)/k1i
                B2(ib,jb) = ib*NL+jb
             endif   
          enddo 
      enddo 
       
!dvm$ parallel (i,j) on B2(i,j), reduction( min( erri ),sum(s) )
      do i=1,BN1
          do j=1,BN2
            s = s + B2(i,j)
            if (B2(i,j) .eq.(i*NL+j)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
          enddo 
      enddo 
!dvm$ end region
!dvm$ get_actual(erri,s)
  
      cs = 0              
      do i=1,BN1
          do j=1,BN2
                cs = cs + i*NL+j
          enddo 
      enddo 
     
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
          else
          call ansno(tname)
          write (*,*) erri,s,cs
      endif 
      deallocate (B2,A1)
      end
C ----------------------------------------------------align123
c 123			                    ALIGN arrB[][ ] WITH arrA[]	 
      subroutine align123
      integer, parameter ::  AN1=16,BN1=4,BN2=4,NL=1000,ER=10000
c     parameters for ALIGN arrB(*,*) WITH arrA[*]                                                
      integer, parameter ::  k1i=0,k2i=0,li=0
      character*9 tname
      integer, allocatable :: A1(:),B2(:,:)
      integer s,erri,i,j,ib,jb
!dvm$ distribute A1(BLOCK)    
!dvm$ ALIGN B2(*,*) WITH A1(*)


      tname='align123'
      allocate (A1(AN1),B2(BN1,BN2))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A1,B2)
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) =i*NL+j     
          enddo
      enddo 

!dvm$ parallel (i) on A1(i), reduction( min( erri )), private(ib,jb)
      do i=1,AN1
        do ib=1,BN1
          do jb=1,BN2
            if (B2(ib,jb) .eq.(ib*NL+jb)) then     
            else
               erri = min(erri,ib*NL/10+jb)
            endif 
          enddo 
        enddo 
      enddo 
!dvm$ end region
!dvm$ get_actual(erri)
       
     
      if ((erri .eq.ER) 
     *     ) then     
          call ansyes(tname)
          else
          call ansno(tname)
          write (*,*) erri
      endif
      deallocate (B2,A1)

      end
C ------------------------------------------------------------


      subroutine ansyes(name)
      character*9 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*9 name
      print *,name,'  -  ***error'
      end