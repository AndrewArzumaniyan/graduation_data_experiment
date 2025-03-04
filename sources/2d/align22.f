      program ALIGN22

c    TESTING align CLAUSE .       

      print *,'===START OF align22========================'
C --------------------------------------------------
c 221	arrA2[BLOCK][ BLOCK] arrB2[][]	ALIGN arrB[i][j] WITH arrA[i][j]	normal
      call align221
C -------------------------------------------------
c 222		                    	ALIGN arrB[i][j] WITH arrA[i][2*j]	stretching along j
      call align222
C -------------------------------------------------
c 223			                    ALIGN arrB[i][j] WITH arrA[i+4][j]	shift along i
      call align223
C -------------------------------------------------
c 224			                    ALIGN arrB[i][j] WITH arrA[-i+9][j]	reverse on i
c      call align224
C -------------------------------------------------
c 225			                    ALIGN arrB[i][j] WITH arrA[i+4][j+4]shift along i and j
      call align225
      call align2251
C -------------------------------------------------
c 226			                    ALIGN arrB[i][j] WITH arrA[j][i]	rotation
      call align226
C -------------------------------------------------
c 227			                    ALIGN arrB[i][j] WITH arrA[j+1][i]	rotation and shift
      call align227
C -------------------------------------------------
C
C
      print *,'=== END OF align22 ========================= '    
      end

C ----------------------------------------------------align221
c 221	arrA2[BLOCK][ BLOCK]	arrB2[][]	ALIGN arrB[i][j] WITH arrA[i][j]	normal
      subroutine align221
      integer, parameter :: AN1=8,AN2=8,BN1=8,BN2=8,NL=1000,ER=10000
      character*9 tname
      integer, allocatable :: A2(:,:),B2(:,:)
      integer erri,i,j,ia,ja,ib,jb
!dvm$ distribute A2(BLOCK,BLOCK)   
!dvm$ ALIGN B2(i,j) WITH A2(i,j)


      tname='align221'
      allocate (A2(AN1,AN2),B2(BN1,BN2))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2,B2)
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) =0     
          enddo 
      enddo 
  
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             B2(i,j) = i*NL+j
          enddo 
      enddo 

!dvm$ parallel (i,j) on B2(i,j), reduction( min( erri )), private(ia,ja)
      do i=1,BN1
          do j=1,BN2
            if (B2(i,j) .eq.(i*NL+j)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
            ia=i
            ja=j
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
          enddo 
      enddo 
!dvm$ end region
!dvm$ get_actual(erri)
  
     
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B2,A2)
      end


C ----------------------------------------------------align222
c 222		                    	ALIGN arrB[i][j] WITH arrA[i][2*j]	stretching along j
      subroutine align222
      integer, parameter :: AN1=8,AN2=8,BN1=8,BN2=4,NL=1000,ER=10000
c     parameters for ALIGN arrB[i][j] WITH arrA[k1i * i + li][k2j * j + lj]                                                 
      integer, parameter :: k1i=1,k2i=0,li=0,k1j=0,k2j=2,lj=0
      character*9 tname
      integer, allocatable :: A2(:,:),B2(:,:)
      integer erri,i,j,ia,ja,ib,jb              

!dvm$ distribute A2(BLOCK,BLOCK)   
!dvm$ ALIGN B2(i,j) WITH A2(k1i * i + li,k2j * j + lj)

      tname='align222'
      allocate (A2(AN1,AN2),B2(BN1,BN2))
      erri= ER
      NNL=NL
       
!dvm$ actual(erri)
!dvm$ region local(A2,B2)
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) =0     
          enddo 
      enddo 

!dvm$ parallel (i,j) on A2(i,j), private(ib,jb)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             if (((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *          (((i-li)/k1i) .le. BN1)  .and.
     *          (((j-lj)/k2j) .le. BN2))  then 
                ib = (i-li)/k1i
                jb = (j-lj)/k2j  
                B2(ib,jb) = ib*NL+jb
             endif 
          enddo 
      enddo 

!dvm$ parallel (i,j) on B2(i,j), reduction( min( erri )), private(ia,ja)
      do i=1,BN1
          do j=1,BN2
            if (B2(i,j) .eq.(i*NL+j)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
            ia=k1i * i + li
            ja=k2j * j + lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
          enddo 
      enddo 
!dvm$ end region
!dvm$ get_actual(erri)
     
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B2,A2)
      end

C ----------------------------------------------------align223
c 223			                    ALIGN arrB[i][j] WITH arrA[i+4][j]	shift along i
      subroutine align223
      integer, parameter :: AN1=8,AN2=8,BN1=4,BN2=8,NL=1000,ER=10000
c     parameters for ALIGN arrB[i][j] WITH arrA[k1i * i + li][k2j * j + lj]                                                 
      integer, parameter :: k1i=1,k2i=0,li=4,k1j=0,k2j=1,lj=0
      character*9 tname
      integer, allocatable :: A2(:,:),B2(:,:)
      integer erri,i,j,ia,ja,ib,jb
               
!dvm$ distribute A2(BLOCK,BLOCK)    
!dvm$ ALIGN B2(i,j) WITH A2(k1i * i + li,k2j * j + lj)


      tname='align223'
      allocate (A2(AN1,AN2),B2(BN1,BN2))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2,B2)
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) =0     
          enddo 
      enddo 

!dvm$ parallel (i,j) on A2(i,j), private(ib,jb)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             if (((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((j-lj)/k2j) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)  .and.
     *          (((j-lj)/k2j) .le. BN2))  then 
                ib = (i-li)/k1i
                jb = (j-lj)/k2j  
                B2(ib,jb) = ib*NL+jb
             endif 
          enddo 
      enddo 

!dvm$ parallel (i,j) on B2(i,j), reduction( min( erri )), private(ia,ja)
      do i=1,BN1
          do j=1,BN2
            if (B2(i,j) .eq.(i*NL+j)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
            ia=k1i * i + li
            ja=k2j * j + lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
          enddo 
      enddo 
!dvm$ end region
!dvm$ get_actual(erri)
     
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B2,A2)
      end

C ----------------------------------------------------align224
c 224			                    ALIGN arrB[i][j] WITH arrA[-i+9][j]	reverse on i
      subroutine align224
      integer, parameter :: AN1=8,AN2=8,BN1=8,BN2=8,NL=1000,ER=10000
c     parameters for ALIGN arrB[i][j] WITH arrA[k1i * i + li][k2j * j + lj]                                                 
      integer, parameter :: k1i=-1,k2i=0,li=9,k1j=0,k2j=1,lj=0
      character*9 tname
      integer, allocatable :: A2(:,:),B2(:,:)
      integer erri,i,j,ia,ja,ib,jb
               
!dvm$ distribute A2(BLOCK,BLOCK)    
!dvm$ ALIGN B2(i,j) WITH A2(k1i * i + li,k2j * j + lj)


      tname='align224'
      allocate (A2(AN1,AN2),B2(BN1,BN2))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2,B2)

!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) =0     
          enddo 
      enddo 

!dvm$ parallel (i,j) on A2(i,j), private(ib,jb)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             if (((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((j-lj)/k2j) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)  .and.
     *          (((j-lj)/k2j) .le. BN2))  then 
                ib = (i-li)/k1i
                jb = (j-lj)/k2j  
                B2(ib,jb) = ib*NL+jb
             endif 
          enddo 
      enddo 

!dvm$ parallel (i,j) on B2(i,j), reduction( min( erri )), private(ia,ja)
      do i=1,BN1
          do j=1,BN2
            if (B2(i,j) .eq.(i*NL+j)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
            ia=k1i * i + li
            ja=k2j * j + lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
          enddo 
      enddo 
!dvm$ end region
!dvm$ get_actual(erri)
     
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B2,A2)
      end

C ----------------------------------------------------align225
c 225			                    ALIGN arrB[i][j] WITH arrA[i+4][j+4]shift along i and j
      subroutine align225
      integer, parameter :: AN1=8,AN2=8,BN1=4,BN2=4,NL=1000,ER=10000
c     parameters for ALIGN arrB[i][j] WITH arrA[k1i * i + li][k2j * j + lj]                                                 
      integer, parameter :: k1i=1,k2i=0,li=4,k1j=0,k2j=1,lj=4
      character*9 tname
      integer, allocatable :: A2(:,:),B2(:,:)
      integer erri,i,j,ia,ja,ib,jb
               
!dvm$ distribute A2(BLOCK,BLOCK)    
!dvm$ ALIGN B2(i,j) WITH A2(k1i * i + li,k2j * j + lj)


      tname='align225'
      allocate (A2(AN1,AN2),B2(BN1,BN2))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2,B2)
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) =0     
          enddo 
      enddo 

!dvm$ parallel (i,j) on A2(i,j), private(ib,jb)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             if (((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((j-lj)/k2j) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)  .and.
     *          (((j-lj)/k2j) .le. BN2))  then 
                ib = (i-li)/k1i
                jb = (j-lj)/k2j  
                B2(ib,jb) = ib*NL+jb
             endif 
          enddo 
      enddo 

!dvm$ parallel (i,j) on B2(i,j), reduction( min( erri )), private(ia,ja)
      do i=1,BN1
          do j=1,BN2
            if (B2(i,j) .eq.(i*NL+j)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
            ia=k1i * i + li
            ja=k2j * j + lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
          enddo 
      enddo 
!dvm$ end region
!dvm$ get_actual(erri)
     
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B2,A2)
      end
C ----------------------------------------------------align2251
c 2251			                    ALIGN arrB[i][j] WITH arrA[i+1][2*j]shift along i and j
      subroutine align2251
      integer, parameter :: AN1=3,AN2=4,BN1=2,BN2=2,NL=1000,ER=10000
c     parameters for ALIGN arrB[i][j] WITH arrA[k1i * i + li][k2j * j + lj]                                                 
      integer, parameter :: k1i=1,k2i=0,li=1,k1j=0,k2j=2,lj=0
      character*9 tname
      integer, allocatable :: A2(:,:),B2(:,:)
      integer erri,i,j,ia,ja,ib,jb
               
!dvm$ distribute A2(BLOCK,BLOCK)    
!dvm$ ALIGN B2(i,j) WITH A2(k1i * i + li,k2j * j + lj)


      tname='align2251'
      allocate (A2(AN1,AN2),B2(BN1,BN2))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2,B2)
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) =0     
          enddo 
      enddo 

!dvm$ parallel (i,j) on A2(i,j), private(ib,jb)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             if (((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((j-lj)/k2j) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)  .and.
     *          (((j-lj)/k2j) .le. BN2))  then 
                ib = (i-li)/k1i
                jb = (j-lj)/k2j  
                B2(ib,jb) = ib*NL+jb
             endif 
          enddo 
      enddo 

!dvm$ parallel (i,j) on B2(i,j), reduction( min( erri )), private(ia,ja)
      do i=1,BN1
          do j=1,BN2
            if (B2(i,j) .eq.(i*NL+j)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
            ia=k1i * i + li
            ja=k2j * j + lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
          enddo 
      enddo 
!dvm$ end region
!dvm$ get_actual(erri)
     
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B2,A2)
      end
C ----------------------------------------------------align226
c 226			                    ALIGN arrB[i][j] WITH arrA[j][i]	rotation
      subroutine align226
      integer, parameter :: AN1=4,AN2=4,BN1=4,BN2=4,NL=1000,ER=10000
c     parameters for ALIGN arrB[i][j] WITH arrA[k2i * j + li][k1j * i + lj]                                                 
      integer, parameter :: k1i=0,k2i=1,li=0,k1j=1,k2j=0,lj=0
      character*9 tname
      integer, allocatable :: A2(:,:),B2(:,:)
      integer erri,i,j,ia,ja,ib,jb
               
!dvm$ distribute A2(BLOCK,BLOCK)   
!dvm$ ALIGN B2(i,j) WITH A2(k2i * j + li,k1j * i + lj)


      tname='align226'
      allocate (A2(AN1,AN2),B2(BN1,BN2))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2,B2)
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) =0     
          enddo 
      enddo 

!dvm$ parallel (i,j) on A2(i,j), private(ib,jb)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             if (((i-li) .eq.(((i-li)/k2i) * k2i)) .and.
     *          ((j-lj) .eq.(((j-lj)/k1j) *k1j)) .and.
     *          (((i-li)/k2i) .gt. 0)  .and.
     *          (((j-lj)/k1j) .gt. 0)  .and.
     *          (((i-li)/k2i) .le. BN2)  .and.
     *          (((j-lj)/k1j) .le. BN1))  then 
                ib = (j-lj)/k1j
                jb = (i-li)/k2i  
                B2(ib,jb) = ib*NL+jb
             endif 
          enddo 
      enddo 

!dvm$ parallel (i,j) on B2(i,j), reduction( min( erri )), private(ia,ja)
      do i=1,BN1
          do j=1,BN2
            if (B2(i,j) .eq.(i*NL+j)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
            ia=k2i * j + li
            ja=k1j * i + lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
          enddo 
      enddo 
!dvm$ end region
!dvm$ get_actual(erri)
     
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B2,A2)
      end


C ----------------------------------------------------align227
c 227			                    ALIGN arrB[i][j] WITH arrA[j+1][i]	rotation and shift
      subroutine align227
      integer, parameter :: AN1=8,AN2=8,BN1=4,BN2=4,NL=1000,ER=10000
c     parameters for ALIGN arrB[i][j] WITH arrA[k2i * j + li][k1j * i + lj]                                                 
      integer, parameter :: k1i=0,k2i=1,li=1,k1j=1,k2j=0,lj=0
      character*9 tname
      integer, allocatable :: A2(:,:),B2(:,:)
      integer erri,i,j,ia,ja,ib,jb
               
!dvm$ distribute A2(BLOCK,BLOCK)    
!dvm$ ALIGN B2(i,j) WITH A2(k2i * j + li,k1j * i + lj)

      tname='align227'
      allocate (A2(AN1,AN2),B2(BN1,BN2))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2,B2)
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) =0     
          enddo 
      enddo 

!dvm$ parallel (i,j) on A2(i,j), private(ib,jb)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             if (((i-li) .eq.(((i-li)/k2i) * k2i)) .and.
     *          ((j-lj) .eq.(((j-lj)/k1j) *k1j)) .and.
     *          (((i-li)/k2i) .gt. 0)  .and.
     *          (((j-lj)/k1j) .gt. 0)  .and.
     *          (((i-li)/k2i) .le. BN2)  .and.
     *          (((j-lj)/k1j) .le. BN1))  then 
                ib = (j-lj)/k1j
                jb = (i-li)/k2i  
                B2(ib,jb) = ib*NL+jb
             endif 
          enddo 
      enddo 

!dvm$ parallel (i,j) on B2(i,j), reduction( min( erri )), private(ia,ja)
      do i=1,BN1
          do j=1,BN2
            if (B2(i,j) .eq.(i*NL+j)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
            ia=k2i * j + li
            ja=k1j * i + lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
               erri = min(erri,i*NL/10+j)
            endif 
          enddo 
      enddo 
!dvm$ end region
!dvm$ get_actual(erri)
  
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      deallocate (B2,A2)
      end


C -------------------------------------------------

      subroutine ansyes(name)
      character*9 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*9 name
      print *,name,'  -  ***error'
      end