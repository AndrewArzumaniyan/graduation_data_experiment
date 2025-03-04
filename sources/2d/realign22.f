      program REALIGN22

c    Testing REALIGN directive        

      print *,'===START OF realign22===================='
C -------------------------------------------------
c 221 ALIGN arrB[i][j] WITH arrA[i][j] REALIGN arrB[i][j] WITH arrA[3*i-2][2*j+1]	
      call realign221
C -------------------------------------------------
c 222 ALIGN arrB[i][j] WITH arrA[j+1][i] REALIGN arrB[i][j] WITH arrA[i+4][j]	
      call realign222
C -------------------------------------------------
c 223 ALIGN arrB[i][*] WITH arrA[*][i]  REALIGN arrB[i][j] WITH arrA[i+4][j+4]	
      call realign223
C -------------------------------------------------
c 224 ALIGN arrB[*][*] WITH arrA[*][1]  REALIGN arrB[i][j] WITH arrA[i+4][j+4] shift along i and j
      call realign224
C -------------------------------------------------
c 225 ALIGN arrB[i][j] WITH arrA[i][j] REALIGN arrB[*][*] WITH arrA[*][2]	
      call realign225
C -------------------------------------------------
c 226 ALIGN arrB[i][j] WITH arrA[i][j] REALIGN arrB[i][j] WITH arrA[2*j+1][3*i-2]	
      call realign226
C -------------------------------------------------
C
      print *,'=== END OF realign22 ===================='
C
      end
C ----------------------------------------------------realign221
c 221 ALIGN arrB[i][j] WITH arrA[i][j] REALIGN arrB[i][j] WITH arrA[3*i-2][2*j+1]	
      subroutine realign221
      integer, parameter :: AN1=10,AN2=10,BN1=4,BN2=4,NL=1000,ER=10000
      integer :: erria=ER, errib=ER
      integer :: i,j,ia,ja,ib,jb
c     parameters for ALIGN arrB[i][j] WITH arrA[k1i * i + li][k2j * j + lj]                                                 
      integer, parameter ::  k1i=1,k2i=0,li=0,k1j=0,k2j=1,lj=0
c     parameters for REALIGN arrB[i][j] WITH arrA[kr1i * i + lri][kr2j * j + lrj]                                                 
      integer, parameter ::  kr1i=3,kr2i=0,lri=-2,kr1j=0,kr2j=2,lrj=1
      integer, allocatable :: A2(:,:),B2(:,:)
      character(10) ::  tname = 'realign221'
               
!dvm$ distribute A2(BLOCK,BLOCK)   
!dvm$ ALIGN B2(i,j) WITH A2(k1i * i + li,k2j * j + lj)
!dvm$ DYNAMIC B2

      allocate (A2(AN1,AN2),B2(BN1,BN2))
       
!dvm$ region
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) = 0     
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
!dvm$ end region

!dvm$ REALIGN B2(i,j) WITH A2(kr1i * i + lri,kr2j * j + lrj)

!dvm$ actual(erria,errib)
!dvm$ region in(A2,B2), out(A2,B2)
!dvm$ parallel (i,j) on B2(i,j), private(ia,ja), 
!dvm$*       reduction(min(erria),min(errib))
      do i=1,BN1
          do j=1,BN2
            if (B2(i,j) /= (i*NL+j)) then     
               errib = min(errib,i*NL/10+j)
            endif 
            ia=kr1i * i + lri
            ja=kr2j * j + lrj
            if (A2(ia,ja) /= (ia*NL+ja)) then     
               erria = min(erria,i*NL/10+j)
            endif 
          enddo 
      enddo
!dvm$ end region
  
!dvm$ get_actual(erria,errib)
      if ((erria == ER) .and. (errib == ER)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (B2,A2)

      end

C ----------------------------------------------------realign222
c 222 ALIGN arrB[i][j] WITH arrA[j+1][i] REALIGN arrB[i][j] WITH arrA[i+4][j]	
      subroutine realign222
      integer, parameter :: AN1=8,AN2=8,BN1=4,BN2=4,NL=1000,ER=10000
      integer :: erria=ER, errib=ER
      integer :: i,j,ia,ja,ib,jb
c     parameters for ALIGN arrB[i][j] WITH arrA[k2i * j + li][k1j * i + lj]                                                 
      integer, parameter ::  k1i=0,k2i=1,li=1,k1j=1,k2j=0,lj=0
c     parameters for REALIGN arrB[i][j] WITH arrA[kr1i * i + lri][kr2j * j + lrj]                                                 
      integer, parameter ::  kr1i=1,kr2i=0,lri=0,kr1j=0,kr2j=1,lrj=0
      integer, allocatable :: A2(:,:),B2(:,:)
      character(10) ::  tname = 'realign222'
               
!dvm$ distribute A2(BLOCK,BLOCK)   
!dvm$ ALIGN B2(i,j) WITH A2(k2i * j + li,k1j * i + lj)
!dvm$ DYNAMIC B2

      allocate (A2(AN1,AN2),B2(BN1,BN2))
       
!dvm$ region
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) = 1     
          enddo 
      enddo

!dvm$ parallel (i,j) on A2(i,j), private(ib,jb)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = (i*NL+j)*2
             if (((i-li) .eq.(((i-li)/k2i) * k2i)) .and.
     *          ((j-lj) .eq.(((j-lj)/k1j) *k1j)) .and.
     *          (((i-li)/k2i) .gt. 0)  .and.
     *          (((j-lj)/k1j) .gt. 0)  .and.
     *          (((i-li)/k2i) .le. BN2)  .and.
     *          (((j-lj)/k1j) .le. BN1))  then 
                ib = (j-lj)/k1j
                jb = (i-li)/k2i  
                B2(ib,jb) = B2(ib,jb) + ib*NL+jb
             endif 
          enddo 
      enddo
!dvm$ end region

!dvm$ REALIGN B2(i,j) WITH A2(kr1i * i + lri,kr2j * j + lrj)

!dvm$ actual(erria,errib)
!dvm$ region 
!dvm$ parallel (i,j) on B2(i,j),
!dvm$*       reduction(min(erria),min(errib)),
!dvm$*       private(ia,ja) 

      do i=1,BN1
          do j=1,BN2
            if (B2(i,j) /= (i*NL+j+1)) then     
               errib = min(errib,i*NL/10+j)
            endif 
            ia=kr1i * i + lri
            ja=kr2j * j + lrj
            if (A2(ia,ja) /= (ia*NL+ja)*2) then     
               erria = min(erria,i*NL/10+j)
            endif 
          enddo 
      enddo
!dvm$ end region
  
!dvm$ get_actual(erria,eriib)
      if ((erria == ER) .and. (errib == ER)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (B2,A2)

      end

C ----------------------------------------------------realign223
c 223 ALIGN arrB[i][*] WITH arrA[*][i]  REALIGN arrB[i][j] WITH arrA[i+4][j+4]	
      subroutine realign223
      integer, parameter :: AN1=10,AN2=10,BN1=4,BN2=4,NL=1000,ER=10000
      integer :: erria=ER, errib=ER
      integer :: i,j,ia,ja,ib,jb
c     parameters for ALIGN arrB[i][*] WITH arrA[*][k1j*i + lj]                                                 
      integer, parameter ::  k1i=0,k2i=0,li=0,k1j=1,k2j=0,lj=0
c     parameters for REALIGN arrB[i][j] WITH arrA[kr1i * i + lri][kr2j * j + lrj]                                                 
      integer, parameter ::  kr1i=1,kr2i=0,lri=4,kr1j=0,kr2j=1,lrj=4
      integer, allocatable :: A2(:,:),B2(:,:)
      character(10) ::  tname = 'realign223'
               
!dvm$ distribute A2(BLOCK,BLOCK)   
!dvm$ ALIGN B2(i,*) WITH A2(*,k1j * i + lj)
!dvm$ DYNAMIC B2

      allocate (A2(AN1,AN2),B2(BN1,BN2))
       
      B2 = 0     

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), private(ib,jb,k)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j
             do k=1,BN2
              if (
     *          ((j-lj) .eq.(((j-lj)/k1j) *k1j)) .and.
     *          (((j-lj)/k1j) .gt. 0)  .and.
     *          (((j-lj)/k1j) .le. BN1)
     *          )  then 
                ib = ((j-lj)/k1j)
                jb = k 
!               B2(ib,jb) = B2(ib,jb) + ib*NL+jb
                B2(ib,jb) = ib*NL+jb+5
              endif 
             enddo
          enddo 
      enddo
!dvm$ end region

!dvm$ REALIGN B2(i,j) WITH A2(kr1i * i + lri,kr2j * j + lrj)

!dvm$ actual(erria,errib)
!dvm$ region in(A2,B2), local(A2,B2)
!dvm$ parallel (i,j) on B2(i,j), private(ia,ja),
!dvm$*       reduction(min(erria),min(errib))
      do i=1,BN1
          do j=1,BN2
            if (B2(i,j) /= (i*NL+j+5)) then     
               errib = min(errib,i*NL/10+j)
            endif 
            ia=kr1i * i + lri
            ja=kr2j * j + lrj
            if (A2(ia,ja) /= (ia*NL+ja)) then     
               erria = min(erria,i*NL/10+j)
            endif 
          enddo 
      enddo
!dvm$ end region
     
!dvm$ get_actual(erria,errib)
      if ((erria == ER) .and. (errib == ER)) then     
          call ansyes(tname)
      else
          call ansno(tname)
!         print *,erria, errib
      endif 

      deallocate (B2,A2)

      end

C ----------------------------------------------------realign224
c 224 ALIGN arrB[*][*] WITH arrA[*][1]  REALIGN arrB[i][j] WITH arrA[i+4][j+4] 
      subroutine realign224
      integer, parameter :: AN1=10,AN2=10,BN1=4,BN2=4,NL=1000,ER=10000
      integer :: erria=ER, errib=ER
      integer :: i,j,ia,ja,ib,jb
c     parameters for ALIGN arrB[*][*] WITH arrA[*][lj]                                                 
      integer, parameter ::  k1i=0,k2i=0,li=0,k1j=0,k2j=0,lj=1
c     parameters for REALIGN arrB[i][j] WITH arrA[kr1i * i + lri][kr2j * j + lrj]                                                 
      integer, parameter ::  kr1i=1,kr2i=0,lri=4,kr1j=0,kr2j=1,lrj=4
      integer, allocatable :: A2(:,:),B2(:,:)
      character(10) ::  tname = 'realign224'
               
!dvm$ distribute A2(BLOCK,BLOCK)   
!dvm$ ALIGN B2(*,*) WITH A2(*,lj)
!dvm$ DYNAMIC B2

      allocate (A2(AN1,AN2),B2(BN1,BN2))

      B2 = 0
 
!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), private(ib,jb,k,n)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = i*NL+j+3
             if (j == (lj)) then
                do k=1,BN1
                    do n=1,BN2
                         ib = k
                         jb = n 
!                        B2(ib,jb) = B2(ib,jb) + (ib*NL+jb)*2
                         B2(ib,jb) = (ib*NL+jb)*2
                    enddo
                enddo
            endif
          enddo 
       enddo
!dvm$ end region

!dvm$ REALIGN B2(i,j) WITH A2(kr1i * i + lri,kr2j * j + lrj)

!dvm$ actual(erria,errib)
!dvm$ region
!dvm$ parallel (i,j) on B2(i,j), private(ia,ja),
!dvm$*       reduction(min(erria),min(errib))
      do i=1,BN1
          do j=1,BN2
            if (B2(i,j) /= (i*NL+j)*2) then     
               errib = min(errib,i*NL/10+j)
            endif 
            ia=kr1i * i + lri
            ja=kr2j * j + lrj
            if (A2(ia,ja) /= (ia*NL+ja+3)) then     
               erria = min(erria,i*NL/10+j)
            endif 
          enddo 
      enddo
!dvm$ end region
     
!dvm$ get_actual(erria,errib)
      if ((erria == ER) .and. (errib == ER)) then     
          call ansyes(tname)
      else
          call ansno(tname)
!         print *,erria, errib
      endif 

      deallocate (B2,A2)

      end

C ----------------------------------------------------realign225
c 225	ALIGN arrB[i][j] WITH arrA[i][j] REALIGN arrB[*][*] WITH arrA[*][2]	
      subroutine realign225
      integer, parameter :: AN1=10,AN2=10,BN1=4,BN2=4,NL=1000,ER=10000
      integer :: erria=ER, errib=ER
      integer :: i,j,ia,ja,ib,jb
c     parameters for ALIGN arrB[i][j] WITH arrA[k1i * i + li][k2j * j + lj]                                                 
      integer, parameter ::  k1i=1,k2i=0,li=0,k1j=0,k2j=1,lj=0
c     parameters for REALIGN arrB[*][*] WITH arrA[*][lrj]                                                 
      integer, parameter ::  kr1i=0,kr2i=0,lri=0,kr1j=0,kr2j=0,lrj=2
      integer, allocatable :: A2(:,:),B2(:,:)
      character(10) ::  tname = 'realign225'
               
!dvm$ distribute A2(BLOCK,BLOCK)   
!dvm$ ALIGN B2(i,j) WITH A2(k1i * i + li,k2j * j + lj)
!dvm$ DYNAMIC B2

      allocate (A2(AN1,AN2),B2(BN1,BN2))
       
!dvm$ region
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) = 0     
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
!dvm$ end region

!dvm$ REALIGN B2(*,*) WITH A2(*,lrj)

!dvm$ actual(errib)
!dvm$ region
!dvm$ parallel (i,j) on B2(i,j), reduction( min( errib ) )
      do i=1,BN1
          do j=1,BN2
            if (B2(i,j) /= (i*NL+j)) then     
               errib = min(errib,i*NL/10+j)
            endif 
          enddo 
      enddo
!dvm$ end region
  
!dvm$ get_actual(errib)
      if (errib == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (B2,A2)

      end
C ----------------------------------------------------realign226
c 226 ALIGN arrB[i][j] WITH arrA[i][j] REALIGN arrB[i][j] WITH arrA[2*j+1][3*i-2]	
      subroutine realign226
      integer, parameter :: AN1=16,AN2=18,BN1=6,BN2=4,NL=1000,ER=10000
      integer :: erria=ER, errib=ER
      integer :: i,j,ia,ja,ib,jb
c     parameters for ALIGN arrB[i][j] WITH arrA[k1i * i + li][k2j * j + lj]                                                 
      integer, parameter ::  k1i=1,li=0,k2j=1,lj=0
c     parameters for REALIGN arrB[i][j] WITH arrA[kr1i * i + lri][kr2j * j + lrj]                                                 
      integer, parameter ::  kr1i=3,lri=-2,kr2j=2,lrj=1
      integer, allocatable :: A2(:,:),B2(:,:)
      character(10) ::  tname = 'realign226'
               
!dvm$ distribute A2(BLOCK,BLOCK)   
!dvm$ ALIGN B2(i,j) WITH A2(k1i * i + li,k2j * j + lj)
!dvm$ DYNAMIC B2

      allocate (A2(AN1,AN2),B2(BN1,BN2))
       
!dvm$ region
!dvm$ parallel (i,j) on B2(i,j)
      do i=1,BN1
          do j=1,BN2
            B2(i,j) = 0     
          enddo 
      enddo

!dvm$ parallel (i,j) on A2(i,j), private(ib,jb)
      do i=1,AN1
          do j=1,AN2
             A2(i,j) = (i*NL+j) * 3
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
!dvm$ end region

!dvm$ REALIGN B2(i,j) WITH A2(kr2j * j + lrj,kr1i * i + lri)

!dvm$ actual(erria,errib)
!dvm$ region inlocal(A2,B2)
!dvm$ parallel (i,j) on B2(i,j), private(ia,ja),
!dvm$*       reduction(min(erria),min(errib))
      do i=1,BN1
          do j=1,BN2
            if (B2(i,j) /= (i*NL+j)) then     
               errib = min(errib,i*NL/10+j)
            endif 
            ia=kr2j * j + lrj
            ja=kr1i * i + lri
            if (A2(ia,ja) /= (ia*NL+ja)*3) then     
               erria = min(erria,i*NL/10+j)
            endif 
          enddo 
      enddo 
!dvm$ end region

!dvm$ get_actual(erria,errib)
      if ((erria == ER) .and. (errib == ER)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (B2,A2)

      end

C ---------------------------------------------------
      subroutine ansyes(name)
      character(*) name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character(*) name
      print *,name,'  -  ***error'
      end
   