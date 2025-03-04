      program PARALLELPLUS2

c    TESTING parallel CLAUSE .       
c    arrA2[*][ BLOCK] 
c    or arrA2[ BLOCK][*] 

      print *,'===START OF paralplus12======================'
C --------------------------------------------------
c 21    PARALLEL ON arrA[i][2*j]  stretching along j
      call parallel21
C --------------------------------------------------
c 22    PARALLEL ON arrA[i+4][j]  shift along i
      call parallel22
C --------------------------------------------------
c 23    PARALLEL ON arrA[-i+8][j] reverse on i
c      call parallel23
C --------------------------------------------------
c 24    PARALLEL ON arrA[i+4][j+4]  shift along i and j
      call parallel24
C --------------------------------------------------
c 25    PARALLEL ON arrA[i][2*j]  stretching along j
      call parallel21
C --------------------------------------------------
c 26    PARALLEL ON arrA[i+4][j]  shift along i
      call parallel22
C --------------------------------------------------
c 27    PARALLEL ON arrA[-i+8][j] reverse on i
c      call parallel23
C --------------------------------------------------
c 28    PARALLEL ON arrA[i+4][j+4]  shift along i and j
      call parallel24
C --------------------------------------------------
      print *,'=== END OF paralplus12======================== '
      end

C ----------------------------------------------------parallel21
c 21    PARALLEL ON arrA[i][2*j]  stretching along j
      subroutine parallel21
      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
c     parameters for PARALLEL arrA2[k1i * i + li][k2j * j + lj]                                                 
      integer, parameter :: k1i=1,k2i=0,li=0,k1j=0,k2j=2,lj=0

      character*11 tname
      integer, allocatable :: A2(:,:)
      integer erri,i,j,n,m,ia,ja,na,ma
               
!dvm$ distribute A2(*,BLOCK)    

      tname='paral+1221'
      allocate (A2(AN1,AN2))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2)
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo

!dvm$ parallel (i,j) on A2(k1i*i+li,k2j*j+lj), reduction (min (erri))
!dvm$* ,private(ia,ja)
      do i=1,((AN1-li)/k1i)
          do j=1,((AN2-lj)/k2j)
            ia=k1i * i + li
            ja=k2j * j + lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
               erri = min(erri,ia*NL+ja)
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
      deallocate (A2)

      end
C ----------------------------------------------------parallel22
c 22    PARALLEL ON arrA[i+4][j]  shift along i
      subroutine parallel22
      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
c     parameters for PARALLEL arrA2[k1i * i + li][k2j * j + lj]                                                 
      integer, parameter :: k1i=1,k2i=0,li=4,k1j=0,k2j=1,lj=0
      character*11 tname
      integer, allocatable :: A2(:,:)
      integer erri,i,j,n,m,ia,ja,na,ma
               
!dvm$ distribute A2(*,BLOCK)    

      tname='paral+1222'
      allocate (A2(AN1,AN2))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2)
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo

!dvm$ parallel (i,j) on A2(k1i*i+li,k2j*j+lj), reduction( min( erri ))
!dvm$* ,private(ia,ja)
      do i=1,((AN1-li)/k1i)
          do j=1,((AN2-lj)/k2j)
            ia=k1i * i + li
            ja=k2j * j + lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
               erri = min(erri,ia*NL+ja)
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
      deallocate (A2)

      end
C ----------------------------------------------------parallel23
c 23    PARALLEL ON arrA[-i+8][j] reverse on i
      subroutine parallel23
      integer, parameter :: AN1=7,AN2=8,NL=1000,ER=10000
c     parameters for PARALLEL arrA2[k1i * i + li][k2j * j + lj]                                                 
      integer, parameter :: k1i=-1,k2i=0,li=8,k1j=0,k2j=1,lj=0
      character*11 tname
      integer, allocatable :: A2(:,:)
      integer erri,i,j,n,m,ia,ja,na,ma
               
!dvm$ distribute A2(*,BLOCK)    

      tname='paral+1223'
      allocate (A2(AN1,AN2))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2)
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo

!dvm$ parallel (i,j) on A2(k1i*i+li,k2j*j+lj), reduction( min( erri ) )
!dvm$* ,private(ia,ja)
      do i=1,AN1
          do j=1,((AN2-lj)/k2j)
            ia=k1i * i + li
            ja=k2j * j + lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
               erri = min(erri,ia*NL+ja)
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
      deallocate (A2)

      end
C ----------------------------------------------------parallel24
c 24    PARALLEL ON arrA[i+4][j+4]  shift along i and j
      subroutine parallel24
      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
c     parameters for PARALLEL arrA2[k1i * i + li][k2j * j + lj]                                                 
      integer, parameter :: k1i=1,k2i=0,li=4,k1j=0,k2j=1,lj=4
      character*11 tname
      integer, allocatable :: A2(:,:)
      integer erri,i,j,n,m,ia,ja,na,ma
               
!dvm$ distribute A2(*,BLOCK)    

      tname='paral+1224'
      allocate (A2(AN1,AN2))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2)
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo

!dvm$ parallel (i,j) on A2(k1i*i+li,k2j*j+lj), reduction( min( erri ) )
!dvm$* ,private(ia,ja)
      do i=1,((AN1-li)/k1i)
          do j=1,((AN2-lj)/k2j)
            ia=k1i * i + li
            ja=k2j * j + lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
               erri = min(erri,ia*NL+ja)
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
      deallocate (A2)

      end
C ----------------------------------------------------parallel25
c 25    PARALLEL ON arrA[i][2*j]  stretching along j
      subroutine parallel25
      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
c     parameters for PARALLEL arrA2[k1i * i + li][k2j * j + lj]                                                 
      integer, parameter :: k1i=1,k2i=0,li=0,k1j=0,k2j=2,lj=0
      character*11 tname
      integer, allocatable :: A2(:,:)
      integer erri,i,j,n,m,ia,ja,na,ma
               
!dvm$ distribute A2(BLOCK,*)    

      tname='paral+1225'
      allocate (A2(AN1,AN2))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2)
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo

!dvm$ parallel (i,j) on A2(k1i*i+li,k2j*j+lj), reduction (min (erri))
!dvm$* ,private(ia,ja)
      do i=1,((AN1-li)/k1i)
          do j=1,((AN2-lj)/k2j)
            ia=k1i * i + li
            ja=k2j * j + lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
               erri = min(erri,ia*NL+ja)
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
      deallocate (A2)

      end
C ----------------------------------------------------parallel26
c 26    PARALLEL ON arrA[i+4][j]  shift along i
      subroutine parallel26
      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
c     parameters for PARALLEL arrA2[k1i * i + li][k2j * j + lj]                                                 
      integer, parameter :: k1i=1,k2i=0,li=4,k1j=0,k2j=1,lj=0
      character*11 tname
      integer, allocatable :: A2(:,:)
      integer erri,i,j,n,m,ia,ja,na,ma
               
!dvm$ distribute A2(BLOCK,*)    

      tname='paral+1226'
      allocate (A2(AN1,AN2))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2)
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo

!dvm$ parallel (i,j) on A2(k1i*i+li,k2j*j+lj), reduction( min( erri ))
!dvm$* ,private(ia,ja)
      do i=1,((AN1-li)/k1i)
          do j=1,((AN2-lj)/k2j)
            ia=k1i * i + li
            ja=k2j * j + lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
               erri = min(erri,ia*NL+ja)
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
      deallocate (A2)

      end
C ----------------------------------------------------parallel27
c 27    PARALLEL ON arrA[-i+8][j] reverse on i
      subroutine parallel27
      integer, parameter :: AN1=7,AN2=8,NL=1000,ER=10000
c     parameters for PARALLEL arrA2[k1i * i + li][k2j * j + lj]                                                 
      integer, parameter :: k1i=-1,k2i=0,li=8,k1j=0,k2j=1,lj=0
      character*11 tname
      integer, allocatable :: A2(:,:)
      integer erri,i,j,n,m,ia,ja,na,ma
               
!dvm$ distribute A2(BLOCK,*)    

      tname='paral+1227'
      allocate (A2(AN1,AN2))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2)
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo

!dvm$ parallel (i,j) on A2(k1i*i+li,k2j*j+lj), reduction( min( erri ) )
!dvm$* ,private(ia,ja)
      do i=1,AN1
          do j=1,((AN2-lj)/k2j)
            ia=k1i * i + li
            ja=k2j * j + lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
               erri = min(erri,ia*NL+ja)
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
      deallocate (A2)

      end
C ----------------------------------------------------parallel28
c 28    PARALLEL ON arrA[i+4][j+4]  shift along i and j
      subroutine parallel28
      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
c     parameters for PARALLEL arrA2[k1i * i + li][k2j * j + lj]                                                 
      integer, parameter :: k1i=1,k2i=0,li=4,k1j=0,k2j=1,lj=4
      character*11 tname
      integer, allocatable :: A2(:,:)
      integer erri,i,j,n,m,ia,ja,na,ma
               
!dvm$ distribute A2(BLOCK,*)    

      tname='paral+1228'
      allocate (A2(AN1,AN2))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A2)
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo

!dvm$ parallel (i,j) on A2(k1i*i+li,k2j*j+lj), reduction( min( erri ) )
!dvm$* ,private(ia,ja)
      do i=1,((AN1-li)/k1i)
          do j=1,((AN2-lj)/k2j)
            ia=k1i * i + li
            ja=k2j * j + lj
            if (A2(ia,ja) .eq.(ia*NL+ja)) then     
            else
               erri = min(erri,ia*NL+ja)
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
      deallocate (A2)
      end

C -------------------------------------------------

      subroutine ansyes(name)
      character*11 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*11 name
      print *,name,'  -  ***error'
      end

