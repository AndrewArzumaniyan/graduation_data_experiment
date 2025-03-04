      program DISTRM3

!    Testing DISTRIBUTE and REDISTRIBUTE directive       
!            use MULT_BLOCK distribution

      print *,'===START OF distrmult3========================'

C -------------------------------------------------
c 311 DISTRIBUTE arrA3[MULT_BLOCK][MULT_BLOCK][MULT_BLOCK]
c                REDISTRIBUTE [BLOCK][BLOCK][BLOCK]
      call distrm311
C -------------------------------------------------
c 312 DISTRIBUTE arrA3DISTRIBUTE [BLOCK][BLOCK][BLOCK]
c                REDISTRIBUTE  [MULT_BLOCK][MULT_BLOCK][MULT_BLOCK]
      call distrm312  
C -------------------------------------------------
c 313 DISTRIBUTE arrA2[_BLOCK][BLOCK][WGT_BLOCK] 
c                REDISTRIBUTE arrA3[BLOCK][MULT_BLOCK][BLOCK]
      call distrm313
C -------------------------------------------------
c 314 DISTRIBUTE arrA3[BLOCK][MULT_BLOCK][BLOCK] 
c                REDISTRIBUTE arrA3[MULT_BLOCK][BLOCK][MULT_BLOCK]
      call distrm314
C -------------------------------------------------
c 315  DISTRIBUTE arrA3[MULT_BLOCK][MULT_BLOCK][MULT_BLOCK]
c              REDISTRIBUTE [MULT_BLOCK][MULT_BLOCK][MULT_BLOCK] other m1,m2,m3 
      call distrm315
C -------------------------------------------------
c 316 DISTRIBUTE arrA3[MULT_BLOCK][MULT_BLOCK][MULT_BLOCK] 
c                REDISTRIBUTE arrA2[*][*][*]
      call distrm316
C -------------------------------------------------
c 317  DISTRIBUTE arrA3[*][*][*] 
c                REDISTRIBUTE arrA3[MULT_BLOCK][MULT_BLOCK][MULT_BLOCK]
      call distrm317
C -------------------------------------------------
c 318 DISTRIBUTE arrA3[MULT_BLOCK][*][MULT_BLOCK] 
c                REDISTRIBUTE arrA3[*][MULT_BLOCK][*]
      call distrm318
C -------------------------------------------------
c 319  DISTRIBUTE arrA3[MULT_BLOCK][MULT_BLOCK][*] 
c                REDISTRIBUTE arrA3[*][MULT_BLOCK][MULT_BLOCK]
      call distrm319
C -------------------------------------------------
c 43  DISTRIBUTE arrA4[MULT_BLOCK][*][MULT_BLOCK][*]
c                REDISTRIBUTE arrA4[[*][MULT_BLOCK][*][MULT_BLOCK]
      call distrm43
C -------------------------------------------------
C
      print *,'=== END OF distrmult3 ========================= '    

      end

C ----------------------------------------------------distrm311
c 311 DISTRIBUTE arrA3[MULT_BLOCK][MULT_BLOCK][MULT_BLOCK]
c                REDISTRIBUTE [BLOCK][BLOCK][BLOCK]

      subroutine distrm311

      integer, parameter :: AN1=14,AN2=12,AN3=10,NL=1000,ER=10000
      integer :: erri=ER,i,j,k
      integer, parameter :: m1 = 7, m2 = 3, m3 = 5
      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrm311 '

!dvm$ distribute A3(MULT_BLOCK(m1),MULT_BLOCK(m2),MULT_BLOCK(m3))   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

      A3 = 1 

!dvm$ actual (A3)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) + i*NL/10 + j*NL/100 + k    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A3(BLOCK,BLOCK,BLOCK)   

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k) + 1) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region
     
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end subroutine distrm311

C ----------------------------------------------------distrm312
c 312 DISTRIBUTE arrA3[BLOCK][BLOCK][BLOCK]
c                REDISTRIBUTE [MULT_BLOCK][MULT_BLOCK][MULT_BLOCK]

      subroutine distrm312

      integer, parameter :: AN1=15,AN2=15,AN3=25,NL=1000,ER=10000
      integer :: erri=ER,i,j,k
      integer, parameter :: m1 = 5, m2 = 5, m3 = 5 
      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrm312 '

!dvm$ distribute A3(BLOCK,BLOCK,BLOCK)   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = i*NL/10 + j*NL/100 + k    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A3(MULT_BLOCK(m1),MULT_BLOCK(m2),MULT_BLOCK(m3))   

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region
     
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end subroutine distrm312

C ----------------------------------------------------distrm313
c 313 DISTRIBUTE arrA2[MULT_BLOCK][BLOCK][MULT_BLOCK] 
c                REDISTRIBUTE arrA3[BLOCK][MULT_BLOCK][BLOCK]

      subroutine distrm313

      integer, parameter :: AN1=24,AN2=24,AN3=24,NL=1000,ER=10000
      integer :: erri=ER,i,j,k
      integer, parameter :: m1 = 2, m2 = 3, m3 = 4 
      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrm313 '

!dvm$ distribute A3(MULT_BLOCK(m1),BLOCK,MULT_BLOCK(m3))   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

      A3 = 3

!dvm$ actual (A3)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) + i*NL/10 + j*NL/100 + k    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A3(BLOCK,MULT_BLOCK(m2),BLOCK)   

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                A3(i,j,k) = A3(i,j,k) - 3    
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region
     
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end subroutine distrm313

C ----------------------------------------------------distrm314
c 314 DISTRIBUTE arrA3[BLOCK][MULT_BLOCK][BLOCK] 
c                REDISTRIBUTE arrA3[MULT_BLOCK][BLOCK][MULT_BLOCK]

      subroutine distrm314

      integer, parameter :: AN1=20,AN2=30,AN3=30,NL=1000,ER=10000
      integer :: erri=ER,i,j,k
      integer, parameter :: m1 = 5, m2 = 3, m3 = 3
      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrm314 '

!dvm$ distribute A3(BLOCK, MULT_BLOCK(m2),BLOCK)   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = i*NL/10 + j*NL/100 + k*2    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A3(MULT_BLOCK(m1),BLOCK,MULT_BLOCK(m3))   

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k*2)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region
     
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end subroutine distrm314

C ----------------------------------------------------distrm315
c 315  DISTRIBUTE arrA3[MULT_BLOCK][MULT_BLOCK][MULT_BLOCK]
c            REDISTRIBUTE [MULT_BLOCK][MULT_BLOCK][MULT_BLOCK] other m1,m2,m3

      subroutine distrm315

      integer, parameter :: AN1=16,AN2=16,AN3=16,NL=1000,ER=10000
      integer :: erri=ER,i,j,k
      integer, parameter :: m1 = 2, m2 = 4, m3 = 8 
      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrm315 '

!dvm$ distribute A3(MULT_BLOCK(m1),MULT_BLOCK(m2),MULT_BLOCK(m3))   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

      A3 = 5

!dvm$ actual (A3)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) + i*NL/10 + j*NL/100 + k    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute 
!dvm$* A3(MULT_BLOCK(m1),MULT_BLOCK(m2),MULT_BLOCK(m3))   

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k)+ 5) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region
     
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end subroutine distrm315

C ----------------------------------------------------distrm316
c 316 DISTRIBUTE arrA3[MULT_BLOCK][MULT_BLOCK][MULT_BLOCK] 
c                REDISTRIBUTE arrA2[*][*][*]

      subroutine distrm316

      integer, parameter :: AN1=12,AN2=12,AN3=48,NL=1000,ER=10000
      integer :: erri=ER,i,j,k
      integer, parameter :: m1 = 3, m2 = 2, m3 = 6 
      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrm316 '
               
!dvm$ distribute A3(MULT_BLOCK(m1),MULT_BLOCK(m2),MULT_BLOCK(m3))   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = i*NL/10 + j*NL/100 + k    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A3(*,*,*)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region
     
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end subroutine distrm316

C ----------------------------------------------------distrm317
c 317  DISTRIBUTE arrA3[*][*][*] 
c                REDISTRIBUTE arrA3[MULT_BLOCK][MULT_BLOCK][MULT_BLOCK]

      subroutine distrm317

      integer, parameter :: AN1= 10, AN2=35, AN3=10,NL=1000,ER=10000
      integer :: erri=ER,i,j,k
      integer, parameter :: m1 = 2, m2 = 5, m3 = 2 
      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrm317 '

!dvm$ distribute A3(*,*,*)    
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

      A3 = 7

!dvm$ actual (A3)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) + i*NL/10 + j*NL/100 + k    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute
!dvm$*    A3(MULT_BLOCK(m1),MULT_BLOCK(m2),MULT_BLOCK(m3))   

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                A3(i,j,k) = A3(i,j,k) - 7   
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region
     
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end subroutine distrm317

C ----------------------------------------------------distrm318
c 318 DISTRIBUTE arrA3[MULT_BLOCK][*][MULT_BLOCK] 
c                REDISTRIBUTE arrA3[*][MULT_BLOCK][*]

      subroutine distrm318

      integer, parameter :: AN1=11,AN2=14,AN3=24,NL=1000,ER=10000
      integer :: erri=ER,i,j,k
      integer, parameter :: m1 = 1, m2 = 2, m3 = 6 
      integer :: A3(AN1,AN2,AN3)   !static array
      character(10), parameter :: tname='distrm318 '
               
!dvm$ distribute A3(MULT_BLOCK(m1),*,MULT_BLOCK(m3))   
!dvm$ dynamic A3

      A3 = 8

!dvm$ actual (A3)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) + i*NL/10 + j*NL/100 + k    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A3(*,MULT_BLOCK(m2),*)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
                if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k) + 8) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region
     
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      end subroutine distrm318

C ----------------------------------------------------distrm319
c 319  DISTRIBUTE arrA3[MULT_BLOCK][MULT_BLOCK][*] 
c                REDISTRIBUTE arrA3[*][MULT_BLOCK][MULT_BLOCK]

      subroutine distrm319

      integer, parameter :: AN1= 30, AN2=12, AN3=30,NL=1000,ER=10000
      integer :: erri=ER,i,j,k
      integer, parameter :: m11 = 2, m21 = 2, m31 = 2 
      integer, parameter :: m12 = 5, m22 = 4, m32 = 10 
      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrm319 '

!dvm$ distribute :: A3
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!dvm$ redistribute A3(MULT_BLOCK(m11),MULT_BLOCK(m21),*)    

      A3 = -1

!dvm$ actual (A3)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k)
      do i=1,AN1
          do j=1,AN2
             do k=1,AN3
                A3(i,j,k) = A3(i,j,k) + i*NL/10 + j*NL/100 + k    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute
!dvm$*    A3(*,MULT_BLOCK(m21),MULT_BLOCK(m32))   

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,k) on A3(i,j,k), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do k=1,AN3
               A3(i,j,k) = A3(i,j,k) + 2
               if (A3(i,j,k) /= (i*NL/10 + j*NL/100 + k) + 1) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + k)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region
     
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end subroutine distrm319

C ----------------------------------------------------distrm43
c 43  DISTRIBUTE arrA4[MULT_BLOCK][*][MULT_BLOCK][*]
c                REDISTRIBUTE arrA4[[*][MULT_BLOCK][*][MULT_BLOCK]

      subroutine distrm43

      integer, parameter :: AN1=16,AN2=16,AN3=16,AN4=16,NL=1000,ER=100000
      integer, parameter :: m1 = 2, m2 = 4, m3 = 2, m4 = 4 
      integer :: erri=ER,i,j,n,m
      integer, allocatable ::  A4(:,:,:,:)
      character(10), parameter :: tname='distrm43  '
               
!dvm$ distribute
!dvm$*  A4(MULT_BLOCK(m1),*,MULT_BLOCK(m3),*)   
!dvm$ dynamic A4

      allocate (A4(AN1,AN2,AN3,AN4))

!dvm$ region
!dvm$ parallel (i,j,n,m) on A4(i,j,n,m)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                do m=1,AN4
                     A4(i,j,n,m) = i*NL/10+j*NL/100+n*NL/1000+m
                enddo
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A4(*,MULT_BLOCK(m2),*,MULT_BLOCK(m4))   

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
              do m=1,AN4
                if (A4(i,j,n,m) /= (i*NL/10+j*NL/100+n*NL/1000+m)) then     
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000+m)
                endif 
              enddo
            enddo
          enddo
      enddo
!dvm$ end region
 
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A4)

      end

C -------------------------------------------------

      subroutine ansyes(name)
      character(*) name
      print *,name,'  -  complete'
      end

      subroutine ansno(name)
      character(*) name
      print *,name,'  -  ***error'
      end
