      program DISTRW3

!    Testing DISTRIBUTE and REDISTRIBUTE directives       
!            WGT_BLOCK distribution

      print *,'===START OF distrwgt3========================'

C -------------------------------------------------
c 39 DISTRIBUTE arrA3[BLOCK][BLOCK][BLOCK]
c                REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK][WGT_BLOCK]
      call distrw39
C -------------------------------------------------
c 310 DISTRIBUTE arrA3[WGT_BLOCK][WGT_BLOCK][WGT_BLOCK]
c                REDISTRIBUTE [BLOCK][BLOCK][BLOCK]
      call distrw310
C -------------------------------------------------
c 311  DISTRIBUTE arrA3[WGT_BLOCK][WGT_BLOCK][WGT_BLOCK]
c                REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK][WGT_BLOCK] other weigths
      call distrw311
C -------------------------------------------------
c 312 DISTRIBUTE arrA2[WGT_BLOCK][BLOCK][WGT_BLOCK] 
c                REDISTRIBUTE arrA3[BLOCK][WGT_BLOCK][BLOCK]
      call distrw312
C -------------------------------------------------
c 313 DISTRIBUTE arrA3[BLOCK][WGT_BLOCK][BLOCK] 
c                REDISTRIBUTE arrA3[WGT_BLOCK][BLOCK][WGT_BLOCK]
      call distrw313
C -------------------------------------------------
c 314 DISTRIBUTE arrA3[WGT_BLOCK][WGT_BLOCK][WGT_BLOCK] 
c                REDISTRIBUTE arrA2[*][*][*]
      call distrw314
C -------------------------------------------------
c 315  DISTRIBUTE arrA3[*][*][*] 
c                REDISTRIBUTE arrA3[WGT_BLOCK][WGT_BLOCK][WGT_BLOCK]
      call distrw315
C -------------------------------------------------
c 316  DISTRIBUTE arrA3[WGT_BLOCK][WGT_BLOCK][WGT_BLOCK]   with zero weigths
c                REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK][WGT_BLOCK] 
      call distrw316
C -------------------------------------------------
c 317  DISTRIBUTE arrA3[WGT_BLOCK][BLOCK][WGT_BLOCK]       with zero weigths
c                REDISTRIBUTE [BLOCK][WGT_BLOCK][BLOCK]
c                REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK][BLOCK]
      call distrw317
C -------------------------------------------------
c 318  DISTRIBUTE arrA3[WGT_BLOCK][WGT_BLOCK][*]           with zero weigths
c                REDISTRIBUTE [*][WGT_BLOCK][*]
c                REDISTRIBUTE [WGT_BLOCK][*][WGT_BLOCK]
      call distrw318
C -------------------------------------------------
c 43  DISTRIBUTE arrA4[WGT_BLOCK][*][WGT_BLOCK][WGT_BLOCK]
c                REDISTRIBUTE arrA4[BLOCK][WGT_BLOCK][BLOCK][*]
      call distrw43
C -------------------------------------------------
C
      print *,'=== END OF distrwgt3 ========================= '    

      end

C ----------------------------------------------------distrw310
c 39 DISTRIBUTE arrA3[BLOCK][BLOCK][BLOCK]
c                REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK][WGT_BLOCK]

      subroutine distrw39

      integer, parameter :: AN1=16,AN2=16,AN3=16,NL=1000,ER=10000
      integer :: erri=ER,i,j,n,m

      double precision, dimension(6) ::
     >                  WB1=(/3.0,1.,2.,2.0, 2.5, 1.2/)     
      double precision, dimension(7) ::
     >                  WB2=(/1.,3.,4.0,1.,2.,2.,4./)     
      double precision, dimension(8) ::
     >                  WB3=(/5.0,1.,3.,6.0,2.,4.,3.,1./)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrw39  '

!dvm$ distribute A3(BLOCK,BLOCK,BLOCK)   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                A3(i,j,n) = i*NL/10 + j*NL/100 + n + 6   
             enddo
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute
!dvm$*    A3(WGT_BLOCK(WB1,6),WGT_BLOCK(WB2,7),WGT_BLOCK(WB3,8))   

!dvm$ actual (erri) 

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                if (A3(i,j,n) /= (i*NL/10 + j*NL/100 + n) +6 )
     >          then     
                    erri = min(erri,i*NL/10 + j*NL/100 + n)
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

      end subroutine distrw39

C ----------------------------------------------------distrw310
c 310 DISTRIBUTE arrA3[WGT_BLOCK][WGT_BLOCK][WGT_BLOCK]
c                REDISTRIBUTE [BLOCK][BLOCK][BLOCK]

      subroutine distrw310

      integer, parameter :: AN1=12,AN2=12,AN3=24,NL=1000,ER=10000
      integer :: erri=ER,i,j,n,m

      double precision, dimension(6) :: WB1=(/2.0,1.,1.,3.0, 2., 1./)     
      double precision, dimension(8):: WB2=(/1.0,1.,2.,2.0,1.,1.,2.,2./)     
      double precision, dimension(6) :: WB3=(/2.0,2.,2.,3.0, 1., 1./)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrw310 '

!dvm$ distribute A3(WGT_BLOCK(WB1,6),WGT_BLOCK(WB2,6),WGT_BLOCK(WB3,6))   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

      A3 = 2

!dvm$ actual (A3) 

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                A3(i,j,n) = A3(i,j,n) + i*NL/10 + j*NL/100 + n   
             enddo
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A3(BLOCK,BLOCK,BLOCK)   

!dvm$ actual (erri) 

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                if (A3(i,j,n) /= (i*NL/10 + j*NL/100 + n) + 2 )
     >          then     
                    erri = min(erri,i*NL/10 + j*NL/100 + n)
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

      end subroutine distrw310

C ----------------------------------------------------distrw311
c 311  DISTRIBUTE arrA3[WGT_BLOCK][WGT_BLOCK][WGT_BLOCK]
c                REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK][WGT_BLOCK] other weigths

      subroutine distrw311

      integer, parameter :: AN1=8,AN2=12,AN3=10,NL=1000,ER=10000
      integer :: erri=ER,i,j,n,m

      double precision,dimension(6):: WB1=(/2.0,1.,1.,3.0, 2.,1./)     
      double precision,dimension(8):: WB2=(/1.0,1.,2.,2.0,1.,1.,2.,2./)     
      double precision,dimension(6):: WB3=(/2.0,2.,2.,3.0, 1., 1./)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrw311 '

               
!dvm$ distribute A3(WGT_BLOCK(WB1,5),WGT_BLOCK(WB2,8),WGT_BLOCK(WB3,6))   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

      A3 = 5

!dvm$ actual(A3)

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                A3(i,j,n) = A3(i,j,n) + i*NL/10 + j*NL/100 + n    
             enddo
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute 
!dvm$* A3(WGT_BLOCK(WB3,6),WGT_BLOCK(WB1,6),WGT_BLOCK(WB2,6))   

!dvm$ actual (erri) 

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                if (A3(i,j,n) /= (i*NL/10 + j*NL/100 + n) + 5) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + n)
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

      end subroutine distrw311

C ----------------------------------------------------distrw312
c 312 DISTRIBUTE arrA2[WGT_BLOCK][BLOCK][WGT_BLOCK] 
c                REDISTRIBUTE arrA3[BLOCK][WGT_BLOCK][BLOCK]

      subroutine distrw312

      integer, parameter :: AN1=30,AN2=10,AN3=5,NL=1000,ER=10000
      integer :: erri=ER,i,j,n,m

      double precision, dimension(7) ::
     >            WB1=(/2.0,1.,1.,3.0, 2.,4., 1./)     
      double precision, dimension(8):: WB2=(/1.0,1.,2.,2.0,1.,1.,2.,2./)     
      double precision, dimension(6) :: WB3=(/2.0,2.,2.,3.0, 1., 1./)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrw312 '

               
!dvm$ distribute A3(WGT_BLOCK(WB1,7),BLOCK,WGT_BLOCK(WB3,6))   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                A3(i,j,n) = i*NL/10 + j*NL/100 + n + 10   
             enddo
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A3(BLOCK,WGT_BLOCK(WB2,8),BLOCK)   

!dvm$ actual (erri) 

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                if (A3(i,j,n) /= (i*NL/10 + j*NL/100 + n)+ 10) then
                    erri = min(erri,i*NL/10 + j*NL/100 + n)
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

      end subroutine distrw312

C ----------------------------------------------------distrw313
c 313 DISTRIBUTE arrA3[BLOCK][WGT_BLOCK][BLOCK] 
c                REDISTRIBUTE arrA3[WGT_BLOCK][BLOCK][WGT_BLOCK]

      subroutine distrw313

      integer, parameter :: AN1=8,AN2=8,AN3=8,NL=1000,ER=10000
      integer :: erri=ER,i,j,n,m

      double precision, dimension(7) ::
     >            WB1=(/2.0,1.,1.,3.0, 2., 1., 3.5/)     
      double precision, dimension(7) ::
     >            WB2=(/1.0,1.,2.,2.0,1.,1.,2./)     
      double precision, dimension(6) :: WB3=(/2.0,2.,2.,3.0, 1., 1./)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrw313 '

!dvm$ distribute A3(BLOCK, WGT_BLOCK(WB2,7),BLOCK)   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                A3(i,j,n) = i*NL/10 + j*NL/100 + n    
             enddo
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A3(WGT_BLOCK(WB1,7),BLOCK,WGT_BLOCK(WB3,6))   

!dvm$ actual (erri) 

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                if (A3(i,j,n) /= (i*NL/10 + j*NL/100 + n)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + n)
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

      end subroutine distrw313

C ----------------------------------------------------distrw314
c 314 DISTRIBUTE arrA3[WGT_BLOCK][WGT_BLOCK][WGT_BLOCK] 
c                REDISTRIBUTE arrA2[*][*][*]

      subroutine distrw314

      integer, parameter :: AN1=8,AN2=15,AN3=24,NL=1000,ER=10000
      integer :: erri=ER,i,j,n,m

      double precision, dimension(6)  :: WB1=(/2.0,1.,1.,3.0, 2., 1./)     
      double precision, dimension(10) :: 
     >                 WB2=(/1.0,1.,2.,2.0,1.,1.,2.,2., 4., 6./)     
      double precision, dimension(8) ::
     >                 WB3=(/2.0,2.,2.,3.0, 1., 1., 3., 2.6/)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrw314 '

!dvm$ distribute A3(WGT_BLOCK(WB1,6),WGT_BLOCK(WB2,10),WGT_BLOCK(WB3,8))   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

      A3 = 2

      !dvm$ actual (A3) 

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                A3(i,j,n) = i*NL/10 + j*NL/100 + n - A3(i,j,n)
             enddo
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A3(*,*,*)    

!dvm$ actual (erri) 

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                if (A3(i,j,n) /= (i*NL/10 + j*NL/100 + n) - 2) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + n)
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

      end subroutine distrw314

C ----------------------------------------------------distrw315
c 315  DISTRIBUTE arrA3[*][*][*] 
c                REDISTRIBUTE arrA3[WGT_BLOCK][WGT_BLOCK][WGT_BLOCK]

      subroutine distrw315

      integer, parameter :: AN1=12,AN2=10,AN3=8,NL=1000,ER=10000
      integer :: erri=ER,i,j,n,m

      double precision, dimension(6) :: WB1=(/2.0,1.,1.,3.0, 2., 1./)     
      double precision,dimension(8) :: WB2=(/1.0,1.,2.,2.0,1.,1.,2.,2./)     
      double precision, dimension(6) :: WB3=(/2.0,2.,2.,3.0, 1., 1./)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrw315 '

!dvm$ distribute A3(*,*,*)    
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                A3(i,j,n) = i*NL/10 + j*NL/100 + n    
             enddo
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute
!dvm$*    A3(WGT_BLOCK(WB3,6),WGT_BLOCK(WB2,8),WGT_BLOCK(WB1,6))   

!dvm$ actual (erri) 

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                if (A3(i,j,n) /= (i*NL/10 + j*NL/100 + n)) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + n)
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

      end subroutine distrw315

C ----------------------------------------------------distrw316
c 316  DISTRIBUTE arrA3[WGT_BLOCK][WGT_BLOCK][WGT_BLOCK]  with zero weigths
c                REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK][WGT_BLOCK] 

      subroutine distrw316

      integer, parameter :: AN1=8,AN2=12,AN3=10,NL=1000,ER=10000
      integer :: erri=ER,i,j,n,m

      double precision,dimension(7)::
     >           WB1=(/2.0,1.,0.,1.,3.0, 2.,0./)     
      double precision,dimension(8)::
     >           WB2=(/0.,4.,2.,3.,1.,1.,2.,0./)     
      double precision,dimension(8)::
     >           WB3=(/2.0,3.,2.,4.,0.,0.,1.,2./)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrw316 '

               
!dvm$ distribute A3(WGT_BLOCK(WB1,7),WGT_BLOCK(WB2,8),WGT_BLOCK(WB3,8))   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

      A3 = 2

!dvm$ actual(A3)

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                A3(i,j,n) = A3(i,j,n) + i*NL/10 + j*NL/100 + n    
             enddo
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute 
!dvm$* A3(WGT_BLOCK(WB3,6),WGT_BLOCK(WB1,6),WGT_BLOCK(WB2,6))   

!dvm$ actual (erri) 

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                if (A3(i,j,n) /= (i*NL/10 + j*NL/100 + n) + 2) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + n)
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

      end subroutine distrw316

C ----------------------------------------------------distrw317
c 317  DISTRIBUTE arrA3[WGT_BLOCK][BLOCK][WGT_BLOCK]       with zero weigths
c                REDISTRIBUTE [BLOCK][WGT_BLOCK][BLOCK]
c                REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK][BLOCK]
      subroutine distrw317

      integer, parameter :: AN1=12,AN2=10,AN3=8,NL=1000,ER=10000
      integer :: erri=ER,i,j,n,m

      double precision,dimension(7)::
     >           WB1=(/1.,0.5,0.,0.4, 2.,0.8, 0./)     
      double precision,dimension(8)::
     >           WB2=(/0.,4.,2.,3.,1.2,1.,0., 2.4/)     
      double precision,dimension(10)::
     >           WB3=(/2.0,3.,2.,4.,0.,0.,1.,2.,0.,2./)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrw317 '

               
!dvm$ distribute A3(WGT_BLOCK(WB1,7),BLOCK,WGT_BLOCK(WB3,10))   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!dvm$ actual(A3)

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                A3(i,j,n) = i*NL/10 + j*NL/100 + n    
             enddo
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute 
!dvm$* A3(BLOCK,WGT_BLOCK(WB2,8),BLOCK)   

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                A3(i,j,n) = A3(i,j,n) + 4  
             enddo
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A3(WGT_BLOCK(WB1,6),WGT_BLOCK(WB2,6),BLOCK)   

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                A3(i,j,n) = A3(i,j,n) - 1  
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ actual (erri) 

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                if (A3(i,j,n) /= (i*NL/10 + j*NL/100 + n) + 3) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + n)
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

      end subroutine distrw317

C ----------------------------------------------------distrw318
c 318  DISTRIBUTE arrA3[WGT_BLOCK][WGT_BLOCK][*]           with zero weigths
c                REDISTRIBUTE [*][WGT_BLOCK][*]
c                REDISTRIBUTE [WGT_BLOCK][*][WGT_BLOCK]
      subroutine distrw318

      integer, parameter :: AN1=22,AN2=12,AN3=15,NL=1000,ER=10000
      integer :: erri=ER,i,j,n,m

      double precision,dimension(5)::
     >           WB1=(/0.,1.5,0.7,0.,2./)     
      double precision,dimension(8)::
     >           WB2=(/2.0,4.2,0.,3.,2.2,3.,0.4, 2.4/)     
      double precision,dimension(7)::
     >           WB3=(/3.,2.,4.,0.,1.,2.,0./)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrw318 '

               
!dvm$ distribute A3(WGT_BLOCK(WB1,5),WGT_BLOCK(WB2,8),*)   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

      A3 = 6

!dvm$ actual(A3)

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                A3(i,j,n) = A3(i,j,n) + i*NL/10 + j*NL/100 + n    
             enddo
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute 
!dvm$* A3(*,WGT_BLOCK(WB2,8),*)   

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                A3(i,j,n) = A3(i,j,n) + 4  
             enddo
          enddo
      enddo
!dvm$ end region 

!dvm$ redistribute A3(WGT_BLOCK(WB2,6),*,WGT_BLOCK(WB3,6))   

!dvm$ actual (erri) 

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                if (A3(i,j,n) /= (i*NL/10 + j*NL/100 + n) + 10) then     
                    erri = min(erri,i*NL/10 + j*NL/100 + n)
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

      end subroutine distrw318

C ----------------------------------------------------distrw43
c 43  DISTRIBUTE arrA4[WGT_BLOCK][*][WGT_BLOCK][WGT_BLOCK]
c                REDISTRIBUTE arrA4[BLOCK][WGT_BLOCK][BLOCK][*]

      subroutine distrw43

      integer, parameter :: AN1=24,AN2=16,AN3=8,AN4=8,NL=1000,ER=100000
      integer :: erri=ER,i,j,n,m

      double precision, dimension(6) :: WB1=(/2.0,1.,1.,3.0, 2., 1./)     
      double precision,dimension(8):: WB2=(/1.0,1.,2.,2.0,1.,1.,2.,2./)     
      double precision, dimension(6) :: WB3=(/2.0,2.,2.,3.0, 1., 1./)     

      integer, allocatable ::  A4(:,:,:,:)
      character(10), parameter :: tname='distrw43  '
               
!dvm$ distribute
!dvm$*  A4(WGT_BLOCK(WB1,6),*,WGT_BLOCK(WB2,8),WGT_BLOCK(WB3,6))   
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

!dvm$ redistribute A4(BLOCK,WGT_BLOCK(WB3,6),BLOCK,*)   

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
