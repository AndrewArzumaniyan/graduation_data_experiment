      program DISTRW2

!    Testing DISTRIBUTE and REDISTRIBUTE directives       
!            WGT_BLOCK distribution
       
      print *,'===START OF distrwgt2========================'

C -------------------------------------------------
c 24  DISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]  REDISTRIBUTE arrA2[*][*]
      call distrw24
C -------------------------------------------------
c 25  DISTRIBUTE arrA2[*][*]  REDISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]
      call distrw25
C -------------------------------------------------
c 26  DISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK] 
C                             REDISTRIBUTE arrA2[BLOCK][BLOCK]
      call distrw26
C -------------------------------------------------
c 27  DISTRIBUTE arrA2[BLOCK][BLOCK]
c                             REDISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]
      call distrw27
C -------------------------------------------------
c 28  DISTRIBUTE arrA2[WGT_BLOCK][BLOCK]
c                         REDISTRIBUTE arrA2[BLOCK][WGT_BLOCK]
      call distrw28
C -------------------------------------------------
c 29  DISTRIBUTE arrA2[BLOCK][WGT_BLOCK]
c                         REDISTRIBUTE arrA2[WGT_BLOCK][BLOCK]
      call distrw29
C -------------------------------------------------
c 210  DISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]    other weigths
c                      REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK]
      call distrw210
C -------------------------------------------------
c 211  DISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]   with zero weigths
c                      REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK]
      call distrw211
C -------------------------------------------------
c 212  DISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]   with zero weigths
c                      REDISTRIBUTE [BLOCK][WGT_BLOCK]
c                      REDISTRIBUTE [WGT_BLOCK][BLOCK]
      call distrw212
C -------------------------------------------------
c 213  DISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]   with zero weigths
c                      REDISTRIBUTE [*][*]
c                      REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK]
      call distrw213
C -------------------------------------------------
c 214  DISTRIBUTE arrA2[WGT_BLOCK][*]           with zero weigths
c                      REDISTRIBUTE [*][WGT_BLOCK]
c                      REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK]
      call distrw214
C -------------------------------------------------
c 32  DISTRIBUTE  arrA3[WGT_BLOCK][WGT_BLOCK] [*] 
c                      REDISTRIBUTE arrA3[*][WGT_BLOCK][WGT_BLOCK]
      call distrw32
C -------------------------------------------------
c 33  DISTRIBUTE  arrA3[WGT_BLOCK][WGT_BLOCK][*]
c                      REDISTRIBUTE arrA3[WGT_BLOCK][*][BLOCK]
      call distrw33
C -------------------------------------------------
c 34  DISTRIBUTE  arrA3[WGT_BLOCK][*][WGT_BLOCK] 
c                      REDISTRIBUTE arrA3[WGT_BLOCK][WGT_BLOCK][*]
      call distrw34
C -------------------------------------------------
c 35  DISTRIBUTE  arrA3[WGT_BLOCK][WGT_BLOCK][*]
c                      REDISTRIBUTE arrA3[*][*][WGT_BLOCK]
      call distrw35
C -------------------------------------------------
c 36  DISTRIBUTE  arrA3[WGT_BLOCK][*][BLOCK]
c                      REDISTRIBUTE arrA3[BLOCK][*][WGT_BLOCK]
      call distrw36
C -------------------------------------------------
c 37  DISTRIBUTE  arrA3[WGT_BLOCK][BLOCK][*]
c                      REDISTRIBUTE arrA3[BLOCK][*][WGT_BLOCK]
      call distrw37
C -------------------------------------------------
c 38  DISTRIBUTE  arrA3[BLOCK][*][WGT_BLOCK]
c                      REDISTRIBUTE arrA3[*][WGT_BLOCK][BLOCK]
      call distrw38
C -------------------------------------------------
c 41  DISTRIBUTE arrA4[*][*][WGT_BLOCK][WGT_BLOCK]
c                     REDISTRIBUTE arrA4[*][*][*][*]
      call distrw41
C -------------------------------------------------
c 42  DISTRIBUTE arrA4[WGT_BLOCK][*][WGT_BLOCK][*]
c                     REDISTRIBUTE arrA4[*][WGT_BLOCK][WGT_BLOCK][*]
      call distrw42
C -------------------------------------------------
C
      print *,'=== END OF distrwgt2 ========================= '    

      end

C ----------------------------------------------------distrw24
c 24  DISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]  REDISTRIBUTE arrA2[*][*]

      subroutine distrw24

      integer, parameter :: AN1=12,AN2=12,NL=1000,ER=10000
      integer :: erri= ER,i,j

      double precision, dimension(6) ::
     >        WB1=(/2., 2., 3., 1., 5., 1./)     
      double precision, dimension(7) ::
     >        WB2=(/3., 2., 2., 3., 1., 1., 4./)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrw24  '
               
!dvm$ distribute A2(WGT_BLOCK(WB1,6),WGT_BLOCK(WB2,7))   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(*,*)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A2)

      end subroutine distrw24

C ----------------------------------------------------distrw25
c 25  DISTRIBUTE arrA2[*][*]  REDISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]

      subroutine distrw25

      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
      integer :: erri= ER,i,j

      double precision, dimension(5) :: WB1=(/1.0,2.,2.,3.0, 0./)     
      double precision, dimension(7) :: WB2=(/1.0,1.,2.,1.0, 1.,1.,1./)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrw25  '
               
!dvm$ distribute A2(*,*)   
!dvm$ dynamic A2

       allocate (A2(AN1,AN2))

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j+10     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(WGT_BLOCK(WB1,5), WGT_BLOCK(WB2,7))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min(erri) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)+10) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A2)

      end subroutine distrw25

C ----------------------------------------------------distrw26
c 26  DISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]  REDISTRIBUTE arrA2[BLOCK][BLOCK]

      subroutine distrw26

      integer, parameter :: AN1=12,AN2=12,NL=1000,ER=10000
      integer :: erri= ER,i,j

      double precision, dimension(6) :: WB=(/1.0,4.,1.,1.0, 2., 1./)     

      integer, allocatable :: A2(:,:)
      character(10), parameter :: tname='distrw26  '
               
!dvm$ distribute A2(WGT_BLOCK(WB,6),WGT_BLOCK(WB,6))   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

      A2 = 3

!dvm$ actual (A2)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =A2(i,j) + i*NL+j     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(BLOCK,BLOCK)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j) + 3) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A2)

      end subroutine distrw26

C ----------------------------------------------------distrw27
c 27  DISTRIBUTE arrA2[BLOCK][BLOCK]  REDISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]

      subroutine distrw27

      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
      integer :: erri= ER,i,j

      double precision, dimension(6) :: WB=(/2.0,1.,3.,2.0, 1., 1./)     

      integer, allocatable :: A2(:,:)
      character(10), parameter :: tname='distrw27'
               
!dvm$ distribute A2(BLOCK,BLOCK)   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =(i*NL+j) * 2     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(WGT_BLOCK(WB,6),WGT_BLOCK(WB,4))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) / 2 
            if (A2(i,j) /= (i*NL+j)) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A2)

      end subroutine distrw27

C ----------------------------------------------------distrw28
c 28  DISTRIBUTE arrA2[WGT_BLOCK][BLOCK]  REDISTRIBUTE arrA2[BLOCK][WGT_BLOCK]

      subroutine distrw28

      integer, parameter :: AN1=12,AN2=12,NL=1000,ER=10000
      integer :: erri= ER,i,j

      double precision, dimension(8) :: WB1=(/1.,2.,2.,3.,1.,1.,2.,4./)
      double precision, dimension(6) :: WB2=(/1.0,2.,2.,3.0, 1., 1./)

      integer, allocatable :: A2(:,:)
      character(10) :: tname='distrw28  '
               
!dvm$ distribute A2(WGT_BLOCK(WB1,8),BLOCK)   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(BLOCK,WGT_BLOCK(WB2,6))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A2)

      end subroutine distrw28

C ----------------------------------------------------distrw29
c 29  DISTRIBUTE arrA2[BLOCK][WGT_BLOCK]  REDISTRIBUTE arrA2[WGT_BLOCK][BLOCK]

      subroutine distrw29

      integer, parameter :: AN1=12,AN2=12,NL=1000,ER=10000
      integer :: erri= ER,i,j

      double precision, dimension(6) :: WB1=(/1.0,2.,2.,3.,3.,1./)
      double precision, dimension(6) :: WB2=(/1.0,2.,2.,3.0, 1., 1./)     

      integer, allocatable :: A2(:,:)
      character(10), parameter :: tname='distrw29'
               
!dvm$ distribute A2(BLOCK,WGT_BLOCK(WB1,6))   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))
      
      A2 = 8

!dvm$ actual (A2)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =A2(i,j) * (i*NL+j)     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(WGT_BLOCK(WB2,6),BLOCK)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)*8) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A2)

      end subroutine distrw29

C ----------------------------------------------------distrw210
c 210  DISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]
c                REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK] with other weigths

      subroutine distrw210

      integer, parameter :: AN1=10,AN2=8,NL=1000,ER=10000
      integer :: erri= ER,i,j

      double precision, dimension(8) ::
     >       WB1 = (/1.0, 2., 1., 1.0, 3.2, 2., 3., 1./)     
      double precision, dimension(6) ::
     >       WB2 = (/1.0, 1., 2., 1.0, 2., 1./)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrw210 '
               
!dvm$ distribute A2(WGT_BLOCK(WB1,8),WGT_BLOCK(WB2,6))   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(WGT_BLOCK(WB2,6),WGT_BLOCK(WB1,7))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min(erri) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region
                                                    
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A2)

      end subroutine distrw210

C ----------------------------------------------------distrw211
c 211  DISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]
c                REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK] with zero weigths

      subroutine distrw211

      integer, parameter :: AN1=8,AN2=17,NL=1000,ER=10000
      integer :: erri= ER,i,j

      double precision, dimension(6) ::
     >       WB1=(/0., 1.0, 2., 1., 1.0, 0./)     
      double precision, dimension(9) ::
     >       WB2=(/1.0, 1., 0., 2., 0., 0., 1.0, 2., 1./)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrw211 '
               
!dvm$ distribute A2(WGT_BLOCK(WB1,6),WGT_BLOCK(WB2,9))   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

      A2 = 1

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) + i*NL+j     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(WGT_BLOCK(WB2,7),WGT_BLOCK(WB1,5))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min(erri) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j+1)) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region
                                                    
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A2)

      end subroutine distrw211

C ----------------------------------------------------distrw212
c 212  DISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]   with zero weigths
c                      REDISTRIBUTE [BLOCK][WGT_BLOCK]
c                      REDISTRIBUTE [WGT_BLOCK][BLOCK]

      subroutine distrw212

      integer, parameter :: AN1=10,AN2=12,NL=1000,ER=10000
      integer :: erri= ER,i,j

      double precision, dimension(7) ::
     >       WB1=(/2., 0., 2., 1.0, 1.0, 1., 0./)     
      double precision, dimension(8) ::
     >       WB2=(/3.2, 2., 3.1, 2., 1.0, 4., 0., 1./)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrw212 '
               
!dvm$ distribute A2(WGT_BLOCK(WB1,7),WGT_BLOCK(WB2,8))   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

      A2 = 0

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(BLOCK, WGT_BLOCK(WB1,6))    

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) * 2     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(WGT_BLOCK(WB2,7), BLOCK)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min(erri) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j)*2) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region
                                                    
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A2)

      end subroutine distrw212

C ----------------------------------------------------distrw213
c 213  DISTRIBUTE arrA2[WGT_BLOCK][WGT_BLOCK]   with zero weigths
c                      REDISTRIBUTE [*][*]
c                      REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK]
      subroutine distrw213

      integer, parameter :: AN1=16,AN2=7,NL=1000,ER=10000
      integer :: erri= ER,i,j

      double precision, dimension(8) ::
     >       WB1=(/2., 4., 2., 1.5, 1., 0.5, 0., 3./)     
      double precision, dimension(8) ::
     >       WB2=(/0., 0., 3.1, 2., 1.0, 4., 0., 1./)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrw213 '
               
!dvm$ distribute A2(WGT_BLOCK(WB1,8),WGT_BLOCK(WB2,8))   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

      A2 = 4

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) + i*NL+j     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(*,*)    

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) * 3     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(WGT_BLOCK(WB2,6),WGT_BLOCK(WB1,6))   

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min(erri) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j+4)*3) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region
                                                    
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A2)

      end subroutine distrw213

C ----------------------------------------------------distrw214
c 214  DISTRIBUTE arrA2[WGT_BLOCK][*]           with zero weigths
c                      REDISTRIBUTE [*][WGT_BLOCK]
c                      REDISTRIBUTE [WGT_BLOCK][WGT_BLOCK]
      subroutine distrw214

      integer, parameter :: AN1=12,AN2=10,NL=1000,ER=10000
      integer :: erri= ER,i,j

      double precision, dimension(7) ::
     >       WB1=(/ 4., 0., 1.5, 1., 2., 0.5, 0./)     
      double precision, dimension(8) ::
     >       WB2=(/1.7, 0., 3.1, 2., 2.5, 4., 0., 1./)     

      integer, allocatable :: A2(:,:)
      character(*), parameter :: tname='distrw214 '
               
!dvm$ distribute A2(WGT_BLOCK(WB1,6),*)   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

      A2 = 0

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) + i*NL+j     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(*,WGT_BLOCK(WB2,8))    

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) = A2(i,j) + 3     
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(WGT_BLOCK(WB2,7),WGT_BLOCK(WB1,7))   

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min(erri) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) /= (i*NL+j+3)) then     
               erri = min(erri,i*NL/10+j)
            endif 
          enddo
      enddo
!dvm$ end region
                                                    
!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A2)

      end subroutine distrw214

C ----------------------------------------------------distrw32
c 32  DISTRIBUTE  arrA3[WGT_BLOCK][WGT_BLOCK][*]
c                 REDISTRIBUTE arrA3[*][WGT_BLOCK][WGT_BLOCK]

      subroutine distrw32

      integer, parameter :: AN1=16,AN2=12,AN3=8,NL=1000,ER=10000
      integer :: erri = ER,i,j,n

      double precision, dimension(7) :: WB1=(/1.,1.,2.,1.0,2.,2.,3.0/)
      double precision, dimension(8) :: WB2=(/1.,2.,2.,3.,2.,1.,1.,1./)

      integer, allocatable ::  A3(:,:,:)
      character(10), parameter :: tname='distrw32  '
               
!dvm$ distribute A3(WGT_BLOCK(WB1,7),WGT_BLOCK(WB2,8),*)   
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

!dvm$ redistribute A3(*,WGT_BLOCK(WB2,7),WGT_BLOCK(WB1,6))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,n) on A3(i,j,n), reduction( min(erri) )
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

      end subroutine distrw32

C ----------------------------------------------------distrw33
c 33  DISTRIBUTE  arrA3[WGT_BLOCK][WGT_BLOCK][*]
c                 REDISTRIBUTE arrA3[WGT_BLOCK][*][BLOCK]

      subroutine distrw33

      integer, parameter :: AN1=16,AN2=16,AN3=8,NL=1000,ER=10000
      integer :: erri = ER,i,j,n

      double precision, dimension(10) ::
     >                  WB=(/1.,2.,2.,3., 2., 4., 2., 1.,1., 1./)

      integer, allocatable :: A3(:,:,:)
      character(*), parameter :: tname='distrw33  '
               
!dvm$ distribute A3(WGT_BLOCK(WB,6),WGT_BLOCK(WB,8),*)   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))
      
      A3 = 5

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

!dvm$ redistribute A3(WGT_BLOCK(WB,10),*,WGT_BLOCK(WB,6))    

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

      end subroutine distrw33

C ----------------------------------------------------distrw34
c 34  DISTRIBUTE  arrA3[WGT_BLOCK][*][WGT_BLOCK]
c                 REDISTRIBUTE arrA3[WGT_BLOCK][WGT_BLOCK][*]

      subroutine distrw34

      integer, parameter :: AN1=8,AN2=8,AN3=8,NL=1000,ER=10000
      integer :: erri=ER,i,j,n

      double precision, dimension(8) ::
     >                   WB=(/1.0,2.,2.,3.,1.,2., 1., 1./)

      integer, allocatable ::  A3(:,:,:)
      character(10) :: tname='distrw34'
               
!dvm$ distribute A3(WGT_BLOCK(WB,6),*,WGT_BLOCK(WB,8))   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))

!dvm$ region
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                A3(i,j,n) = (i*NL/10 + j*NL/100 + n) * 7    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A3(WGT_BLOCK(WB,6),WGT_BLOCK(WB,8),*)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,n) on A3(i,j,n), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3

                if (A3(i,j,n) /= (i*NL/10 + j*NL/100 + n) * 7) then     
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

      end subroutine distrw34

C ----------------------------------------------------distrw35
c 35  DISTRIBUTE  arrA3[WGT_BLOCK][WGT_BLOCK][*]
c                         REDISTRIBUTE arrA3[*][*][WGT_BLOCK]

      subroutine distrw35

      integer, parameter :: AN1=8,AN2=8,AN3=8,NL=1000,ER=10000
      integer :: erri=ER,i,j,n

      double precision, dimension(6) :: WB1=(/1.0,2.,2.,3.0,1.5, 2.5/)     
      double precision, dimension(6) :: WB2=(/1.0,2.,2.,3.0, 1., 1./)     

      integer, allocatable :: A3(:,:,:)
      character(10), parameter :: tname='distrw35  '
               
!dvm$ distribute A3(WGT_BLOCK(WB1,6),BLOCK,*)   
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

!dvm$ redistribute A3(*,*,WGT_BLOCK(WB2,6))    

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

      end subroutine distrw35

C ----------------------------------------------------distrw36
c 36  DISTRIBUTE  arrA3[WGT_BLOCK][*][BLOCK]
c                         REDISTRIBUTE arrA3[BLOCK][*][WGT_BLOCK]

      subroutine distrw36

      integer, parameter :: AN1=8,AN2=8,AN3=8,NL=1000,ER=10000
      integer :: erri=ER,i,j,n

      double precision, dimension(6) :: WB=(/1.0,2.,2.,3.0, 1., 1./)     

      integer, allocatable ::  A3(:,:,:)
      character(10), parameter :: tname='distrw36  '
               
!dvm$ distribute A3(WGT_BLOCK(WB,6),*,BLOCK)   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))
     
      A3 = 2

!dvm$ actual (A3)

!dvm$ region
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                A3(i,j,n) = (i*NL/10 + j*NL/100 + n) + A3(i,j,n)    
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A3(BLOCK,*,WGT_BLOCK(WB,6))    

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
 
      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end subroutine distrw36

C ----------------------------------------------------distrw37
c 37  DISTRIBUTE  arrA3[WGT_BLOCK][BLOCK][*]
c                         REDISTRIBUTE arrA3[BLOCK][*][WGT_BLOCK]

      subroutine distrw37

      integer, parameter :: AN1=8,AN2=8,AN3=8,NL=1000,ER=10000
      integer :: erri=ER,i,j,n

      double precision,dimension(6) :: WB1=(/0.5, 1.,1.,2.,2.,3./)
      double precision,dimension(8) :: WB2=(/1.,2.,2.,3.,0.5,2.,1.,1./)

      integer, allocatable :: A3(:,:,:)
      character(*), parameter :: tname='distrw37  '
               
!dvm$ distribute A3(WGT_BLOCK(WB1,6),BLOCK,*)   
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

!dvm$ redistribute A3(BLOCK,*,WGT_BLOCK(WB2,8))    

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

      end subroutine distrw37

C ----------------------------------------------------distrw38
c 38  DISTRIBUTE  arrA3[BLOCK][*][WGT_BLOCK] REDISTRIBUTE arrA3[*][WGT_BLOCK][WGT_BLOCK]

      subroutine distrw38

      integer, parameter :: AN1=8,AN2=8,AN3=8,NL=1000,ER=10000
      integer :: erri=ER,i,j,n

      double precision, dimension(6) :: WB1=(/1.0,2.,2.,3.0, 4.,5./)
      double precision, dimension(6) :: WB2=(/1.0,2.,2.,3.0, 1., 1./)     

      integer, allocatable :: A3(:,:,:)
      character(10) :: tname='distrw38'
               
!dvm$ distribute A3(BLOCK, *, WGT_BLOCK(WB1,6))   
!dvm$ dynamic A3

      allocate (A3(AN1,AN2,AN3))
      
      A3 = 5

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

!dvm$ redistribute A3(*,WGT_BLOCK(WB2,6),WGT_BLOCK(WB1,6))    

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

      end subroutine distrw38

C ----------------------------------------------------distrw41
c 41  DISTRIBUTE arrA4[*][*][WGT_BLOCK][WGT_BLOCK]
c                REDISTRIBUTE arrA4[*][*][*][*]

      subroutine distrw41

      integer, parameter :: AN1=8,AN2=8,AN3=8,AN4=8,NL=1000,ER=100000
      integer :: erri=ER,i,j,n,m

      double precision, dimension(8) :: WB=(/1.,2.,2.,3.,1.,1.,2.,1./)

      integer, allocatable :: A4(:,:,:,:)
      character(10), parameter :: tname='distrw41  '
               
!dvm$ distribute A4(*,*,WGT_BLOCK(WB,6),WGT_BLOCK(WB,8))   
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

!dvm$ redistribute A4(*,*,*,*)    

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

      end subroutine distrw41

C ----------------------------------------------------distrw42
c 42  DISTRIBUTE arrA4[WGT_BLOCK][*][WGT_BLOCK][*] REDISTRIBUTE arrA4[*][WGT_BLOCK][WGT_BLOCK][*]

      subroutine distrw42

      integer, parameter :: AN1=8,AN2=8,AN3=8,AN4=8,NL=1000,ER=100000
      integer :: erri=ER,i,j,n,m

      double precision, dimension(6) :: WB1=(/1.0,2.,2.,3.0, 1., 1./)     

      integer, allocatable :: A4(:,:,:,:)
      character(10) :: tname='distrw42  '
               
!dvm$ distribute A4(WGT_BLOCK(wb1,6),*,WGT_BLOCK(wb1,6),*)   
!dvm$ dynamic A4

      allocate (A4(AN1,AN2,AN3,AN4))
    
      A4 = 3

!dvm$ actual (A4)

!dvm$ region
!dvm$ parallel (i,j,n,m) on A4(i,j,n,m)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                do m=1,AN4
                  A4(i,j,n,m) = A4(i,j,n,m) +
     >                          i*NL/10+j*NL/100+n*NL/1000+m
                enddo
             enddo
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A4(*,WGT_BLOCK(wb1,6),WGT_BLOCK(wb1,6),*)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
              do m=1,AN4
               if (A4(i,j,n,m) /= (i*NL/10+j*NL/100+n*NL/1000+m)+3)
     >         then     
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

      end subroutine distrw42

C -------------------------------------------------

      subroutine ansyes(name)
      character(*) name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character(*) name
      print *,name,'  -  ***error'
      end
