      program DISTRW1

!    Testing DISTRIBUTE and REDISTRIBUTE directives       
!            WGT_BLOCK distribution

      print *,'===START OF distrwgt1========================'

C --------------------------------------------------
c 11  DISTRIBUTE arrA1[BLOCK]       REDISTRIBUTE arrA1[WGT_BLOCK] 
      call distrw11
C --------------------------------------------------
c 12  DISTRIBUTE arrA1[WGT_BLOCK]   REDISTRIBUTE arrA1[BLOCK] 
      call distrw12
C --------------------------------------------------
c 13  DISTRIBUTE arrA1[BLOCK]       REDISTRIBUTE arrA1[WGT_BLOCK] small array
      call distrw13
C --------------------------------------------------
c 14  DISTRIBUTE arrA1[WGT_BLOCK]   REDISTRIBUTE arrA1[BLOCK] small array
      call distrw14
C --------------------------------------------------
c 15  DISTRIBUTE arrA1[WGT_BLOCK]   REDISTRIBUTE arrA1[WGT_BLOCK] other weigts
      call distrw15
C --------------------------------------------------
c 16  DISTRIBUTE arrA1[WGT_BLOCK]   REDISTRIBUTE arrA1[*]
      call distrw16
C --------------------------------------------------
c 17  DISTRIBUTE arrA1[*]           REDISTRIBUTE arrA1[WGT_BLOCK] 
      call distrw17
C --------------------------------------------------
c 18  DISTRIBUTE arrA1[WGT_BLOCK]   REDISTRIBUTE arrA1[WGT_BLOCK] with zero weigts
      call distrw18
C --------------------------------------------------
c 181 DISTRIBUTE arrA1[WGT_BLOCK]   REDISTRIBUTE arrA1[BLOCK] with zero weigts
c                                   REDISTRIBUTE arrA1[WGT_BLOCK]
      call distrw181
C --------------------------------------------------
c 182 DISTRIBUTE arrA1[WGT_BLOCK]   REDISTRIBUTE arrA1[*] with zero weigts
c                                   REDISTRIBUTE arrA1[WGT_BLOCK]
      call distrw182
C --------------------------------------------------
c 21  DISTRIBUTE arrA2[WGT_BLOCK][*]   REDISTRIBUTE arrA2[*][WGT_BLOCK]
      call distrw21
C --------------------------------------------------
c 22  DISTRIBUTE arrA2[*][WGT_BLOCK]   REDISTRIBUTE arrA2[*][*]
      call distrw22
C --------------------------------------------------
c 23  DISTRIBUTE arrA2[*][*]           REDISTRIBUTE arrA2[*][WGT_BLOCK]
      call distrw23
C -------------------------------------------------
C
      print *,'=== END OF distrwgt1 ========================= '    

      end

C ----------------------------------------------------distrw11
c 11  DISTRIBUTE arrA1[BLOCK]       REDISTRIBUTE arrA1[WGT_BLOCK] 

      subroutine distrw11

      integer, parameter :: AN1=16,NL=1000,ER=10000
      integer :: erri=ER,i

      double precision, dimension(6) :: WB=(/1.0, 2., 2., 3.0, 1., 1./)     

      integer, allocatable :: A1(:)
      character(*), parameter :: tname='distrw11  '
               
!dvm$ distribute A1(BLOCK)    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = i     
      enddo
!dvm$ end region

!dvm$ redistribute A1(WGT_BLOCK(WB,6))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
          if (A1(i) /= i) then     
             erri = min(erri,i)
          endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A1)

      end subroutine distrw11

C ---------------------------------------------distrw12
c 12  DISTRIBUTE arrA1[WGT_BLOCK]   REDISTRIBUTE arrA1[BLOCK] 

      subroutine distrw12

      integer, parameter :: AN1=8,NL=1000,ER=10000
      integer :: erri=ER,i

      double precision, dimension(6) :: WB=(/1.0, 2., 2., 3.0, 1., 1./)     

      integer, allocatable :: A1(:)
      character(10), parameter :: tname='distrw12'
               
!dvm$ distribute A1(WGT_BLOCK(WB,6))    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = i ** 2     
      enddo
!dvm$ end region

!dvm$ redistribute A1(BLOCK)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min(erri) )
      do i=1,AN1
            if (A1(i) /= i ** 2) then     
               erri = min(erri,i)
            endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A1)

      end
C ----------------------------------------------------distrw13
c 13  DISTRIBUTE arrA1[BLOCK]       REDISTRIBUTE arrA1[WGT_BLOCK] small array

      subroutine distrw13

      integer, parameter :: AN1=5,NL=1000,ER=10000
      integer :: erri=ER,i

      double precision, dimension(6) :: WB=(/1.0, 2., 2., 3.0, 1., 1./)     

      integer, allocatable :: A1(:)
      character(10) :: tname='distrw13'
               
!dvm$ distribute A1(BLOCK)    
!dvm$ dynamic A1

      allocate (A1(AN1))

      A1 = 2

!dvm$ actual (A1)

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i) + i      
      enddo
!dvm$ end region

!dvm$ redistribute A1(WGT_BLOCK(WB,6))  
  
!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min(erri) )
      do i=1,AN1
            if (A1(i) /= i + 2) then     
               erri = min(erri,i)
            endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A1)

      end
C ---------------------------------------------distrw14
c 14  DISTRIBUTE arrA1[WGT_BLOCK]   REDISTRIBUTE arrA1[BLOCK] small array

      subroutine distrw14

      integer, parameter :: AN1=5,NL=1000,ER=10000
      integer :: erri=ER,i

      double precision, dimension(6) :: WB=(/1.0, 2., 2., 3.0, 1., 1./)     

      integer, allocatable :: A1(:)
      character(10) :: tname='distrw14'

!dvm$ distribute A1(WGT_BLOCK(WB,6))    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = i     
      enddo
!dvm$ end region

!dvm$ redistribute A1(BLOCK)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) /= i) then     
               erri = min(erri,i)
            endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A1)

      end

C ----------------------------------------------------distrw15
c 15  DISTRIBUTE arrA1[WGT_BLOCK]   REDISTRIBUTE arrA1[WGT_BLOCK] other weigts

      subroutine distrw15

      integer, parameter :: AN1=16,NL=1000,ER=10000
      integer :: erri=ER,i

      double precision, dimension(6) :: WB1=(/1.0, 2., 2., 3.0, 1., 1./)     
      double precision, dimension(6) :: WB2=(/2.0, 1., 2., 2.0, 2., 1./)     

      integer, allocatable :: A1(:)
      character(*), parameter :: tname='distrw15  '
               
!dvm$ distribute A1(WGT_BLOCK(WB1,6))    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = i * 3    
      enddo
!dvm$ end region

!dvm$ redistribute A1(WGT_BLOCK(WB2,6))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
          if (A1(i) /= i * 3) then     
             erri = min(erri,i)
          endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A1)

      end subroutine distrw15

C ----------------------------------------------------distrw16
c 16  DISTRIBUTE arrA1[WGT_BLOCK]   REDISTRIBUTE arrA1[*]

      subroutine distrw16

      integer, parameter :: AN1=8,NL=1000,ER=10000
      integer :: erri=ER,i

      double precision, dimension(6) :: WB=(/1.0, 2., 2., 3.0, 1., 1./)     

      integer, allocatable :: A1(:)
      character(*), parameter :: tname='distrw16  '
               
!dvm$ distribute A1(WGT_BLOCK(WB,6))    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = i + 5     
      enddo
!dvm$ end region

!dvm$ redistribute A1(*)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min(erri) )
      do i=1,AN1
            A1(i) = A1(i) - 5   
            if (A1(i) /= i) then     
               erri = min(erri,i)
            endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A1)

      end subroutine distrw16

C ---------------------------------------------distrw17
c 17  DISTRIBUTE arrA1[*]           REDISTRIBUTE arrA1[WGT_BLOCK] 

      subroutine distrw17

      integer, parameter :: AN1=28,NL=1000,ER=10000
      integer :: erri=ER,i

      double precision, dimension(6) :: WB=(/1.0, 2., 2., 3.0, 1., 1./)     

      integer, allocatable :: A1(:)
      character(10), parameter :: tname='distrw17'
               
!dvm$ distribute A1(*)    
!dvm$ dynamic A1

      allocate (A1(AN1))

      A1 = 6

!dvm$ actual (A1)

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i) - i     
      enddo
!dvm$ end region

!dvm$ redistribute A1(WGT_BLOCK(WB,6))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            if (A1(i) /= 6 - i) then     
               erri = min(erri,i)
            endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A1)

      end subroutine distrw17

C ----------------------------------------------------distrw18
c 18  DISTRIBUTE arrA1[WGT_BLOCK] REDISTRIBUTE arrA1[WGT_BLOCK] with zero weigts

      subroutine distrw18

      integer, parameter :: AN1=17,NL=1000,ER=10000
      integer :: erri=ER,i

      double precision, dimension(6):: WB1=(/1.0, 2., 2., 0., 1., 1./)     
      double precision, dimension(8):: WB2=(/0.,1.,0.2,2.,3.,1.,1.5,0./)     

      integer, allocatable :: A1(:)
      character(*), parameter :: tname='distrw18  '
               
!dvm$ distribute A1(WGT_BLOCK(WB1,6))    
!dvm$ dynamic A1

      allocate (A1(AN1))

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = i * 3    
      enddo
!dvm$ end region

!dvm$ redistribute A1(WGT_BLOCK(WB2,6))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
          if (A1(i) /= i * 3) then     
             erri = min(erri,i)
          endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
 
      deallocate (A1)

      end subroutine distrw18

C --------------------------------------------------distrw181
c 181 DISTRIBUTE arrA1[WGT_BLOCK]   REDISTRIBUTE arrA1[BLOCK] with zero weigts
c                                   REDISTRIBUTE arrA1[WGT_BLOCK]
      subroutine distrw181

      integer, parameter :: AN1=11,NL=1000,ER=10000
      integer :: erri=ER,i

      double precision, dimension(7) ::
     >         WB1=(/0., 2., 2., 0., 1., 1., 0./)     
      double precision, dimension(8) ::
     >         WB2=(/0., 1., 0., 2., 0., 3., 1.2, 1.5/)     

      integer, allocatable :: A1(:)
      character(10) :: tname='distrw181'
               
!dvm$ distribute A1(WGT_BLOCK(WB1,7))   
!dvm$ dynamic A1

      allocate (A1(AN1))

      A1 = 2

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i) + i     
      enddo
!dvm$ end region

!dvm$ redistribute A1(BLOCK)    

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i) * 2     
      enddo
!dvm$ end region

!dvm$ redistribute A1(WGT_BLOCK(WB2,8))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            A1(i) = A1(i) / 2       
            if (A1(i) /= (i+2)) then     
               erri = min(erri,i)
            endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A1)

      end subroutine distrw181

C --------------------------------------------------distrw182
c 182 DISTRIBUTE arrA1[WGT_BLOCK]   REDISTRIBUTE arrA1[*] with zero weigts
c                                   REDISTRIBUTE arrA1[WGT_BLOCK]
      subroutine distrw182

      integer, parameter :: AN1=8,NL=1000,ER=10000
      integer :: erri=ER,i

      double precision, dimension(7) ::
     >         WB1=(/0.2, 2., 0., 0., 0., 1., 0./)     
      double precision, dimension(8) ::
     >         WB2=(/0., 1.1, 0., 2.5, 0., 3.3, 2.2, 0./)     

      integer, allocatable :: A1(:)
      character(10) :: tname='distrw182'
               
!dvm$ distribute A1(WGT_BLOCK(WB1,7))   
!dvm$ dynamic A1

      allocate (A1(AN1))

      A1 = -5

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i) + i     
      enddo
!dvm$ end region

!dvm$ redistribute A1(*)    

!dvm$ region
!dvm$ parallel (i) on A1(i)
      do i=1,AN1
            A1(i) = A1(i) * 3     
      enddo
!dvm$ end region

!dvm$ redistribute A1(WGT_BLOCK(WB2,8))    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i) on A1(i), reduction( min( erri ) )
      do i=1,AN1
            A1(i) = A1(i) / 3       
            if (A1(i) /= (i-5)) then     
               erri = min(erri,i)
            endif 
      enddo
!dvm$ end region

!dvm$ get_actual (erri)

      if (erri == ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A1)

      end subroutine distrw182

C ----------------------------------------------------distrw21
c 21  DISTRIBUTE arrA2[WGT_BLOCK][*]   REDISTRIBUTE arrA2[*][WGT_BLOCK]

      subroutine distrw21

      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
      integer :: erri=ER,i

      double precision, dimension(6) :: WB=(/1.0, 2., 2., 3.0, 1., 1./)     

      integer, allocatable :: A2(:,:)
      character(10) :: tname='distrw21'
               
!dvm$ distribute A2(WGT_BLOCK(WB,6),*)   
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

!dvm$ redistribute A2(*,WGT_BLOCK(WB,6))    

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

      end subroutine distrw21

C ----------------------------------------------------distrw22
c 22  DISTRIBUTE arrA2[*][WGT_BLOCK]   REDISTRIBUTE arrA2[*][*]

      subroutine distrw22

      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
      integer :: erri=ER,i

      double precision, dimension(6) :: WB=(/1.0, 2., 2., 3.0, 1., 1./)     

      integer, allocatable :: A2(:,:)
      character(10) :: tname='distrw22'
               
!dvm$ distribute A2(*,WGT_BLOCK(WB,6))   
!dvm$ dynamic A2

      allocate (A2(AN1,AN2))

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j)
      do i=1,AN1
          do j=1,AN2
            A2(i,j) =i*NL+j + 10    
          enddo
      enddo
!dvm$ end region

!dvm$ redistribute A2(*,*)    

!dvm$ actual (erri)

!dvm$ region
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
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

      end subroutine distrw22

C ----------------------------------------------------distrw23
c 23  DISTRIBUTE arrA2[*][*]           REDISTRIBUTE arrA2[*][WGT_BLOCK]

      subroutine distrw23

      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
      integer :: erri=ER,i

      double precision, dimension(6) :: WB=(/1.0, 2., 2., 3.0, 1., 1./)     

      integer, allocatable :: A2(:,:)
      character(10) :: tname='distrw23'
               
!dvm$ distribute A2(*,*)   
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

!dvm$ redistribute A2(*,WGT_BLOCK(WB,6))    

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
