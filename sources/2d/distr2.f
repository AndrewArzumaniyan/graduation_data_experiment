      program DISTR2

c    TESTING distr CLAUSE .       

      print *,'===START OF distr2========================'
C -------------------------------------------------
c 24  DISTRIBUTE arrA2[BLOCK][BLOCK]  REDISTRIBUTE arrA2[*][*]
      call distr24
C -------------------------------------------------
c 32  DISTRIBUTE  arrA3[BLOCK][*][ BLOCK] REDISTRIBUTE arrA3[*][BLOCK][BLOCK]
      call distr32
C -------------------------------------------------
c 33  DISTRIBUTE  arrA3[BLOCK][*][ BLOCK] REDISTRIBUTE arrA3[*][BLOCK][*]
      call distr33
C -------------------------------------------------
c 41  DISTRIBUTE arrA4[*][*][BLOCK][BLOCK] REDISTRIBUTE arrA4[*][*][*][*]
      call distr41
C -------------------------------------------------
c 42  DISTRIBUTE arrA4[BLOCK][*][BLOCK][*] REDISTRIBUTE arrA4[*][BLOCK][BLOCK][*]
      call distr42
C -------------------------------------------------
C
C
      print *,'=== END OF distr2 ========================= '    
      end


C ----------------------------------------------------distr24
c 24  DISTRIBUTE arrA2[BLOCK][BLOCK]  REDISTRIBUTE arrA2[*][*]
      subroutine distr24
      integer, parameter :: AN1=8,AN2=8,NL=1000,ER=10000
      integer :: erri=ER,i,j,ia,ja,ib,jb
      integer, allocatable :: A2(:,:)
      character(9) :: tname = 'distr24'
              
!dvm$ distribute A2(BLOCK,BLOCK)   
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

!dvm$ actual(erri)
!dvm$ region 
!dvm$ parallel (i,j) on A2(i,j), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            if (A2(i,j) .eq.(i*NL+j)) then     
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

      deallocate (A2)

      end

C ----------------------------------------------------distr32
c 32  DISTRIBUTE  arrA3[BLOCK] [][ BLOCK] REDISTRIBUTE arrA3[] [BLOCK][BLOCK]
      subroutine distr32
      integer, parameter :: AN1=8,AN2=8,AN3=8,NL=1000,ER=10000
      integer :: erri=ER,i,j,n,ia,ja,na,ib,jb,nb
      integer, allocatable ::  A3(:,:,:)
      character(9) :: tname = 'distr32'
               
!dvm$ distribute A3(BLOCK,*,BLOCK)   
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

!dvm$ redistribute A3(*,BLOCK,BLOCK)    
!dvm$ actual(erri)

!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                if (A3(i,j,n) .eq.(i*NL/10 + j*NL/100 + n)) then     
                else
                    erri = min(erri,i*NL/10 + j*NL/100 + n)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region   
 
!dvm$ get_actual(erri) 
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end

C ----------------------------------------------------distr33
c 33  DISTRIBUTE  arrA3[BLOCK] [][ BLOCK] REDISTRIBUTE arrA3[] [BLOCK][BLOCK]
      subroutine distr33
      integer, parameter :: AN1=8,AN2=8,AN3=8,NL=1000,ER=10000
      integer :: erri=ER,i,j,n,ia,ja,na,ib,jb,nb
      integer, allocatable :: A3(:,:,:)
      character(9) :: tname = 'distr33'
               
!dvm$ distribute A3(BLOCK,*,BLOCK)   
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

!dvm$ redistribute A3(*,BLOCK,*)    

!dvm$ actual(erri)
!dvm$ region 
!dvm$ parallel (i,j,n) on A3(i,j,n), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                if (A3(i,j,n) .eq.(i*NL/10 + j*NL/100 + n)) then     
                else
                    erri = min(erri,i*NL/10 + j*NL/100 + n)
                endif 
            enddo
          enddo
      enddo
!dvm$ end region   
 
!dvm$ get_actual(erri) 
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A3)

      end

C ----------------------------------------------------distr41
c 41  DISTRIBUTE arrA4[*][*][BLOCK][BLOCK] REDISTRIBUTE arrA4[*][*][*][*]
      subroutine distr41
      integer, parameter :: AN1=8,AN2=8,AN3=8,AN4=8,NL=1000,ER=100000
      integer :: erri=ER,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb
      integer, allocatable :: A4(:,:,:,:)
      character(9) :: tname = 'distr41'
               
!dvm$ distribute A4(*,*,BLOCK,BLOCK)   
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

!dvm$ actual(erri)
!dvm$ region 
!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
              do m=1,AN4
                if (A4(i,j,n,m) .eq.(i*NL/10+j*NL/100+n*NL/1000+m)) then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000+m)
                endif 
              enddo
            enddo
          enddo
      enddo
!dvm$ end region   
 
!dvm$ get_actual(erri) 
      if (erri .eq.ER) then     
          call ansyes(tname)
          else
          call ansno(tname)
      endif 

      deallocate (A4)

      end


C ----------------------------------------------------distr42
c 42  DISTRIBUTE arrA4[BLOCK][*][BLOCK][*] REDISTRIBUTE arrA4[*][BLOCK][BLOCK][*]
      subroutine distr42
      integer, parameter :: AN1=8,AN2=8,AN3=8,AN4=8,NL=1000,ER=100000
      integer :: erri=ER,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb
      integer, allocatable :: A4(:,:,:,:)
      character(9) :: tname = 'distr42'
               
!dvm$ distribute A4(BLOCK,*,BLOCK,*)   
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

!dvm$ redistribute A4(*,BLOCK,BLOCK,*)    

!dvm$ actual(erri)
!dvm$ region 
!dvm$ parallel (i,j,n,m) on A4(i,j,n,m), reduction( min( erri ) )
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
              do m=1,AN4
                if (A4(i,j,n,m) .eq.(i*NL/10+j*NL/100+n*NL/1000+m)) then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000+m)
                endif 
              enddo
            enddo
          enddo
      enddo
!dvm$ end region   
 
!dvm$ get_actual(erri) 
      if (erri .eq.ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      deallocate (A4)

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
