      program DISTR3

c    TESTING distr CLAUSE .       

      print *,'===START OF distr3========================'
C -------------------------------------------------
c 31  DISTRIBUTE  arrA3[BLOCK][BLOCK][ BLOCK] REDISTRIBUTE arrA3[*][*][*]
      call distr31
C -------------------------------------------------
c 43  DISTRIBUTE arrA4[BLOCK][*][BLOCK][BLOCK] REDISTRIBUTE arrA4[BLOCK][BLOCK][BLOCK][*]
      call distr43
C -------------------------------------------------
C
      print *,'=== END OF distr3 ========================= '    
      end


C ----------------------------------------------------distr31
c 31  DISTRIBUTE  arrA3[BLOCK][BLOCK][ BLOCK] REDISTRIBUTE arrA3[*][*][*]
      subroutine distr31
      integer, parameter :: AN1=8,AN2=8,AN3=8,NL=1000,ER=10000
      integer :: erri = ER,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb
      integer, allocatable :: A3(:,:,:)
      character(9) :: tname = 'distr31'
               
!dvm$ distribute A3(BLOCK,BLOCK,BLOCK)   
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

!dvm$ redistribute A3(*,*,*)    
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

C ----------------------------------------------------distr43
c 43  DISTRIBUTE arrA4[BLOCK][*][BLOCK][BLOCK] REDISTRIBUTE arrA4[BLOCK][BLOCK][BLOCK][*]
      subroutine distr43
      integer, parameter :: AN1=8,AN2=8,AN3=8,AN4=8,NL=1000,ER=100000
      integer :: erri=ER,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb
      integer, allocatable ::  A4(:,:,:,:)
      character(9), parameter :: tname = 'distr43'
               
!dvm$ distribute A4(BLOCK,*,BLOCK,BLOCK)   
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

!dvm$ redistribute A4(BLOCK,BLOCK,BLOCK,*)    
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
