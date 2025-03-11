      program DISTR4

c    TESTING distr CLAUSE .       

      print *,'===START OF distr4========================'
C -------------------------------------------------
c 44  DISTRIBUTE arrA4[*][*][*][*] REDISTRIBUTE arrA4[BLOCK][BLOCK][BLOCK][BLOCK]
      call distr44
C -------------------------------------------------
c 45  DISTRIBUTE arrA4[BLOCK][BLOCK][BLOCK][BLOCK] REDISTRIBUTE arrA4[*][*][*][*]
      call distr45
C -------------------------------------------------
c 46  DISTRIBUTE arrA4[*][*][*][*] REDISTRIBUTE arrA4[BLOCK][BLOCK][BLOCK][BLOCK]
c     small array
      call distr46
C -------------------------------------------------
c 47  DISTRIBUTE arrA4[BLOCK][BLOCK][BLOCK][BLOCK] REDISTRIBUTE arrA4[*][*][*][*]
c     small array
      call distr47
C -------------------------------------------------
C
      print *,'=== END OF distr4 ========================= '    
      end


C ----------------------------------------------------distr44
c 44  DISTRIBUTE arrA4[*][*][*][*] REDISTRIBUTE arrA4[BLOCK][BLOCK][BLOCK][BLOCK]
      subroutine distr44
      integer, parameter :: AN1=8,AN2=8,AN3=8,AN4=8,NL=1000,ER=100000
      integer :: erri=ER,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb
      integer, allocatable :: A4(:,:,:,:)
      character(9), parameter :: tname = 'distr44'
               
!dvm$ distribute A4(*,*,*,*)   
!dvm$ dynamic A4

      allocate ( A4(AN1,AN2,AN3,AN4))     

c *dvm$ parallel (i,j,n,m) on A4(i,j,n,m)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                do m=1,AN4
                     A4(i,j,n,m) = i*NL/10+j*NL/100+n*NL/1000+m
                enddo
             enddo
          enddo
      enddo

!dvm$ redistribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    

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

C ----------------------------------------------------distr45
c 45  DISTRIBUTE arrA4[BLOCK][BLOCK][BLOCK][BLOCK] REDISTRIBUTE arrA4[*][*][*][*]
      subroutine distr45
      integer, parameter :: AN1=8,AN2=8,AN3=8,AN4=8,NL=1000,ER=100000
      integer :: erri=ER,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb
      integer, allocatable :: A4(:,:,:,:)
      character(9), parameter :: tname = 'distr45'
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)   
!dvm$ dynamic A4

      allocate ( A4(AN1,AN2,AN3,AN4))

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

C ----------------------------------------------------distr46
c 46  DISTRIBUTE arrA4[*][*][*][*] REDISTRIBUTE arrA4[BLOCK][BLOCK][BLOCK][BLOCK]
      subroutine distr46
      integer, parameter :: AN1=5,AN2=4,AN3=3,AN4=2,NL=1000,ER=100000
      integer :: erri=ER,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb
      integer, allocatable :: A4(:,:,:,:)
      character(9), parameter :: tname = 'distr46'
               
!dvm$ distribute A4(*,*,*,*)   
!dvm$ dynamic A4

      allocate ( A4(AN1,AN2,AN3,AN4))

c *dvm$ parallel (i,j,n,m) on A4(i,j,n,m)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                do m=1,AN4
                     A4(i,j,n,m) = i*NL/10+j*NL/100+n*NL/1000+m
                enddo
             enddo
          enddo
      enddo

!dvm$ redistribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    

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

C ----------------------------------------------------distr47
c 47  DISTRIBUTE arrA4[BLOCK][BLOCK][BLOCK][BLOCK] REDISTRIBUTE arrA4[*][*][*][*]
      subroutine distr47
      integer, parameter :: AN1=1,AN2=2,AN3=3,AN4=4,NL=1000,ER=100000
      integer :: erri=ER,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb
      integer, allocatable :: A4(:,:,:,:)
      character(9), parameter :: tname = 'distr47'
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)   
!dvm$ dynamic A4

      allocate ( A4(AN1,AN2,AN3,AN4))

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
C -------------------------------------------------

      subroutine ansyes(name)
      character*9 name
      print *,name,'  -  complete'
      end

      subroutine ansno(name)
      character*9 name
      print *,name,'  -  ***error'
      end
