      program PARALLEL4

c    TESTING parallel CLAUSE .       

      print *,'===START OF parallel4======================'
C -------------------------------------------------
c 41     PARALLEL ON  arrA[i][2* j][k][3*l] stretching
       call paral41
C -------------------------------------------------
c 42     PARALLEL ON  arrA[i+2][ j][k][ l+3]  shift
       call paral42
C -------------------------------------------------
c 43     PARALLEL ON  arrA[i][ j][-k+8][- l+8]  reverse
c      call paral43
C -------------------------------------------------
c 44     PARALLEL ON  arrA[i][ j][2][ l]  
c                                               compression            !!
      call paral44
C -------------------------------------------------
c 45     PARALLEL ON  arrA[i][ j][ ][ k] 
c                                               replication            
      call paral45
C -------------------------------------------------
c 46     PARALLEL ON  arrA[i][ j][ ][3] 
c                                               compression and replication            
      call paral46
C -------------------------------------------------
C
      print *,'=== END OF parallel4 ====================== '
      end

C ----------------------------------------------------paral41
c 41 arrA4[BLOCK][BLOCK] [BLOCK] [BLOCK] 
c       PARALLEL ON  arrA[i][2* j][k][3*l] stretching      

      subroutine paral41
      integer, parameter :: AN1=6,AN2=6,AN3=6,AN4=6
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[k1i*i+li][k2j*j+lj][k3n*n+ln][k4m*m+lm]                                               
      integer, parameter :: k1i=1,k2i=0,k3i=0,k4i=0,li=0
      integer, parameter :: k1j=0,k2j=2,k3j=0,k4j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=1,k4n=0,ln=0
      integer, parameter :: k1m=0,k2m=0,k3m=0,k4m=3,lm=0    
      character*9 tname
      integer, allocatable :: A4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    

      tname='paral41'
      allocate (A4(AN1,AN2,AN3,AN4))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A4)
!dvm$ parallel (i,j,n,m) on A4(i,j,n,m)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                do m=1,AN4
                    A4(i,j,n,m) =i*NL/10+j*NL/100+n*NL/1000+m     
                enddo 
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n,m) on A4(k1i*i+li,k2j*j+lj,k3n*n+ln,k4m*m+lm),
!dvm$*                                          reduction (min (erri))
!dvm$* ,private(ia,ja,na,ma)
      do i=1,((AN1-li)/k1i)
          do j=1,((AN2-lj)/k2j)
            do n=1,((AN3-ln)/k3n)
              do m=1,((AN4-lm)/k4m)
                ia=k1i * i + li
                ja=k2j * j + lj
                na=k3n * n + ln
                ma=k4m * m + lm
                if (A4(ia,ja,na,ma).eq.
     *             (ia*NL/10+ja*NL/100+na*NL/1000+ma)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000+m)
                endif 
              enddo
            enddo
          enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(erri) 

      s=0
      cs=0		
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
           write (*,*) erri
c          print *,A4  
      endif 
      deallocate (A4)

      end

C ----------------------------------------------------paral42
c 42     PARALLEL ON  arrA[i+2][ j][k][ l+3]  shift

      subroutine paral42
      integer, parameter :: AN1=6,AN2=6,AN3=6,AN4=6
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[k1i*i+li][k2j*j+lj][k3n*n+ln][k4m*m+lm]                                               
      integer, parameter :: k1i=1,k2i=0,k3i=0,k4i=0,li=2
      integer, parameter :: k1j=0,k2j=1,k3j=0,k4j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=1,k4n=0,ln=0
      integer, parameter :: k1m=0,k2m=0,k3m=0,k4m=1,lm=3
      character*9 tname
      integer, allocatable :: A4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    

      tname='paral42'
      allocate (A4(AN1,AN2,AN3,AN4))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A4)
!dvm$ parallel (i,j,n,m) on A4(i,j,n,m)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                do m=1,AN4
                    A4(i,j,n,m) =i*NL/10+j*NL/100+n*NL/1000+m     
                enddo 
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n,m) on A4(k1i*i+li,k2j*j+lj,k3n*n+ln,k4m*m+lm),
!dvm$*                                          reduction (min (erri))
!dvm$* ,private(ia,ja,na,ma)
      do i=1,((AN1-li)/k1i)
          do j=1,((AN2-lj)/k2j)
            do n=1,((AN3-ln)/k3n)
              do m=1,((AN4-lm)/k4m)
                ia=k1i * i + li
                ja=k2j * j + lj
                na=k3n * n + ln
                ma=k4m * m + lm
                if (A4(ia,ja,na,ma).eq.
     *             (ia*NL/10+ja*NL/100+na*NL/1000+ma)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000+m)
                endif 
              enddo
            enddo
          enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(erri) 
     
      s=0
      cs=0		
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
           write (*,*) erri
c          print *,A4  
      endif 
      deallocate (A4)

      end

C ----------------------------------------------------paral43
c 43     PARALLEL ON  arrA[i][ j][-k+8][- l+8]  reverse

      subroutine paral43
      integer, parameter :: AN1=6,AN2=6,AN3=7,AN4=7
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[k1i*i+li][k2j*j+lj][k3n*n+ln][k4m*m+lm]                                               
      integer, parameter :: k1i=1,k2i=0,k3i=0,k4i=0,li=0
      integer, parameter :: k1j=0,k2j=1,k3j=0,k4j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=-1,k4n=0,ln=8
      integer, parameter :: k1m=0,k2m=0,k3m=0,k4m=-1,lm=8
      character*9 tname
      integer, allocatable :: A4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    

      tname='paral42'
      allocate (A4(AN1,AN2,AN3,AN4))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A4)
!dvm$ parallel (i,j,n,m) on A4(i,j,n,m)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                do m=1,AN4
                    A4(i,j,n,m) =i*NL/10+j*NL/100+n*NL/1000+m     
                enddo 
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n,m) on A4(k1i*i+li,k2j*j+lj,k3n*n+ln,k4m*m+lm),
!dvm$*                                          reduction (min (erri))
!dvm$* ,private(ia,ja,na,ma)
      do i=1,((AN1-li)/k1i)
          do j=1,((AN2-lj)/k2j)
            do n=1,((AN3-ln)/k3n)
              do m=1,((AN4-lm)/k4m)
                ia=k1i * i + li
                ja=k2j * j + lj
                na=k3n * n + ln
                ma=k4m * m + lm
                if (A4(ia,ja,na,ma).eq.
     *             (ia*NL/10+ja*NL/100+na*NL/1000+ma)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000+m)
                endif 
              enddo
            enddo
          enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(erri) 
     
      s=0
      cs=0		
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
           write (*,*) erri
c          print *,A4  
      endif 
      deallocate (A4)

      end

C ----------------------------------------------------paral44
c 44     PARALLEL ON  arrA[i][ j][2][ l]  

      subroutine paral44
      integer, parameter :: AN1=6,AN2=6,AN3=6,AN4=6
      integer, parameter :: BN1=3,BN2=3,BN3=3,BN4=3
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[k1i*i+li][k2j*j+lj][ln][k4m*m+lm]                                               
      integer, parameter :: k1i=1,k2i=0,k3i=0,k4i=0,li=0
      integer, parameter :: k1j=0,k2j=1,k3j=0,k4j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=0,k4n=0,ln=2
      integer, parameter :: k1m=0,k2m=0,k3m=0,k4m=1,lm=0
      character*9 tname
      integer, allocatable :: A4(:,:,:,:),B4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B4(i,j,n,m) WITH A4(k1i*i+li,k2j*j+lj,ln,k4m*m+lm)

      tname='paral44'
      allocate (A4(AN1,AN2,AN3,AN4),B4(BN1,BN2,BN3,BN4))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A4,B4)
!dvm$ parallel (i,j,n,m) on B4(i,j,n,m)
      do  i=1,BN1
          do j=1,BN2
             do n=1,BN3
                do m=1,BN4
                    B4(i,j,n,m) =i*NL/10+j*NL/100+n*NL/1000+m     
                enddo
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n,m) on A4(i,j,n,m)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                do m=1,AN4
                    A4(i,j,n,m) =i*NL/10+j*NL/100+n*NL/1000+m     
                enddo 
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n,m) on A4(k1i*i+li,k2j*j+lj,ln,k4m*m+lm),
!dvm$*                                          reduction (min (erri))
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
              do m=1,BN4
                if (B4(i,j,n,m).eq.(i*NL/10+j*NL/100+n*NL/1000+m)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000+m)
                endif 
              enddo
            enddo
          enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(erri) 
     
      s=0
      cs=0		
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
c           write (*,*) erri
c          print *,A4  
      endif 
      deallocate (B4,A4)

      end

C ----------------------------------------------------paral45
c 45     PARALLEL ON  arrA[i][ j][ ][ k] 

      subroutine paral45
      integer, parameter :: AN1=6,AN2=6,AN3=6,AN4=6
      integer, parameter :: BN1=3,BN2=3,BN3=3,BN4=3
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[k1i*i+li][k2j*j+lj][][k4m*m+lm]                                               
      integer, parameter :: k1i=1,k2i=0,k3i=0,k4i=0,li=0
      integer, parameter :: k1j=0,k2j=1,k3j=0,k4j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=0,k4n=0,ln=0
      integer, parameter :: k1m=0,k2m=0,k3m=0,k4m=1,lm=0
      character*9 tname
      integer, allocatable :: A4(:,:,:,:),B4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B4(i,j,n,m) WITH A4(k1i*i+li,k2j*j+lj,*,k4m*m+lm)

      tname='paral45'
      allocate (A4(AN1,AN2,AN3,AN4),B4(BN1,BN2,BN3,BN4))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A4,B4)
!dvm$ parallel (i,j,n,m) on B4(i,j,n,m)
      do  i=1,BN1
          do j=1,BN2
             do n=1,BN3
                do m=1,BN4
                    B4(i,j,n,m) =i*NL/10+j*NL/100+n*NL/1000+m     
                enddo
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n,m) on A4(i,j,n,m)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                do m=1,AN4
                    A4(i,j,n,m) =i*NL/10+j*NL/100+n*NL/1000+m     
                enddo 
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n,m) on A4(k1i*i+li,k2j*j+lj,*,k4m*m+lm),
!dvm$*                                          reduction (min (erri))
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
              do m=1,BN4
                if (B4(i,j,n,m).eq.(i*NL/10+j*NL/100+n*NL/1000+m)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000+m)
                endif 
              enddo
            enddo
          enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(erri) 
     
      s=0
      cs=0		
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
c           write (*,*) erri
c          print *,A4  
      endif 
      deallocate (B4,A4)

      end

C ----------------------------------------------------paral46
c 46     PARALLEL ON  arrA[i][ j][ ][3] 

      subroutine paral46
      integer, parameter :: AN1=6,AN2=6,AN3=6,AN4=6
      integer, parameter :: BN1=3,BN2=3,BN3=3,BN4=3
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[k1i*i+li][k2j*j+lj][*][lm]
      integer, parameter :: k1i=1,k2i=0,k3i=0,k4i=0,li=0
      integer, parameter :: k1j=0,k2j=1,k3j=0,k4j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=0,k4n=0,ln=0
      integer, parameter :: k1m=0,k2m=0,k3m=0,k4m=0,lm=3
      character*9 tname
      integer, allocatable :: A4(:,:,:,:),B4(:,:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A4(BLOCK,BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B4(i,j,n,m) WITH A4(k1i*i+li,k2j*j+lj,*,lm)

      tname='paral46'
      allocate (A4(AN1,AN2,AN3,AN4),B4(BN1,BN2,BN3,BN4))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A4,B4)
!dvm$ parallel (i,j,n,m) on B4(i,j,n,m)
      do  i=1,BN1
          do j=1,BN2
             do n=1,BN3
                do m=1,BN4
                    B4(i,j,n,m) =i*NL/10+j*NL/100+n*NL/1000+m     
                enddo
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n,m) on A4(i,j,n,m)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                do m=1,AN4
                    A4(i,j,n,m) =i*NL/10+j*NL/100+n*NL/1000+m     
                enddo 
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n,m) on A4(k1i*i+li,k2j*j+lj,*,lm),
!dvm$*                                          reduction (min (erri))
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
              do m=1,BN4
                if (B4(i,j,n,m).eq.(i*NL/10+j*NL/100+n*NL/1000+m)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000+m)
                endif 
              enddo
            enddo
          enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(erri) 
     
      s=0
      cs=0		
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
c           write (*,*) erri
c          print *,A4  
      endif 
      deallocate (B4,A4)

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
