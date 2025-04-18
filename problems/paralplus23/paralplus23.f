      program PARALLELPLUS23
c    TESTING parallel CLAUSE .       
c    arrA3[*][ BLOCK][BLOCK] 
c    or arrA3[ BLOCK][*][BLOCK] 

      print *, '===START OF paralplus23================='
C -------------------------------------------------
c 31     PARALLEL ON  arrA[i][2* j][k] stretching
       call paral31
C -------------------------------------------------
c 32     PARALLEL ON  arrA[i+2][ j][k]  shift
       call paral32
C -------------------------------------------------
c 33     PARALLEL ON  arrA[i][ j][-k+8]  reverse
c      call paral33
C -------------------------------------------------
c 34     PARALLEL ON  arrA[i][ j][2]  
c                                               compression             !!
       call paral34
C -------------------------------------------------
c 35     PARALLEL ON  arrA[][ j][ k] 
c                                                replication            
      call paral35
C -------------------------------------------------
c 36     PARALLEL ON  arrA[1][i][3] 
c                                               compression and replication            
      call paral36
C -------------------------------------------------
c 37     PARALLEL ON  arrA[i][2* j][k] stretching
       call paral37
C -------------------------------------------------
c 38     PARALLEL ON  arrA[i+2][ j][k]  shift
       call paral38
C -------------------------------------------------
c 39     PARALLEL ON  arrA[i][ j][-k+8]  reverse
c      call paral39
C -------------------------------------------------
c 310     PARALLEL ON  arrA[i][ j][2]  
c                                               compression             !!
       call paral310
C -------------------------------------------------
c 311     PARALLEL ON  arrA[][ j][ k] 
c                                                replication            
      call paral311
C -------------------------------------------------
c 312     PARALLEL ON  arrA[1][i][3] 
c                                               compression and replication            
      call paral312
C -------------------------------------------------
      print *, '=== END OF paralplus23================='
C
      end

C ----------------------------------------------------paral31
c 31 arrA4[*][BLOCK] [BLOCK] 
c       PARALLEL ON  arrA[i][2* j][k] stretching      

      subroutine paral31
      integer, parameter :: AN1=6,AN2=6,AN3=4
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[k1i*i+li][k2j*j+lj][k3n*n+ln]                                              
      integer, parameter :: k1i=1,k2i=0,k3i=0,li=0
      integer, parameter :: k1j=0,k2j=2,k3j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=1,ln=0
      character*11 tname
      integer, allocatable :: A3(:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
!dvm$ distribute A3(*,BLOCK,BLOCK)    

      tname='paral+2331'
      allocate (A3(AN1,AN2,AN3))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A3)
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                    A3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(k1i*i+li,k2j*j+lj,k3n*n+ln),
!dvm$*                                          reduction (min (erri))
!dvm$* , private(ia,ja,na)
      do i=1,((AN1-li)/k1i)
          do j=1,((AN2-lj)/k2j)
            do n=1,((AN3-ln)/k3n)
                ia=k1i * i + li
                ja=k2j * j + lj
                na=k3n * n + ln
                if (A3(ia,ja,na).eq.(ia*NL/10+ja*NL/100+na*NL/1000)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000)
                endif 
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
c           print *,A3  
      endif 
      deallocate (A3)

      end

C ----------------------------------------------------paral32
c 32     PARALLEL ON  arrA[i+2][ j][k]  shift

      subroutine paral32
      integer, parameter :: AN1=5,AN2=5,AN3=5
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[k1i*i+li][k2j*j+lj][k3n*n+ln]                                              
      integer, parameter :: k1i=1,k2i=0,k3i=0,li=2
      integer, parameter :: k1j=0,k2j=1,k3j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=1,ln=0
      character*11 tname
      integer, allocatable :: A3(:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A3(*,BLOCK,BLOCK)    

      tname='paral+2332'
      allocate (A3(AN1,AN2,AN3))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A3)
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                    A3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(k1i*i+li,k2j*j+lj,k3n*n+ln),
!dvm$*                                          reduction (min (erri))
!dvm$* , private(ia,ja,na)
      do i=1,((AN1-li)/k1i)
          do j=1,((AN2-lj)/k2j)
            do n=1,((AN3-ln)/k3n)
                ia=k1i * i + li
                ja=k2j * j + lj
                na=k3n * n + ln
                if (A3(ia,ja,na).eq.(ia*NL/10+ja*NL/100+na*NL/1000)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000)
                endif 
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
c           print *,A3  
      endif 
      deallocate (A3)

      end

C ----------------------------------------------------paral33
c 33     PARALLEL ON  arrA[i][ j][-k+8]  reverse

      subroutine paral33
      integer, parameter :: AN1=5,AN2=5,AN3=5
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[k1i*i+li][k2j*j+lj][k3n*n+ln]                                              
      integer, parameter :: k1i=1,k2i=0,k3i=0,li=2
      integer, parameter :: k1j=0,k2j=1,k3j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=-1,ln=6
      character*11 tname
      integer, allocatable :: A3(:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A3(*,BLOCK,BLOCK)    

      tname='paral+2333'
      allocate (A3(AN1,AN2,AN3))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A3)
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                    A3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(k1i*i+li,k2j*j+lj,k3n*n+ln),
!dvm$*                                          reduction (min (erri))
!dvm$* , private(ia,ja,na)
      do i=1,((AN1-li)/k1i)
          do j=1,((AN2-lj)/k2j)
            do n=1,((AN3-ln)/k3n)
                ia=k1i * i + li
                ja=k2j * j + lj
                na=k3n * n + ln
                if (A3(ia,ja,na).eq.(ia*NL/10+ja*NL/100+na*NL/1000)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000)
                endif 
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
c           print *,A3  
      endif 
      deallocate (A3)

      end

C ----------------------------------------------------paral34
c 34     PARALLEL ON  arrA[i][ j][2]  

      subroutine paral34
      integer, parameter :: AN1=6,AN2=6,AN3=6,BN1=3,BN2=3,BN3=3
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[k1i*i+li][k2j*j+lj][ln]                                              
      integer, parameter :: k1i=1,k2i=0,k3i=0,li=0
      integer, parameter :: k1j=0,k2j=1,k3j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=0,ln=2
      character*11 tname
      integer, allocatable :: A3(:,:,:),B3(:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A3(*,BLOCK,BLOCK)    
!dvm$ ALIGN B3(i,j,n) WITH A3(k1i*i+li,k2j*j+lj,ln)

      tname='paral+2334'
      allocate (A3(AN1,AN2,AN3), B3(BN1,BN2,BN3))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A3,B3)
!dvm$ parallel (i,j,n) on B3(i,j,n)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                    B3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                    A3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(k1i*i+li,k2j*j+lj,ln),
!dvm$*                                          reduction (min (erri))
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                if (B3(i,j,n).eq.(i*NL/10+j*NL/100+n*NL/1000)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000)
                endif 
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
      deallocate (B3,A3)

      end

C ----------------------------------------------------paral35
c 35     PARALLEL ON  arrA[][ j][ k] 

      subroutine paral35
      integer, parameter :: AN1=6,AN2=6,AN3=6,BN1=6,BN2=6,BN3=6
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[*][k2j*j+lj][k3n*n+ln]                                              
      integer, parameter :: k1i=0,k2i=0,k3i=0,li=0
      integer, parameter :: k1j=0,k2j=1,k3j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=1,ln=0
      character*11 tname
      integer, allocatable :: A3(:,:,:),B3(:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A3(*,BLOCK,BLOCK)    
!dvm$ ALIGN B3(i,j,n) WITH A3(*,k2j*j+lj,k3n*n+ln)

      tname='paral+2335'
      allocate (A3(AN1,AN2,AN3), B3(BN1,BN2,BN3))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A3,B3)
!dvm$ parallel (i,j,n) on B3(i,j,n)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                    B3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                    A3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(*,k2j*j+lj,k3n*n+ln),
!dvm$*                                          reduction (min (erri))
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                if (B3(i,j,n).eq.(i*NL/10+j*NL/100+n*NL/1000)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000)
                endif 
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
      deallocate (B3,A3)

      end

C ----------------------------------------------------paral36
c 36     PARALLEL ON  arrA[1][i][3] 

      subroutine paral36
      integer, parameter :: AN1=6,AN2=6,AN3=6,BN1=3,BN2=3,BN3=3
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[li][k2j*j+lj][ln]                                              
      integer, parameter :: k1i=0,k2i=0,k3i=0,li=1
      integer, parameter :: k1j=0,k2j=1,k3j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=0,ln=3
      character*11 tname
      integer, allocatable :: A3(:,:,:),B3(:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A3(*,BLOCK,BLOCK)    
!dvm$ ALIGN B3(*,j,*) WITH A3(li,k2j*j+lj,ln)

      tname='paral+2336'
      allocate (A3(AN1,AN2,AN3), B3(BN1,BN2,BN3))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region out(A3,B3)
!dvm$ parallel (i,j,n) on B3(i,j,n)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                    B3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                    A3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo
!dvm$ end region   
!dvm$ get_actual(A3,B3) 

      do i=1,BN1
            do n=1,BN3
!dvm$ parallel (j) on A3(li,k2j*j+lj,ln),
!dvm$*                                          reduction (min (erri))
          do j=1,BN2
                if (B3(i,j,n).eq.(i*NL/10+j*NL/100+n*NL/1000)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000)
                endif 
            enddo 
          enddo 
      enddo
     
      s=0
      cs=0    
      if ((erri .eq.ER) .and.
     *     (s .eq. cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
c           write (*,*) erri
c          print *,B3  
      endif 
      deallocate (B3,A3)

      end

C ----------------------------------------------------paral37
c 37 arrA4[BLOCK][BLOCK] [BLOCK] 
c       PARALLEL ON  arrA[i][2* j][k] stretching      

      subroutine paral37
      integer, parameter :: AN1=6,AN2=6,AN3=4
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[k1i*i+li][k2j*j+lj][k3n*n+ln]                                              
      integer, parameter :: k1i=1,k2i=0,k3i=0,li=0
      integer, parameter :: k1j=0,k2j=2,k3j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=1,ln=0
      character*11 tname
      integer, allocatable :: A3(:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A3(BLOCK,*,BLOCK)    

      tname='paral+2337'
      allocate (A3(AN1,AN2,AN3))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A3)
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                    A3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(k1i*i+li,k2j*j+lj,k3n*n+ln),
!dvm$* private (ia,ja,na), reduction (min (erri))

      do i=1,((AN1-li)/k1i)
          do j=1,((AN2-lj)/k2j)
            do n=1,((AN3-ln)/k3n)
                ia=k1i * i + li
                ja=k2j * j + lj
                na=k3n * n + ln
                if (A3(ia,ja,na).eq.(ia*NL/10+ja*NL/100+na*NL/1000)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000)
                endif 
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
c           print *,A3  
      endif 
      deallocate (A3)

      end

C ----------------------------------------------------paral38
c 38     PARALLEL ON  arrA[i+2][ j][k]  shift

      subroutine paral38
      integer, parameter :: AN1=5,AN2=5,AN3=5
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[k1i*i+li][k2j*j+lj][k3n*n+ln]                                              
      integer, parameter :: k1i=1,k2i=0,k3i=0,li=2
      integer, parameter :: k1j=0,k2j=1,k3j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=1,ln=0
      character*11 tname
      integer, allocatable :: A3(:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A3(BLOCK,*,BLOCK)    

      tname='paral+2338'
      allocate (A3(AN1,AN2,AN3))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A3)
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                    A3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(k1i*i+li,k2j*j+lj,k3n*n+ln),
!dvm$* reduction (min (erri))
!dvm$* , private(ia,ja,na)
      do i=1,((AN1-li)/k1i)
          do j=1,((AN2-lj)/k2j)
            do n=1,((AN3-ln)/k3n)
                ia=k1i * i + li
                ja=k2j * j + lj
                na=k3n * n + ln
                if (A3(ia,ja,na).eq.(ia*NL/10+ja*NL/100+na*NL/1000)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000)
                endif 
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
c           print *,A3  
      endif
      deallocate (A3)

      end

C ----------------------------------------------------paral39
c 39     PARALLEL ON  arrA[i][ j][-k+8]  reverse

      subroutine paral39
      integer, parameter :: AN1=5,AN2=5,AN3=5
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[k1i*i+li][k2j*j+lj][k3n*n+ln]                                              
      integer, parameter :: k1i=1,k2i=0,k3i=0,li=2
      integer, parameter :: k1j=0,k2j=1,k3j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=-1,ln=6
      character*11 tname
      integer, allocatable :: A3(:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A3(BLOCK,*,BLOCK)    

      tname='paral+2339'
      allocate (A3(AN1,AN2,AN3))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A3)
!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                    A3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(k1i*i+li,k2j*j+lj,k3n*n+ln),
!dvm$*                                          reduction (min (erri))
!dvm$* , private(ia,ja,na)
      do i=1,((AN1-li)/k1i)
          do j=1,((AN2-lj)/k2j)
            do n=1,((AN3-ln)/k3n)
                ia=k1i * i + li
                ja=k2j * j + lj
                na=k3n * n + ln
                if (A3(ia,ja,na).eq.(ia*NL/10+ja*NL/100+na*NL/1000)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000)
                endif 
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
c           print *,A3  
      endif 
      deallocate (A3)

      end

C ----------------------------------------------------paral310
c 310     PARALLEL ON  arrA[i][ j][2]  

      subroutine paral310
      integer, parameter :: AN1=6,AN2=6,AN3=6,BN1=3,BN2=3,BN3=3
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[k1i*i+li][k2j*j+lj][ln]                                              
      integer, parameter :: k1i=1,k2i=0,k3i=0,li=0
      integer, parameter :: k1j=0,k2j=1,k3j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=0,ln=2     
      character*11 tname
      integer, allocatable :: A3(:,:,:),B3(:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A3(BLOCK,*,BLOCK)    
!dvm$ ALIGN B3(i,j,n) WITH A3(k1i*i+li,k2j*j+lj,ln)

      tname='paral+23310'
      allocate (A3(AN1,AN2,AN3), B3(BN1,BN2,BN3))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A3,B3)
!dvm$ parallel (i,j,n) on B3(i,j,n)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                    B3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                    A3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(k1i*i+li,k2j*j+lj,ln),
!dvm$*                                          reduction (min (erri))
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                if (B3(i,j,n).eq.(i*NL/10+j*NL/100+n*NL/1000)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000)
                endif 
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
      deallocate (B3,A3)

      end

C ----------------------------------------------------paral311
c 311     PARALLEL ON  arrA[][ j][ k] 

      subroutine paral311
      integer, parameter :: AN1=6,AN2=6,AN3=6,BN1=6,BN2=6,BN3=6
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[*][k2j*j+lj][k3n*n+ln]                                              
      integer, parameter :: k1i=0,k2i=0,k3i=0,li=0
      integer, parameter :: k1j=0,k2j=1,k3j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=1,ln=0
      character*11 tname
      integer, allocatable :: A3(:,:,:),B3(:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A3(BLOCK,*,BLOCK)    
!dvm$ ALIGN B3(i,j,n) WITH A3(*,k2j*j+lj,k3n*n+ln)

      tname='paral+23311'
      allocate (A3(AN1,AN2,AN3), B3(BN1,BN2,BN3))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A3,B3)
!dvm$ parallel (i,j,n) on B3(i,j,n)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                    B3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                    A3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(*,k2j*j+lj,k3n*n+ln),
!dvm$*                                          reduction (min (erri))
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                if (B3(i,j,n).eq.(i*NL/10+j*NL/100+n*NL/1000)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000)
                endif 
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
      deallocate (B3,A3)

      end

C ----------------------------------------------------paral312
c 312     PARALLEL ON  arrA[1][i][3] 

      subroutine paral312
      integer, parameter :: AN1=6,AN2=6,AN3=6,BN1=3,BN2=3,BN3=3
      integer, parameter :: NL=10000,ER=100000
c     parameters for PARALLEL ON  arrA[li][k2j*j+lj][ln]                                              
      integer, parameter :: k1i=0,k2i=0,k3i=0,li=1
      integer, parameter :: k1j=0,k2j=1,k3j=0,lj=0
      integer, parameter :: k1n=0,k2n=0,k3n=0,ln=3
      character*11 tname
      integer, allocatable :: A3(:,:,:),B3(:,:,:)
      integer s,cs,erri,i,j,n,m,ia,ja,na,ma,ib,jb,nb,mb,Avalue,Bvalue
               
!dvm$ distribute A3(BLOCK,*,BLOCK)    
!dvm$ ALIGN B3(i,j,n) WITH A3(li,k2j*j+lj,ln)

      tname='paral+23312'
      allocate (A3(AN1,AN2,AN3), B3(BN1,BN2,BN3))
      erri= ER
      NNL=NL 

!dvm$ actual(erri)
!dvm$ region local(A3,B3)
!dvm$ parallel (i,j,n) on B3(i,j,n)
      do i=1,BN1
          do j=1,BN2
             do n=1,BN3
                    B3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(i,j,n)
      do i=1,AN1
          do j=1,AN2
             do n=1,AN3
                    A3(i,j,n) =i*NL/10+j*NL/100+n*NL/1000     
             enddo
          enddo
      enddo

!dvm$ parallel (i,j,n) on A3(li,k2j*j+lj,ln),
!dvm$*                                          reduction (min (erri))
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                if (B3(i,j,n).eq.(i*NL/10+j*NL/100+n*NL/1000)) 
     *          then     
                else
                    erri = min(erri,i*NL/10+j*NL/100+n*NL/1000)
                endif 
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
      deallocate (B3,A3)

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

