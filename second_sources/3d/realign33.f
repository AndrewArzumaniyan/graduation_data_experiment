      program REALIGN33

!    Testing  ALIGN and REALIGN directives       

      print *,'===START OF realign33========================'

! --------------------------------------------------
! 331 ALIGN arrB3(i,j,n)   WITH arrA3(i,j,n)    
!     REALIGN arrB3(i,j,n) WITH arrA3(i+1,j+2,n+3) 

      call realign331
! --------------------------------------------------
! 332 ALIGN arrB3(i,j,n)   WITH arrA3(i,j,n)    
!     REALIGN arrB3(i,j,n) WITH arrA3(2*i,3*j,5*n) 
 
      call realign332
! --------------------------------------------------
! 333 ALIGN arrB3(i,j,n)   WITH arrA3(i+2,j+4,n+3)    
!     REALIGN arrB3(i,j,n) WITH arrA3(2*i-1,2*n,j+1) 
 
      call realign333
! --------------------------------------------------
! 334 ALIGN arrB3(i,j,n)   WITH arrA3(n+1,3*i+1,j+2)    
!     REALIGN arrB3(i,j,n) WITH arrA3(2*j,i+1,2*n+1) 
 
      call realign334
! --------------------------------------------------
! 335 ALIGN arrB3(*,*,*)   WITH arrA3(*,*,*)    
!     REALIGN arrB3(i,j,n) WITH arrA3(i,j,n) 
       
      call realign335
! --------------------------------------------------
! 336 ALIGN arrB3(i,j,n)   WITH arrA3(i,j+1,2*n+1)    
!     REALIGN arrB3(*,j,n) WITH arrA3(j+1,n,1) 

      call realign336
! -------------------------------------------------
!
      print *,'=== END OF realign33 ========================= '    

      end

! ----------------------------------------------------realign331
! 331 ALIGN arrB3(i,j,n)   WITH arrA3(i,j,n)    
!     REALIGN arrB3(i,j,n) WITH arrA3(i+1,j+2,n+3) 

      subroutine realign331
      integer, parameter :: AN1=10,AN2=10,AN3=10,BN1=9,BN2=8,BN3=6
      integer, parameter :: NL=10000,ER=100000
!     parameters for ALIGN                                               
      integer, parameter :: k1i=1, li=0
      integer, parameter :: k2j=1, lj=0
      integer, parameter :: k3n=1, ln=0
!     parameters for REALIGN                                               
      integer, parameter :: kr1i=1, lri=1
      integer, parameter :: kr2j=1, lrj=2
      integer, parameter :: kr3n=1, lrn=3

      integer :: erria = ER, errib = ER
      integer s,cs,i,j,n,ia,ja,na,ib,jb,nb

      integer, allocatable :: A3(:,:,:),B3(:,:,:)
      character(*), parameter :: tname ='realign331'
             
!dvm$ distribute A3(BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B3(i,j,n) WITH A3(k1i*i+li,k2j*j+lj,k3n*n+ln)
!dvm$ DYNAMIC B3

      allocate (A3(AN1,AN2,AN3), B3(BN1,BN2,BN3))

!dvm$ region  out(A3,B3)
!dvm$ parallel (i,j,n) on B3(i,j,n)
      do i=1,BN1
          do  j=1,BN2
             do n=1,BN3
                    B3(i,j,n) = 0     
             enddo 
          enddo 
      enddo 

!dvm$ parallel (i,j,n) on A3(i,j,n), private(ib,jb,nb)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                      A3(i,j,n)=i*NL/10+j*NL/100+n*NL/1000
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k2j) * k2j)) .and.
     *                  ((n-ln) .eq.(((n-ln)/k3n) * k3n)) .and.
     *                  (((i-li)/k1i) .gt. 0)  .and.
     *                  (((j-lj)/k2j) .gt. 0)  .and.
     *                  (((n-ln)/k3n) .gt. 0)  .and.
     *                  (((i-li)/k1i) .le. BN1)  .and.
     *                  (((j-lj)/k2j) .le. BN2)  .and.
     *                  (((n-ln)/k3n) .le. BN3)  
     *                  ) then 
                            ib = (i-li)/k1i
                            jb = (j-lj)/k2j
                            nb = (n-ln)/k3n
                            B3(ib,jb,nb)=ib*NL/10+jb*NL/100+nb*NL/1000
                      endif 
              enddo 
          enddo 
      enddo 
!dvm$ end region

!dvm$ REALIGN B3(i,j,n) WITH A3(kr1i*i+lri,kr2j*j+lrj,kr3n*n+lrn)

      s=0 

!dvm$ actual(erria, errib, s)
!dvm$ region inlocal(A3,B3)
!dvm$ parallel (i,j,n) on B3(i,j,n), 
!dvm$*reduction(min(erria),min(errib),sum(s)),private(ia,ja,na)
      do i=1,BN1
         do j=1,BN2
            do n=1,BN3
               s = s + B3(i,j,n)
               if (B3(i,j,n) /= (i*NL/10+j*NL/100+n*NL/1000)) then     
                   errib = min(errib,i*NL/10 + j*NL/100 + n*NL/1000)
               endif
               ia=kr1i * i + lri
               ja=kr2j * j + lrj
               na=kr3n * n + lrn
               if (A3(ia,ja,na) /= (ia*NL/10+ja*NL/100+na*NL/1000))
     *         then     
                 erria = min(erria,ia*NL/10 + ja*NL/100 + na*NL/1000)
               endif
           enddo 
        enddo 
      enddo 
!dvm$ end region

      cs = 0              
      do i=1,BN1
         do j=1,BN2
            do n=1,BN3
                    cs = cs + i*NL/10 + j*NL/100 + n*NL/1000
            enddo 
         enddo 
      enddo 
     
!dvm$ get_actual(erria, errib, s)
      if ((erria == ER) .and. (errib == ER) 
     *    .and. (s == cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
!         write (*,*) erria,errib,s,cs
      endif 

      deallocate (B3,A3)

      end subroutine realign331

! ----------------------------------------------------realign332
! 332 ALIGN arrB3(i,j,n)   WITH arrA3(i,j,n)    
!     REALIGN arrB3(i,j,n) WITH arrA3(2*i,3*j,5*n) 

      subroutine realign332
      integer, parameter :: AN1=12,AN2=16,AN3=25,BN1=4,BN2=3,BN3=5
      integer, parameter :: NL=10000,ER=100000
!     parameters for ALIGN                                               
      integer, parameter :: k1i=1, li=0
      integer, parameter :: k2j=1, lj=0
      integer, parameter :: k3n=1, ln=0
!     parameters for REALIGN                                               
      integer, parameter :: kr1i=2, lri=0
      integer, parameter :: kr2j=3, lrj=0
      integer, parameter :: kr3n=5, lrn=0

      integer :: erria = ER, errib = ER
      integer s,cs,i,j,n,ia,ja,na,ib,jb,nb

      integer, allocatable :: A3(:,:,:),B3(:,:,:)
      character(*), parameter :: tname ='realign332'
             
!dvm$ distribute A3(BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B3(i,j,n) WITH A3(k1i*i+li,k2j*j+lj,k3n*n+ln)
!dvm$ DYNAMIC B3

      allocate (A3(AN1,AN2,AN3), B3(BN1,BN2,BN3))

      A3 = 0
      B3 = 0

!dvm$ region inout(A3,B3)
!dvm$ parallel (i,j,n) on A3(i,j,n), private(ib,jb,nb)
      do i=1,AN1
        do j=1,AN2
           do n=1,AN3
              A3(i,j,n)=i*NL/10+j*NL/100+n*NL/1000 + 10
              if ( 
     *           ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *           ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *           ((n-ln) .eq.(((n-ln)/k3n) * k3n)) .and.
     *           (((i-li)/k1i) .gt. 0)  .and.
     *           (((j-lj)/k2j) .gt. 0)  .and.
     *           (((n-ln)/k3n) .gt. 0)  .and.
     *           (((i-li)/k1i) .le. BN1)  .and.
     *           (((j-lj)/k2j) .le. BN2)  .and.
     *           (((n-ln)/k3n) .le. BN3)  
     *           ) then 
                      ib = (i-li)/k1i
                      jb = (j-lj)/k2j
                      nb = (n-ln)/k3n
                      B3(ib,jb,nb)=ib*NL/10+jb*NL/100+nb*NL/1000 + 5
               endif 
           enddo 
        enddo 
      enddo 
!dvm$ end region

!dvm$ REALIGN B3(i,j,n) WITH A3(kr1i*i+lri, kr2j*j+lrj, kr3n*n+lrn)

      s=0 

!dvm$ actual(erria, errib, s)
!dvm$ region inlocal(A3),inlocal(B3)
!dvm$ parallel (i,j,n) on B3(i,j,n),
!dvm$*reduction(min(erria),min(errib),sum(s)),private(ia,ja,na)
      do i=1,BN1
         do j=1,BN2
            do n=1,BN3
               s = s + B3(i,j,n)
               if (B3(i,j,n) /= (i*NL/10+j*NL/100+n*NL/1000) + 5) then 
                   errib = min(errib,i*NL/10 + j*NL/100 + n*NL/1000)
               endif
               ia=kr1i * i + lri
               ja=kr2j * j + lrj
               na=kr3n * n + lrn
               if (A3(ia,ja,na) /= (ia*NL/10+ja*NL/100+na*NL/1000)+10)
     *         then
                  erria = min(erria,ia*NL/10 + ja*NL/100 + na*NL/1000)
               endif
            enddo 
         enddo 
      enddo 
!dvm$ end region
  
      cs = 0              
      do i=1,BN1
         do j=1,BN2
            do n=1,BN3
                    cs = cs + i*NL/10 + j*NL/100 + n*NL/1000 + 5
            enddo 
         enddo 
      enddo 
     
!dvm$ get_actual(erria, errib, s)
      if ((erria == ER) .and. (errib == ER) 
     *    .and. (s == cs)) then     
          call ansyes(tname)
      else                     
          call ansno(tname)
!          write (*,*) erria,errib,s,cs
!          print *,B3  
      endif 

      deallocate (B3,A3)

      end subroutine realign332

! --------------------------------------------------realign333
! 333 ALIGN arrB3(i,j,n)   WITH arrA3(i+2,j+4,n+3)    
!     REALIGN arrB3(i,j,n) WITH arrA3(2*i-1,2*n,j+1) 
      
      subroutine realign333
      integer, parameter :: AN1=12,AN2=16,AN3=25,BN1=4,BN2=3,BN3=5
      integer, parameter :: NL=10000,ER=100000
!     parameters for ALIGN                                                
      integer, parameter :: k1i=1, li=2
      integer, parameter :: k2j=1, lj=4
      integer, parameter :: k3n=1, ln=3
!     parameters for REALIGN                                                
      integer, parameter :: kr1i=2, lri=-1
      integer, parameter :: kr2j=1, lrj=1
      integer, parameter :: kr3n=2, lrn=0

      integer :: erria = ER, errib = ER
      integer s,cs,i,j,n,ia,ja,na,ib,jb,nb

      integer A3(AN1,AN2,AN3)
      integer, allocatable :: B3(:,:,:)
      character(*), parameter :: tname ='realign333'
             
!dvm$ distribute A3(BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B3(i,j,n) WITH A3(k1i*i+li,k2j*j+lj,k3n*n+ln)
!dvm$ DYNAMIC B3

      allocate (B3(BN1,BN2,BN3))

      A3 = 1
      B3 = 2

!dvm$ region inout(A3),inout(B3)
!dvm$ parallel (i,j,n) on A3(i,j,n), private(ib,jb,nb)
      do i=1,AN1
        do j=1,AN2
          do n=1,AN3
             A3(i,j,n) = A3(i,j,n)+ i*NL/10+j*NL/100+n*NL/1000 
             if ( 
     *          ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *          ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *          ((n-ln) .eq.(((n-ln)/k3n) * k3n)) .and.
     *          (((i-li)/k1i) .gt. 0)  .and.
     *          (((j-lj)/k2j) .gt. 0)  .and.
     *          (((n-ln)/k3n) .gt. 0)  .and.
     *          (((i-li)/k1i) .le. BN1)  .and.
     *          (((j-lj)/k2j) .le. BN2)  .and.
     *          (((n-ln)/k3n) .le. BN3)  
     *          ) then 
                   ib = (i-li)/k1i
                   jb = (j-lj)/k2j
                   nb = (n-ln)/k3n
                   B3(ib,jb,nb) = B3(ib,jb,nb) + 
     *                            ib*NL/10+jb*NL/100+nb*NL/1000
               endif 
             enddo 
          enddo 
      enddo 
!dvm$ end region

!dvm$ REALIGN B3(i,j,n) WITH A3(kr1i*i+lri, kr3n*n+lrn, kr2j*j+lrj)

      s=0 

!dvm$ actual(erria, errib, s)
!dvm$ region
!dvm$ parallel (i,j,n) on B3(i,j,n), private(ia,ja,na),
!dvm$*reduction(min(erria),min(errib),sum(s))
      do i=1,BN1
         do j=1,BN2
            do n=1,BN3
               s = s + B3(i,j,n)
               if (B3(i,j,n) /= (i*NL/10+j*NL/100+n*NL/1000) + 2) then 
                   errib = min(errib,i*NL/10 + j*NL/100 + n*NL/1000)
               endif
               ia=kr1i * i + lri
               ja=kr3n * n + lrn
               na=kr2j * j + lrj
               if (A3(ia,ja,na) /= (ia*NL/10+ja*NL/100+na*NL/1000)+1)
     *         then  
                  erria = min(erria,ia*NL/10 + ja*NL/100 + na*NL/1000)
               endif
            enddo 
         enddo 
      enddo 
!dvm$ end region
  
      cs = 0              
      do i=1,BN1
         do j=1,BN2
            do n=1,BN3
                    cs = cs + i*NL/10 + j*NL/100 + n*NL/1000 + 2
            enddo 
         enddo 
      enddo 
     
!dvm$ get_actual(erria, errib, s)
      if ((erria == ER) .and. (errib == ER) 
     *    .and. (s == cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
!          write (*,*) erria,errib,s,cs
!          print *,B3  
      endif 

      deallocate (B3)

      end subroutine realign333

! ----------------------------------------------------realign334
! 334 ALIGN arrB3(i,j,n)   WITH arrA3(n+1,3*i+1,j+2)    
!     REALIGN arrB3(i,j,n) WITH arrA3(2*j,i+1,2*n+1) 
      
      subroutine realign334
      integer, parameter :: AN1=15,AN2=28,AN3=20,BN1=4,BN2=6,BN3=6
      integer, parameter :: NL=10000,ER=100000
!     parameters for ALIGN                                              
      integer, parameter :: k1i=3, li=1
      integer, parameter :: k2j=1, lj=2
      integer, parameter :: k3n=1, ln=1
!     parameters for REALIGN                                                
      integer, parameter :: kr1i=1, lri=1
      integer, parameter :: kr2j=2, lrj=0
      integer, parameter :: kr3n=2, lrn=1

      integer :: erria = ER, errib = ER
      integer s,cs,i,j,n,ia,ja,na,ib,jb,nb

      integer A3(AN1,AN2,AN3),B3(BN1,BN2,BN3)
      character(*), parameter :: tname ='realign334'
             
!dvm$ distribute A3(BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B3(i,j,n) WITH A3(k3n*n+ln,k1i*i+li,k2j*j+lj)
!dvm$ DYNAMIC B3

      A3 = 0
      B3 = 0

!dvm$ region
!dvm$ parallel (i,j,n) on A3(i,j,n), private(ib,jb,nb)
      do i=1,AN1
         do j=1,AN2
            do n=1,AN3
                  A3(i,j,n) = A3(i,j,n) + i*NL/10+j*NL/100+n*NL/1000 
                  if ( 
     *               ((i-ln) .eq.(((i-ln)/k3n) * k3n)) .and.
     *               ((j-li) .eq.(((j-li)/k1i) * k1i)) .and.
     *               ((n-lj) .eq.(((n-lj)/k2j) * k2j)) .and.
     *               (((i-ln)/k3n) .gt. 0) .and.
     *               (((j-li)/k1i) .gt. 0) .and.
     *               (((n-lj)/k2j) .gt. 0) .and.
     *               (((i-ln)/k3n) .le. BN3) .and.
     *               (((j-li)/k1i) .le. BN1) .and.
     *               (((n-lj)/k2j) .le. BN2)  
     *               ) then 
                        ib = (j-li)/k1i
                        jb = (n-lj)/k2j
                        nb = (i-ln)/k3n
                        B3(ib,jb,nb) = B3(ib,jb,nb) + 
     *                         ib*NL/10+jb*NL/100+nb*NL/1000
                      endif 
              enddo 
          enddo 
      enddo 
!dvm$ end region

!dvm$ REALIGN B3(i,j,n) WITH A3(kr2j*j+lrj, kr1i*i+lri, kr3n*n+lrn)

      s=0 

!dvm$ actual(erria, errib, s)
!dvm$ region
!dvm$ parallel (i,j,n) on B3(i,j,n),
!dvm$*reduction(min(erria),min(errib),sum(s)),
!dvm$*private(ia,ja,na)
      do i=1,BN1
        do j=1,BN2
          do n=1,BN3
             s = s + B3(i,j,n)
             if (B3(i,j,n) /= (i*NL/10+j*NL/100+n*NL/1000)) then     
                 errib = min(errib,i*NL/10 + j*NL/100 + n*NL/1000)
             endif
             ia=kr2j * j + lrj
             ja=kr1i * i + lri
             na=kr3n * n + lrn
             if (A3(ia,ja,na) /= (ia*NL/10+ja*NL/100+na*NL/1000)) then     
                  erria = min(erria,ia*NL/10 + ja*NL/100 + na*NL/1000)
!                 print *, ia, ja, na
             endif
          enddo 
        enddo 
      enddo 
!dvm$ end region
  
      cs = 0              
      do i=1,BN1
         do j=1,BN2
            do n=1,BN3
                    cs = cs + i*NL/10 + j*NL/100 + n*NL/1000
            enddo 
         enddo 
      enddo 
     
!dvm$ get_actual(erria, errib, s)
      if ((erria == ER) .and. (errib == ER) 
     *    .and. (s == cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 

      end subroutine realign334

! ----------------------------------------------------realign335
! 335 ALIGN arrB3(*,*,*)   WITH arrA3(*,*,*)    
!     REALIGN arrB3(i,j,n) WITH arrA3(i,j,n) 

      subroutine realign335
      integer, parameter :: AN1=10,AN2=10,AN3=10,BN1=4,BN2=8,BN3=4
      integer, parameter :: NL=10000,ER=100000
!     parameters for  ALIGN        
      integer, parameter :: k1i=0, li=0
      integer, parameter :: k2j=0, lj=0
      integer, parameter :: k3n=0, ln=0
!     parameters for REALIGN                                               
      integer, parameter :: kr1i=1, lri=0
      integer, parameter :: kr2j=1, lrj=0
      integer, parameter :: kr3n=1, lrn=0

      integer :: erria = ER, errib = ER
      integer s,cs,i,j,n,ia,ja,na,ib,jb,nb

      integer, allocatable :: A3(:,:,:),B3(:,:,:)
      character(*), parameter ::  tname = 'realign335'
               
!dvm$ distribute A3(BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B3(*,*,*) WITH A3(*,*,*)
!dvm$ DYNAMIC B3

      allocate (A3(AN1,AN2,AN3), B3(BN1,BN2,BN3))

      A3 = 0
      B3 = 6

!dvm$ actual (A3,B3)
!dvm$ region
!dvm$ parallel (i,j,n) on A3(i,j,n), private(ib,jb,nb)
      do i=1,AN1
        do j=1,AN2
          do n=1,AN3
             A3(i,j,n) = A3(i,j,n) + i*NL/10+j*NL/100+n*NL/1000
           enddo 
        enddo 
      enddo 

!dvm$ parallel (i,j,n) on B3(i,j,n), private(ib,jb,nb)
      do i=1,BN1
        do j=1,BN2
          do n=1,BN3
                B3(i,j,n) = B3(i,j,n) + i*NL/10+j*NL/100+n*NL/1000              
           enddo 
        enddo 
      enddo 

!dvm$ end region

!dvm$ REALIGN B3(i,j,n) WITH A3(kr1i*i+lri, kr2j*j+lrj, kr3n*n+lrn)

      s=0 

!dvm$ actual(erria, errib, s)
!dvm$ region  inlocal(A3)
!dvm$ parallel (i,j,n) on B3(i,j,n),
!dvm$* reduction(min(erria),min(errib),sum(s)),private(ia,ja,na)
      do i=1,BN1
         do j=1,BN2
            do n=1,BN3
               s = s + B3(i,j,n)
               if (B3(i,j,n) /= (i*NL/10+j*NL/100+n*NL/1000)+ 6) then     
                  errib = min(errib,i*NL/10 + j*NL/100 + n*NL/1000)
               endif
               ia=kr1i * i + lri
               ja=kr2j * j + lrj
               na=kr3n * n + lrn
               if (A3(ia,ja,na) /= (ia*NL/10+ja*NL/100+na*NL/1000)) then     
                    erria = min(erria,ia*NL/10 + ja*NL/100 + na*NL/1000)
               endif
           enddo         
         enddo 
      enddo 
!dvm$ end region
  
      cs = 0              
      do i=1,BN1
         do j=1,BN2
            do n=1,BN3
                    cs = cs + i*NL/10 + j*NL/100 + n*NL/1000 + 6
            enddo 
         enddo 
      enddo 
     
!dvm$ get_actual(erria, errib, s)
      if ((erria == ER) .and. (errib == ER) 
     *    .and. (s == cs)) then     
          call ansyes(tname)
      else                     
          call ansno(tname)
!          write (*,*) erria,errib,s,cs
!          print *,B3  
      endif 

      deallocate (B3,A3)

      end subroutine realign335

! ----------------------------------------------------realign336
! 336 ALIGN arrB3(i,j,n)   WITH arrA3(i,j+1,2*n+1)    
!     REALIGN arrB3(*,j,n) WITH arrA3(j+1,n,1) 

      subroutine realign336
      integer, parameter ::  AN1=8,AN2=8,AN3=8
      integer, parameter ::  BN1=3,BN2=4,BN3=3
      integer, parameter ::  NL=10000,ER=100000
!     parameters for ALIGN                                                
      integer, parameter ::  k1i=1,li=0
      integer, parameter ::  k2j=1,lj=1
      integer, parameter ::  k3n=2,ln=1
!     parameters for REALIGN                                                
      integer, parameter ::  kr1i=0,lri=1
      integer, parameter ::  kr2j=1,lrj=1
      integer, parameter ::  kr3n=1,lrn=0
      integer, allocatable :: A3(:,:,:),B3(:,:,:)
      integer :: s,cs,erria = ER, errib = ER,
     >           i,j,n,m,ia,ja,na,ib,jb,nb
      character(10) :: tname='realign336'

!dvm$ distribute A3(BLOCK,BLOCK,BLOCK)    
!dvm$ ALIGN B3(i,j,n) WITH A3(k1i*i+li,k2j*j+lj,k3n*n+ln)
!dvm$ DYNAMIC B3

      allocate (A3(AN1,AN2,AN3),B3(BN1,BN2,BN3))

      B3 = 0     

!dvm actual (B3)
!dvm$ region inout(B3), inout(A3)
!dvm$ parallel (i,j,n) on A3(i,j,n), private(ib,jb,nb)
      do i=1,AN1
          do j=1,AN2
            do n=1,AN3
                A3(i,j,n) = i*NL/10+j*NL/100+n*NL/1000
                      if ( 
     *                  ((i-li) .eq.(((i-li)/k1i) * k1i)) .and.
     *                  ((j-lj) .eq.(((j-lj)/k2j) *k2j)) .and.
     *                  ((n-ln) .eq.(((n-ln)/k3n) * k3n)) .and.
     *                  (((i-li)/k1i) .gt. 0)  .and.
     *                  (((j-lj)/k2j) .gt. 0)  .and.
     *                  (((n-ln)/k3n) .gt. 0)  .and.
     *                  (((i-li)/k1i) .le. BN1)  .and.
     *                  (((j-lj)/k2j) .le. BN2)  .and.
     *                  (((n-ln)/k3n) .le. BN3)  
     *                  )  then 
                        ib = (i-li)/k1i
                        jb = (j-lj)/k2j
                        nb = (n-ln)/k3n
                        B3(ib,jb,nb)=ib*NL/10+jb*NL/100+nb*NL/1000 
                      endif 
             enddo
          enddo 
      enddo
!dvm$ end region

!dvm$ REALIGN B3(*,j,n) WITH A3(kr2j*j+lrj,kr3n*n+lrn,lri)

      s=0 

!dvm$ actual(erria, errib, s)
!dvm$ region
!dvm$ parallel (i,j,n) on B3(i,j,n),
!dvm$* reduction(min(erria), min(errib), sum(s)) 
!dvm$*,private(ia,ja,na)
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                  s = s + B3(i,j,n)
                  if (B3(i,j,n) /= (i*NL/10+j*NL/100+n*NL/1000))then     
                      errib = min(errib,i*NL/10 + j*NL/100+ n*NL/1000)
                  endif
                  ia=kr2j*j+lrj
                  ja=kr3n*n+lrn
                  na=lri
                  if (A3(ia,ja,na)/=
     *               (ia*NL/10+ja*NL/100+na*NL/1000))then     
                      erria = min(erria,i*NL/10 + j*NL/100+ n*NL/1000)
                  endif
            enddo
          enddo 
      enddo
!dvm$ end region
  
      cs = 0              
      do i=1,BN1
          do j=1,BN2
            do n=1,BN3
                    cs = cs + i*NL/10 + j*NL/100+ n*NL/1000
             enddo
          enddo
      enddo
     
!dvm$ get_actual(erria, errib, s)


      if ((erria == ER) .and. (errib == ER) .and.
     *     (s ==  cs)) then     
          call ansyes(tname)
      else
          call ansno(tname)
!         print *, erria, errib
      endif 

      deallocate (B3,A3)

      end

! ----------------------------------------------------
      subroutine ansyes(name)
      character(*) name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character(*) name
      print *,name,'  -  ***error'
      end
