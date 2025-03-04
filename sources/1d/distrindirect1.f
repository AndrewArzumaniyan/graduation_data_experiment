      program DISTRINDIRECT1
!    Testing DISTRIBUTE and REDISTRIBUTE directives
!            INDIRECT  distribution
	print *,'=== START OF distrindirect1 ========================'
      call distrindirect11
      print *,'=== END OF distrindirect1 ========================= '    
      end

        subroutine distrindirect11
        parameter    (L=10,  ER=100000)
        integer:: x_t, y_t, z_t, erri = ER
        integer A(L*L*L), B(L*L*L), AS(L*L*L), BS(L*L*L)
        integer,dimension(:),allocatable:: ib1,ib2,ib3,ib4,ib5,ib6 
        integer,dimension(L*L*L):: indir_x, indir_y,indir_z
        integer MAP(L*L*L)
        character*15:: tname="distrindirect11"

!DVM$   DISTRIBUTE MAP (BLOCK)
!DVM$   TEMPLATE E(L*L*L)
!DVM$   DISTRIBUTE :: E
!DVM$   ALIGN :: A,B
!DVM$   ALIGN :: indir_x, indir_y,indir_z
!DVM$   ALIGN :: ib1,ib2,ib3,ib4,ib5,ib6

      call distrindirect11_s(AS,BS)
      call fillMap(MAP,L)
      allocate( ib1(L*L*L),ib2(L*L*L),ib3(L*L*L)  &
     &         ,ib4(L*L*L),ib5(L*L*L),ib6(L*L*L) )
!DVM$ REDISTRIBUTE E(INDIRECT(MAP))
!DVM$ REALIGN (I)  WITH  E(I) :: A,B
!DVM$ REALIGN (I)  WITH  E(I) :: indir_x, indir_y,indir_z
!DVM$ REALIGN (I)  WITH  E(I) :: ib1,ib2,ib3,ib4,ib5,ib6
      do i = 1,L*L*L
      
        x_t = (i-1) / (L*L)
        y_t = mod((i-1) / L, L)
        z_t = mod(i-1 , L)

        indir_x(i) = x_t
        indir_y(i) = y_t
        indir_z(i) = z_t

        if (x_t.gt.0) then
            ib1(i) = i - (L*L)
        else
            ib1(i) = -1
        endif
        if ((x_t+1).lt.L) then
            ib2(i) = i+(L*L)
        else
            ib2(i) = -1
        endif
        if (y_t.gt.0) then
            ib3(i) = i-L
        else
            ib3(i) = -1
        endif
        if ((y_t+1).lt.L) then
            ib4(i) = i+L
        else
            ib4(i) = -1
        endif
        if (z_t.gt.0) then
            ib5(i) = i-1
        else
            ib5(i) = -1
        endif
        if ((z_t+1).lt.L) then
            ib6(i) = i+1
        else
            ib6(i) = -1
        endif
      enddo
	  
!DVM$ SHADOW_ADD (E((ib1(i)) with E(@i)) = "nei1") include_to A
!DVM$ SHADOW_ADD (E((ib2(i)) with E(@i)) = "nei2") include_to A
!DVM$ SHADOW_ADD (E((ib3(i)) with E(@i)) = "nei3") include_to A
!DVM$ SHADOW_ADD (E((ib4(i)) with E(@i)) = "nei4") include_to A
!DVM$ SHADOW_ADD (E((ib5(i)) with E(@i)) = "nei5") include_to A
!DVM$ SHADOW_ADD (E((ib6(i)) with E(@i)) = "nei6") include_to A

!DVM$ LOCALIZE(ib1 => A(:))
!DVM$ LOCALIZE(ib2 => A(:))
!DVM$ LOCALIZE(ib3 => A(:))
!DVM$ LOCALIZE(ib4 => A(:))
!DVM$ LOCALIZE(ib5 => A(:))
!DVM$ LOCALIZE(ib6 => A(:))

!DVM$ REGION
!DVM$ PARALLEL (i) ON B(i)
      do i = 1, L*L*L
        A(i) = 0
        if (indir_x(i) == 0 .or. indir_x(i) == L-1 .or.   &
     &      indir_y(i) == 0 .or. indir_y(i) == L-1 .or.   &
     &      indir_z(i) == 0 .or. indir_z(i) == L-1) then  

            B(i) = 0
        else
            B(i) = 4 + indir_x(i) + indir_y(i) + indir_z(i)
        endif
      enddo
!DVM$   PARALLEL (i) ON B(i), SHADOW_RENEW (A)
        do i = 1, L*L*L
           if (indir_x(i) /= 0 .and. indir_x(i) /= L-1 .and.  &
     &         indir_y(i) /= 0 .and. indir_y(i) /= L-1 .and.  &
     &         indir_z(i) /= 0 .and. indir_z(i) /= L-1) then
               B(i) = (A(ib1(i)) + A(ib2(i)) + A(ib3(i)) +    &
     &                 A(ib4(i)) + A(ib5(i)) + A(ib6(i))) / 6.0
           endif
        enddo
!DVM$ PARALLEL (i) ON B(i), REDUCTION(min(erri))
        do i = 1, L*L*L
          if (indir_x(i) /= 0 .and. indir_x(i) /= L-1 .and.  &  
     &        indir_y(i) /= 0 .and. indir_y(i) /= L-1 .and.  &  
     &        indir_z(i) /= 0 .and. indir_z(i) /= L-1) then
            if(B(i) .ne. BS(i)) erri =  min(erri, ABS(B(i)-BS(i)))
          endif
        enddo

!DVM$ END REGION

!DVM$ GET_ACTUAL(erri)
      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif

      deallocate(ib1,ib2,ib3,ib4,ib5,ib6)
      end subroutine

!---------------------------------------------------------------
      subroutine fillMap(MAP,L)
      integer  numproc 
      integer i,L
      real:: x=1
      integer MAP(L*L*L)
      intrinsic INT
      NUMBER_OF_PROCESSORS() = 1
!DVM$ INHERIT MAP
      numproc = NUMBER_OF_PROCESSORS() 
!DVM$ PARALLEL (i) ON MAP(i)
        do i=1,L*L*L
           call RANDOM_NUMBER(x) 
           MAP(i) = MOD(INT(x*10), numproc) 
        enddo
      end subroutine    

!---------------------------------------------------------------
        subroutine distrindirect11_s (A,B)
        parameter    (L=10)
        integer:: x_t, y_t, z_t
        integer A(L*L*L), B(L*L*L)
        integer,dimension(:),allocatable:: ib1,ib2,ib3,ib4,ib5,ib6 
        integer,dimension(L*L*L):: indir_x, indir_y,indir_z      

      allocate( ib1(L*L*L),ib2(L*L*L),ib3(L*L*L)  &
     &         ,ib4(L*L*L),ib5(L*L*L),ib6(L*L*L) )
      do i = 1,L*L*L
      
        x_t = (i-1) / (L*L)
        y_t = mod((i-1) / L, L)
        z_t = mod(i-1 , L)

        indir_x(i) = x_t
        indir_y(i) = y_t
        indir_z(i) = z_t

        if (x_t.gt.0) then
            ib1(i) = i - (L*L)
        else
            ib1(i) = -1
        endif
        if ((x_t+1).lt.L) then
            ib2(i) = i+(L*L)
        else
            ib2(i) = -1
        endif
        if (y_t.gt.0) then
            ib3(i) = i-L
        else
            ib3(i) = -1
        endif
        if ((y_t+1).lt.L) then
            ib4(i) = i+L
        else
            ib4(i) = -1
        endif
        if (z_t.gt.0) then
            ib5(i) = i-1
        else
            ib5(i) = -1
        endif
        if ((z_t+1).lt.L) then
            ib6(i) = i+1
        else
            ib6(i) = -1
        endif
      enddo
	  
      do i = 1, L*L*L
        A(i) = 0
        if (indir_x(i) == 0 .or. indir_x(i) == L-1 .or.   &
     &      indir_y(i) == 0 .or. indir_y(i) == L-1 .or.   &
     &      indir_z(i) == 0 .or. indir_z(i) == L-1) then  

            B(i) = 0
        else
            B(i) = 4 + indir_x(i) + indir_y(i) + indir_z(i)
        endif
      enddo

        do i = 1, L*L*L
           if (indir_x(i) /= 0 .and. indir_x(i) /= L-1 .and.  &
     &         indir_y(i) /= 0 .and. indir_y(i) /= L-1 .and.  &
     &         indir_z(i) /= 0 .and. indir_z(i) /= L-1) then
               B(i) = (A(ib1(i)) + A(ib2(i)) + A(ib3(i)) +    &
     &                 A(ib4(i)) + A(ib5(i)) + A(ib6(i))) / 6.0
           endif
        enddo
      deallocate(ib1,ib2,ib3,ib4,ib5,ib6)
      end subroutine

!---------------------------------------------------------------
      subroutine ansyes(name)
      character*14 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*9 name
      print *,name,'  -  ***error'
      end


     
