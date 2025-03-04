program taskst31
    integer, parameter :: k = 8, n1 = 4, itmax = 20, n2 = k - n1, er = 10000
    real, allocatable :: a( :, :, :, : ), a1( :, :, :, : ), a2( :, :, :, : )
    real, allocatable :: b( :, :, :, : ), b1( :, :, :, : ), b2( :, :, :, : )
    integer lp( 2 ), hp( 2 ), errt
    character*8 :: tname = 'taskst31'
    !dvm$ processors p( processors_size( 1 ), processors_size( 2 ), processors_size( 3 ) )
    !dvm$ task mb( 2 )

    !dvm$ distribute a( *, block, block, block ) onto p
    !dvm$ align b( i, j, ii, jj ) with a( i, j, ii, jj )

    !dvm$ distribute :: a1, a2
    !dvm$ align b1( i, j, ii, jj ) with a1( i, j, ii, jj )
    !dvm$ align b2( i, j, ii, jj ) with a2( i, j, ii, jj )
    
    print *,  '===START OF taskst31 ====================='

    call dpt( lp, hp, 2 )
    !dvm$ map mb( 1 ) onto p( lp( 1 ) : hp( 1 ), :, : )
    allocate( a1( n1 + 1, k, k, k ) )
    !dvm$ redistribute a1( *, block, block, block ) onto mb( 1 )
    allocate( b1( n1 + 1, k, k, k ) )

    !dvm$ map mb( 2 ) onto p( lp( 2 ) : hp( 2 ), :, : )
    allocate( a2( n2 + 1, k, k, k ) )
    !dvm$ redistribute a2( *, block, block, block ) onto mb( 2 )
    allocate( b2( n2 + 1, k, k, k ) )

    allocate( a( k, k, k, k ), b( k, k, k, k ) )

    !initialization
    !dvm$ task_region mb
    !dvm$ on mb( 1 )
    !dvm$ region
	!dvm$ parallel ( jj, ii, j, i ) on a1( i, j, ii, jj )
	do jj = 1, k
	    do ii = 1, k
	        do j = 1, k
	            do i = 1, n1
	                if( i .eq. 1 .or. &
	                    j .eq. 1 .or. j .eq. k .or. &
	                    ii .eq. 1 .or. ii .eq. k .or. &
	                    jj .eq. 1 .or. jj .eq. k ) then
	                    a1( i, j, ii, jj ) = 0.
	                    b1( i, j, ii, jj ) = 0.
	                else
	                    b1( i, j, ii, jj ) = 1. + i + j + ii + jj
	                    a1( i, j, ii, jj ) = b1( i, j, ii, jj )
	                endif
	            enddo
	        enddo
	    enddo
	enddo
    !dvm$ end region
    !dvm$ end on

    !dvm$ on mb( 2 )
    !dvm$ region
        !dvm$ parallel ( jj, ii, j, i ) on a2( i, j, ii, jj )
        do jj = 1, k
            do ii = 1, k
                do j = 1, k
                    do i = 2, n2 + 1
                        if( i .eq. n2 + 1 .or. &
                            j .eq. 1 .or. j .eq. k .or. &
                            ii .eq. 1 .or. ii .eq. k .or. &
                            jj .eq. 1 .or. jj .eq. k ) then
                            a2( i, j, ii, jj ) = 0.
                            b2( i, j, ii, jj ) = 0.
                        else
                            b2( i, j, ii, jj ) = 1. + ( i + n1 - 1 ) + j + ii + jj
                            a2( i, j, ii, jj ) = b2( i, j, ii, jj )
                        endif
                    enddo
                enddo
            enddo
        enddo
    !dvm$ end region
    !dvm$ end on
    !dvm$ end task_region

    do it = 1, itmax
     
        !exchange bounds
        !dvm$ get_actual(b2(2,:,:,:))
        !dvm$ parallel ( jj, ii, j ) on a1( n1 + 1, j, ii, jj ), remote_access ( b2( 2, j, ii, jj ) )
        do jj = 1, k
            do ii = 1, k
                do j = 1, k
                    a1( n1 + 1, j, ii, jj ) = b2( 2, j, ii, jj )
                enddo
            enddo
        enddo
        !dvm$ actual(a1(n1+1,:,:,:))
        !dvm$ get_actual (b1(n1,:,:,:))
        !dvm$ parallel ( jj, ii, j ) on a2( 1, j, ii, jj ), remote_access (  b1( n1, j, ii, jj ) )
        do jj = 1, k
            do ii = 1, k
                do j = 1, k
                    a2( 1, j, ii, jj ) = b1( n1, j, ii, jj )
                enddo
            enddo
        enddo
        !dvm$ actual(a2(1,:,:,:))
        
        !dvm$ task_region mb
            !dvm$ on mb( 1 )
                !dvm$ region
                    !dvm$ parallel ( jj, ii, j, i ) on b1( i, j, ii, jj ), shadow_renew ( a1 )
                    do jj = 2, k - 1
                        do ii = 2, k - 1
                            do j = 2, k - 1
                                do i = 2, n1
                                    b1( i, j, ii, jj ) = ( a1( i - 1, j, ii, jj ) + a1( i + 1, j, ii, jj ) + &
                                                           a1( i, j - 1, ii, jj ) + a1( i, j + 1, ii, jj ) + &
                                                           a1( i, j, ii - 1, jj ) + a1( i, j, ii + 1, jj ) + &
                                                           a1( i, j, ii, jj - 1 ) + a1( i, j, ii, jj + 1 ) ) / 8
                                enddo
                            enddo
                        enddo
                    enddo

                    !dvm$ parallel ( jj, ii, j, i ) on a1( i, j, ii, jj )
                    do jj = 2, k - 1
                        do ii = 2, k - 1
                            do j = 2, k - 1
                                do i = 2, n1
                                    a1( i, j, ii, jj ) = b1( i, j, ii, jj )
                                enddo
                            enddo
                        enddo
                    enddo
                !dvm$ end region
            !dvm$ end on

            !dvm$ on mb( 2 )
                !dvm$ region
                    !dvm$ parallel ( jj, ii, j, i ) on b2( i, j, ii, jj ), shadow_renew ( a2 )
                    do jj = 2, k - 1
                        do ii = 2, k - 1
                            do j = 2, k - 1
                                do i = 2, n2
                                    b2( i, j, ii, jj ) = ( a2( i - 1, j, ii, jj ) + a2( i + 1, j, ii, jj ) + &
                                                           a2( i, j - 1, ii, jj ) + a2( i, j + 1, ii, jj ) + &
                                                           a2( i, j, ii - 1, jj ) + a2( i, j, ii + 1, jj ) + &
                                                           a2( i, j, ii, jj - 1 ) + a2( i, j, ii, jj + 1 ) ) / 8
                                enddo
                            enddo
                        enddo
                    enddo
                    !dvm$ parallel ( jj, ii, j, i ) on a2( i, j, ii, jj )
                    do jj = 2, k - 1
                        do ii = 2, k - 1
                            do j = 2, k - 1
                                do i = 2, n2
                                    a2( i, j, ii, jj ) = b2( i, j, ii, jj )
                                enddo
                            enddo
                        enddo
                    enddo
                !dvm$ end region
            !dvm$ end on
        !dvm$ end task_region
    enddo

    !1 - task jacobi
    !dvm$ region
        !dvm$ parallel ( jj, ii, j, i ) on a( i, j, ii, jj )
        do jj = 1, k
            do ii = 1, k
                do j = 1, k
                    do i = 1, k
                        a( i, j, ii, jj ) = 0.
                        if( i .eq. 1 .or. j .eq. 1 .or. &
                            i .eq. k .or. j .eq. k .or. &
                            ii .eq. 1 .or. ii .eq. k .or. &
                            jj .eq. 1 .or. jj .eq. k ) then
                            b( i, j, ii, jj ) = 0.
                        else
                            b( i, j, ii, jj ) = ( 1. + i + j + ii + jj )
                        endif
                    enddo
                enddo
            enddo
        enddo
    !dvm$ end region

    do it = 1, itmax
        !dvm$ region
            !dvm$ parallel ( jj, ii, j, i ) on a( i, j, ii, jj )
            do jj = 2, k - 1
                do ii = 2, k - 1
                    do j = 2, k - 1
                        do i = 2, k - 1
                            a( i, j, ii, jj ) = b( i, j, ii, jj )
                        enddo
                    enddo
                enddo
            enddo
            !dvm$ parallel ( jj, ii, j, i ) on b( i, j, ii, jj ), shadow_renew( a )
            do jj = 2, k - 1
                do ii = 2, k - 1
                    do j = 2, k - 1
                        do i = 2, k - 1
                            b( i, j, ii, jj ) = ( a( i - 1, j, ii, jj ) + a( i + 1, j, ii, jj ) + &
                                                  a( i, j - 1, ii, jj ) + a( i, j + 1, ii, jj ) + &
                                                  a( i, j, ii - 1, jj ) + a( i, j, ii + 1, jj ) + &
                                                  a( i, j, ii, jj - 1 ) + a( i, j, ii, jj + 1 ) ) / 8
                        enddo
                    enddo
                enddo
            enddo
        !dvm$ end region
    enddo

    ! compare 2 - task jacobi with 1 - task jacobi
    !dvm$ get_actual(b,b1,b2)
    a(2:n1,:,:,:) = b1(2:n1,:,:,:)
    a(n1+1:n1+n2-1,:,:,:) = b2(2:n2,:,:,:)
    errt = er
    !dvm$ parallel ( jj, ii, j, i ) on b( i, j, ii, jj ), reduction(min(errt))
    do jj = 2, k - 1
        do ii = 2, k - 1
            do j = 2, k - 1
                do i = 2, k - 1
                    if(a( i, j, ii, jj) .ne. b( i, j, ii, jj))  errt = min(errt, i)
                enddo        
            enddo
        enddo
    enddo
    if (errt .eq. er)  then
        call ansyes(tname)
    else
        call ansno (tname)
    endif
    deallocate(b,b1,b2,a,a1,a2)
    print *,  '=== END OF taskst31 ====================='
end

subroutine dpt( lp, hp, nt )
    !distributing processors for nt tasks ( nt = 2 )
    integer lp( 2 ), hp( 2 )
    processors_size( i ) = 1
    !dvm$ debug 1 ( d = 0 )
        np = processors_size( 1 )
        ntp = np/nt
        if( np .eq. 1 ) then
            lp( 1 ) = 1
            hp( 1 ) = 1
            lp( 2 ) = 1
            hp( 2 ) = 1
        else
            lp( 1 ) = 1
            hp( 1 ) = ntp
            lp( 2 ) = ntp + 1
            hp( 2 ) = np
        end if
    !dvm$ enddebug 1
end

subroutine ansyes(name)
    character*8 name
    print *, name, '  -  complete'
end

subroutine ansno(name)
    character*8 name
    print *, name, '  -  ***error'
end

