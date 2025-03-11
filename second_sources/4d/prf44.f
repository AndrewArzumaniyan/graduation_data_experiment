program prf44
    !TESTING OF THE PREFETCH DIRECTIVE.

    print *, '===START OF PRF44========================'

    call prf4401
    call prf4402
    call prf4403

    print *, '===END OF PRF44=========================='
end

subroutine prf4401
    integer, parameter :: N = 16, M = 8, K = 8, L = 16, NL = 1000, NIT = 3
    integer, allocatable :: A( :, :, :, : ), B( :, :, :, : )
    integer, allocatable :: C( :, :, :, : ), A1( :, :, :, : )
    character * 7 :: tname = 'PRF4401'

    !dvm$ distribute B( block, block, block, block )
    !dvm$ align( :, :, :, : ) with B( :, :, :, : ) :: A, A1

    !dvm$ remote_group GR1
    !dvm$ remote_group GR2
    !dvm$ remote_group GR3

    allocate ( B( N, M, K, L ), A( N, M, K, L ), C( N, M, K, L ), A1( N, M, K, L ) )
    call serial4( C, N, M, K, L, NL )

    !dvm$ parallel ( i, j, ii, jj ) on A( i, j, ii, jj )
    do i = 1, N
        do j = 1, M
            do ii = 1, K
                do jj = 1, L
                    A( i, j, ii, jj ) = NL + i + j + ii + jj
                    A1( i, j, ii, jj ) = NL + i + j + ii + jj
                enddo
            enddo
        enddo
    enddo

    do it = 1, NIT
        !dvm$ prefetch GR1
        !dvm$ prefetch GR2
        !dvm$ prefetch GR3

        !dvm$ remote_access ( GR1:A( N / 2, M / 2, K / 2, L / 2 ) )
        ib1 = A( N / 2, M / 2, K / 2, L / 2 )

        !dvm$ remote_access ( GR1:A( N / 2, M, K, L ) )
        ib2 = A( N / 2, M, K, L )

        !dvm$ remote_access ( GR2:A( N, M / 2, K, L ) )
        ib3 = A( N, M / 2, K, L )

        !dvm$ remote_access ( GR2:A( N, M, K / 2, L ) )
        ib4 = A( N, M, K / 2, L )

        !dvm$ remote_access ( GR3:A( N, M, K, L / 2 ) )
        ib5 = A( N, M, K, L / 2 )

        !dvm$ remote_access ( GR3:A1( 1, M, K, L / 2 ) )
        ib6 = A1( 1, M, K, L / 2 )

        if ( ( ib1 .eq. C( N / 2, M / 2, K / 2, L / 2 ) ) .and. ( ib2 .eq. C( N / 2, M, K, L ) ) .and. &
             ( ib3 .eq. C( N, M / 2, K, L ) ) .and. ( ib4 .eq. C( N, M, K / 2, L ) ) .and. &
             ( ib5 .eq. C( N, M, K, L / 2 ) ) .and. ( ib6 .eq. C( 1, M, K, L / 2 ) ) ) then
            call ansyes( tname )
        else
            call ansno( tname )
        endif

        if ( it .eq. 2 ) cycle

        !dvm$ reset GR1
        !dvm$ reset GR2
        !dvm$ reset GR3
    enddo
    deallocate ( A, B, C, A1 )
end

subroutine prf4402
    integer, parameter ::  N = 16, M = 8, K = 8, L = 16, NL = 1000, NIT = 3
    integer, allocatable :: A( :, :, :, : ), B( :, :, :, : )
    integer, allocatable :: C( :, :, :, : ), D( :, :, :, : )
    character * 7 :: tname = 'PRF4402'

    !dvm$ distribute A( block, block, block, block )
    !dvm$ align( :, :, :, : ) with A( :, :, :, : ) :: B

    !dvm$ remote_group GR1
    !dvm$ remote_group GR2
    !dvm$ remote_group GR3

    allocate ( A( N, M, K, L ), B( N, M, K, L ), C( N, M, K, L ), D( N, M, K, L ) )
    call serial4( C, N, M, K, L, NL )

    !dvm$ parallel ( i, j, ii, jj ) on A( i, j, ii, jj )
    do i = 1, N
        do j = 1, M
            do ii = 1, K
                do jj = 1, L
                    A( i, j, ii, jj ) = NL + i + j + ii + jj
                enddo
            enddo
        enddo
    enddo

    do it = 1, NIT
        !dvm$ prefetch GR1
        !dvm$ prefetch GR2
        !dvm$ prefetch GR3

        isumc1 = 0
        isuma1 = 0
        !dvm$ remote_access ( GR1:A( N / 2, :, :, : ) )
        do j = 1, M
            do ii = 1, K
                do jj = 1, L
                    D( N / 2, j, ii, jj ) = A( N / 2, j, ii, jj )
                    isumc1 = isumc1 + C( N / 2, j, ii, jj )
                    isuma1 = isuma1 + D( N / 2, j, ii, jj )
                enddo
            enddo
        enddo

        isumc2 = 0
        isuma2 = 0
        !dvm$ remote_access ( GR2:A( :, M / 2, :, : ) )
        do i = 1, N
            do ii = 1, K
                do jj = 1, L
                    D( i, M / 2, ii, jj ) = A( i, M / 2, ii, jj )
                    isumc2 = isumc2 + C( i, M / 2, ii, jj )
                    isuma2 = isuma2 + D( i, M / 2, ii, jj )
                enddo
            enddo
        enddo

        isumc3 = 0
        isuma3 = 0
        !dvm$ remote_access ( GR3:A( :, :, K / 2, : ) )
        do i = 1, N
            do j = 1, M
                do jj = 1, L
                    D( i, j, K / 2, jj ) = A( i, j, K / 2, jj )
                    isumc3 = isumc3 + C( i, j, K / 2, jj )
                    isuma3 = isuma3 + D( i, j, K / 2, jj )
                enddo
            enddo
        enddo

        isumc4 = 0
        isuma4 = 0
        !dvm$ remote_access ( GR3:A( :, :, :, L / 2 ) )
        do i = 1, N
            do j = 1, M
                do ii = 1, K
                    D( i, j, ii, L / 2 ) = A( i, j, ii, L / 2 )
                    isumc4 = isumc4 + C( i, j, ii, L / 2 )
                    isuma4 = isuma4 + D( i, j, ii, L / 2 )
                enddo
            enddo
        enddo

        if ( ( isumc1 .eq. isuma1 ) .and. ( isumc2 .eq. isuma2 ) .and. &
              ( isumc3 .eq. isuma3 ) .and. ( isumc4 .eq. isuma4 ) ) then
            call ansyes( tname )
        else
            call ansno( tname )
        endif

        if ( it .eq. 2 ) cycle

        !dvm$ reset GR1
        !dvm$ reset GR2
        !dvm$ reset GR3
    enddo
    deallocate( A, B, C, D )
end

subroutine prf4403
    integer, parameter ::  N = 16, M = 8, K = 8, L = 16, NL = 1000, NIT = 3
    integer, allocatable :: A( :, :, :, : ), B( :, :, :, : ), C( :, :, :, : )
    character * 7 :: tname = 'PRF4403'

    !dvm$ distribute A( block, block, block, block )
    !dvm$ align( :, :, :, : ) with A( :, :, :, : ) :: B

    !dvm$ remote_group GR1
    !dvm$ remote_group GR2
    !dvm$ remote_group GR3

    allocate ( A( N, M, K, L ), B( N, M, K, L ), C( N, M, K, L ) )
    call serial4( C, N, M, K, L, NL )

    !dvm$ parallel ( i, j, ii, jj ) on A( i, j, ii, jj )
    do i = 1, N
        do j = 1, M
            do ii = 1, K
                do jj = 1, L
                    A( i, j, ii, jj ) = NL + i + j + ii + jj
                enddo
            enddo
        enddo
    enddo

    do it = 1, NIT
        !dvm$ prefetch GR1
        !dvm$ prefetch GR2
        !dvm$ prefetch GR3

        nloopi1 = NL
        nloopj1 = NL
        nloopii1 = NL
        nloopjj1 = NL
        !dvm$ parallel ( i, j, ii, jj ) on B( i, j, ii, jj ), remote_access( GR1:A( N / 2, M / 2, K / 2, L / 2 ) )
        do i = 1, N
            do j = 1, M
                do ii = 1, K
                    do jj = 1, L
                        B( i, j, ii, jj ) = A( N / 2, M / 2, K / 2, L / 2 )
                    enddo
                enddo
            enddo
        enddo
        !dvm$ parallel ( i, j, ii, jj ) on B( i, j, ii, jj ), reduction( min( nloopi1 ), min( nloopj1 ), min( nloopii1 ), min( nloopjj1 ) )
        do i = 1, N
            do j = 1, M
                do ii = 1, K
                    do jj = 1, L
                        if ( B( i, j, ii, jj ) .ne. C( N / 2, M / 2, K / 2, L / 2 ) ) then
                            nloopi1 = min( nloopi1, i )
                            nloopj1 = min( nloopj1, j )
                            nloopii1 = min( nloopii1, ii )
                            nloopjj1 = min( nloopjj1, jj )
                        endif
                    enddo
                enddo
            enddo
        enddo

        nloopi2 = NL
        nloopj2 = NL
        nloopii2 = NL
        nloopjj2 = NL
        !dvm$ parallel ( i, j, ii, jj ) on A( i, j, ii, jj ), remote_access( GR2:A( N / 2, :, :, : ) )
        do i = 1, N
            do j = 1, M
                do ii = 1, K
                    do jj = 1, L
                        B( i, j, ii, jj ) = A( N / 2, j, ii, jj )
                    enddo
                enddo
            enddo
        enddo
        !dvm$ parallel ( i, j, ii, jj ) on B( i, j, ii, jj ), reduction( min( nloopi2 ), min( nloopj2 ), min( nloopii2 ), min( nlooopjj4 ) )
        do i = 1, N
            do j = 1, M
                do ii = 1, K
                    do jj = 1, L
                        if ( B( i, j, ii, jj ) .ne. C( N / 2, j, ii, jj ) ) then
                            nloopi2 = min( nloopi2, i )
                            nloopj2 = min( nloopj2, j )
                            nloopii2 = min( nloopii2, ii )
                            nloopjj2 = min( nloopjj2, jj )
                        endif
                    enddo
                enddo
            enddo
        enddo

        nloopi3 = NL
        nloopj3 = NL
        nloopii3 = NL
        nloopjj3 = NL
        !dvm$ parallel ( i, j, ii, jj ) on B( i, j, ii, jj ), remote_access( GR2:A( :, M / 2, :, : ) )
        do i = 1, N
            do j = 1, M
                do ii = 1, K
                    do jj = 1, L
                        B( i, j, ii, jj ) = A( i, M / 2, ii, jj )
                    enddo
                enddo
            enddo
        enddo
        !dvm$ parallel ( i, j, ii, jj ) on B( i, j, ii, jj ), reduction( min( nloopi3 ), min( nloopj3 ), min( nloopii3 ), min( nloopjj3 ) )
        do i = 1, N
            do j = 1, M
                do ii = 1, K
                    do jj = 1, L
                        if ( B( i, j, ii, jj ) .ne. C( i, M / 2, ii, jj ) ) then
                            nloopi3 = min( nloopi3, i )
                            nloopj3 = min( nloopj3, j )
                            nloopii3 = min( nloopii3, ii )
                            nloopjj3 = min( nloopjj3, jj )
                        endif
                    enddo
                enddo
            enddo
        enddo

        nloopi4 = NL
        nloopj4 = NL
        nloopii4 = NL
        nloopjj4 = NL
        !dvm$ parallel ( i, j, ii, jj ) on B( i, j, ii, jj ), remote_access( GR3:A( :, :, K / 2, : ) )
        do i = 1, N
            do j = 1, M
                do ii = 1, K
                    do jj = 1, L
                        B( i, j, ii, jj ) = A( i, j, K / 2, jj )
                    enddo
                enddo
            enddo
        enddo
        !dvm$ parallel ( i, j, ii, jj ) on B( i, j, ii, jj ), reduction( min( nloopi4 ), min( nloopj4 ), min( nloopii4 ), min( nloopjj4 ) )
        do i = 1, N
            do j = 1, M
                do ii = 1, K
                    do jj = 1, L
                        if ( B( i, j, ii, jj ) .ne. C( i, j, K / 2, jj ) ) then
                            nloopi4 = min( nloopi4, i )
                            nloopj4 = min( nloopj4, j )
                            nloopii4 = min( nloopii4, ii )
                            nloopjj4 = min( nloopjj4, jj )
                        endif
                    enddo
                enddo
            enddo
        enddo

        nloopi5 = NL
        nloopj5 = NL
        nloopii5 = NL
        nloopjj5 = NL
        !dvm$ parallel ( i, j, ii, jj ) on B( i, j, ii, jj ), remote_access( GR3:A( :, :, :, L / 2 ) )
        do i = 1, N
            do j = 1, M
                do ii = 1, K
                    do jj = 1, L
                        B( i, j, ii, jj ) = A( i, j, ii, L / 2 )
                    enddo
                enddo
            enddo
        enddo
        !dvm$ parallel ( i, j, ii, jj ) on B( i, j, ii, jj ), reduction( min( nloopi5 ), min( nloopj5 ), min( nloopii5 ), min( nloopjj5 ) )
        do i = 1, N
            do j = 1, M
                do ii = 1, K
                    do jj = 1, L
                        if ( B( i, j, ii, jj ) .ne. C( i, j, ii, L / 2 ) ) then
                            nloopi5 = min( nloopi5, i )
                            nloopj5 = min( nloopj5, j )
                            nloopii5 = min( nloopii5, ii )
                            nloopjj5 = min( nloopjj5, jj )
                        endif
                    enddo
                enddo
            enddo
        enddo

        if ( ( nloopi1 .eq. NL ) .and. ( nloopi2 .eq. NL ) .and. &
             ( nloopi3 .eq. NL ) .and. ( nloopi4 .eq. NL ) .and. &
             ( nloopi5 .eq. NL ) ) then
            call ansyes( tname )
        else
            call ansno( tname )
        endif

        if ( it .eq. 2 ) cycle

        !dvm$ reset GR1
        !dvm$ reset GR2
        !dvm$ reset GR3
    enddo
    deallocate( A, B, C )
end

subroutine serial4( AR, N, M, K, L, NL )
    integer AR( N, M, K, L )
    integer NL
    do i = 1, N
        do j = 1, M
            do ii = 1, K
                do jj = 1, L
                    AR( i, j, ii, jj ) = NL + i + j + ii + jj
                enddo
            enddo
        enddo
    enddo
end

subroutine ansyes( name )
    character * 7 name
    print *, name, '  -  complete'
end

subroutine ansno( name )
    character * 7 name
    print *, name, '  -  ***error'
end
