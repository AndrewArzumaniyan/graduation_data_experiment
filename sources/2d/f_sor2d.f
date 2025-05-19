        PROGRAM SOR2D_float
        PARAMETER (L=8000, ITMAX=100)
        REAL EPS, MAXEPS, A(L, L), W, S
        DOUBLE PRECISION STARTT, ENDT, dvtime
!DVM$   DISTRIBUTE(BLOCK, BLOCK) :: A

        MAXEPS = 0.5
        W = 0.5
!DVM$   REGION
!DVM$   PARALLEL(J, I) ON A(I, J), CUDA_BLOCK(32, 8)
!        nest of two parallel loops, iteration (i, j) will be executed on
!        processor, which is owner of element A(i, j)
          DO J = 1, L
            DO I = 1, L
              IF (I.EQ.1 .OR. J.EQ.1 .OR. I.EQ.L .OR. J.EQ.L ) THEN
                A(I, J) = 0.
              ELSE
                A(I, J) = (1. + I + J)
              ENDIF
            ENDDO
          ENDDO
!DVM$   END REGION
!DVM$   BARRIER
        STARTT = 0
        DO IT = 1, ITMAX
          EPS = 0.
!DVM$     ACTUAL(EPS)
!DVM$     REGION

!DVM$ PARALLEL (J, I) ON A(I, J), ACROSS(A(1:1,1:1)),
!DVM$& REDUCTION(MAX(EPS)), PRIVATE(S)
            DO J = 2, L - 1
              DO I = 2, L - 1
                S = A(I, J)
                A(I, J) = (W / 6. ) * 
     >            (A(I, J-1) + A(I-1, J) + A(I+1, J) + A(I, J+1))
     >          + (1 - W) * A(I, J)
                EPS = MAX(EPS, ABS(S - A(I, J)))
              ENDDO
            ENDDO
!DVM$     END REGION
!DVM$     GET_ACTUAL(EPS)
          PRINT 200, IT, EPS
200       FORMAT (' IT = ', I4, '   EPS = ', E14.7)
          IF (EPS .LT. MAXEPS) EXIT
        ENDDO
!DVM$   BARRIER
        ENDT = 0

        PRINT *, 'SOR2D_float Benchmark Completed.'
        PRINT 201, L, L
201     FORMAT (' Size            =    ', I6, ' x ', I6)
        PRINT 202, ITMAX
202     FORMAT (' Iterations      =       ', I12)
        PRINT 203, ENDT - STARTT
203     FORMAT (' Time in seconds =       ', F12.2)
        PRINT *, 'Operation type  =     floating point'
        IF (ABS(EPS - 0.4247670) .LT. 1.0E-4) THEN
          PRINT *, 'Verification    =         SUCCESSFUL'
        ELSE
          PRINT *, 'Verification    =       UNSUCCESSFUL'
        ENDIF

        PRINT *, 'END OF SOR2D_float Benchmark'
        END