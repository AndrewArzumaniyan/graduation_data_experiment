       MODULE FOR_JAC
         PARAMETER    (K=8,  ITMAX=20)
         REAL AA(K,K), EPS, MAXEPS, BB(K,K)
CDVM$    DISTRIBUTE (BLOCK, BLOCK) :: AA
CDVM$    ALIGN BB(I,J) WITH  AA(I,J)
       END MODULE

       MODULE MOD1
          USE  FOR_JAC
       END MODULE

       PROGRAM  MODULE21
       USE MOD1,A=>AA,B=>BB  !FOR_JAC

CDVM$  REDUCTION_GROUP REPS 
       REAL A1(K,K), EPS1, B1(K,K)
CDVM$  DISTRIBUTE (BLOCK, BLOCK) :: A1
CDVM$  ALIGN B1(I,J) WITH A1(I,J)

       PRINT *,  '======== START OF MODULE21  ========'
CDVM$  SHADOW_GROUP  SA (A)
C      creation of descriptor for operations with imported/exported 
C      elements of array A
       MAXEPS = 0.5E - 7

CDVM$  PARALLEL  ( J, I)   ON  A( I,  J)
       DO  J =  1,  K
         DO  I =  1,  K
             A( I,   J)  =  0.
             IF(I.EQ.1 .OR. J.EQ.1 .OR. I.EQ.K .OR. J.EQ.K) THEN
                B(I,  J) = 0.
             ELSE
                B(I,  J)  = ( 1. + I + J )
             ENDIF
         ENDDO
       ENDDO
       DO  IT = 1,  ITMAX
         EPS = 0.
CDVM$    PARALLEL  ( J,  I)   ON  A( I,  J), SHADOW_START SA,
CDVM$*   REDUCTION(REPS:MAX(EPS))
C        the loops iteration order is changed: at first
C        exported (boundary) elements of A are calculated and sent
C        then internal elements of array A are calculated     
         DO J =  2, K-1
            DO I =  2,  K-1
               EPS =  MAX  ( EPS,  ABS( B( I, J) - A( I, J)))
               A( I, J) = B( I, J)
            ENDDO
         ENDDO

CDVM$    REDUCTION_START  REPS
CDVM$    PARALLEL  ( J,  I)  ON B( I,   J),  SHADOW_WAIT SA
         DO  J =  2,  K-1
            DO I =  2,  K-1
               B(I, J) =   (A( I-1, J ) + A( I, J-1 ) + A( I+1, J ) +
     *            A( I, J+1 ))/4 
            ENDDO
         ENDDO  
CDVM$    REDUCTION_WAIT REPS
         IF  (EPS .LT. MAXEPS) GO TO  3
      ENDDO

    3 CONTINUE
CDVM$ PARALLEL  ( J, I)   ON  A( I,  J)
C		nest of parallel loops for initialization of arrays
       DO J =  1, K
         DO I =  1, K
             A1( I,   J)  =  0.
             IF(I.EQ.1 .OR. J.EQ.1 .OR. I.EQ.K .OR. J.EQ.K) THEN
                B1(I,  J) = 0.
             ELSE
                B1(I,  J)  = ( 1. + I + J )
             ENDIF
         ENDDO
       ENDDO
       DO  IT = 1,  ITMAX
         EPS1 = 0.
CDVM$    PARALLEL  ( J,  I)   ON  A1( I,  J), 
CDVM$*   REDUCTION (MAX(EPS1))
         DO J =  2, K-1
            DO I =  2,  K-1
               EPS1 =  MAX (EPS1,  ABS(B1( I, J) - A1(I, J)))
               A1(I, J) = B1( I, J)
            ENDDO
         ENDDO

CDVM$    PARALLEL ( J, I) ON B1 (I,  J),  SHADOW_RENEW(A1)
         DO  J =  2,  K-1
            DO I =  2,  K-1
               B1(I, J) = (A1 (I-1, J) + A1 (I, J-1) + A1 (I+1, J) +
     *            A1( I, J+1 ))/4 
            ENDDO
         ENDDO  
         IF  (EPS1 .LT. MAXEPS ) GO TO 4
      ENDDO
       
    4 IF (EPS .EQ. EPS1) THEN
          call ansyes('module21')
      ELSE
          call ansno('module21')
      ENDIF
      PRINT *,  '=== END OF MODULE21 =================='

      END

      subroutine ansyes(name)
      character(*) name
      print *,name,'  -  complete'
      end

      subroutine ansno(name)
      character(*) name
      print *,name,'  -  ***error'
      end
