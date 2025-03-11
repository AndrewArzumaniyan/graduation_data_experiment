      program CONS34

c      TESTING OF THE CONSISTENT CLAUSE'.
c      CHECKING DISTRIBUTION WITH THREE BLOCKS.

      print *,'===START OF CONS34========================'
C --------------------------------------------------
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
      call cons3401
C --------------------------------------------------
C consistent arrays with 1 dimensions
C two consistent arrays and one distributed array
      call cons3402
C --------------------------------------------------
C consistent arrays with 1 dimensions
C two consistent arrays and two distributed arrays
      call cons3403
C --------------------------------------------------
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
C big data
      call cons3404
C --------------------------------------------------
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
      call cons3405
C --------------------------------------------------
C consistent arrays with 2 dimensions
C two consistent arrays and one distributed array
      call cons3406
C --------------------------------------------------
C consistent arrays with 2 dimensions
C two consistent arrays and two distributed arrays
      call cons3407
C --------------------------------------------------
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
C big data
      call cons3408
C --------------------------------------------------
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
      call cons3409
C --------------------------------------------------
C consistent arrays with 3 dimensions
C two consistent arrays and one distributed array
      call cons3410
C --------------------------------------------------
C consistent arrays with 3 dimensions
C two consistent arrays and two distributed arrays
      call cons3411
C --------------------------------------------------
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
C big data
      call cons3412
C --------------------------------------------------
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
      call cons3413
C --------------------------------------------------
C consistent arrays with 4 dimensions
C two consistent arrays and one distributed array
      call cons3414
C --------------------------------------------------
C consistent arrays with 4 dimensions
C two consistent arrays and two distributed arrays
      call cons3415
C --------------------------------------------------
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
C big data
      call cons3416
C --------------------------------------------------

C --------------------------------------------------
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
      call cons3417
C --------------------------------------------------
C consistent arrays with 1 dimensions
C two consistent arrays and one distributed array
      call cons3418
C --------------------------------------------------
C consistent arrays with 1 dimensions
C two consistent arrays and two distributed arrays
      call cons3419
C --------------------------------------------------
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
C big data
      call cons3420
C --------------------------------------------------
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
      call cons3421
C --------------------------------------------------
C consistent arrays with 2 dimensions
C two consistent arrays and one distributed array
      call cons3422
C --------------------------------------------------
C consistent arrays with 2 dimensions
C two consistent arrays and two distributed arrays
      call cons3423
C --------------------------------------------------
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
C big data
      call cons3424
C --------------------------------------------------
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
      call cons3425
C --------------------------------------------------
C consistent arrays with 3 dimensions
C two consistent arrays and one distributed array
      call cons3426
C --------------------------------------------------
C consistent arrays with 3 dimensions
C two consistent arrays and two distributed arrays
      call cons3427
C --------------------------------------------------
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
C big data
      call cons3428
C --------------------------------------------------
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
      call cons3429
C --------------------------------------------------
C consistent arrays with 4 dimensions
C two consistent arrays and one distributed array
      call cons3430
C --------------------------------------------------
C consistent arrays with 4 dimensions
C two consistent arrays and two distributed arrays
      call cons3431
C --------------------------------------------------
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
C big data
      call cons3432
C --------------------------------------------------

C --------------------------------------------------
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
      call cons3433
C --------------------------------------------------
C consistent arrays with 1 dimensions
C two consistent arrays and one distributed array
      call cons3434
C --------------------------------------------------
C consistent arrays with 1 dimensions
C two consistent arrays and two distributed arrays
      call cons3435
C --------------------------------------------------
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
C big data
      call cons3436
C --------------------------------------------------
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
      call cons3437
C --------------------------------------------------
C consistent arrays with 2 dimensions
C two consistent arrays and one distributed array
      call cons3438
C --------------------------------------------------
C consistent arrays with 2 dimensions
C two consistent arrays and two distributed arrays
      call cons3439
C --------------------------------------------------
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
C big data
      call cons3440
C --------------------------------------------------
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
      call cons3441
C --------------------------------------------------
C consistent arrays with 3 dimensions
C two consistent arrays and one distributed array
      call cons3442
C --------------------------------------------------
C consistent arrays with 3 dimensions
C two consistent arrays and two distributed arrays
      call cons3443
C --------------------------------------------------
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
C big data
      call cons3444
C --------------------------------------------------
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
      call cons3445
C --------------------------------------------------
C consistent arrays with 4 dimensions
C two consistent arrays and one distributed array
      call cons3446
C --------------------------------------------------
C consistent arrays with 4 dimensions
C two consistent arrays and two distributed arrays
      call cons3447
C --------------------------------------------------
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
C big data
      call cons3448
C --------------------------------------------------

C --------------------------------------------------
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
      call cons3449
C --------------------------------------------------
C consistent arrays with 1 dimensions
C two consistent arrays and one distributed array
      call cons3450
C --------------------------------------------------
C consistent arrays with 1 dimensions
C two consistent arrays and two distributed arrays
      call cons3451
C --------------------------------------------------
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
C big data
      call cons3452
C --------------------------------------------------
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
      call cons3453
C --------------------------------------------------
C consistent arrays with 2 dimensions
C two consistent arrays and one distributed array
      call cons3454
C --------------------------------------------------
C consistent arrays with 2 dimensions
C two consistent arrays and two distributed arrays
      call cons3455
C --------------------------------------------------
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
C big data
      call cons3456
C --------------------------------------------------
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
      call cons3457
C --------------------------------------------------
C consistent arrays with 3 dimensions
C two consistent arrays and one distributed array
      call cons3458
C --------------------------------------------------
C consistent arrays with 3 dimensions
C two consistent arrays and two distributed arrays
      call cons3459
C --------------------------------------------------
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
C big data
      call cons3460
C --------------------------------------------------
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
      call cons3461
C --------------------------------------------------
C consistent arrays with 4 dimensions
C two consistent arrays and one distributed array
      call cons3462
C --------------------------------------------------
C consistent arrays with 4 dimensions
C two consistent arrays and two distributed arrays
      call cons3463
C --------------------------------------------------
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
C big data
      call cons3464
C --------------------------------------------------
C
      print *,'=== END OF CONS34 ========================= '
      end
C ---------------------------------------------cons3401
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
      subroutine CONS3401
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N),C(N))
      tname='CONS3401'
      DO I = 1, N
            C(I)=I+(N-1)+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1,1)
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO I = 1, N
            IF(V(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3402
C consistent arrays with 1 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3402
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:),W(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N),W(N),C(N))
      tname='CONS3402'
      DO I = 1, N
            C(I)=I+(N-1)+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1,1)
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(W(I))
      DO I = 1, N
            W(I) = B(I,1,1,1)
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO I = 1, N
            IF(V(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      DO I = 1, N
            IF(W(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons3403
C consistent arrays with 1 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3403
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:),W(:)
     *,C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N),W(N),C(N))
      tname='CONS3403'
      DO I = 1, N
            C(I)=I+(N-1)+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (L,K,J,I)   ON   A(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1,1)
      ENDDO
!DVM$   PARALLEL  (I)   ON  A(I,1,1,1), CONSISTENT(W(I))
      DO I = 1, N
            W(I) = A(I,1,1,1)
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO I = 1, N
            IF(V(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      DO I = 1, N
            IF(W(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons3404
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3404
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N),C(N))
      tname='CONS3404'
      DO I = 1, N
            C(I)=I+(N-1)+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1,1)
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO I = 1, N
            IF(V(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3405
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
      subroutine CONS3405
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N),C(N,N))
      tname='CONS3405'
      DO J = 1, N
      DO I = 1, N
            C(I,J)=I+(N-1)*J+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3406
C consistent arrays with 2 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3406
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:),W(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N,N),W(N,N),C(N,N))
      tname='CONS3406'
      DO J = 1, N
      DO I = 1, N
            C(I,J)=I+(N-1)*J+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(W(I,J))
      DO J = 1, N
      DO I = 1, N
            W(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons3407
C consistent arrays with 2 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3407
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:,:),W(:,:)
     *,C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N,N),W(N,N),C(N,N))
      tname='CONS3407'
      DO J = 1, N
      DO I = 1, N
            C(I,J)=I+(N-1)*J+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (L,K,J,I)   ON   A(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  A(I,J,1,1), CONSISTENT(W(I,J))
      DO J = 1, N
      DO I = 1, N
            W(I,J) = A(I,J,1,1)
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons3408
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3408
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N),C(N,N))
      tname='CONS3408'
      DO J = 1, N
      DO I = 1, N
            C(I,J)=I+(N-1)*J+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3409
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
      subroutine CONS3409
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N),C(N,N,N))
      tname='CONS3409'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3410
C consistent arrays with 3 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3410
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:),W(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N,N,N),W(N,N,N),C(N,N,N))
      tname='CONS3410'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(W(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons3411
C consistent arrays with 3 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3411
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:,:,:),W(:,:,:)
     *,C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N,N,N),W(N,N,N),C(N,N,N))
      tname='CONS3411'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (L,K,J,I)   ON   A(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  A(I,J,K,1), CONSISTENT(W(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K) = A(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons3412
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3412
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N),C(N,N,N))
      tname='CONS3412'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3413
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
      subroutine CONS3413
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N,N),C(N,N,N,N))
      tname='CONS3413'
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(V(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3414
C consistent arrays with 4 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3414
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:,:),W(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N,N,N,N),W(N,N,N,N),C(N,N,N,N))
      tname='CONS3414'
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(V(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(W(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons3415
C consistent arrays with 4 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3415
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:,:,:,:),W(:,:,:,:)
     *,C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N,N,N,N),W(N,N,N,N),C(N,N,N,N))
      tname='CONS3415'
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (L,K,J,I)   ON   A(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(V(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  A(I,J,K,L), CONSISTENT(W(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K,L)=A(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons3416
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3416
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( *, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N,N),C(N,N,N,N))
      tname='CONS3416'
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(V(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3417
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
      subroutine CONS3417
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N),C(N))
      tname='CONS3417'
      DO I = 1, N
            C(I)=I+(N-1)+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1,1)
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO I = 1, N
            IF(V(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3418
C consistent arrays with 1 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3418
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:),W(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N),W(N),C(N))
      tname='CONS3418'
      DO I = 1, N
            C(I)=I+(N-1)+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1,1)
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(W(I))
      DO I = 1, N
            W(I) = B(I,1,1,1)
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO I = 1, N
            IF(V(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      DO I = 1, N
            IF(W(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons3419
C consistent arrays with 1 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3419
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:),W(:)
     *,C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N),W(N),C(N))
      tname='CONS3419'
      DO I = 1, N
            C(I)=I+(N-1)+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (L,K,J,I)   ON   A(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1,1)
      ENDDO
!DVM$   PARALLEL  (I)   ON  A(I,1,1,1), CONSISTENT(W(I))
      DO I = 1, N
            W(I) = A(I,1,1,1)
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO I = 1, N
            IF(V(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      DO I = 1, N
            IF(W(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons3420
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3420
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N),C(N))
      tname='CONS3420'
      DO I = 1, N
            C(I)=I+(N-1)+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1,1)
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO I = 1, N
            IF(V(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3421
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
      subroutine CONS3421
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N),C(N,N))
      tname='CONS3421'
      DO J = 1, N
      DO I = 1, N
            C(I,J)=I+(N-1)*J+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3422
C consistent arrays with 2 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3422
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:),W(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N,N),W(N,N),C(N,N))
      tname='CONS3422'
      DO J = 1, N
      DO I = 1, N
            C(I,J)=I+(N-1)*J+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(W(I,J))
      DO J = 1, N
      DO I = 1, N
            W(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons3423
C consistent arrays with 2 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3423
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:,:),W(:,:)
     *,C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N,N),W(N,N),C(N,N))
      tname='CONS3423'
      DO J = 1, N
      DO I = 1, N
            C(I,J)=I+(N-1)*J+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (L,K,J,I)   ON   A(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  A(I,J,1,1), CONSISTENT(W(I,J))
      DO J = 1, N
      DO I = 1, N
            W(I,J) = A(I,J,1,1)
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons3424
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3424
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N),C(N,N))
      tname='CONS3424'
      DO J = 1, N
      DO I = 1, N
            C(I,J)=I+(N-1)*J+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3425
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
      subroutine CONS3425
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N),C(N,N,N))
      tname='CONS3425'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3426
C consistent arrays with 3 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3426
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:),W(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N,N,N),W(N,N,N),C(N,N,N))
      tname='CONS3426'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(W(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons3427
C consistent arrays with 3 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3427
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:,:,:),W(:,:,:)
     *,C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N,N,N),W(N,N,N),C(N,N,N))
      tname='CONS3427'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (L,K,J,I)   ON   A(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  A(I,J,K,1), CONSISTENT(W(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K) = A(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons3428
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3428
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N),C(N,N,N))
      tname='CONS3428'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3429
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
      subroutine CONS3429
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N,N),C(N,N,N,N))
      tname='CONS3429'
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(V(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3430
C consistent arrays with 4 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3430
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:,:),W(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N,N,N,N),W(N,N,N,N),C(N,N,N,N))
      tname='CONS3430'
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(V(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(W(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons3431
C consistent arrays with 4 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3431
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:,:,:,:),W(:,:,:,:)
     *,C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N,N,N,N),W(N,N,N,N),C(N,N,N,N))
      tname='CONS3431'
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (L,K,J,I)   ON   A(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(V(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  A(I,J,K,L), CONSISTENT(W(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K,L)=A(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons3432
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3432
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, *, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N,N),C(N,N,N,N))
      tname='CONS3432'
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(V(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3433
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
      subroutine CONS3433
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N),C(N))
      tname='CONS3433'
      DO I = 1, N
            C(I)=I+(N-1)+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1,1)
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO I = 1, N
            IF(V(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3434
C consistent arrays with 1 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3434
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:),W(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N),W(N),C(N))
      tname='CONS3434'
      DO I = 1, N
            C(I)=I+(N-1)+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1,1)
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(W(I))
      DO I = 1, N
            W(I) = B(I,1,1,1)
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO I = 1, N
            IF(V(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      DO I = 1, N
            IF(W(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons3435
C consistent arrays with 1 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3435
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:),W(:)
     *,C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N),W(N),C(N))
      tname='CONS3435'
      DO I = 1, N
            C(I)=I+(N-1)+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (L,K,J,I)   ON   A(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1,1)
      ENDDO
!DVM$   PARALLEL  (I)   ON  A(I,1,1,1), CONSISTENT(W(I))
      DO I = 1, N
            W(I) = A(I,1,1,1)
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO I = 1, N
            IF(V(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      DO I = 1, N
            IF(W(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons3436
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3436
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N),C(N))
      tname='CONS3436'
      DO I = 1, N
            C(I)=I+(N-1)+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1,1)
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO I = 1, N
            IF(V(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3437
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
      subroutine CONS3437
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N),C(N,N))
      tname='CONS3437'
      DO J = 1, N
      DO I = 1, N
            C(I,J)=I+(N-1)*J+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3438
C consistent arrays with 2 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3438
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:),W(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N,N),W(N,N),C(N,N))
      tname='CONS3438'
      DO J = 1, N
      DO I = 1, N
            C(I,J)=I+(N-1)*J+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(W(I,J))
      DO J = 1, N
      DO I = 1, N
            W(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons3439
C consistent arrays with 2 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3439
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:,:),W(:,:)
     *,C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N,N),W(N,N),C(N,N))
      tname='CONS3439'
      DO J = 1, N
      DO I = 1, N
            C(I,J)=I+(N-1)*J+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (L,K,J,I)   ON   A(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  A(I,J,1,1), CONSISTENT(W(I,J))
      DO J = 1, N
      DO I = 1, N
            W(I,J) = A(I,J,1,1)
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons3440
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3440
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N),C(N,N))
      tname='CONS3440'
      DO J = 1, N
      DO I = 1, N
            C(I,J)=I+(N-1)*J+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3441
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
      subroutine CONS3441
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N),C(N,N,N))
      tname='CONS3441'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3442
C consistent arrays with 3 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3442
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:),W(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N,N,N),W(N,N,N),C(N,N,N))
      tname='CONS3442'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(W(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons3443
C consistent arrays with 3 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3443
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:,:,:),W(:,:,:)
     *,C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N,N,N),W(N,N,N),C(N,N,N))
      tname='CONS3443'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (L,K,J,I)   ON   A(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  A(I,J,K,1), CONSISTENT(W(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K) = A(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons3444
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3444
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N),C(N,N,N))
      tname='CONS3444'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3445
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
      subroutine CONS3445
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N,N),C(N,N,N,N))
      tname='CONS3445'
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(V(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3446
C consistent arrays with 4 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3446
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:,:),W(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N,N,N,N),W(N,N,N,N),C(N,N,N,N))
      tname='CONS3446'
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(V(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(W(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons3447
C consistent arrays with 4 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3447
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:,:,:,:),W(:,:,:,:)
     *,C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N,N,N,N),W(N,N,N,N),C(N,N,N,N))
      tname='CONS3447'
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (L,K,J,I)   ON   A(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(V(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  A(I,J,K,L), CONSISTENT(W(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K,L)=A(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons3448
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3448
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, *, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N,N),C(N,N,N,N))
      tname='CONS3448'
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(V(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3449
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
      subroutine CONS3449
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N),C(N))
      tname='CONS3449'
      DO I = 1, N
            C(I)=I+(N-1)+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1,1)
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO I = 1, N
            IF(V(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3450
C consistent arrays with 1 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3450
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:),W(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N),W(N),C(N))
      tname='CONS3450'
      DO I = 1, N
            C(I)=I+(N-1)+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1,1)
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(W(I))
      DO I = 1, N
            W(I) = B(I,1,1,1)
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO I = 1, N
            IF(V(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      DO I = 1, N
            IF(W(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons3451
C consistent arrays with 1 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3451
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:),W(:)
     *,C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N),W(N),C(N))
      tname='CONS3451'
      DO I = 1, N
            C(I)=I+(N-1)+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (L,K,J,I)   ON   A(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1,1)
      ENDDO
!DVM$   PARALLEL  (I)   ON  A(I,1,1,1), CONSISTENT(W(I))
      DO I = 1, N
            W(I) = A(I,1,1,1)
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO I = 1, N
            IF(V(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      DO I = 1, N
            IF(W(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons3452
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3452
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N),C(N))
      tname='CONS3452'
      DO I = 1, N
            C(I)=I+(N-1)+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I,1,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1,1)
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO I = 1, N
            IF(V(I) .NE. C(I)) THEN
                  IERR = C(I)
                  EXIT
            ENDIF
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3453
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
      subroutine CONS3453
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N),C(N,N))
      tname='CONS3453'
      DO J = 1, N
      DO I = 1, N
            C(I,J)=I+(N-1)*J+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3454
C consistent arrays with 2 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3454
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:),W(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N,N),W(N,N),C(N,N))
      tname='CONS3454'
      DO J = 1, N
      DO I = 1, N
            C(I,J)=I+(N-1)*J+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(W(I,J))
      DO J = 1, N
      DO I = 1, N
            W(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons3455
C consistent arrays with 2 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3455
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:,:),W(:,:)
     *,C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N,N),W(N,N),C(N,N))
      tname='CONS3455'
      DO J = 1, N
      DO I = 1, N
            C(I,J)=I+(N-1)*J+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (L,K,J,I)   ON   A(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  A(I,J,1,1), CONSISTENT(W(I,J))
      DO J = 1, N
      DO I = 1, N
            W(I,J) = A(I,J,1,1)
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons3456
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3456
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N),C(N,N))
      tname='CONS3456'
      DO J = 1, N
      DO I = 1, N
            C(I,J)=I+(N-1)*J+(N-1)*(N-1)+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (J,I)   ON  B(I,J,1,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1,1)
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J) .NE. C(I,J)) THEN
                  IERR = C(I,J)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3457
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
      subroutine CONS3457
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N),C(N,N,N))
      tname='CONS3457'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3458
C consistent arrays with 3 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3458
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:),W(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N,N,N),W(N,N,N),C(N,N,N))
      tname='CONS3458'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(W(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons3459
C consistent arrays with 3 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3459
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:,:,:),W(:,:,:)
     *,C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N,N,N),W(N,N,N),C(N,N,N))
      tname='CONS3459'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (L,K,J,I)   ON   A(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  A(I,J,K,1), CONSISTENT(W(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K) = A(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons3460
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3460
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N),C(N,N,N))
      tname='CONS3460'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (K,J,I)   ON  B(I,J,K,1), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K,1)
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K) .NE. C(I,J,K)) THEN
                  IERR = C(I,J,K)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3461
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
      subroutine CONS3461
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N,N),C(N,N,N,N))
      tname='CONS3461'
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(V(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons3462
C consistent arrays with 4 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3462
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:,:),W(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N,N,N,N),W(N,N,N,N),C(N,N,N,N))
      tname='CONS3462'
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(V(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(W(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons3463
C consistent arrays with 4 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3463
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:,:,:,:),W(:,:,:,:)
     *,C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N,N,N,N),W(N,N,N,N),C(N,N,N,N))
      tname='CONS3463'
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (L,K,J,I)   ON   A(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(V(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  A(I,J,K,L), CONSISTENT(W(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K,L)=A(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V, W)
      IERR = ER
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(W(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons3464
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3464
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, * )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N,N),C(N,N,N,N))
      tname='CONS3464'
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (L,K,J,I)   ON   B(I,J,K,L)
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K,L)=I+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), CONSISTENT(V(I,J,K,L))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K,L)=B(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!dvm$ end region
!dvm$ get_actual (V)
      IERR = ER
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            IF(V(I,J,K,L) .NE. C(I,J,K,L)) THEN
                  IERR = C(I,J,K,L)
                  EXIT
            ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ERROR = ER
!DVM$  PARALLEL  (L,K,J,I)   ON  B(I,J,K,L), REDUCTION(MIN(ERROR))
      DO L = 1, N
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END

C --------------------------------------------------
      subroutine ansyes(name)
      character*8 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*8 name
      print *,name,'  -  ***error'
      end
