      program CONS33

c      TESTING OF THE CONSISTENT CLAUSE'.
c      CHECKING ( BLOCK, BLOCK, BLOCK ) DISTRIBUTION.

      print *,'===START OF CONS33========================'
C --------------------------------------------------
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
      call cons3301
C --------------------------------------------------
C consistent arrays with 1 dimensions
C two consistent arrays and one distributed array
      call cons3302
C --------------------------------------------------
C consistent arrays with 1 dimensions
C two consistent arrays and two distributed arrays
      call cons3303
C --------------------------------------------------
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
C big data
      call cons3304
C --------------------------------------------------
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
      call cons3305
C --------------------------------------------------
C consistent arrays with 2 dimensions
C two consistent arrays and one distributed array
      call cons3306
C --------------------------------------------------
C consistent arrays with 2 dimensions
C two consistent arrays and two distributed arrays
      call cons3307
C --------------------------------------------------
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
C big data
      call cons3308
C --------------------------------------------------
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
      call cons3309
C --------------------------------------------------
C consistent arrays with 3 dimensions
C two consistent arrays and one distributed array
      call cons3310
C --------------------------------------------------
C consistent arrays with 3 dimensions
C two consistent arrays and two distributed arrays
      call cons3311
C --------------------------------------------------
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
C big data
      call cons3312
C --------------------------------------------------
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
      call cons3313
C --------------------------------------------------
C consistent arrays with 4 dimensions
C two consistent arrays and one distributed array
      call cons3314
C --------------------------------------------------
C consistent arrays with 4 dimensions
C two consistent arrays and two distributed arrays
      call cons3315
C --------------------------------------------------
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
C big data
      call cons3316
C --------------------------------------------------

C
      print *,'=== END OF CONS33 ========================= '
      end
C ---------------------------------------------cons3301
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
      subroutine CONS3301
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:),V(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N),V(N),C(N))
      tname='CONS3301'
      DO I = 1, N
            C(I)  = I + (N - 1) + (N - 1) * (N - 1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (K,J,I)   ON   B(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (I) ON B(I,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1)
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
!DVM$  PARALLEL  (K,J,I)   ON  B(I,J,K), REDUCTION(MIN(ERROR))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
C ---------------------------------------------cons3302
C consistent arrays with 1 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3302
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:),V(:),W(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N),V(N),W(N),C(N))
      tname='CONS3302'
      DO I = 1, N
            C(I)  = I + (N - 1) + (N - 1) * (N - 1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (K,J,I)   ON   B(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (I) ON B(I,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1)
      ENDDO
!DVM$   PARALLEL (I) ON B(I,1,1), CONSISTENT(W(I))
      DO I = 1, N
            W(I) = B(I,1,1)
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
!DVM$  PARALLEL  (K,J,I)   ON  B(I,J,K), REDUCTION(MIN(ERROR))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
C ---------------------------------------------cons3303
C consistent arrays with 1 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3303
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:),A(:,:,:),V(:),W(:)
     *,C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N),A(N,N,N),V(N),W(N),C(N))
      tname='CONS3303'
      DO I = 1, N
            C(I)  = I + (N - 1) + (N - 1) * (N - 1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (K,J,I)   ON   B(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (K,J,I)   ON   A(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (I) ON B(I,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1)
      ENDDO
!DVM$   PARALLEL (I) ON A(I,1,1), CONSISTENT(W(I))
      DO I = 1, N
            W(I) = A(I,1,1)
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
!DVM$  PARALLEL  (K,J,I)   ON  B(I,J,K), REDUCTION(MIN(ERROR))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
C ---------------------------------------------cons3304
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3304
      INTEGER,PARAMETER:: N=16, ER=100000
      integer,allocatable:: B(:,:,:),V(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N),V(N),C(N))
      tname='CONS3304'
      DO I = 1, N
            C(I)  = I + (N - 1) + (N - 1) * (N - 1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (K,J,I)   ON   B(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (I) ON B(I,1,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1,1)
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
!DVM$  PARALLEL  (K,J,I)   ON  B(I,J,K), REDUCTION(MIN(ERROR))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
C ---------------------------------------------cons3305
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
      subroutine CONS3305
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:),V(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N),V(N,N),C(N,N))
      tname='CONS3305'
      DO J = 1, N
      DO I = 1, N
            C(I,J)  = I + (N - 1) * J + (N - 1) * (N - 1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (K,J,I)   ON   B(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1)
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
!DVM$  PARALLEL  (K,J,I)   ON  B(I,J,K), REDUCTION(MIN(ERROR))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
C ---------------------------------------------cons3306
C consistent arrays with 2 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3306
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:),V(:,:),W(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N),V(N,N),W(N,N),C(N,N))
      tname='CONS3306'
      DO J = 1, N
      DO I = 1, N
            C(I,J)  = I + (N - 1) * J + (N - 1) * (N - 1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (K,J,I)   ON   B(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1)
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J,1), CONSISTENT(W(I,J))
      DO J = 1, N
      DO I = 1, N
            W(I,J) = B(I,J,1)
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
!DVM$  PARALLEL  (K,J,I)   ON  B(I,J,K), REDUCTION(MIN(ERROR))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
C ---------------------------------------------cons3307
C consistent arrays with 2 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3307
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:),A(:,:,:),V(:,:),W(:,:)
     *,C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N),A(N,N,N),V(N,N),W(N,N),C(N,N))
      tname='CONS3307'
      DO J = 1, N
      DO I = 1, N
            C(I,J)  = I + (N - 1) * J + (N - 1) * (N - 1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (K,J,I)   ON   B(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (K,J,I)   ON   A(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1)
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON A(I,J,1), CONSISTENT(W(I,J))
      DO J = 1, N
      DO I = 1, N
            W(I,J) = A(I,J,1)
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
!DVM$  PARALLEL  (K,J,I)   ON  B(I,J,K), REDUCTION(MIN(ERROR))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
C ---------------------------------------------cons3308
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3308
      INTEGER,PARAMETER:: N=16, ER=100000
      integer,allocatable:: B(:,:,:),V(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N),V(N,N),C(N,N))
      tname='CONS3308'
      DO J = 1, N
      DO I = 1, N
            C(I,J)  = I + (N - 1) * J + (N - 1) * (N - 1)
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (K,J,I)   ON   B(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J,1), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J,1)
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
!DVM$  PARALLEL  (K,J,I)   ON  B(I,J,K), REDUCTION(MIN(ERROR))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
C ---------------------------------------------cons3309
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
      subroutine CONS3309
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:),V(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N),V(N,N,N),C(N,N,N))
      tname='CONS3309'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)  = I + (N - 1) * J + (N - 1) * (N - 1) * K
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (K,J,I)   ON   B(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (K,J,I) ON B(I,J,K), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K)
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
!DVM$  PARALLEL  (K,J,I)   ON  B(I,J,K), REDUCTION(MIN(ERROR))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
C ---------------------------------------------cons3310
C consistent arrays with 3 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3310
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:),V(:,:,:),W(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N),V(N,N,N),W(N,N,N),C(N,N,N))
      tname='CONS3310'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)  = I + (N - 1) * J + (N - 1) * (N - 1) * K
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (K,J,I)   ON   B(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (K,J,I) ON B(I,J,K), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K)
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (K,J,I) ON B(I,J,K), CONSISTENT(W(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K) = B(I,J,K)
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
!DVM$  PARALLEL  (K,J,I)   ON  B(I,J,K), REDUCTION(MIN(ERROR))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
C ---------------------------------------------cons3311
C consistent arrays with 3 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3311
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:),A(:,:,:),V(:,:,:),W(:,:,:)
     *,C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N),A(N,N,N),V(N,N,N),W(N,N,N),C(N,N,N))
      tname='CONS3311'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)  = I + (N - 1) * J + (N - 1) * (N - 1) * K
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (K,J,I)   ON   B(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (K,J,I)   ON   A(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (K,J,I) ON B(I,J,K), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K)
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (K,J,I) ON A(I,J,K), CONSISTENT(W(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            W(I,J,K) = A(I,J,K)
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
!DVM$  PARALLEL  (K,J,I)   ON  B(I,J,K), REDUCTION(MIN(ERROR))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
C ---------------------------------------------cons3312
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3312
      INTEGER,PARAMETER:: N=16, ER=100000
      integer,allocatable:: B(:,:,:),V(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N),V(N,N,N),C(N,N,N))
      tname='CONS3312'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)  = I + (N - 1) * J + (N - 1) * (N - 1) * K
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (K,J,I)   ON   B(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (K,J,I) ON B(I,J,K), CONSISTENT(V(I,J,K))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            V(I,J,K) = B(I,J,K)
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
!DVM$  PARALLEL  (K,J,I)   ON  B(I,J,K), REDUCTION(MIN(ERROR))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
C ---------------------------------------------cons3313
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
      subroutine CONS3313
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:),V(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N),V(N,N,N,N),C(N,N,N,N))
      tname='CONS3313'
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
!DVM$   PARALLEL    (K,J,I)   ON   B(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (K,J,I) ON B(I,J,K), private(L), CONSISTENT(V(I,J,K,:))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
      DO L = 1, N
            V(I,J,K,L)=B(I,J,K)+(N-1)*(N-1)*(N-1)*L
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
!DVM$  PARALLEL  (K,J,I)   ON  B(I,J,K), REDUCTION(MIN(ERROR))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
C ---------------------------------------------cons3314
C consistent arrays with 4 dimensions
C two consistent arrays and one distributed array
      subroutine CONS3314
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:),V(:,:,:,:),W(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N),V(N,N,N,N),W(N,N,N,N),C(N,N,N,N))
      tname='CONS3314'
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
!DVM$   PARALLEL    (K,J,I)   ON   B(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (K,J,I) ON B(I,J,K), private(L), CONSISTENT(V(I,J,K,:))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
      DO L = 1, N
            V(I,J,K,L)=B(I,J,K)+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (K,J,I) ON B(I,J,K), private(L), CONSISTENT(W(I,J,K,:))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
      DO L = 1, N
            W(I,J,K,L)=B(I,J,K)+(N-1)*(N-1)*(N-1)*L
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
!DVM$  PARALLEL  (K,J,I)   ON  B(I,J,K), REDUCTION(MIN(ERROR))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
C ---------------------------------------------cons3315
C consistent arrays with 4 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS3315
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:),A(:,:,:),V(:,:,:,:),W(:,:,:,:)
     *,C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N),A(N,N,N),V(N,N,N,N),W(N,N,N,N),C(N,N,N,N))
      tname='CONS3315'
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
!DVM$   PARALLEL    (K,J,I)   ON   B(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL    (K,J,I)   ON   A(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            A(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (K,J,I) ON B(I,J,K), private(L), CONSISTENT(V(I,J,K,:))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
      DO L = 1, N
            V(I,J,K,L)=B(I,J,K)+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (K,J,I) ON A(I,J,K), private(L), CONSISTENT(W(I,J,K,:))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
      DO L = 1, N
            W(I,J,K,L)=A(I,J,K)+(N-1)*(N-1)*(N-1)*L
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
!DVM$  PARALLEL  (K,J,I)   ON  B(I,J,K), REDUCTION(MIN(ERROR))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
C ---------------------------------------------cons3316
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS3316
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:),V(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N),V(N,N,N,N),C(N,N,N,N))
      tname='CONS3316'
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
!DVM$   PARALLEL    (K,J,I)   ON   B(I,J,K)
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            B(I,J,K) = I+(N-1)*J+(N-1)*(N-1)*K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (K,J,I) ON B(I,J,K), private(L), CONSISTENT(V(I,J,K,:))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
      DO L = 1, N
            V(I,J,K,L)=B(I,J,K)+(N-1)*(N-1)*(N-1)*L
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
!DVM$  PARALLEL  (K,J,I)   ON  B(I,J,K), REDUCTION(MIN(ERROR))
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
