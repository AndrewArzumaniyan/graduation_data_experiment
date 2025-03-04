      program CONS11

c     TESTING OF THE CONSISTENT CLAUSE'.
c      CHECKING ( BLOCK ) DISTRIBUTION.

      print *,'===START OF CONS11========================'
C --------------------------------------------------
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
      call cons1101
C --------------------------------------------------
C consistent arrays with 1 dimensions
C two consistent arrays and one distributed array
      call cons1102
C --------------------------------------------------
C consistent arrays with 1 dimensions
C two consistent arrays and two distributed arrays
      call cons1103
C --------------------------------------------------
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
C big data
      call cons1104
C --------------------------------------------------
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
      call cons1105
C --------------------------------------------------
C consistent arrays with 2 dimensions
C two consistent arrays and one distributed array
      call cons1106
C --------------------------------------------------
C consistent arrays with 2 dimensions
C two consistent arrays and two distributed arrays
      call cons1107
C --------------------------------------------------
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
C big data
      call cons1108
C --------------------------------------------------
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
      call cons1109
C --------------------------------------------------
C consistent arrays with 3 dimensions
C two consistent arrays and one distributed array
      call cons1110
C --------------------------------------------------
C consistent arrays with 3 dimensions
C two consistent arrays and two distributed arrays
      call cons1111
C --------------------------------------------------
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
C big data
      call cons1112
C --------------------------------------------------
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
      call cons1113
C --------------------------------------------------
C consistent arrays with 4 dimensions
C two consistent arrays and one distributed array
      call cons1114
C --------------------------------------------------
C consistent arrays with 4 dimensions
C two consistent arrays and two distributed arrays
      call cons1115
C --------------------------------------------------
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
C big data
      call cons1116
C --------------------------------------------------

C
      print *,'=== END OF CONS11 ========================= '
      end
C ---------------------------------------------cons1101
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
      subroutine CONS1101
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:),V(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N),V(N),C(N))
      tname='CONS1101'
      DO I = 1, N
            C(I)  = I
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (I)   ON   B(I)
      DO I = 1, N
            B(I) = I
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I)
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
!DVM$  PARALLEL  (I)   ON  B(I), REDUCTION(MIN(ERROR))
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons1102
C consistent arrays with 1 dimensions
C two consistent arrays and one distributed array
      subroutine CONS1102
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:),V(:),W(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N),V(N),W(N),C(N))
      tname='CONS1102'
      DO I = 1, N
            C(I)  = I
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (I)   ON   B(I)
      DO I = 1, N
                  B(I) = I
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I)
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), CONSISTENT(W(I))
      DO I = 1, N
            W(I) = B(I)
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
!DVM$  PARALLEL  (I)   ON  B(I), REDUCTION(MIN(ERROR))
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons1103
C consistent arrays with 1 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS1103
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:),A(:),V(:),W(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N),A(N),V(N),W(N),C(N))
      tname='CONS1103'
      DO I = 1, N
            C(I)  = I
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (I)   ON   B(I)
      DO I = 1, N
                  B(I) = I
      ENDDO
!DVM$   PARALLEL    (I)   ON   A(I)
      DO I = 1, N
                  A(I) = I
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I)
      ENDDO
!DVM$   PARALLEL  (I)   ON  A(I), CONSISTENT(W(I))
      DO I = 1, N
            W(I) = A(I)
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
!DVM$  PARALLEL  (I)   ON  B(I), REDUCTION(MIN(ERROR))
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons1104
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS1104
      INTEGER,PARAMETER:: N=16, ER=10000
      integer,allocatable:: B(:),V(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N),V(N),C(N))
      tname='CONS1104'
      DO I = 1, N
            C(I)  = I
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (I)   ON   B(I)
      DO I = 1, N
                  B(I) = I
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I)
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
!DVM$  PARALLEL  (I)   ON  B(I), REDUCTION(MIN(ERROR))
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons1105
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
      subroutine CONS1105
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:),V(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N),V(N,N),C(N,N))
      tname='CONS1105'
      DO J = 1, N
      DO I = 1, N
                  C(I,J)  = I + (N - 1) * J
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (I)   ON   B(I)
      DO I = 1, N
            B(I) = I
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), private(J), CONSISTENT(V(I,:))
      DO I = 1, N
      DO J = 1, N
                  V(I,J) = B(I) + (N - 1) * J
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
!DVM$  PARALLEL  (I)   ON  B(I), REDUCTION(MIN(ERROR))
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons1106
C consistent arrays with 2 dimensions
C two consistent arrays and one distributed array
      subroutine CONS1106
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:),V(:,:),W(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N),V(N,N),W(N,N),C(N,N))
      tname='CONS1106'
      DO J = 1, N
      DO I = 1, N
                  C(I,J)  = I + (N - 1) * J
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (I)   ON   B(I)
      DO I = 1, N
                  B(I) = I
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), private(J), CONSISTENT(V(I,:))
      DO I = 1, N
      DO J = 1, N
                  V(I,J) = B(I) + (N - 1) * J
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), private(J), CONSISTENT(W(I,:))
      DO I = 1, N
      DO J = 1, N
                  W(I,J) = B(I) + (N - 1) * J
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
!DVM$  PARALLEL  (I)   ON  B(I), REDUCTION(MIN(ERROR))
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons1107
C consistent arrays with 2 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS1107
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:),A(:),V(:,:),W(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N),A(N),V(N,N),W(N,N),C(N,N))
      tname='CONS1107'
      DO J = 1, N
      DO I = 1, N
                  C(I,J)  = I + (N - 1) * J
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (I)   ON   B(I)
      DO I = 1, N
                  B(I) = I
      ENDDO
!DVM$   PARALLEL    (I)   ON   A(I)
      DO I = 1, N
                  A(I) = I
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), private(J), CONSISTENT(V(I,:))
      DO I = 1, N
      DO J = 1, N
                  V(I,J) = B(I) + (N - 1) * J
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  A(I), private(J), CONSISTENT(W(I,:))
      DO I = 1, N
      DO J = 1, N
                  W(I,J) = A(I) + (N - 1) * J
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
!DVM$  PARALLEL  (I)   ON  B(I), REDUCTION(MIN(ERROR))
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons1108
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS1108
      INTEGER,PARAMETER:: N=16, ER=10000
      integer,allocatable:: B(:),V(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N),V(N,N),C(N,N))
      tname='CONS1108'
      DO J = 1, N
      DO I = 1, N
                  C(I,J)  = I + (N - 1) * J
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (I)   ON   B(I)
      DO I = 1, N
                  B(I) = I
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), private(J), CONSISTENT(V(I,:))
      DO I = 1, N
      DO J = 1, N
                  V(I,J) = B(I) + (N - 1) * J
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
!DVM$  PARALLEL  (I)   ON  B(I), REDUCTION(MIN(ERROR))
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons1109
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
      subroutine CONS1109
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:),V(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N),V(N,N,N),C(N,N,N))
      tname='CONS1109'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
                  C(I,J,K)  = I + (N - 1) * J + (N - 1) * (N - 1) * K
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (I)   ON   B(I)
      DO I = 1, N
            B(I) = I
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), private(J,K), CONSISTENT(V(I,:,:))
      DO I = 1, N
      DO J = 1, N
      DO K = 1, N
            V(I,J,K) = B(I) + (N - 1) * J + (N - 1) * (N - 1) * K
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
!DVM$  PARALLEL  (I)   ON  B(I), REDUCTION(MIN(ERROR))
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons1110
C consistent arrays with 3 dimensions
C two consistent arrays and one distributed array
      subroutine CONS1110
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:),V(:,:,:),W(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N),V(N,N,N),W(N,N,N),C(N,N,N))
      tname='CONS1110'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
                  C(I,J,K)  = I + (N - 1) * J + (N - 1) * (N - 1) * K
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (I)   ON   B(I)
      DO I = 1, N
                  B(I) = I
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), private(J,K), CONSISTENT(V(I,:,:))
      DO I = 1, N
      DO J = 1, N
      DO K = 1, N
            V(I,J,K) = B(I) + (N - 1) * J + (N - 1) * (N - 1) * K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), private(J,K), CONSISTENT(W(I,:,:))
      DO I = 1, N
      DO J = 1, N
      DO K = 1, N
            W(I,J,K) = B(I) + (N - 1) * J + (N - 1) * (N - 1) * K
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
!DVM$  PARALLEL  (I)   ON  B(I), REDUCTION(MIN(ERROR))
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons1111
C consistent arrays with 3 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS1111
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:),A(:),V(:,:,:),W(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N),A(N),V(N,N,N),W(N,N,N),C(N,N,N))
      tname='CONS1111'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
                  C(I,J,K)  = I + (N - 1) * J + (N - 1) * (N - 1) * K
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (I)   ON   B(I)
      DO I = 1, N
                  B(I) = I
      ENDDO
!DVM$   PARALLEL    (I)   ON   A(I)
      DO I = 1, N
                  A(I) = I
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), private(J,K), CONSISTENT(V(I,:,:))
      DO I = 1, N
      DO J = 1, N
      DO K = 1, N
            V(I,J,K) = B(I) + (N - 1) * J + (N - 1) * (N - 1) * K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  A(I), private(J,K), CONSISTENT(W(I,:,:))
      DO I = 1, N
      DO J = 1, N
      DO K = 1, N
            W(I,J,K) = A(I) + (N - 1) * J + (N - 1) * (N - 1) * K
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
!DVM$  PARALLEL  (I)   ON  B(I), REDUCTION(MIN(ERROR))
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons1112
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS1112
      INTEGER,PARAMETER:: N=16, ER=100000
      integer,allocatable:: B(:),V(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N),V(N,N,N),C(N,N,N))
      tname='CONS1112'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
                  C(I,J,K)  = I + (N - 1) * J + (N - 1) * (N - 1) * K
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (I)   ON   B(I)
      DO I = 1, N
                  B(I) = I
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), private(J,K), CONSISTENT(V(I,:,:))
      DO I = 1, N
      DO J = 1, N
      DO K = 1, N
            V(I,J,K) = B(I) + (N - 1) * J + (N - 1) * (N - 1) * K
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
!DVM$  PARALLEL  (I)   ON  B(I), REDUCTION(MIN(ERROR))
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons1113
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
      subroutine CONS1113
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:),V(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N),V(N,N,N,N),C(N,N,N,N))
      tname='CONS1113'
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
!DVM$   PARALLEL    (I)   ON   B(I)
      DO I = 1, N
            B(I) = I
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), private(J,K,L), CONSISTENT(V(I,:,:,:))
      DO I = 1, N
      DO J = 1, N
      DO K = 1, N
      DO L = 1, N
            V(I,J,K,L)=B(I)+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
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
!DVM$  PARALLEL  (I)   ON  B(I), REDUCTION(MIN(ERROR))
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons1114
C consistent arrays with 4 dimensions
C two consistent arrays and one distributed array
      subroutine CONS1114
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:),V(:,:,:,:),W(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N),V(N,N,N,N),W(N,N,N,N),C(N,N,N,N))
      tname='CONS1114'
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
!DVM$   PARALLEL    (I)   ON   B(I)
      DO I = 1, N
                  B(I) = I
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), private(J,K,L), CONSISTENT(V(I,:,:,:))
      DO I = 1, N
      DO J = 1, N
      DO K = 1, N
      DO L = 1, N
            V(I,J,K,L)=B(I)+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), private(J,K,L), CONSISTENT(W(I,:,:,:))
      DO I = 1, N
      DO J = 1, N
      DO K = 1, N
      DO L = 1, N
            W(I,J,K,L)=B(I)+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
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
!DVM$  PARALLEL  (I)   ON  B(I), REDUCTION(MIN(ERROR))
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons1115
C consistent arrays with 4 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS1115
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:),A(:),V(:,:,:,:),W(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N),A(N),V(N,N,N,N),W(N,N,N,N),C(N,N,N,N))
      tname='CONS1115'
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
!DVM$   PARALLEL    (I)   ON   B(I)
      DO I = 1, N
                  B(I) = I
      ENDDO
!DVM$   PARALLEL    (I)   ON   A(I)
      DO I = 1, N
                  A(I) = I
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), private(J,K,L), CONSISTENT(V(I,:,:,:))
      DO I = 1, N
      DO J = 1, N
      DO K = 1, N
      DO L = 1, N
            V(I,J,K,L)=B(I)+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL  (I)   ON  A(I), private(J,K,L), CONSISTENT(W(I,:,:,:))
      DO I = 1, N
      DO J = 1, N
      DO K = 1, N
      DO L = 1, N
            W(I,J,K,L)=A(I)+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
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
!DVM$  PARALLEL  (I)   ON  B(I), REDUCTION(MIN(ERROR))
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons1116
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS1116
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:),V(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N),V(N,N,N,N),C(N,N,N,N))
      tname='CONS1116'
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
!DVM$   PARALLEL    (I)   ON   B(I)
      DO I = 1, N
                  B(I) = I
      ENDDO
!DVM$   PARALLEL  (I)   ON  B(I), private(J,K,L), CONSISTENT(V(I,:,:,:))
      DO I = 1, N
      DO J = 1, N
      DO K = 1, N
      DO L = 1, N
            V(I,J,K,L)=B(I)+(N-1)*J+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
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
!DVM$  PARALLEL  (I)   ON  B(I), REDUCTION(MIN(ERROR))
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
