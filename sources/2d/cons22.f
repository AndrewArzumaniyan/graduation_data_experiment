      program CONS22

c      TESTING OF THE CONSISTENT CLAUSE'.
c      CHECKING ( BLOCK, BLOCK ) DISTRIBUTION.

      print *,'===START OF CONS22========================'
C --------------------------------------------------
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
      call cons2201
C --------------------------------------------------
C consistent arrays with 1 dimensions
C two consistent arrays and one distributed array
      call cons2202
C --------------------------------------------------
C consistent arrays with 1 dimensions
C two consistent arrays and two distributed arrays
      call cons2203
C --------------------------------------------------
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
C big data
      call cons2204
C --------------------------------------------------
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
      call cons2205
C --------------------------------------------------
C consistent arrays with 2 dimensions
C two consistent arrays and one distributed array
      call cons2206
C --------------------------------------------------
C consistent arrays with 2 dimensions
C two consistent arrays and two distributed arrays
      call cons2207
C --------------------------------------------------
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
C big data
      call cons2208
C --------------------------------------------------
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
      call cons2209
C --------------------------------------------------
C consistent arrays with 3 dimensions
C two consistent arrays and one distributed array
      call cons2210
C --------------------------------------------------
C consistent arrays with 3 dimensions
C two consistent arrays and two distributed arrays
      call cons2211
C --------------------------------------------------
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
C big data
      call cons2212
C --------------------------------------------------
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
      call cons2213
C --------------------------------------------------
C consistent arrays with 4 dimensions
C two consistent arrays and one distributed array
      call cons2214
C --------------------------------------------------
C consistent arrays with 4 dimensions
C two consistent arrays and two distributed arrays
      call cons2215
C --------------------------------------------------
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
C big data
      call cons2216
C --------------------------------------------------

C
      print *,'=== END OF CONS22 ========================= '
      end
C ---------------------------------------------cons2201
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
      subroutine CONS2201
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:),V(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N),V(N),C(N))
      tname='CONS2201'
      DO I = 1, N
            C(I)  = I + (N - 1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (J,I)   ON   B(I,J)
      DO J = 1, N
      DO I = 1, N
            B(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL (I) ON B(I,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1)
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
!DVM$  PARALLEL  (J,I)   ON  B(I,J), REDUCTION(MIN(ERROR))
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons2202
C consistent arrays with 1 dimensions
C two consistent arrays and one distributed array
      subroutine CONS2202
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:),V(:),W(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N),V(N),W(N),C(N))
      tname='CONS2202'
      DO I = 1, N
            C(I)  = I + (N - 1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (J,I)   ON   B(I,J)
      DO J = 1, N
      DO I = 1, N
            B(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL (I) ON B(I,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1)
      ENDDO
!DVM$   PARALLEL (I) ON B(I,1), CONSISTENT(W(I))
      DO I = 1, N
            W(I) = B(I,1)
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
!DVM$  PARALLEL  (J,I)   ON  B(I,J), REDUCTION(MIN(ERROR))
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons2203
C consistent arrays with 1 dimensions
      subroutine CONS2203
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:),A(:,:),V(:),W(:)
     *,C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N),A(N,N),V(N),W(N),C(N))
      tname='CONS2203'
      DO I = 1, N
            C(I)  = I + (N - 1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (J,I)   ON   B(I,J)
      DO J = 1, N
      DO I = 1, N
            B(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL    (J,I)   ON   A(I,J)
      DO J = 1, N
      DO I = 1, N
            A(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL (I) ON B(I,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1)
      ENDDO
!DVM$ PARALLEL (I) ON A(I,1), CONSISTENT(W(I))
      DO I = 1, N
            W(I) = A(I,1)
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
!DVM$  PARALLEL  (J,I)   ON  B(I,J), REDUCTION(MIN(ERROR))
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons2204
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS2204
      INTEGER,PARAMETER:: N=16, ER=10000
      integer,allocatable:: B(:,:),V(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N),V(N),C(N))
      tname='CONS2204'
      DO I = 1, N
            C(I)  = I + (N - 1)
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (J,I)   ON   B(I,J)
      DO J = 1, N
      DO I = 1, N
            B(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL (I) ON B(I,1), CONSISTENT(V(I))
      DO I = 1, N
            V(I) = B(I,1)
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
!DVM$  PARALLEL  (J,I)   ON  B(I,J), REDUCTION(MIN(ERROR))
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons2205
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
      subroutine CONS2205
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:),V(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N),V(N,N),C(N,N))
      tname='CONS2205'
      DO J = 1, N
      DO I = 1, N
            C(I,J)  = I + (N - 1) * J
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (J,I)   ON   B(I,J)
      DO J = 1, N
      DO I = 1, N
            B(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J)
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
!DVM$  PARALLEL  (J,I)   ON  B(I,J), REDUCTION(MIN(ERROR))
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons2206
C consistent arrays with 2 dimensions
C two consistent arrays and one distributed array
      subroutine CONS2206
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:),V(:,:),W(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N),V(N,N),W(N,N),C(N,N))
      tname='CONS2206'
      DO J = 1, N
      DO I = 1, N
            C(I,J)  = I + (N - 1) * J
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (J,I)   ON   B(I,J)
      DO J = 1, N
      DO I = 1, N
            B(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J)
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J), CONSISTENT(W(I,J))
      DO J = 1, N
      DO I = 1, N
            W(I,J) = B(I,J)
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
!DVM$  PARALLEL  (J,I)   ON  B(I,J), REDUCTION(MIN(ERROR))
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons2207
C consistent arrays with 2 dimensions
      subroutine CONS2207
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:),A(:,:),V(:,:),W(:,:)
     *,C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N),A(N,N),V(N,N),W(N,N),C(N,N))
      tname='CONS2207'
      DO J = 1, N
      DO I = 1, N
            C(I,J)  = I + (N - 1) * J
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (J,I)   ON   B(I,J)
      DO J = 1, N
      DO I = 1, N
            B(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL    (J,I)   ON   A(I,J)
      DO J = 1, N
      DO I = 1, N
            A(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J)
      ENDDO
      ENDDO
!DVM$ PARALLEL (J,I) ON A(I,J), CONSISTENT(W(I,J))
      DO J = 1, N
      DO I = 1, N
            W(I,J) = A(I,J)
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
!DVM$  PARALLEL  (J,I)   ON  B(I,J), REDUCTION(MIN(ERROR))
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons2208
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS2208
      INTEGER,PARAMETER:: N=16, ER=10000
      integer,allocatable:: B(:,:),V(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N),V(N,N),C(N,N))
      tname='CONS2208'
      DO J = 1, N
      DO I = 1, N
            C(I,J)  = I + (N - 1) * J
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (J,I)   ON   B(I,J)
      DO J = 1, N
      DO I = 1, N
            B(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J), CONSISTENT(V(I,J))
      DO J = 1, N
      DO I = 1, N
            V(I,J) = B(I,J)
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
!DVM$  PARALLEL  (J,I)   ON  B(I,J), REDUCTION(MIN(ERROR))
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons2209
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
      subroutine CONS2209
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:),V(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N),V(N,N,N),C(N,N,N))
      tname='CONS2209'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)  = I + (N - 1) * J + (N - 1) * (N - 1) * K
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (J,I)   ON   B(I,J)
      DO J = 1, N
      DO I = 1, N
            B(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J), private(K), CONSISTENT(V(I,J,:))
      DO J = 1, N
      DO I = 1, N
      DO K = 1, N
            V(I,J,K) = B(I,J) + (N - 1) * (N - 1) * K
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
!DVM$  PARALLEL  (J,I)   ON  B(I,J), REDUCTION(MIN(ERROR))
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons2210
C consistent arrays with 3 dimensions
C two consistent arrays and one distributed array
      subroutine CONS2210
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:),V(:,:,:),W(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N),V(N,N,N),W(N,N,N),C(N,N,N))
      tname='CONS2210'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)  = I + (N - 1) * J + (N - 1) * (N - 1) * K
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (J,I)   ON   B(I,J)
      DO J = 1, N
      DO I = 1, N
            B(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J), private(K), CONSISTENT(V(I,J,:))
      DO J = 1, N
      DO I = 1, N
      DO K = 1, N
            V(I,J,K) = B(I,J) + (N - 1) * (N - 1) * K
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J), private(K), CONSISTENT(W(I,J,:))
      DO J = 1, N
      DO I = 1, N
      DO K = 1, N
            W(I,J,K) = B(I,J) + (N - 1) * (N - 1) * K
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
!DVM$  PARALLEL  (J,I)   ON  B(I,J), REDUCTION(MIN(ERROR))
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons2211
C consistent arrays with 3 dimensions
      subroutine CONS2211
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:),A(:,:),V(:,:,:),W(:,:,:)
     *,C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N),A(N,N),V(N,N,N),W(N,N,N),C(N,N,N))
      tname='CONS2211'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)  = I + (N - 1) * J + (N - 1) * (N - 1) * K
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (J,I)   ON   B(I,J)
      DO J = 1, N
      DO I = 1, N
            B(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL    (J,I)   ON   A(I,J)
      DO J = 1, N
      DO I = 1, N
            A(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J), private(K), CONSISTENT(V(I,J,:))
      DO J = 1, N
      DO I = 1, N
      DO K = 1, N
            V(I,J,K) = B(I,J) + (N - 1) * (N - 1) * K
      ENDDO
      ENDDO
      ENDDO
!DVM$ PARALLEL (J,I) ON A(I,J), private(K), CONSISTENT(W(I,J,:))
      DO J = 1, N
      DO I = 1, N
      DO K = 1, N
            W(I,J,K) = A(I,J) + (N - 1) * (N - 1) * K
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
!DVM$  PARALLEL  (J,I)   ON  B(I,J), REDUCTION(MIN(ERROR))
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons2212
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS2212
      INTEGER,PARAMETER:: N=16, ER=100000
      integer,allocatable:: B(:,:),V(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N),V(N,N,N),C(N,N,N))
      tname='CONS2212'
      DO K = 1, N
      DO J = 1, N
      DO I = 1, N
            C(I,J,K)  = I + (N - 1) * J + (N - 1) * (N - 1) * K
      ENDDO
      ENDDO
      ENDDO
!dvm$ region
!DVM$   PARALLEL    (J,I)   ON   B(I,J)
      DO J = 1, N
      DO I = 1, N
            B(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J), private(K), CONSISTENT(V(I,J,:))
      DO J = 1, N
      DO I = 1, N
      DO K = 1, N
            V(I,J,K) = B(I,J) + (N - 1) * (N - 1) * K
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
!DVM$  PARALLEL  (J,I)   ON  B(I,J), REDUCTION(MIN(ERROR))
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons2213
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
      subroutine CONS2213
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:),V(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N),V(N,N,N,N),C(N,N,N,N))
      tname='CONS2213'
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
!DVM$   PARALLEL    (J,I)   ON   B(I,J)
      DO J = 1, N
      DO I = 1, N
            B(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J), private(K,L), CONSISTENT(V(I,J,:,:))
      DO J = 1, N
      DO I = 1, N
      DO L = 1, N
      DO K = 1, N
            V(I,J,K,L)=B(I,J)+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
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
!DVM$  PARALLEL  (J,I)   ON  B(I,J), REDUCTION(MIN(ERROR))
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, C)
      END
C ---------------------------------------------cons2214
C consistent arrays with 4 dimensions
C two consistent arrays and one distributed array
      subroutine CONS2214
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:),V(:,:,:,:),W(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N),V(N,N,N,N),W(N,N,N,N),C(N,N,N,N))
      tname='CONS2214'
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
!DVM$   PARALLEL    (J,I)   ON   B(I,J)
      DO J = 1, N
      DO I = 1, N
            B(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J), private(K,L), CONSISTENT(V(I,J,:,:))
      DO J = 1, N
      DO I = 1, N
      DO L = 1, N
      DO K = 1, N
            V(I,J,K,L)=B(I,J)+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J), private(K,L), CONSISTENT(W(I,J,:,:))
      DO J = 1, N
      DO I = 1, N
      DO L = 1, N
      DO K = 1, N
            W(I,J,K,L)=B(I,J)+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
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
!DVM$  PARALLEL  (J,I)   ON  B(I,J), REDUCTION(MIN(ERROR))
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, V, W, C)
      END
C ---------------------------------------------cons2215
C consistent arrays with 4 dimensions
      subroutine CONS2215
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:),A(:,:),V(:,:,:,:),W(:,:,:,:)
     *,C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N),A(N,N),V(N,N,N,N),W(N,N,N,N),C(N,N,N,N))
      tname='CONS2215'
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
!DVM$   PARALLEL    (J,I)   ON   B(I,J)
      DO J = 1, N
      DO I = 1, N
            B(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL    (J,I)   ON   A(I,J)
      DO J = 1, N
      DO I = 1, N
            A(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J), private(K,L), CONSISTENT(V(I,J,:,:))
      DO J = 1, N
      DO I = 1, N
      DO L = 1, N
      DO K = 1, N
            V(I,J,K,L)=B(I,J)+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
      ENDDO
      ENDDO
      ENDDO
      ENDDO
!DVM$ PARALLEL (J,I) ON A(I,J), private(K,L), CONSISTENT(W(I,J,:,:))
      DO J = 1, N
      DO I = 1, N
      DO L = 1, N
      DO K = 1, N
            W(I,J,K,L)=A(I,J)+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
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
!DVM$  PARALLEL  (J,I)   ON  B(I,J), REDUCTION(MIN(ERROR))
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
      ENDDO
      ENDDO
      IF(ERROR .EQ. ER) THEN
            call ansyes(tname)
      ELSE
            call ansno(tname)
      ENDIF
      deallocate (B, A, V, W, C)
      END
C ---------------------------------------------cons2216
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS2216
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:),V(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N),V(N,N,N,N),C(N,N,N,N))
      tname='CONS2216'
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
!DVM$   PARALLEL    (J,I)   ON   B(I,J)
      DO J = 1, N
      DO I = 1, N
            B(I,J) = I+(N-1)*J
      ENDDO
      ENDDO
!DVM$   PARALLEL (J,I) ON B(I,J), private(K,L), CONSISTENT(V(I,J,:,:))
      DO J = 1, N
      DO I = 1, N
      DO L = 1, N
      DO K = 1, N
            V(I,J,K,L)=B(I,J)+(N-1)*(N-1)*K+(N-1)*(N-1)*(N-1)*L
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
!DVM$  PARALLEL  (J,I)   ON  B(I,J), REDUCTION(MIN(ERROR))
      DO J = 1, N
      DO I = 1, N
            ERROR = MIN(ERROR,IERR)
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
