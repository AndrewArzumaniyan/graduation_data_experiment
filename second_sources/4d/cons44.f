      program CONS44

c      TESTING OF THE CONSISTENT CLAUSE'.
c      CHECKING ( BLOCK, BLOCK, BLOCK, BLOCK ) DISTRIBUTION.

      print *,'===START OF CONS44========================'
C --------------------------------------------------
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
      call cons4401
C --------------------------------------------------
C consistent arrays with 1 dimensions
C two consistent arrays and one distributed array
      call cons4402
C --------------------------------------------------
C consistent arrays with 1 dimensions
C two consistent arrays and two distributed arrays
      call cons4403
C --------------------------------------------------
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
C big data
      call cons4404
C --------------------------------------------------
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
      call cons4405
C --------------------------------------------------
C consistent arrays with 2 dimensions
C two consistent arrays and one distributed array
      call cons4406
C --------------------------------------------------
C consistent arrays with 2 dimensions
C two consistent arrays and two distributed arrays
      call cons4407
C --------------------------------------------------
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
C big data
      call cons4408
C --------------------------------------------------
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
      call cons4409
C --------------------------------------------------
C consistent arrays with 3 dimensions
C two consistent arrays and one distributed array
      call cons4410
C --------------------------------------------------
C consistent arrays with 3 dimensions
C two consistent arrays and two distributed arrays
      call cons4411
C --------------------------------------------------
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
C big data
      call cons4412
C --------------------------------------------------
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
      call cons4413
C --------------------------------------------------
C consistent arrays with 4 dimensions
C two consistent arrays and one distributed array
      call cons4414
C --------------------------------------------------
C consistent arrays with 4 dimensions
C two consistent arrays and two distributed arrays
      call cons4415
C --------------------------------------------------
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
C big data
      call cons4416
C --------------------------------------------------

C
      print *,'=== END OF CONS44 ========================= '
      end
C ---------------------------------------------cons4401
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
      subroutine CONS4401
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N),C(N))
      tname='CONS4401'
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
C ---------------------------------------------cons4402
C consistent arrays with 1 dimensions
C two consistent arrays and one distributed array
      subroutine CONS4402
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:),W(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N),W(N),C(N))
      tname='CONS4402'
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
C ---------------------------------------------cons4403
C consistent arrays with 1 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS4403
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:),W(:)
     *,C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N),W(N),C(N))
      tname='CONS4403'
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
C ---------------------------------------------cons4404
C consistent arrays with 1 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS4404
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:),C(:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N),C(N))
      tname='CONS4404'
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
C ---------------------------------------------cons4405
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
      subroutine CONS4405
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N),C(N,N))
      tname='CONS4405'
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
C ---------------------------------------------cons4406
C consistent arrays with 2 dimensions
C two consistent arrays and one distributed array
      subroutine CONS4406
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:),W(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N,N),W(N,N),C(N,N))
      tname='CONS4406'
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
C ---------------------------------------------cons4407
C consistent arrays with 2 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS4407
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:,:),W(:,:)
     *,C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N,N),W(N,N),C(N,N))
      tname='CONS4407'
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
C ---------------------------------------------cons4408
C consistent arrays with 2 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS4408
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:,:),C(:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N),C(N,N))
      tname='CONS4408'
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
C ---------------------------------------------cons4409
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
      subroutine CONS4409
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N),C(N,N,N))
      tname='CONS4409'
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
C ---------------------------------------------cons4410
C consistent arrays with 3 dimensions
C two consistent arrays and one distributed array
      subroutine CONS4410
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:),W(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N,N,N),W(N,N,N),C(N,N,N))
      tname='CONS4410'
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
C ---------------------------------------------cons4411
C consistent arrays with 3 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS4411
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:,:,:),W(:,:,:)
     *,C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N,N,N),W(N,N,N),C(N,N,N))
      tname='CONS4411'
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
C ---------------------------------------------cons4412
C consistent arrays with 3 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS4412
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:,:,:),C(:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N),C(N,N,N))
      tname='CONS4412'
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
C ---------------------------------------------cons4413
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
      subroutine CONS4413
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N,N),C(N,N,N,N))
      tname='CONS4413'
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
C ---------------------------------------------cons4414
C consistent arrays with 4 dimensions
C two consistent arrays and one distributed array
      subroutine CONS4414
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable:: B(:,:,:,:),V(:,:,:,:),W(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),V(N,N,N,N),W(N,N,N,N),C(N,N,N,N))
      tname='CONS4414'
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
C ---------------------------------------------cons4415
C consistent arrays with 4 dimensions
C two consistent arrays and two distributed arrays
      subroutine CONS4415
      INTEGER,PARAMETER:: N=8, ER=10000
      integer,allocatable::B(:,:,:,:),A(:,:,:,:),V(:,:,:,:),W(:,:,:,:)
     *,C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   A
!DVM$   CONSISTENT V
!DVM$   CONSISTENT W
      allocate (B(N,N,N,N),A(N,N,N,N),V(N,N,N,N),W(N,N,N,N),C(N,N,N,N))
      tname='CONS4415'
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
C ---------------------------------------------cons4416
C consistent arrays with 4 dimensions
C one consistent array and one distributed array
C big data
      subroutine CONS4416
      INTEGER,PARAMETER:: N=16, ER=1000000
      integer,allocatable:: B(:,:,:,:),V(:,:,:,:),C(:,:,:,:)
      character*8 tname
      INTEGER ERROR,IERR
!DVM$   DISTRIBUTE     ( BLOCK, BLOCK, BLOCK, BLOCK )   ::   B
!DVM$   CONSISTENT V
      allocate (B(N,N,N,N),V(N,N,N,N),C(N,N,N,N))
      tname='CONS4416'
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
