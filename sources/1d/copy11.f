      PROGRAM COPY11
      PARAMETER (ERR=100, L=10, ITMAX=1)
      INTEGER A(L),X(L), C(L),B(L),D(L)
      INTEGER ERRI,i
CDVM$ DISTRIBUTE (BLOCK) :: A
CDVM$ ALIGN C(I) WITH A(I)
CDVM$ ALIGN D(I) WITH A(I)

CDVM$ ASYNCID GR
      CHARACTER*7 tname
      PRINT *,  '======== START OF COPY11  ========'
      tname='COPY11'
      ERRI= ERR
      do I=1,L
        X(I)=I
      enddo
      B(1)=X(1)*ITMAX
      do I=2,L
        B(I)=B(I-1)+X(I)
        B(I)=ITMAX*B(I)
      enddo

CDVM$ ASYNCHRONOUS GR
      D(:)=B(:)
CDVM$ END ASYNCHRONOUS

CDVM$ PARALLEL  ( I)   ON  A( I)
      DO I =  1,  L
        A( I)  =  I
      ENDDO
        
      C(1)=A(1)
                           
      DO  IT = 1,  ITMAX
CDVM$   PARALLEL (I) ON A( I), ACROSS(C(1:1))
        DO I =  2, L
          C(I) = C(I-1) + A(I)
        ENDDO
      ENDDO

CDVM$ ASYNCWAIT GR

CDVM$ PARALLEL (I) ON C(I), reduction (min(ERRI))
      do i=1,L
        if (D(i) .ne. C(i)) then     
          ERRI = min (I,ERRI)
        endif
      enddo
     
      if (ERRI .eq.ERR) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      print *,'=== END OF COPY11 ======================'
      end

      subroutine ansyes(name)
      character*7 name
      print *,name,'  -  complete'
      end
      subroutine ansno(name)
      character*7 name
      print *,name,'  -  ***error'
      end
