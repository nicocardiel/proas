C------------------------------------------------------------------------------
C                                                               File: rmblank.f
C------------------------------------------------------------------------------
Comment
C
C SUBROUTINE RMBLANK(C1,C2,L)
C
C Input: C1
C Output: C2,L
C
C Remove blanks in character string C1, returning C2 with a true length L
C
C CHARACTER*(*) C1 -> input character string
C CHARACTER*(*) C2 -> output character string (C1 without blanks)
C INTEGER       L -> true len of C2
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE RMBLANK(C1,C2,L)
        IMPLICIT NONE
        INTEGER L
        CHARACTER*(*) C1,C2
C
        INTEGER I,K
C------------------------------------------------------------------------------
        K=0
        DO I=1,LEN(C1)
          IF(C1(I:I).NE.CHAR(32))THEN
            K=K+1
            C2(K:K)=C1(I:I)
          END IF
        END DO
        L=K
        END
