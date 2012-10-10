C------------------------------------------------------------------------------
C                                                               File: chlower.f
C $Id: chlower.f 558 2007-10-10 16:10:49Z spr $
C------------------------------------------------------------------------------
Comment
C
C SUBROUTINE CHLOWER(CADENA)
C
C Input: CADENA
C Output: CADENA
C
C Upper case characters in CADENA are transformed to lower case
C
C CHARACTER*(*) CADENA -> character string to be transformed
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE CHLOWER(CADENA)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
C
        INTEGER I,N
C------------------------------------------------------------------------------
        DO I=1,LEN(CADENA)
          N=ICHAR(CADENA(I:I))
          IF((N.GE.65).AND.(N.LE.90)) CADENA(I:I)=CHAR(N+32)
        END DO
        END
