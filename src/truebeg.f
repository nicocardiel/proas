C------------------------------------------------------------------------------
C                                                               File: truebeg.f
C------------------------------------------------------------------------------
Comment
C
C INTEGER FUNCTION TRUEBEG(CADENA)
C
C Input: CADENA
C Output: TRUEBEG (function)
C
C Return the position of the first non-blank character in CADENA (ignoring
C also control characters with ASCII value < 32)
C
C CHARACTER*(*) CADENA -> input character string
C
Comment
C------------------------------------------------------------------------------
        INTEGER FUNCTION TRUEBEG(CADENA)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
C
        INTEGER I,L
C------------------------------------------------------------------------------
        L=LEN(CADENA)
C
        DO I=1,L
          IF(ICHAR(CADENA(I:I)).GT.32)THEN
            TRUEBEG=I
            RETURN
          END IF
        END DO
        TRUEBEG=0
        END
